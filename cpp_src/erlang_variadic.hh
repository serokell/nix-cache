#if __cplusplus >= 201703L

#pragma once
#include <memory>
#include <nifpp.h>
#include <functional>
#include <type_traits>

namespace nifpp_variadic {
  template <typename... Types>
  struct typelist { static const int n = sizeof...(Types); };
  template <typename F>
  struct N : N<decltype(std::function{std::declval<F>()})> {};

  template <typename R, typename... A>
  struct N<std::function<R(A...)>>
  { using args = typelist<A...>; };


  template<typename F, typename ...Filled> inline
  ERL_NIF_TERM recursive(ErlNifEnv *env, const ERL_NIF_TERM argv[], F fun, typelist<> t, Filled&&... filled) {
    return nifpp::make(env, fun(std::forward<Filled>(filled)...));
  }
  template<typename F, typename T0, typename ...Empty, typename ...Filled> inline
  ERL_NIF_TERM recursive(ErlNifEnv *env, const ERL_NIF_TERM argv[], F fun, typelist<T0, Empty...> t, Filled&&... filled) {
    typename std::remove_reference<T0>::type a0;
    nifpp::get_throws(env, argv[0], a0);
    return recursive(env, &(argv[1]), fun, typelist<Empty...>(), filled..., a0);
  }
  template<typename F> inline
  ERL_NIF_TERM helper(ErlNifEnv *env, const ERL_NIF_TERM argv[], F fun) {
    if (env == NULL && argv == NULL) return N<F>::args::n;
    try {
      return template_fuckery::recursive(env, argv, fun, typename N<F>::args{});
    }
    catch (nifpp::badarg) {
      return enif_make_badarg(env);
    }
    catch (nix::InvalidPath) {
      return enif_raise_exception(env, nifpp::make(env, nifpp::str_atom("nix_invalid_path")));
    }
    catch (nix::Error &e) {
      return enif_raise_exception(env, nifpp::make(env, (std::string)e.what()));
    }
  }
}; // namespace nifpp_variadic

#define DEFUNC(name, dirty, ...)                                         \
  static ERL_NIF_TERM _##name(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) \
  { return template_fuckery::helper(env, argv, __VA_ARGS__); }
#define DEFUN(name, ...) DEFUNC(name, 0, __VA_ARGS__)

#endif

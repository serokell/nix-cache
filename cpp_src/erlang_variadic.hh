#ifndef ERLANG_VARIADIC
#define ERLANG_VARIADIC
#include <memory>
#include <nifpp.h>
#include <functional>
#include <type_traits>

namespace template_fuckery {
  template <typename... Types>
  struct typelist { static const int n = sizeof...(Types); };
  template <typename F>
  struct N : N<decltype(std::function{std::declval<F>()})> {};

  template <typename R, typename... A>
  struct N<std::function<R(A...)>>
  { using args = typelist<A...>; };


  template<typename F, typename ...Filled> inline
  ERL_NIF_TERM recursive(ErlNifEnv *env, const ERL_NIF_TERM argv[], F fun, typelist<> t, Filled&&... filled) {
    return invoke_with_catch(env, fun, filled...);
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
    return template_fuckery::recursive(env, argv, fun, typename N<F>::args{});
  }
}; // namespace template_fuckery


#define DEFUN(name, ...) \
  static ERL_NIF_TERM _##name(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) \
  { return template_fuckery::helper(env, argv, __VA_ARGS__); }

#endif

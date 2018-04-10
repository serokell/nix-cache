#ifndef FUNC_ONLY
#include <nix/hash.hh>
#include <nix/local-store.hh>
#include <nix/nar-info.hh>
#include <nix/store-api.hh>

#include <nifpp.h>
#include "./erlang_variadic.hh"

namespace nix_store_nif {
using namespace nix;

typedef ref<const ValidPathInfo> ValidPathRef;

static std::shared_ptr<Store> store_ptr;

static ref<Store> store() {
  if (!store_ptr) {
    settings.loadConfFile();
    settings.lockCPU = false;
    store_ptr = nix::openStore();
  }

  return ref<Store>(store_ptr);
}

static ref<LocalFSStore> ensure_local_store() {
  auto store_ref = store().dynamic_pointer_cast<LocalFSStore>();
  if (!store_ref)
    throw Error("you don't have sufficient rights to use this command");
  return ref<LocalFSStore>(store_ref);
}


static int on_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM info) {
  nifpp::register_resource<ValidPathRef>(env, nullptr, "ValidPathInfo");
  nix_store_nif::store();
  // todo error handling
  return 0;
}

static void on_unload(ErlNifEnv *env, void *priv_data) {
  store_ptr.reset();
}

static int on_upgrade(ErlNifEnv *env, void **priv, void **old_priv_data,
                      ERL_NIF_TERM load_info) {
  // Never could get this to work, doesn't update
  return on_load(env, priv, load_info);
}

  template<typename F, typename ...Params> inline
  ERL_NIF_TERM invoke_with_catch(ErlNifEnv *env, F fun, Params&&... params) {
    try {
      return nifpp::make(env, fun(std::forward<Params>(params)...));
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
#endif

  DEFUN(get_real_store_dir, []() {
      return nix_store_nif::ensure_local_store()->getRealStoreDir();
    })

  // TODO: allow passing compression, url, fileHash, fileSize
  DEFUN(path_info_narinfo, [](ValidPathRef *pathref) {
      // auto narInfo = pathref->dynamic_pointer_cast<const nix::NarInfo>();
      // if (narInfo) { // when can we do this?
      //  return MAKE(narInfo->to_string());
      //} else {
      nix::NarInfo narInfo(**pathref);
      narInfo.compression = "none";
      narInfo.fileHash = narInfo.narHash;
      narInfo.fileSize = narInfo.narSize;
      narInfo.url = nix::storePathToHash(narInfo.path) + ".nar";

      return narInfo.to_string();
    })

  DEFUN(path_info_narsize, [](ValidPathRef *pathref) {
      return (*pathref)->narSize;
    })

  DEFUN(query_path_from_hash_part, [](nix::Path &path) {
      return nix_store_nif::store()->queryPathFromHashPart(path);
    })

  DEFUN(query_path_info, [](nix::Path &path) {
      // TODO: async
      // caveat: caches wrong (prefix) lookups
      auto pathinfo = nix_store_nif::store()->queryPathInfo(path);
      return nifpp::construct_resource<decltype(pathinfo)>(pathinfo);
    })

  DEFUN(sign, [](ValidPathRef *pathref, std::string &key) {
      // doesn't save, don't need root
      decltype(*pathref) vpi(*pathref);
      nix::SecretKey secretKey(key);
      std::string signature = secretKey.signDetached(vpi->fingerprint());
      if (vpi->sigs.count(signature))
        return nifpp::construct_resource<ValidPathRef>(vpi);
      //return NULL; //nifpp::str_atom("duplicate");
      // copy as non-const
      ref<ValidPathInfo> info2 = nix::make_ref<ValidPathInfo>(*vpi);
      info2->sigs.insert(signature);
      
      return nifpp::construct_resource<ValidPathRef>(info2);
    })


  #ifndef FUNC_ONLY
static ErlNifFunc nif_funcs[] = {
  // you may have been wondering what FUNC_ONLY was about
  #define FUNC_ONLY 1
  #undef DEFUN
  #define DEFUN(name, ...)                                       \
      { #name, (unsigned int)(_##name(NULL, 0, NULL)), _##name},

  #include "nix_store_nif.cpp"
};

ERL_NIF_INIT(nix_store_nif, nif_funcs, on_load, NULL, on_upgrade, on_unload);
}; // namespace nix_store_nif

#endif


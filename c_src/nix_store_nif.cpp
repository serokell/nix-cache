#include <nix/store-api.hh>
#include <nix/local-store.hh>
#include <nix/nar-info.hh>
#include <nix/hash.hh>
#include <memory>
#include <nifpp.h>

namespace nixnif {
  
  using namespace nix;
  static std::shared_ptr<Store> _store;
  static ref<Store> store() {
    if (!_store) {
      // try {
      settings.loadConfFile();
      settings.lockCPU = false;
      _store = nix::openStore();
      //} catch (Error & e) {
      // croak("%s", e.what());
      //}
    }
    return ref<Store>(_store);
  }
  static ref<LocalFSStore> ensureLocalStore() {
    auto store2 = store().dynamic_pointer_cast<LocalFSStore>();
    if (!store2) throw Error("you don't have sufficient rights to use this command");
    return ref<LocalFSStore>(store2);
  }
};


extern "C" {
  using nix::ValidPathInfo;
  using nix::ref;
  
#define NMK(x) nifpp::make(env, x)
#define NARG(type, n) nifpp::get<type>(env, argv[n])
#define DEFUN(name) static ERL_NIF_TERM do_##name (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
#define CATCH_BADARG catch (nifpp::badarg) { return enif_make_badarg(env); }

  static int on_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info) {
    nifpp::register_resource<ref<const ValidPathInfo>>(env, nullptr, "ValidPathInfo");
    nixnif::store();
    return 0;
  }

  static void on_unload(ErlNifEnv* env, void* priv_data)
  {
    nixnif::_store.~shared_ptr();
  }

  static int on_upgrade(ErlNifEnv* env, void** priv, void** old_priv_data, ERL_NIF_TERM load_info)
  {
    return 1; // never could get this to work, just crashes
    return on_load(env, priv, load_info);
  }
  DEFUN(queryPathFromHashPart) {
    try {
      return NMK(nixnif::store()->queryPathFromHashPart(NARG(nix::Path, 0)));
    } CATCH_BADARG;
  }

  DEFUN(sign) {
    // doesn't save, don't need root
    try {
      ref<const ValidPathInfo> *pathref;
      nifpp::get(env, argv[0], pathref);
      decltype(*pathref) vpi(*pathref);

      nix::SecretKey secretKey(NARG(std::string, 1));
      std::string signature = secretKey.signDetached(vpi->fingerprint());
      if (vpi->sigs.count(signature)) return NMK(nifpp::str_atom("duplicate"));
      // copy as non-const
      ref<ValidPathInfo> info2 = nix::make_ref<ValidPathInfo>(*vpi);
      info2->sigs.insert(signature);
      
      return NMK(nifpp::construct_resource<ref<const ValidPathInfo>>(info2));
    } CATCH_BADARG
      catch (nix::Error & e) { return enif_raise_exception(env, NMK((std::string)e.what())); }
  }

  DEFUN(getUri) {
    return NMK(nixnif::store()->getUri());
  };

  DEFUN(getStorePath) {
    return NMK(nixnif::ensureLocalStore()->getRealStoreDir());
  };

  DEFUN(isValidPath) {
    // todo: crashes on really invalid path
    try {
      return NMK(nixnif::store()->isValidPath(NARG(nix::Path, 0)));
    } CATCH_BADARG;
  }

  DEFUN(pathInfo) {
    // todo: async
    // caveat: caches wrong (prefix) lookups
    try {
      auto pathinfo = nixnif::store()->queryPathInfo(NARG(nix::Path, 0));
      return NMK(nifpp::construct_resource<decltype(pathinfo)>(pathinfo));
    } CATCH_BADARG
    catch (nix::InvalidPath) { return enif_raise_exception(env, NMK(nifpp::str_atom("nix_invalid_path"))); }
  }

  DEFUN(pathInfo2map) {
    try {
      ref<const ValidPathInfo> *pathref;
      nifpp::get(env, argv[0], pathref);
      decltype(*pathref) vpi(*pathref);

      std::map<nifpp::str_atom, nifpp::TERM> result;

      result["path"] = NMK(vpi->path);
      result["deriver"] = NMK(vpi->deriver);
      result["ca"] = NMK(vpi->ca);
      result["narHash"] = NMK(vpi->narHash.to_string());
      result["references"] = NMK(vpi->references);
      result["narSize"] = NMK(vpi->narSize);
      result["sigs"] = NMK(vpi->sigs);
      result["registrationTime"] = NMK(vpi->registrationTime);
      
      
      auto narInfo = pathref->dynamic_pointer_cast<const nix::NarInfo>();
      if (narInfo) { // didn't test this, might crash ;)
        result["system"] = NMK(narInfo->system);
        result["compression"] = NMK(narInfo->compression);
        result["fileHash"] = NMK(narInfo->fileHash.to_string());
        result["fileSize"] = NMK(narInfo->fileSize);
      }
      
      return NMK(result);
    } CATCH_BADARG
  }

  DEFUN(pathInfo2NarInfo) {
    // todo: allow passing compression, url, fileHash, fileSize
    try {
      ref<const ValidPathInfo> *pathref;
      nifpp::get(env, argv[0], pathref);
      decltype(*pathref) vpi(*pathref);
      // auto narInfo = pathref->dynamic_pointer_cast<const nix::NarInfo>();
      //if (narInfo) { // when can we do this?
      //  return NMK(narInfo->to_string());
      //} else {
      nix::NarInfo narInfo(*vpi);
      narInfo.compression = "none";
      narInfo.fileHash = narInfo.narHash;
      narInfo.fileSize = narInfo.narSize;
      narInfo.url = nix::storePathToHash(narInfo.path) + ".nar";
      
      return NMK(narInfo.to_string());
        //}
    } CATCH_BADARG
  }
  // TODO: dump to port
static ErlNifFunc nif_funcs[] = {
  {"queryPathFromHashPart", 1, do_queryPathFromHashPart},
  {"sign", 2, do_sign},
  {"getUri", 0, do_getUri},
  {"getStorePath", 0, do_getStorePath},
  {"pathInfo2NarInfo", 1, do_pathInfo2NarInfo},
  {"pathInfo2map", 1, do_pathInfo2map},
  {"isValidPath", 1, do_isValidPath},
  {"pathInfo", 1, do_pathInfo},
};

ERL_NIF_INIT(nix_store_nif, nif_funcs, on_load, NULL, on_upgrade, on_unload);

};

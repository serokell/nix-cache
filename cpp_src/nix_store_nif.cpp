#include <memory>
#include <nifpp.h>

#include <nix/hash.hh>
#include <nix/local-store.hh>
#include <nix/nar-info.hh>
#include <nix/store-api.hh>

namespace nix_store_nif {
using namespace nix;

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
}; // namespace nix_store_nif

extern "C" {
using nix::ValidPathInfo;
using nix::ref;

#define ARGN(type, n) nifpp::get<type>(env, argv[n])

#define CATCH_BADARG                                                           \
  catch (nifpp::badarg) {                                                      \
    return enif_make_badarg(env);                                              \
  }

#define DEFUN(name)                                                            \
  static ERL_NIF_TERM _##name(ErlNifEnv *env, int argc,                      \
			      const ERL_NIF_TERM argv[])

#define MAKE(x) nifpp::make(env, x)

static int on_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM info) {
  nifpp::register_resource<ref<const ValidPathInfo>>(env, nullptr,
                                                     "ValidPathInfo");
  nix_store_nif::store();
  return 0;
}

static void on_unload(ErlNifEnv *env, void *priv_data) {
  nix_store_nif::store_ptr.~shared_ptr();
}

static int on_upgrade(ErlNifEnv *env, void **priv, void **old_priv_data,
                      ERL_NIF_TERM load_info) {
  // Never could get this to work, just crashes:
  // return on_load(env, priv, load_info);

  return 1;
}

DEFUN(getRealStoreDir) {
  return MAKE(nix_store_nif::ensure_local_store()->getRealStoreDir());
};

DEFUN(pathInfoToMap) {
  try {
    ref<const ValidPathInfo> *pathref;
    nifpp::get(env, argv[0], pathref);
    decltype(*pathref) vpi(*pathref);

    std::map<nifpp::str_atom, nifpp::TERM> result;

    result["path"] = MAKE(vpi->path);
    result["deriver"] = MAKE(vpi->deriver);
    result["ca"] = MAKE(vpi->ca);
    result["narHash"] = MAKE(vpi->narHash.to_string());
    result["references"] = MAKE(vpi->references);
    result["narSize"] = MAKE(vpi->narSize);
    result["sigs"] = MAKE(vpi->sigs);
    result["registrationTime"] = MAKE(vpi->registrationTime);

    auto narInfo = pathref->dynamic_pointer_cast<const nix::NarInfo>();
    if (narInfo) { // didn't test this, might crash ;)
      result["system"] = MAKE(narInfo->system);
      result["compression"] = MAKE(narInfo->compression);
      result["fileHash"] = MAKE(narInfo->fileHash.to_string());
      result["fileSize"] = MAKE(narInfo->fileSize);
    }

    return MAKE(result);
  }
  CATCH_BADARG
}

// TODO: allow passing compression, url, fileHash, fileSize
DEFUN(pathInfoNarInfo) {
  try {
    ref<const ValidPathInfo> *pathref;
    nifpp::get(env, argv[0], pathref);
    decltype(*pathref) vpi(*pathref);
    // auto narInfo = pathref->dynamic_pointer_cast<const nix::NarInfo>();
    // if (narInfo) { // when can we do this?
    //  return MAKE(narInfo->to_string());
    //} else {
    nix::NarInfo narInfo(*vpi);
    narInfo.compression = "none";
    narInfo.fileHash = narInfo.narHash;
    narInfo.fileSize = narInfo.narSize;
    narInfo.url = nix::storePathToHash(narInfo.path) + ".nar";

    return MAKE(narInfo.to_string());
    //}
  }
  CATCH_BADARG
}

DEFUN(pathInfoNarSize) {
  try {
    ref<const ValidPathInfo> *pathref;
    nifpp::get(env, argv[0], pathref);
    decltype(*pathref) vpi(*pathref);

    return MAKE(vpi->narSize);
  }
  CATCH_BADARG
}

DEFUN(pathInfoPath) {
  try {
    ref<const ValidPathInfo> *pathref;
    nifpp::get(env, argv[0], pathref);
    decltype(*pathref) vpi(*pathref);

    return MAKE(vpi->path);
  }
  CATCH_BADARG
}

DEFUN(queryPathFromHashPart) {
  try {
    return MAKE(
        nix_store_nif::store()->queryPathFromHashPart(ARGN(nix::Path, 0)));
  }
  CATCH_BADARG;
}

DEFUN(queryPathInfo) {
  // TODO: async
  // caveat: caches wrong (prefix) lookups

  try {
    auto pathinfo = nix_store_nif::store()->queryPathInfo(ARGN(nix::Path, 0));
    return MAKE(nifpp::construct_resource<decltype(pathinfo)>(pathinfo));
  }
  CATCH_BADARG
  catch (nix::InvalidPath) {
    return enif_raise_exception(env, MAKE(nifpp::str_atom("nix_invalid_path")));
  }
}

DEFUN(sign) {
  // doesn't save, don't need root
  try {
    ref<const ValidPathInfo> *pathref;
    nifpp::get(env, argv[0], pathref);
    decltype(*pathref) vpi(*pathref);

    nix::SecretKey secretKey(ARGN(std::string, 1));
    std::string signature = secretKey.signDetached(vpi->fingerprint());
    if (vpi->sigs.count(signature))
      return MAKE(nifpp::str_atom("duplicate"));
    // copy as non-const
    ref<ValidPathInfo> info2 = nix::make_ref<ValidPathInfo>(*vpi);
    info2->sigs.insert(signature);

    return MAKE(nifpp::construct_resource<ref<const ValidPathInfo>>(info2));
  }
  CATCH_BADARG
  catch (nix::Error &e) {
    return enif_raise_exception(env, MAKE((std::string)e.what()));
  }
}

// TODO: dump to port
static ErlNifFunc nif_funcs[] = {{"get_real_store_dir", 0, _getRealStoreDir},
				 {"path_info_narinfo", 1, _pathInfoNarInfo},
				 {"path_info_narsize", 1, _pathInfoNarSize},
				 {"path_info_path", 1, _pathInfoPath},
				 {"path_info_to_map", 1, _pathInfoToMap},
				 {"query_path_from_hash_part", 1, _queryPathFromHashPart},
				 {"query_path_info", 1, _queryPathInfo},
				 {"sign", 2, _sign}};

ERL_NIF_INIT(nix_store_nif, nif_funcs, on_load, NULL, on_upgrade, on_unload);
};

#ifndef FUNC_ONLY
#include <nix/hash.hh>
#include <nix/local-store.hh>
#include <nix/nar-info.hh>
#include <nix/store-api.hh>
#include <nix/serialise.hh>

#include <nifpp.h>
#include "./erlang_variadic.hh"

namespace nifpp {
  inline TERM copy(ErlNifEnv * const env, const TERM& orig) {
    return TERM(enif_make_copy(env, orig));
  }
  struct Env {
    ErlNifEnv * const e;
    const bool allocated = false;
    Env() : e(enif_alloc_env()), allocated(true) { };
    explicit Env(ErlNifEnv* e) : e(e), allocated(false) { };
    ~Env() { if (allocated) enif_free_env(e); }
    Env(const Env &) = delete;
    Env & operator=(const Env &) = delete;

    inline operator ErlNifEnv*() const {return e;}
    inline TERM copy(const TERM& orig) {
      return TERM(enif_make_copy(e, orig));
    }
  };
}

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


  using nifpp::str_atom;
  struct PortSink : nix::BufferedSink {
    nifpp::Env env, msgenv;
    ErlNifPid destination;
    nifpp::TERM ref;
    PortSink(const ErlNifPid& pid, const nifpp::TERM& ref) :
      BufferedSink(),
      ref(env.copy(ref)), destination(pid) {}
    virtual void write(const unsigned char * data, size_t len) override {
      nifpp::binary result(len);
      // memcpy can probably be avoided by using enif_alloc_binary for buffer
      memcpy(result.data, data, len);
      this->send(str_atom("data"), result);
    }
    template <typename ...Ts>
    inline void send(Ts&&... ts) {
      enif_send(env, &destination, msgenv,
                nifpp::make(msgenv, std::forward_as_tuple(str_atom("nix_store"), msgenv.copy(ref), ts...)));
      enif_clear_env(msgenv);
    }
    virtual ~PortSink() override {
      assert(bufPos == 0);
    }
  };
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
      // caveat: caches wrong (prefix) lookups. use clearPathInfoCache to fix
      store()->assertStorePath(path);
      auto pathinfo = store()->queryPathInfo(path);
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
      auto info2 = nix::make_ref<ValidPathInfo>(*vpi);
      info2->sigs.insert(signature);
      
      return nifpp::construct_resource<ValidPathRef>(info2);
    })

  DEFUNC(path_nar, ERL_NIF_DIRTY_JOB_IO_BOUND, [](nix::Path &path, ErlNifPid& pid, nifpp::TERM& ref) {
      auto sink = PortSink(pid, ref);
      try {
        store()->narFromPath(path, sink);
        sink.flush();
        sink.send(str_atom("end"));
      } catch (nix::Error & e) {
        sink.flush();
        sink.send(str_atom("nix_error"), e.what());
      }
      return str_atom("ok");
    })


  #ifndef FUNC_ONLY
       
static ErlNifFunc nif_funcs[] = {
  // you may have been wondering what FUNC_ONLY was about
  #define FUNC_ONLY 1
  #undef DEFUNC
  #define DEFUNC(name, dirty, ...)                                   \
      { #name, (unsigned int)(_##name(NULL, 0, NULL)), _##name, dirty},

  #include "nix_store_nif.cpp"
};

ERL_NIF_INIT(nix_store_nif, nif_funcs, on_load, NULL, on_upgrade, on_unload);
}; // namespace nix_store_nif

#endif


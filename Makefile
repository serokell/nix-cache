default: priv/nix_store_nif.so

CPPFLAGS += -fPIC
LDFLAGS += -shared -lerl_interface -lnixstore

priv/%.so: cpp_src/%.cpp priv
	$(CXX) $< $(CPPFLAGS) $(LDFLAGS) -o $@

priv:
	mkdir -p $@
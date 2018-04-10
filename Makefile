default: priv/nix_store_nif.so
clean: ; rm -r priv/nix_store_nif.so
.PHONY: default clean
CPPFLAGS += -fPIC --std=c++17
LDFLAGS += -shared -lerl_interface -lnixstore

priv/%.so: cpp_src/%.cpp cpp_src/*.hh priv
	$(CXX) $< $(CPPFLAGS) $(LDFLAGS) -o $@

priv:
	mkdir -p $@

CWD := $(shell pwd)
ROOT := $(abspath $(CWD)/..)

#ERTS_INCLUDE_DIR := $(shell erl -noshell -s init stop -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)]).")
#ERL_INTERFACE_INCLUDE_DIR := $(shell erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, include)]).")
#ERL_INTERFACE_LIB_DIR := $(shell erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, lib)]).")

CPPFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR)

LDFLAGS += -shared
LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei -lnixstore -lnixutil

C_SRC_DIR = $(CWD)
C_SRC_OUT ?= $(ROOT)/priv/nix_store_nif.so

SOURCES := $(shell find $(C_SRC_DIR) -type f -name "*.cpp")
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

$(C_SRC_OUT): $(OBJECTS)
	mkdir -p $(ROOT)/priv/
	$(CXX) $(OBJECTS) $(LDFLAGS) $(LDLIBS) -o $(C_SRC_OUT)


clean: ; rm -r $(C_SRC_OUT) $(OBJECTS)
.PHONY: clean

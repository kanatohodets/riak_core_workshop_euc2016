BASEDIR = $(shell pwd)
REBAR = rebar3
RELPATH = _build/default/rel/simple_hash
PRODRELPATH = _build/prod/rel/simple_hash
APPNAME = simple_hash
SHELL = /bin/bash

release:
	$(REBAR) release
	mkdir -p $(RELPATH)/../simple_hash_config
	[ -f $(RELPATH)/../simple_hash_config/simple_hash.conf ] || cp $(RELPATH)/etc/simple_hash.conf  $(RELPATH)/../simple_hash_config/simple_hash.conf
	[ -f $(RELPATH)/../simple_hash_config/advanced.config ] || cp $(RELPATH)/etc/advanced.config  $(RELPATH)/../simple_hash_config/advanced.config

console:
	cd $(RELPATH) && ./bin/simple_hash console

prod-release:
	$(REBAR) as prod release
	mkdir -p $(PRODRELPATH)/../simple_hash_config
	[ -f $(PRODRELPATH)/../simple_hash_config/simple_hash.conf ] || cp $(PRODRELPATH)/etc/simple_hash.conf  $(PRODRELPATH)/../simple_hash_config/simple_hash.conf
	[ -f $(PRODRELPATH)/../simple_hash_config/advanced.config ] || cp $(PRODRELPATH)/etc/advanced.config  $(PRODRELPATH)/../simple_hash_config/advanced.config

prod-console:
	cd $(PRODRELPATH) && ./bin/simple_hash console

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) ct

devrel1:
	$(REBAR) as dev1 release

devrel2:
	$(REBAR) as dev2 release

devrel3:
	$(REBAR) as dev3 release

devrel: devrel1 devrel2 devrel3

dev1-console:
	$(BASEDIR)/_build/dev1/rel/simple_hash/bin/$(APPNAME) console

dev2-console:
	$(BASEDIR)/_build/dev2/rel/simple_hash/bin/$(APPNAME) console

dev3-console:
	$(BASEDIR)/_build/dev3/rel/simple_hash/bin/$(APPNAME) console

devrel-start:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/simple_hash/bin/$(APPNAME) start; done

devrel-join:
	for d in $(BASEDIR)/_build/dev{2,3}; do $$d/rel/simple_hash/bin/$(APPNAME)-admin cluster join simple_hash1@127.0.0.1; done

devrel-cluster-plan:
	$(BASEDIR)/_build/dev1/rel/simple_hash/bin/$(APPNAME)-admin cluster plan

devrel-cluster-commit:
	$(BASEDIR)/_build/dev1/rel/simple_hash/bin/$(APPNAME)-admin cluster commit

devrel-status:
	$(BASEDIR)/_build/dev1/rel/simple_hash/bin/$(APPNAME)-admin member-status

devrel-ping:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/simple_hash/bin/$(APPNAME) ping; done

devrel-stop:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/simple_hash/bin/$(APPNAME) stop; done

start:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) start

stop:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) stop

attach:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) attach


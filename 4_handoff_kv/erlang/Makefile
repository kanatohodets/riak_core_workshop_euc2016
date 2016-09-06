BASEDIR = $(shell pwd)
REBAR = rebar3
RELPATH = _build/default/rel/handoff_kv
PRODRELPATH = _build/prod/rel/handoff_kv
APPNAME = handoff_kv
SHELL = /bin/bash

release:
	$(REBAR) release
	mkdir -p $(RELPATH)/../handoff_kv_config
	[ -f $(RELPATH)/../handoff_kv_config/handoff_kv.conf ] || cp $(RELPATH)/etc/handoff_kv.conf  $(RELPATH)/../handoff_kv_config/handoff_kv.conf
	[ -f $(RELPATH)/../handoff_kv_config/advanced.config ] || cp $(RELPATH)/etc/advanced.config  $(RELPATH)/../handoff_kv_config/advanced.config

console:
	cd $(RELPATH) && ./bin/handoff_kv console

prod-release:
	$(REBAR) as prod release
	mkdir -p $(PRODRELPATH)/../handoff_kv_config
	[ -f $(PRODRELPATH)/../handoff_kv_config/handoff_kv.conf ] || cp $(PRODRELPATH)/etc/handoff_kv.conf  $(PRODRELPATH)/../handoff_kv_config/handoff_kv.conf
	[ -f $(PRODRELPATH)/../handoff_kv_config/advanced.config ] || cp $(PRODRELPATH)/etc/advanced.config  $(PRODRELPATH)/../handoff_kv_config/advanced.config

prod-console:
	cd $(PRODRELPATH) && ./bin/handoff_kv console

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
	$(BASEDIR)/_build/dev1/rel/handoff_kv/bin/$(APPNAME) console

dev2-console:
	$(BASEDIR)/_build/dev2/rel/handoff_kv/bin/$(APPNAME) console

dev3-console:
	$(BASEDIR)/_build/dev3/rel/handoff_kv/bin/$(APPNAME) console

devrel-start:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/handoff_kv/bin/$(APPNAME) start; done

devrel-join:
	for d in $(BASEDIR)/_build/dev{2,3}; do $$d/rel/handoff_kv/bin/$(APPNAME)-admin cluster join handoff_kv1@127.0.0.1; done

devrel-cluster-plan:
	$(BASEDIR)/_build/dev1/rel/handoff_kv/bin/$(APPNAME)-admin cluster plan

devrel-cluster-commit:
	$(BASEDIR)/_build/dev1/rel/handoff_kv/bin/$(APPNAME)-admin cluster commit

devrel-status:
	$(BASEDIR)/_build/dev1/rel/handoff_kv/bin/$(APPNAME)-admin member-status

devrel-ping:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/handoff_kv/bin/$(APPNAME) ping; done

devrel-stop:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/handoff_kv/bin/$(APPNAME) stop; done

start:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) start

stop:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) stop

attach:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) attach


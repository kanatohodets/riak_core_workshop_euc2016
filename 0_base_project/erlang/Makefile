BASEDIR = $(shell pwd)
REBAR = rebar3
RELPATH = _build/default/rel/yak_db
PRODRELPATH = _build/prod/rel/yak_db
APPNAME = yak_db
SHELL = /bin/bash

release:
	$(REBAR) release
	mkdir -p $(RELPATH)/../yak_db_config
	[ -f $(RELPATH)/../yak_db_config/yak_db.conf ] || cp $(RELPATH)/etc/yak_db.conf  $(RELPATH)/../yak_db_config/yak_db.conf
	[ -f $(RELPATH)/../yak_db_config/advanced.config ] || cp $(RELPATH)/etc/advanced.config  $(RELPATH)/../yak_db_config/advanced.config

console:
	cd $(RELPATH) && ./bin/yak_db console

prod-release:
	$(REBAR) as prod release
	mkdir -p $(PRODRELPATH)/../yak_db_config
	[ -f $(PRODRELPATH)/../yak_db_config/yak_db.conf ] || cp $(PRODRELPATH)/etc/yak_db.conf  $(PRODRELPATH)/../yak_db_config/yak_db.conf
	[ -f $(PRODRELPATH)/../yak_db_config/advanced.config ] || cp $(PRODRELPATH)/etc/advanced.config  $(PRODRELPATH)/../yak_db_config/advanced.config

prod-console:
	cd $(PRODRELPATH) && ./bin/yak_db console

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
	$(BASEDIR)/_build/dev1/rel/yak_db/bin/$(APPNAME) console

dev2-console:
	$(BASEDIR)/_build/dev2/rel/yak_db/bin/$(APPNAME) console

dev3-console:
	$(BASEDIR)/_build/dev3/rel/yak_db/bin/$(APPNAME) console

devrel-start:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/yak_db/bin/$(APPNAME) start; done

devrel-join:
	for d in $(BASEDIR)/_build/dev{2,3}; do $$d/rel/yak_db/bin/$(APPNAME)-admin cluster join yak_db1@127.0.0.1; done

devrel-cluster-plan:
	$(BASEDIR)/_build/dev1/rel/yak_db/bin/$(APPNAME)-admin cluster plan

devrel-cluster-commit:
	$(BASEDIR)/_build/dev1/rel/yak_db/bin/$(APPNAME)-admin cluster commit

devrel-status:
	$(BASEDIR)/_build/dev1/rel/yak_db/bin/$(APPNAME)-admin member-status

devrel-ping:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/yak_db/bin/$(APPNAME) ping; done

devrel-stop:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/yak_db/bin/$(APPNAME) stop; done

start:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) start

stop:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) stop

attach:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) attach


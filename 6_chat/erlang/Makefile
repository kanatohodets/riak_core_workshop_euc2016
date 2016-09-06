BASEDIR = $(shell pwd)
REBAR = rebar3
RELPATH = _build/default/rel/zap_chat
PRODRELPATH = _build/prod/rel/zap_chat
APPNAME = zap_chat
SHELL = /bin/bash

release:
	$(REBAR) release
	mkdir -p $(RELPATH)/../zap_chat_config
	[ -f $(RELPATH)/../zap_chat_config/zap_chat.conf ] || cp $(RELPATH)/etc/zap_chat.conf  $(RELPATH)/../zap_chat_config/zap_chat.conf
	[ -f $(RELPATH)/../zap_chat_config/advanced.config ] || cp $(RELPATH)/etc/advanced.config  $(RELPATH)/../zap_chat_config/advanced.config

console:
	cd $(RELPATH) && ./bin/zap_chat console

prod-release:
	$(REBAR) as prod release
	mkdir -p $(PRODRELPATH)/../zap_chat_config
	[ -f $(PRODRELPATH)/../zap_chat_config/zap_chat.conf ] || cp $(PRODRELPATH)/etc/zap_chat.conf  $(PRODRELPATH)/../zap_chat_config/zap_chat.conf
	[ -f $(PRODRELPATH)/../zap_chat_config/advanced.config ] || cp $(PRODRELPATH)/etc/advanced.config  $(PRODRELPATH)/../zap_chat_config/advanced.config

prod-console:
	cd $(PRODRELPATH) && ./bin/zap_chat console

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
	$(BASEDIR)/_build/dev1/rel/zap_chat/bin/$(APPNAME) console

dev2-console:
	$(BASEDIR)/_build/dev2/rel/zap_chat/bin/$(APPNAME) console

dev3-console:
	$(BASEDIR)/_build/dev3/rel/zap_chat/bin/$(APPNAME) console

devrel-start:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/zap_chat/bin/$(APPNAME) start; done

devrel-join:
	for d in $(BASEDIR)/_build/dev{2,3}; do $$d/rel/zap_chat/bin/$(APPNAME)-admin cluster join zap_chat1@127.0.0.1; done

devrel-cluster-plan:
	$(BASEDIR)/_build/dev1/rel/zap_chat/bin/$(APPNAME)-admin cluster plan

devrel-cluster-commit:
	$(BASEDIR)/_build/dev1/rel/zap_chat/bin/$(APPNAME)-admin cluster commit

devrel-status:
	$(BASEDIR)/_build/dev1/rel/zap_chat/bin/$(APPNAME)-admin member-status

devrel-ping:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/zap_chat/bin/$(APPNAME) ping; done

devrel-stop:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/zap_chat/bin/$(APPNAME) stop; done

start:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) start

stop:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) stop

attach:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) attach


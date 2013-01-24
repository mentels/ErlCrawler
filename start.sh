#!/bin/bash
VM_NAME="erlcrawler@`hostname -I | awk -F' ' '{print$1}'`"
DEPS="deps/bson/ebin/  deps/mongodb/ebin/  deps/lager/ebin/  apps/crawler_persistence_app/ebin  deps/ibrowse/ebin/  deps/mochiweb/ebin/  apps/crawler_app/ebin/"
erl -pa ${DEPS} -name ${VM_NAME}

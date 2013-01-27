#!/bin/sh
VM_ADD=`hostname -I | awk -F' ' '{print$1}'`
echo ${VM_ADD}
cp -R utils ../ChicagoBoss/deps/
(cd ../ChicagoBoss/deps/utils/src; erlc -o ../ebin/ utils.erl)
cp ../ChicagoBoss/deps/utils/ebin/*.beam ../ChicagoBoss/ebin
./rebar clean compile
erl -- -home ~ -- -pa ../ChicagoBoss/ebin -pa ../ChicagoBoss/deps/aleppo/ebin ../ChicagoBoss/deps/boss_db/ebin ../ChicagoBoss/deps/bson/ebin ../ChicagoBoss/deps/cowboy/ebin ../ChicagoBoss/deps/dynamic_compile/ebin ../ChicagoBoss/deps/epgsql/ebin ../ChicagoBoss/deps/erlmc/ebin ../ChicagoBoss/deps/erlydtl/ebin ../ChicagoBoss/deps/gen_server2/ebin ../ChicagoBoss/deps/gen_smtp/ebin ../ChicagoBoss/deps/jaderl/ebin ../ChicagoBoss/deps/meck/ebin ../ChicagoBoss/deps/medici/ebin ../ChicagoBoss/deps/mimetypes/ebin ../ChicagoBoss/deps/misultin/ebin ../ChicagoBoss/deps/mochicow/ebin ../ChicagoBoss/deps/mochiweb/ebin ../ChicagoBoss/deps/mongodb/ebin ../ChicagoBoss/deps/mysql/ebin ../ChicagoBoss/deps/poolboy/ebin ../ChicagoBoss/deps/protobuffs/ebin ../ChicagoBoss/deps/riak_pb/ebin ../ChicagoBoss/deps/riakc/ebin ../ChicagoBoss/deps/simple_bridge/ebin ../ChicagoBoss/deps/tiny_pq/ebin ../ChicagoBoss/deps/tinymq/ebin ../ChicagoBoss/deps/uuid/ebin -pa ../crawler/ebin -pa ../crawler/deps/*/ebin -boss developing_app crawler -boot start_sasl -config boss -setcookie NUJPVNQEPEZJBTISDVFY -s reloader -s boss -name gui@${VM_ADD}

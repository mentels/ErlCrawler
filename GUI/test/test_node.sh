#!/usr/bin/env sh
erlc data_api.erl
NODE_NAME="T`hostname`@`hostname -I | awk -F' ' '{print$1}'`"
erl -pa . -setcookie NUJPVNQEPEZJBTISDVFY -name ${NODE_NAME}

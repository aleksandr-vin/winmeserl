#!/bin/sh
cd `dirname $0`
SAFE_PWD=`echo $PWD | sed 's#/cygdrive/\(.\)/#\1:/#'`
NODE_NAME=winmeserl-dev
cmd="erl -pa $SAFE_PWD/ebin $SAFE_PWD/deps/*/ebin -sname $NODE_NAME -boot start_sasl -config start-dev -s lager -s winmeserl"
echo $cmd
exec $cmd

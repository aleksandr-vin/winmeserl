#!/bin/sh
cd `dirname $0`
SAFE_PWD=`echo $PWD | sed 's#/cygdrive/\(.\)/#\1:/#'`
NODE_NAME=regama-dev
cmd="erl -pa $SAFE_PWD/ebin $SAFE_PWD/deps/*/ebin -sname $NODE_NAME -boot start_sasl -config start-dev -s lager -s ibrowse -s ssl -s reloader -s regama"
echo $cmd
exec $cmd

#!/bin/sh
cd `dirname $0`
SAFE_PWD=`echo $PWD | sed 's#/cygdrive/\(.\)/#\1:/#'`
NODE_NAME=winmeserl-dev
cmd="erl -pa $SAFE_PWD/ebin $SAFE_PWD/deps/*/ebin $SAFE_PWD/examples/*/ebin -sname $NODE_NAME -boot start_sasl -config start-dev -s lager -s winmeserl -s winmeserl_wm_devicechange_example -s sync go"
echo $cmd
exec $cmd

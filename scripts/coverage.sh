# 0. Clear old counters ;)
# We use -f as that way rm doesnt' return a non 0 error code
# 
set -e
find . -name "*.gcda" | xargs rm -f
rm *.info -f

export PATH=$LCOV_ROOT/usr/bin:$PATH
echo $PATH
# 1. create baseline coverage data file
lcov --no-external -c -i -d . -o baseline.info

#2. perform test
find bin -name "*-tests" -exec gdb --silent \
                                  --batch \
                                  --return-child-result \
                                  -ex=run \
                                  -ex="bt full" \
                                  --args {} \;
# export SERVER=`find . -name "websocket_echo"`
# nohup gdb -ex=run -ex="bt full" --args $SERVER&
# # We need to wait a while so wstest can connect!
# sleep 5
# cd scripts && wstest -m fuzzingclient
# cd ..
# cat nohup.out
# jobs
# kill -INT %1
# 3. create test coverage data file
lcov --no-external -c -d . -o tests.info

# 4. combine baseline and test coverage data
# codecov.io
lcov -a baseline.info -a tests.info -o lcov.info
~/.local/bin/codecov -X gcov

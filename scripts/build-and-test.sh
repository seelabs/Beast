#!/bin/bash -u
# We use set -e and bash with -u to bail on first non zero exit code of any
# processes launched or upon any unbound variable
set -e

echo "using toolset: $CC"
echo "using variant: $VARIANT"
echo "using address-model: $ADDRESS_MODEL"


# if [[ $ADDRESS_MODEL == "64" ]]; then
#   architecture="ia64"
# else
#   architecture="x86"
# fi

$BOOST_ROOT/bjam toolset=$CC \
                 variant=$VARIANT \
                 address-model=$ADDRESS_MODEL
                 # architecture=$architecture

if [[ $VARIANT == "coverage" ]]; then
  bash scripts/coverage.sh
else
  find bin -name "*-tests" -exec gdb --silent \
                                     --batch \
                                     --return-child-result \
                                     -ex=run \
                                     -ex="bt full" \
                                     --args {} \;
fi

#!/bin/sh
mkdir -p static
ALL_JS=`cabal-plan list-bins | awk '{ print $2 ".jsexe/all.js" }'`
RTS_JS=`cabal-plan list-bins | awk '{ print $2 ".jsexe/rts.js" }'`
LIB_JS=`cabal-plan list-bins | awk '{ print $2 ".jsexe/lib.js" }'`
OUT_JS=`cabal-plan list-bins | awk '{ print $2 ".jsexe/out.js" }'`
RUNMAIN_JS=`cabal-plan list-bins | awk '{ print $2 ".jsexe/runmain.js" }'`

ln -f -s "$ALL_JS" static/all.js
ln -f -s "$RTS_JS" static/rts.js
ln -f -s "$LIB_JS" static/lib.js
ln -f -s "$OUT_JS" static/out.js
ln -f -s "$RUNMAIN_JS" static/runmain.js

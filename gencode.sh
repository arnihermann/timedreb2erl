#!/bin/sh

cd src
bnfc -haskell -p Language.Rebeca rebeca.cf

sed -i '' -e'1 i\
{-# LANGUAGE DeriveDataTypeable #-}
' Language/Rebeca/Absrebeca.hs

sed -i '' -e'4 i\
import Data.Generics
' Language/Rebeca/Absrebeca.hs

sed -i '' -e's/deriving (Eq,Ord,Show)/deriving (Eq,Ord,Show,Data,Typeable)/g' Language/Rebeca/Absrebeca.hs


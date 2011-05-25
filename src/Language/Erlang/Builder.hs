module Language.Erlang.Builder where

import Language.Erlang.Syntax

atom = AtomicLiteral
num = NumberLiteral

atomE = ExpVal . AtomicLiteral
listE = ExpL
moduleE = ModExp
numberE = ExpVal . NumberLiteral
stringE = ExpVal . StringLiteral
tupleE = ExpT
varE = ExpVar

atomP = PatVal . AtomicLiteral
listP = PatL
tupleP = PatT
varP = PatVar


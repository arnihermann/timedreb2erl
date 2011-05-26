{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Language.Fold where


class Fold f t r | f t -> r where
    fold :: f -> t -> r


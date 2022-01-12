{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphan where

import           Control.Applicative   (Const (Const, getConst))
import           Data.Binary           as Binary (Binary (..))
import           Data.Constraint       (Dict (..))
import           Data.Extensible
import           Data.Functor.Identity (Identity (..))
import qualified Data.HashMap.Strict   as HM
import           Data.Monoid           (Endo (Endo, appEndo))
import           Text.Mustache         (ToMustache (..))
import           Text.Mustache.Types   (Value (..))

deriving instance ToMustache (h (TargetOf kv)) => ToMustache (Field h kv)

deriving instance ToMustache a => ToMustache (Identity a)

instance Forall (KeyTargetAre KnownSymbol (Instance1 ToMustache h)) xs => ToMustache (xs :& Field h) where
  toMustache = Object . hfoldlWithIndexFor
    (Proxy @ (KeyTargetAre KnownSymbol (Instance1 ToMustache h)))
    (\k m v -> HM.insert (stringKeyOf k) (toMustache v) m)
    HM.empty

deriving instance Binary (h (TargetOf kv)) => Binary (Field h kv)

instance Forall (KeyTargetAre KnownSymbol (Instance1 Binary h)) xs => Binary (xs :& Field h) where
    get = hgenerateFor
      (Proxy @ (KeyTargetAre KnownSymbol (Instance1 Binary h)))
      (const Binary.get)

    put = flip appEndo (return ()) . hfoldMap getConst .
      hzipWith
        (\(Comp Dict) x -> Const $ Endo (Binary.put x >>))
        (library :: xs :& Comp Dict (KeyTargetAre KnownSymbol (Instance1 Binary h)))

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cat.NaturalTransform where

infixr 0 *->

-- | @f ~> g@ is a natural transformation from @f@ to @g@. A natrual transformation @alpha@ need to obey this law:
--
-- [Naturality condition] @alpha . 'fmap' = 'fmap' . alpha@
type (*->) f g = forall x. f x -> g x

{--# RULES
-- "naturality conition" alpha . fmap = fmap . alpha
  #-}

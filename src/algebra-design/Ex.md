## ops

```haskell
beside :: Tile -> Tile -> Tile
cw :: Tile -> Tile
ccw :: Tile -> Tile
flipH :: Tile -> Tile
flipV :: Tile -> Tile
beside :: Tile -> Tile -> Tile
above :: Tile -> Tile -> Tile
quad :: Tile -> Tile -> Tile -> Tile -> Tile
swirl : Tile -> Tile
behind :: Tile -> Tile -> Tile

color :: Double -> Double -> Double -> Tile

empty = color r g b 0

rasterize :: Int -> Int -> Tile -> [[Color]]
```

## laws

```haskell
cw . cw . cw = ccw
cw . cw . cw . cw = id
ccw . cw = id
cw . ccw = id

flipH . flipH = id
flipH . cw = ccw . flipH
flipV . flipV = id
flipV = ccw . flipH . cw
flipV . flipH = cw . cw

flipH (beside t1 t2) = beside (flipH t1) (flipH t2)
above (beside a b) (beside c d) = beside (above a c) (above b d)
quad a b c d = above (beside a b) (beside c d)

cw . color = color
flipH . color = color

behind t (color r g b 1) = color r g b 1
behind t (color r g b 0) = t

∀ (t1 :: Tile) (t2 :: Tile).
    (∀ (w :: Int) (h :: Int).
        rasterize w h t1 == rasterize w h t2) => t1 = t2

rasterize w h (flipV t) = reverse (rasterize w h t)
rasterize w h (flipH t) = fmap reverse (rasterize w h t)
```

```
flipH . cw ^ {2*n} . flipH = cw ^ {2*n}
flipH . cw . cw . cw ^ {2*(n - 1)} . flipH
= flipH . flipH . cw .cw . flipH . cw ^ {2*(n - 1)} . flipH
(flipH . flipH == id) = cw . cw . flipH . cw ^ {2*(n-1)} . flipH
....
= cw ^ {2 * n} . flipH ^ {2 * n}
(flipH . flipH == id) = cw ^ {2*n}
```


```
Exercise Prove flipH (flipH (beside t1 t2)) = beside t1 t2 in two separate ways

flipH (flipH (beside t1 t2))
= flipH (beside (flipH t1) (flipH t2))
= beside (flipH (flipH t1)) (flipH(flipH t2))
```

```
Exercise Recreate figure 2.15, using beside, cw and ccw.

ccw (beside (ccw sandy) (ccw haskell))
```

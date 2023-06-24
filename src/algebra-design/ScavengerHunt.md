# Ex1

## Excercise

Attempt a quick design of this system before continuing. Sketch the core data types and functions necessary for a scavenger hunt system like described above. How would you encode a challenge like “take a selfie at the library, and receive one point afterward.”

### Requirements

- Player run around their city, solving clues, exploring new places and doing challenges
- Track player progresstion
  - Collect periodic location data
  - By geotagged photos sent by player
    - Ex: send a selfie at library to prove sloving a hint
    - there is no telling what the product people might have up their sleeves for what sort of data we can use in the future, so building an extensible system is a plus.
- Reward user for completing challenges
  - Might just be point in the game
  - Or st tangible
- Allow non technical persion to create challenge


### Draft

#### Usecase
- Conditional challenge: finish a before doing b
- Multi-quests: collect both a and b to finish the challenge
- Require either a or b
#### Core data

```haskell
data Challenge b a

data Progress

instance Functor Challenge


just :: a -> Challenge a
after :: Challenge (a -> b)
both :: Challenge a -> Challenge b -> Challenge (a , b)
either :: Challenge a -> Challenge b -> Challenge (Either a b) 

-- Laws

b `after` a == a `before` b
both a b == both b a

either a b == either b a

∀ (poi :: Point) (p :: Point) (pic :: Photo)
    (d :: Distance) (c :: Challenge) (is :: [Input]).
  within poi p d =>
    getRewards (photoWithin poi d c) (photo p pic : is)
      = getRewards c is

data InputFilter

matches :: InputFilter -> Input -> Bool

matches (photoWithin p d) (photo poi) = within point p d
matches (photoAbove h) (photoAbove att) = att > h


matches f i => getRewards (gate f c) (i : is) = getRewards c is

Challenge  
  = empty
  | reward c
  | clue c Challenge
  | both Challenge Challenge
  | andThen Challenge Challenge
  | gate InputFilter Challenge

completes empty _ = true
completes reward c _ = true_
matches f i => completes (gate f c) (i : is) = completes c is
not (matches f i) => completes (gate f c) (i : is) = completes (gate f c) is
completes (clue cl c) is = completes c is
completes (both c1 c2)  = ?
completes (andThen c1 c2) is = ?


```



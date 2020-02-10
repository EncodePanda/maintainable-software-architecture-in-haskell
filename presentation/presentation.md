build-lists: true
footer: © Pawel Szulc, @EncodePanda, paul.szulc@gmail.com
slidenumbers: true

# [fit] Maintainable Software Architeture

# in Haskell (with Polysemy)

---

**maintain** *[ meyn-teyn ]*

*verb (used with object)*
1. to keep in existence
2. to keep in an appropriate condition, operation, or force; keep unimpaired:
3. to keep in a specified state, position, etc.

---

> "~~Socialism~~ Haskell is a ~~system~~ language which heroically overcomes difficulties unknown in any other ~~system~~ language"

---

![inline](img/haskell_pyramid.png)

---

# Emphasis on **what**, less focus on **why?**

---

# Our plan for today

1. Coding Dojo / Hack day
2. Real world example
  - problem
  - approach
  - consequences

---

# [fit] Why writing code sucks (sometimes)?

---

# Coding Kata: Write a sorting algorithm

---

> "As a Billing System user I want to generate an invoice for a given account based on its current system use"

---

# Functions and their nature

1. Manipulate data (`f :: Input -> Output`)
2. Interact with an outside world

---

```haskell
doStuff :: Int -> Int
doStuff i = i + 1
```

# [fit] Why this function soooo good?

+ easy to test
+ you will be notified if its behavior changes

---

# It's easy to maintain function if it only manipulates data.

---

```haskell
-- | take an Int (i) and UUID (uuid) as parameters
-- | fetch existing Int under given uuid from MongoDB
-- | (if does not exist, default to zero)
-- | add them, store the result, return result as text
doStuff :: UUID -> Int -> IO String
doStuff uuid i = do
  maybeOld <- fetch uuid
  let
    oldI = maybe 0 id maybeOld
    newI = oldI + i
  persist uuid newI
  pure ("New value: " ++ (show newI))
```

---

# It's easy to test and maintain function if it only manipulates data.

# Can we change "interactions with the outside world" into data?

---

```haskell
-- | take an Int (i) and UUID (uuid) as parameters
-- | fetch existing Int under given uuid from MongoDB
-- | (if does not exist, default to zero)
-- | add them, store the result, return result as text
doStuff :: UUID -> Int -> IO String
doStuff uuid i = do
  maybeOld <- fetch uuid
  let
    oldI = maybe 0 id maybeOld
    newI = oldI + i
  persist uuid newI
  pure ("New value: " ++ (show newI))
```

---

```haskell
-- | take Int, return +1 as text
doStuff :: Int -> String
doStuff i = "New value: " ++ (show $ i + 1)
```

---

```haskell
prop_returns_plus1 :: Property
prop_returns_plus1 = property do
  -- given
  i <- Gen.int
  -- when
  let res = doStuff i
  -- then
  res === "New value: " ++ (show $ i + 1)
```

---

```haskell
module Main where

main :: IO ()
main = putStrLn $ doStuff 10
```

---

[.code-highlight: 1-2]
```haskell
-- | take Int, store it, return +1 as text
doStuff :: UUID -> Int -> (Storage, String)
doStuff uuid i =
  ( Persist uuid newI
  , "New value: " ++ (show newI)
  )
  where
    newI = i + 1
```
---

![inline](img/01doStuff01.png)

---

![inline](img/01doStuff02.png)

---

![inline](img/01doStuff03.png)

---

![inline](img/01doStuff04.png)

---

![inline](img/01doStuff05.png)

---

![inline](img/01doStuff06.png)

---

![inline](img/01doStuff07.png)

---

![inline](img/01doStuff08.png)

---

![inline](img/01doStuff09.png)

---

![inline](img/01doStuff10.png)

---

```haskell
data Storage = Persist UUID Int
```

```haskell
-- | take Int, store it, return +1 as text
doStuff :: UUID -> Int -> (Storage, String)
doStuff uuid i =
  ( Persist uuid newI
  , "New value: " ++ (show newI)
  )
  where
    newI = i + 1
```

---

```haskell
prop_returns_plus1 :: Property
prop_returns_plus1 = property $ do
  -- given
  i    <- Gen.int
  uuid <- genUUID
  -- when
  let (Persist puuid pi, res) = doStuff uuid i
  -- then
  puuid === uuid
  pi    === i + 1
  res   === "New value: " ++ (show $ i + 1)
```

---

![inline](img/01doStuff05.png)

---

![inline](img/01doStuff10.png)

---

```haskell
doStuff   :: UUID -> Int -> (Storage, String)
interpret ::                (Storage, String) -> IO String
```

---

```haskell
type InMemStorage = M.Map UUID Int

interpret ::
     IORef InMemStorage
  -> (Storage, String)
  -> IO String
interpret ioRef (Persist uuid pi, i) = do
  modifyIORef ioRef (M.insert uuid pi)
  return i
```

---

```haskell
main :: IO ()
main = do
  ioRef <- newIORef M.empty
  uuid  <- nextRandom
  res   <- interpret ioRef (doStuff uuid 10)
  putStrLn res
```

---

```haskell
-- | take Int, store it, return +1 as text
doStuff :: UUID -> Int -> (Storage, String)
doStuff uuid i =
  ( Persist uuid newI
  , "New value: " ++ (show newI)
  )
  where
    newI = i + 1
```

---

```haskell
-- | take Int, store it once, story it twice, return +1 as text
doStuff :: UUID -> Int -> (Storage, String)
doStuff uuid i =
  ( Persist uuid newI
  , "New value: " ++ (show newI)
  )
  where
    newI = i + 1
```

---

```haskell
-- | take Int, store it once, story it twice, return +1 as text
doStuff :: UUID -> Int -> ([Storage], String)
doStuff uuid i =
  ( [(Persist uuid newI)]
  , "New value: " ++ (show newI)
  )
  where
    newI = i + 1
```

---

![inline](img/01doStuff05.png)

---

![inline](img/02doStuff01.png)

---

![inline](img/02doStuff02.png)

---

![inline](img/02doStuff03.png)

---

```haskell
type InMemStorage = M.Map UUID Int

interpret ::
     IORef InMemStorage
  -> (Storage, String)
  -> IO String
interpret ioRef (Persist uuid pi, i) = do
  modifyIORef ioRef (M.insert uuid pi)
  return i
```

---

```haskell
type InMemStorage = M.Map UUID Int

interpret ::
     IORef InMemStorage
  -> ([Storage], String)
  -> IO String
interpret ioRef (actions, i) = do
  traverse perform actions
  return i
  where
    perform (Persist uuid pi) =
	  modifyIORef ioRef (M.insert uuid pi)
```

---

```haskell
prop_returns_plus1 :: Property
prop_returns_plus1 = property $ do
  -- given
  i    <- Gen.int
  uuid <- genUUID
  -- when
  let (Persist puuid pi, res) = doStuff uuid i
  -- then
  puuid === uuid
  pi    === i + 1
  res   === "New value: " ++ (show $ i + 1)
```

---

```haskell
prop_returns_plus1 :: Property
prop_returns_plus1 = property $ do
  -- given
  i    <- Gen.int
  uuid <- genUUID
  -- when
  let ([Persist puuid pi], res) = doStuff uuid i
  -- then
  puuid === uuid
  pi    === i + 1
  res   === "New value: " ++ (show $ i + 1)
```

---

```haskell
main :: IO ()
main = do
  ioRef <- newIORef M.empty
  uuid  <- nextRandom
  res   <- interpret ioRef (doStuff uuid 10)
  putStrLn res
```

---

# Resources

+ [Urban Expansion](https://www.youtube.com/watch?v=AqUSo2hstHI)
+ [Haskell Pyramid](https://patrickmn.com/software/the-haskell-pyramid/)

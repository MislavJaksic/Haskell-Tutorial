## Haskell Tutorial

Install [Haskell Stack](https://docs.haskellstack.org/en/stable/README/).  
```
$: stack new Haskell-Tutorial
$: cd Haskell-Tutorial
$: stack setup
$: stack build
```

```
$: stack ghci
```

### app

For executables.  

```
$: stack exec Haskell-Tutorial-exe
```

### src

For libraries.  

### test

```
$: stack test --fast

$: stack test :Haskell-Tutorial-test-fer-course --fast
```

### package.yaml

Add "dependencies" here.  

```
$: stack ls dependencies
```

Add targets under "executables" or "tests".  
```
$: stack ide targets
```

### Setup.hs

### stack.yaml



### [hspec](https://hspec.github.io/)

A testing framework which works well with QuickCheck, SmallCheck and HUnit.  

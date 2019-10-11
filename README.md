## Haskell Project Template

Install [Haskell Stack](https://docs.haskellstack.org/en/stable/README/).  
```
$: stack new Haskell-Project-Template
$: cd Haskell-Project-Template
$: stack setup
$: stack build
```

```
$: stack ghci
```

### app

For executables.  

```
$: stack exec Haskell-Project-Template-exe
```

### src

For libraries.  

### test

```
$: stack test --fast

$: stack test :Haskell-Project-Template-test-fer-course --fast
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

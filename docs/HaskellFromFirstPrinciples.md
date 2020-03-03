## Haskell Programming From First Principles

### Chapter 1: All You Need is Lambda

Combine expressions.  
Expressions are values, variables and functions.  
Functions are applied to an argument/input and reduced/evaluated.  
Functions are first-class.  
Haskell is referentially transparent pure language which means a function will evaluate the same way for the same input.  

Lambda terms: expressions, variables and abstractions/functions.  
Lx.x -> lx is the head, .x is the body
Alpha equivalence means different variable names don't change the function.  
Lx.x == Ly.y  
Beta reduction applies the head to the body of the function.  
(Lx.x) 2 ---beta reduction---> 2
Lambda calculus is left associative.  
(Lx.x)(Ly.y)z -> (Ly.y)z -> z
Free variables can't be reduced.  
(Lx.xy)z -> zy
Multiple argument notation is a shorthand for nested functions.  
Lxy.xy == Lx.(Ly.xy)

STOPPED AT PAGE 34... I don't get it. Revise Lambda calculus!

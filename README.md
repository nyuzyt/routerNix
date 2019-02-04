# routerNix

### Some notes for haskell
* =>
```
(==) :: (Eq a) => a -> a -> Bool

=>符号: 它左边的部分叫做类型约束。我们可以这样阅读这段类型声明：

    “相等函数取两个相同类型的值作为参数并返回一个布尔值，而这两个参数的类型同在Eq类之中（即类型约束）”
```

* ::
```
removeNonUppercase :: [Char] -> [Char]   

::读作“它的类型为”。凡是明确的类型，其首字母必为大写。
```

* <>
```
ghci> [1,2,3] <> [4,5,6]
[1,2,3,4,5,6]

It's an alias for mappend, from the Data.Monoid module.
```
* <*>
```
GHCi> (+) (Just 3) <*> Just 4
Just 7
```

* . and $
```
putStrLn (show (1 + 1))

putStrLn $ show $ 1 + 1

putStrLn . show $ 1 + 1

f ( g x ) = (f . g) x
```
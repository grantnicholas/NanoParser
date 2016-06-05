Nanoparser is a monadic parser combinator library written in scala

#examples
```
val arrayStream = "[1,1,1,1,1]".toList
val arrayParser = for{
	_ <- parseChar(openBrak)
	values   <- sepBy(parseWord("1"), comma)
	_ <- parseChar(closeBrak)
}yield(values)
val testArrayParser = arrayParser(arrayStream)
assert(testArrayParser == Success((List("1", "1", "1", "1", "1"), List())))
```
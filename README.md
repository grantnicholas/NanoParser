Nanoparser is a monadic parser combinator library written in scala

#Look in the examples folder for a json parser written using Nanoparser
#Here is a small excerpt from the parser
```

"""
We want to parse a json blob string into a List[String]
"""

val arrayStream = "["bobby", "sally", "sam", "george"]".toList

let parseNonSpecials = parsePredicate(ch => !Set('"', '\').contains(ch))

let quotedWord = for{
	_ <- parseChar('"')
	word <- parseNonSpecials().many0()
	_ <- parseChar('"')
}yield(word)

val arrayParser = for{
	_ <- parseChar('[')
	words   <- quotedWord.sepBy(',')
	_ <- parseChar(']')
}yield(words)

val testArrayParser = arrayParser(arrayStream)
assert(testArrayParser == Success((List("bobby", "sally", "sam", "george"), List())))
```
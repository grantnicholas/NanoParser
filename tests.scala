package tests

import nanoparser.parsing._
import nanoparser.ParseUtils._

object Main extends App{

	val tests = () => {
		println("the tests are running")

		val mapOutput = Success(("hello"), List('w', 'o', 'r')).map(hello => hello.drop(2))
		assert(mapOutput == Success(("llo",List('w', 'o', 'r'))))

		val testUnitParser = unit(List(1,2,3))
		val testUnitStream = "wooo".toList
		val testUnit = testUnitParser(testUnitStream)
		assert(testUnit == Success((List(1,2,3),testUnitStream)) )

		val testCharParser = parseChar('a')
		val testCharStream = "abcdef".toList
		val testChar = testCharParser(testCharStream)
		assert(testChar == Success('a', "bcdef".toList))

		val testWordParser = parseWord("hel")
		val testWordStream = "hello world".toList
		val testWord = testWordParser(testWordStream)
		assert(testWord == Success("hel", "lo world".toList))

		val testMaybeParserFail = maybe(parseWord("whoah"))
		val testMaybeParserPass = maybe(parseWord("hello "))
		val testMaybeStream = "hello world".toList
		val testMaybeFail = testMaybeParserFail(testMaybeStream)
		val testMaybePass = testMaybeParserPass(testMaybeStream)
		assert(testMaybeFail == Success((None, testMaybeStream)))
		assert(testMaybePass == Success((Some("hello "), "world".toList)))

		val combineParsers = parseChar('h').flatMap(h => 
		                     parseChar('o').flatMap(o => 
		                     unit((h,o))
		                     ))
		val testCombineStream = "howdy".toList
		val testCombine = combineParsers(testCombineStream)
		assert(testCombine == Success(('h','o'), "wdy".toList))

		val testMapParser = parseChar('h').flatMap(h =>
					        parseChar('o').flatMap(o =>
					        unit(List(h,o))
					        )).map(chars => chars.mkString)

		val testMapStream = "howdy".toList
		val testMap = testMapParser(testMapStream)
		assert(testMap == Success(("ho", "wdy".toList)))

		val testMapParser2 = for {
			h <- parseChar('h')
			o <- parseChar('o')
		}yield(List(h,o).mkString)
		val testMap2 = testMapParser2(testMapStream)
		assert(testMap2 == Success(("ho", "wdy".toList)))

		val testEitherParser = parseWord("howdy") <|> 
							   parseWord("hello")
		val testEitherStream = "hello world".toList
		val testEither = testEitherParser(testEitherStream)
		assert(testEither == Success(("hello"), List(' ', 'w', 'o', 'r', 'l', 'd')))

		val many0Parser = parseWord("badger").many0()
		val testMany0Stream = "badgerbadgerbadgermushroom".toList
		val testMany0 = many0Parser(testMany0Stream)
		assert(testMany0 == Success((List("badger", "badger", "badger"), "mushroom".toList)))

		val many1Parser = parseWord("badger").many1()
		val testMany1Stream = "mushroombadgerbadgerbadger".toList
		val testMany1 = many1Parser(testMany1Stream)
		assert(testMany1 == Failure("Did not match string str"))
		val testMany1Good = many1Parser(testMany0Stream)
		assert(testMany1Good == Success((List("badger", "badger", "badger"), "mushroom".toList)))

		val sepByParser = sepBy(parseWord("1"), comma)
		val sepByStream = "1,1,1,1,1".toList
		val testSepBy = sepByParser(sepByStream)
		assert(testSepBy == Success((List("1", "1", "1", "1", "1"), List())))
	
		val arrayStream = "[1,1,1,1,1]".toList
		val arrayParser = for{
			_ <- parseChar(openBrak)
			values   <- sepBy(parseWord("1"), comma)
			_ <- parseChar(closeBrak)
		}yield(values)
		val testArrayParser = arrayParser(arrayStream)
		assert(testArrayParser == Success((List("1", "1", "1", "1", "1"), List())))
	}

	tests()


}
//////////////////////////////////////////////////////////////////////////
//Implicitly unwrap and wrap the parser as needed
//We just use the parser wrapper for our convenience of chaining methods
import scala.language.implicitConversions
implicit def toParser[A](p: List[Char] => Result[A]):Parser[A] = {
	new Parser(p)
}
implicit def toFunc[A](p: Parser[A]):List[Char] => Result[A] = {
	p.p
}
//////////////////////////////////////////////////////////////////////////

sealed trait Result[+A]{ 
	def map[B](f: A => B) = this match{
		case Success((a, rest)) => Success((f(a), rest))
		case Failure(m) => Failure(m)
	}
}
case class Success[A](result: (A, List[Char])) extends Result[A]
case class Failure[A](message: String) extends Result[A]

case class Parser[A](p: List[Char] => Result[A]){
	def flatMap[B](f: A => Parser[B]):Parser[B] = ParseUtils.flatMap(new Parser(p), f)
	def map[B](f: A => B):Parser[B] = ParseUtils.map(new Parser(p), f)
	def <|>[B](that: Parser[B]) = ParseUtils.either(this, that)

	def many0() = ParseUtils.many0(p)
	def many1() = ParseUtils.many1(p)
}

object ParseUtils{
	def unit[A](item: A):Parser[A] = {
		(chars: List[Char]) => {
			Success(item, chars)
		}
	}

	def parseChar(ch: Char):Parser[Char] = {
		(chars: List[Char]) => chars.headOption match{
			case None    => Failure("No chars left to match")
			case Some(c) => if(c == ch){Success(c, chars.tail)}
							else       {Failure("char not found")}
		}
	}

	def parseWord(str: String):Parser[String] = {
		(chars: List[Char]) => chars.startsWith(str) match {
			case true  => Success((str, chars.drop(str.length)))
			case false => Failure("Did not match string str")
		}
	}

	def flatMap[A,B](p1: Parser[A], f: A => Parser[B]):Parser[B] = {
		(chars: List[Char]) => p1(chars) match{
			case Failure(m1) => Failure(m1)
			case Success((r1, rest1)) => f(r1)(rest1)
		}
	}

	def map[A,B](p: Parser[A], f: A => B):Parser[B] = {
		(chars: List[Char]) => p(chars).map(x => f(x))
	}

	def either[A,B](p1: Parser[A], p2: Parser[B]) = {
		(chars: List[Char]) => p1(chars) match{
			case Success(s1) => Success(s1)
			case Failure(m1) => p2(chars) match{
				case Success(s2) => Success(s2)
				case Failure(m2) => Failure(m1++"|"++m2)
			}
		}
	}

	def many0[A](p: Parser[A]): Parser[List[A]] = {
		(chars: List[Char]) => p(chars) match {
			case Failure(m1) => Success(List[A](), chars)
			case Success((r1, rest1)) => many0(p)(rest1) match {
				case Failure(m2) => Success(List(r1), rest1)
				case Success((r2, rest2)) => Success((List(r1)++r2, rest2))
			}
		}
	}

	def many1[A](p: Parser[A]): Parser[List[A]] = {
		(chars: List[Char]) => p(chars) match {
			case Failure(m1) => Failure(m1)
			case Success((r1, rest1)) => many0(p)(rest1) match {
				case Failure(m2) => Success(List(r1), rest1)
				case Success((r2, rest2)) => Success((List(r1)++r2, rest2))
			}
		}
	}
}

import ParseUtils._

val tests = () => {

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

	val testEitherParser = parseWord("howdy") <|> parseWord("hello")
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
}

tests()

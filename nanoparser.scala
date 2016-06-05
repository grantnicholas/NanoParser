package nanoparser

object parsing{
	//////////////////////////////////////////////////////////////////////////
	//Implicitly unwrap and wrap the parser as needed
	//We just use the parser wrapper for our convenience of chaining methods
	import scala.language.implicitConversions
	import ParseUtils._


	implicit def toParser[A](p: List[Char] => Result[A]):Parser[A] = {
		new Parser(p)
	}
	implicit def toFunc[A](p: Parser[A]):List[Char] => Result[A] = {
		p.p
	}
	//////////////////////////////////////////////////////////////////////////

	sealed trait Result[A]{ 
		def map[B](f: A => B): Result[B] = this match{
			case Success((a, rest)) => Success[B]((f(a), rest))
			case Failure(m) => Failure[B](m)
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

		def maybe() = ParseUtils.maybe(p)
	}
}

object ParseUtils{
	import parsing._

	val comma = ','
	val openBrak = '['
	val closeBrak = ']'

	def unit[A](item: A):Parser[A] = {
		(chars: List[Char]) => {
			Success(item, chars)
		}
	}

	def parseChar(ch: Char):Parser[Char] = {
		(chars: List[Char]) => chars.headOption match{
			case None    => Failure[Char]("No chars left to match")
			case Some(c) => if(c == ch){Success[Char](c, chars.tail)}
							else       {Failure[Char]("char not found")}
		}
	}

	def parseWord(str: String):Parser[String] = {
		(chars: List[Char]) => chars.startsWith(str) match {
			case true  => Success[String]((str, chars.drop(str.length)))
			case false => Failure[String]("Did not match string str")
		}
	}

	def maybe[A](p: Parser[A]): Parser[Option[A]] = {
		(chars: List[Char]) => p(chars) match{
			case Failure(m)        => Success[Option[A]]((None, chars))
			case Success((r,rest)) => Success[Option[A]]((Some(r),rest))
		}
	}


	def flatMap[A,B](p1: Parser[A], f: A => Parser[B]):Parser[B] = {
		(chars: List[Char]) => p1(chars) match{
			case Failure(m1) => Failure[B](m1)
			case Success((r1, rest1)) => f(r1)(rest1)
		}
	}

	def map[A,B](p: Parser[A], f: A => B):Parser[B] = {
		(chars: List[Char]) => p(chars).map(x => f(x))
	}

	def either[A,B](p1: Parser[A], p2: Parser[B]) = {
		(chars: List[Char]) => p1(chars) match{
			case Success(s1) => Success[A](s1)
			case Failure(m1) => p2(chars) match{
				case Success(s2) => Success[B](s2)
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
			case Failure(m1) => Failure[List[A]](m1)
			case Success((r1, rest1)) => many0(p)(rest1) match {
				case Failure(m2) => Success(List(r1), rest1)
				case Success((r2, rest2)) => Success((List(r1)++r2, rest2))
			}
		}
	}


	def sepBy[A](p: Parser[A], ch: Char): Parser[List[A]] = {
		for{
			r   <- p
			rs  <- many0(for {_ <- parseChar(ch); rest <- p} yield rest)
		}yield(r::rs)
	}
}

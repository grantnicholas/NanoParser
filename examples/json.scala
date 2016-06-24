package json

import nanoparser.parsing._
import nanoparser.ParseUtils._
import nanoparser.ParseImpl._

object Main extends App{	
	sealed trait Json extends Product with Serializable
	case class JString(str: String) extends Json 
	case class JNumber(num: Float) extends Json 
	case class JBool(boo: Boolean) extends Json 
	case class JArray(arr: List[Json]) extends Json
	case class JObject(obj: Map[String,Json]) extends Json 
	case class JNull() extends Json 

	object JsonUtils{
		def parseNull:Parser[Json] = {
			for{
				_ <- parseWord("null")
			}yield(JNull())
		}
		def parseBool = {
			val ptrue  = for{_ <- parseWord("true") }yield(JBool(true))
			val pfalse = for{_ <- parseWord("false")}yield(JBool(false)) 
			ptrue <|> pfalse
		}

		def parseString:Parser[Json] = {
			val no_specials = parsePredicate(ch => ch != '"')
			for{
				chars <- between('"', no_specials.many0() , '"')
			}yield(JString(chars.mkString))
		}

		def parseNumber:Parser[Json] = {
			val isDigit   = parsePredicate(ch => Character.isDigit(ch) )
			val isDecimal = parseChar('.')

			val parseFirstPart     = isDigit.many1() 
			val parseSecondPart    = isDigit.many0()

			val parseDecimal:Parser[Json] = for {
				first <- parseFirstPart
				dot <- isDecimal
				second <- parseSecondPart
				chars = first ++ List(dot) ++ second 
				num = chars.mkString.toFloat
			}yield(JNumber(num))

			val parseNonDecimal:Parser[Json] = for {
				chars <- parseFirstPart
				num = chars.mkString.toFloat
			}yield(JNumber(num))

			parseDecimal <|> parseNonDecimal
		}

		def parseArray:Parser[Json] = {
			for {
				_ <- parseChar('[')
				elements <-parseJson.sepBy0(',')
				_ <- parseChar(']')
			}yield(JArray(elements))
		}

		def parseObject:Parser[Json] = {
			val valid_chars = parsePredicate(ch => ch != '"')
			val quotedString = between('"',valid_chars.many0(),'"')
			val parseKVP:Parser[(String,Json)] = for{
				key   <- quotedString
				_     <- parseChar(':')
				value <- parseJson
			}yield(key.mkString,value)
			val parseKVPS = sepBy(parseKVP, ',')

			for{
				_ <- parseChar('{')
				kvps <- parseKVP.sepBy0(',')
				_ <- parseChar('}')
				kvp_map = kvps.toMap
			}yield(JObject(kvp_map))
		}

		def parseJson:Parser[Json] = {
			parseNull <|>
			parseBool <|>
			parseString <|>
			parseNumber <|>
			parseArray <|>  
			parseObject 
		}
	}

	import JsonUtils._

	val jprint = (str:String) => {
		val json = parseJson(str.toList)
		json match {
			case Success(_) => {()}
			case Failure(_) => {println(json); println()}
		}
	}

	val simple_json = """{"name":"Grant","age":"22"}"""
	jprint(simple_json)

	val empty_list = "[]"
	jprint(empty_list)

	val list_json = """[1,2,3,4,5]"""
	jprint(list_json)

	val nested_list = """[1,2,[1,2,3]]"""
	jprint(nested_list)

	val nested_strlist = """["ello","der",["wooo","ya"]]"""
	jprint(nested_strlist)

	val nested_object = """{"name":"root","child":{"name":"child","child":null}}"""
	jprint(nested_object)

	val object_with_list = """{"snacks":[1,2,3]}"""
	jprint(object_with_list)

	val list_with_object = """[{"name":"Grant","age":22}]"""
	jprint(list_with_object)


	val list_with_objects = """[{"name":"child1","children":[]},{"name":"child2","children":[]}]"""
	jprint(list_with_objects)

	val wrapped_list_with_objects = """{"list":[{"name":"child1","children":[]},{"name":"child2","children":[]}]}"""
	jprint(wrapped_list_with_objects)

	val nested_json = """{"name":"root","children":[{"name":"child1","children":[]},{"name":"child2","children":[]}]}"""
	jprint(nested_json)
}
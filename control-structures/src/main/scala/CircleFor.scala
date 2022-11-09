import java.io.{File, FileNotFoundException, FileReader, IOException}

object CircleFor:
  def main(args: Array[String]) =
    val firstArg = if !args.isEmpty then args(0) else ""

    val friend =
      firstArg match
        case "salt" => "pepper"
        case "chips" => "salsa"
        case "eggs" => "bacon"
        case _ => "huh?" 
    println(friend)

  def scalaFiles =
    val filesHere = new File(".").listFiles()
    for
      file <- filesHere
    yield file

  def half(n: Int) =
    if n % 2 == 0 then
      n / 2
    else
      throw new RuntimeException("n must be even")



import java.io.File
import scala.io.Source
import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec

object Main {

  private def printUsage(): Unit = {
    println(
      """stlc-interpreter
        |
        |Usage: stlc-interpreter [options] <source-file>
        |
        |Arguments:
        |  <source-file>              Source file to evaluate (.stlc)
        |
        |Options:
        |  -i, --interpreter <type>   Interpreter type: substitution (default), recursive, trampoline, worklist
        |  -h, --help                 Show this help message
        |""".stripMargin
    )
  }

  private case class Config(
    sourceFile: Option[File] = None,
    interpreterType: String = "substitution"
  )

  private def parseArgs(args: Array[String]): Either[String, Config] = {
    @tailrec
    def parse(remaining: List[String], config: Config): Either[String, Config] = remaining match {
      case Nil => config.sourceFile match {
        case Some(_) => Right(config)
        case None => Left("Error: Source file is required")
      }

      case "-h" :: _ | "--help" :: _ =>
        printUsage()
        System.exit(0)
        Right(config) // unreachable

      case ("-i" | "--interpreter") :: interpreterType :: tail =>
        parse(tail, config.copy(interpreterType = interpreterType))

      case ("-i" | "--interpreter") :: Nil =>
        Left("Error: --interpreter option requires an argument")

      case arg :: tail if arg.startsWith("-") =>
        Left(s"Error: Unknown option: $arg")

      case sourceFile :: tail =>
        config.sourceFile match {
          case None => parse(tail, config.copy(sourceFile = Some(File(sourceFile))))
          case Some(_) => Left(s"Error: Multiple source files specified: ${config.sourceFile.get.getPath} and $sourceFile")
        }
    }

    parse(args.toList, Config())
  }

  def main(args: Array[String]): Unit = parseArgs(args) match {

    case Left(error) => {
      System.err.println(error)
      System.err.println("Use --help for usage information")
      System.exit(1)
    }

    case Right(config) => {
      val sourceFile = config.sourceFile.get

      // Validate file exists
      if (!sourceFile.exists()) {
        System.err.println(s"Error: File not found: ${sourceFile.getPath}")
        System.exit(1)
      }

      // Read source file
      val sourceCode = Try {
        val source = Source.fromFile(sourceFile)
        try source.mkString finally source.close()
      } match {
        case Success(code) => code
        case Failure(exception) =>
          System.err.println(s"Error reading file: ${exception.getMessage}")
          System.exit(1)
          throw new RuntimeException("Unreachable")
      }

      // Select interpreter
      val interpreter: Interpreter = config.interpreterType.toLowerCase match {
        case "s" | "subst"  | "substitution"  => SmallStepSubstInterpreter
        case "r" | "rec"    | "recursive"     => RecursiveInterpreter
        case "t" | "tramp"  | "trampoline"    => TrampolineInterpreter
        case "w" | "wl"     | "worklist"      => WorkListInterpreter
        case unknown =>
          System.err.println(s"Error: Unknown interpreter type: $unknown")
          System.err.println("Valid options are: substitution, recursive, trampoline, worklist")
          System.exit(1)
          throw new RuntimeException("Unreachable")
      }

      // Parse and evaluate
      Try {
        val expr = Parser.parse(sourceCode)
        val term = expr.toTerm()
        val result = interpreter.eval(term)
        result
      } match {
        case Success(value) => println(value)
        case Failure(exception) =>
          System.err.println(s"Error: ${exception.getMessage}")
          exception.printStackTrace()
          System.exit(1)
      }
    }
  }
}

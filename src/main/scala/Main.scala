import picocli.CommandLine
import picocli.CommandLine.{Command, Option, Parameters}

import java.io.File
import scala.io.Source
import scala.util.{Try, Success, Failure}

@Command(
  name = "stlc-interpreter",
  mixinStandardHelpOptions = true,
  version = Array("0.1.0"),
  description = Array("Evaluates STLC source code files with different interpreter strategies")
)
class Main extends Runnable {

  @Parameters(
    index = "0",
    description = Array("Source file to evaluate (.stlc)")
  )
  var sourceFile: File = _

  @Option(
    names = Array("-i", "--interpreter"),
    description = Array("Interpreter type: direct, trampoline, worklist (default: direct)"),
    defaultValue = "direct"
  )
  var interpreterType: String = _

  override def run(): Unit = {
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
      case Failure(exception) => {
        System.err.println(s"Error reading file: ${exception.getMessage}")
        System.exit(1)
        throw new RuntimeException("Unreachable")
      }
    }

    // Select interpreter
    val interpreter: Interpreter = interpreterType.toLowerCase match {
      case "direct" => DirectInterpreter
      case "trampoline" => TrampolineInterpreter
      case "worklist" => WorkListInterpreter
      case unknown =>
        System.err.println(s"Error: Unknown interpreter type: $unknown")
        System.err.println("Valid options are: direct, trampoline, worklist")
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
      case Failure(exception) => {
        System.err.println(s"Error: ${exception.getMessage}")
        exception.printStackTrace()
        System.exit(1)
      }
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val exitCode = new CommandLine(new Main()).execute(args*)
    System.exit(exitCode)
  }
}

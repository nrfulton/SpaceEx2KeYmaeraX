import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._

import scala.xml._
import java.net.URL

import edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXPrettyPrinter

object Converter {
  def apply(xmlFile: URL, cfgFile: URL): Formula = {

    val system = {
      val xml = XML.load(xmlFile)
      val systems = (xml \\ "location").map(parseODE)
      assert(systems.length == 1)
      systems(0)
    }

    val initialConditions = {
      val toRewrite = scala.io.Source.fromURL(cfgFile)
        .mkString
        .split('\n')
        .find((p: String) => p.contains("initially"))
        .getOrElse("initially = false")
        .replaceAll(" ", "")
        .replace("initially=", "")
        .replaceAll("\"", "")
        .replaceAll("==","=")

      toRewrite.asFormula //@todo turn a<=x<=b into a<=x&x<=b.
    }

    //Construct model.
    Imply(initialConditions, Box(system, False)) //@todo where's the post-condition?
  }

  private def parseODE(node : NodeSeq) = {
    val invariantStr = (node \\ "invariant").last.text
    val flowStr = (node \\ "flow").last.text

    assert(flowParsable(flowStr) && invParsable(invariantStr))

    val odeSystemStr = "{" + convertFlow(flowStr) + " & " + convertInvariant(invariantStr) + "}"

    odeSystemStr.asProgram
  }

  private def flowParsable(s: String) =
    try {
      convertFlow(s).asDifferentialProgram;
      true
    } catch {
      case e : Throwable => throw new Exception("Failed to parse the flow.", e)
    }

  private def invParsable(s: String) =
    try {
      convertFlow(s).asFormula;
      true
    } catch {
      case e : Throwable => throw new Exception("Failed to parse the invariant.", e)
    }

  private def convertFlow(flow: String) = {
    flow.replaceAll("&amp;", ",").replaceAll("&",",").replaceAll("==", "=")
  }

  private def convertInvariant(inv: String) = {
    inv.replaceAll("&amp;", "&")
  }
}


object Main {
  def main(argv : Array[String]): Unit = {
//    assert(argv.length == 2, "Usage: /location/to/model/model.xml /location/to/model/model.cfg")
//    val xmlFile = argv(0)
//    val cfgFile = argv(1)

    val xmlFile = new URL("https://nfulton.org/bball.xml")
    val cfgFile = new URL("https://nfulton.org/bball.cfg")//@note the CFG file's initially key is changed...1

    val result = Converter(xmlFile, cfgFile)

    PrettyPrinter.setPrinter(KeYmaeraXPrettyPrinter)
    println(result.prettyString)
  }
}

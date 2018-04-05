import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._

import scala.xml._
import java.net.URL

import edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXPrettyPrinter


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

/** Converts SpaceEx into KeYmaera X using raw string transformations on portions of the XML1. */
object Converter {
  def apply(xmlFile: URL, cfgFile: URL): Formula = {
    //Get the ODESystem.
    val system = {
      val xml = XML.load(xmlFile)
      val systems = (xml \\ "location").map(parseODE)
      assert(systems.length == 1)
      systems(0)
    }

    //Get the initial conditions from the cfg file.
    val initialConditions = {
      val toRewrite = scala.io.Source.fromURL(cfgFile)
        .mkString
        .split('\n')
        .find((p: String) => p.contains("initially"))
        .getOrElse("initially = true")
        .replaceAll(" ", "")
        .replace("initially=", "")
        .replaceAll("\"", "")
        .replaceAll("==","=")

      fixChainedInequalities(toRewrite).asFormula //@todo turn a<=x<=b into a<=x&x<=b.
    }

    //Construct model.
    Imply(initialConditions, Box(system, False)) //@todo where's the post-condition?
  }

  /** Parses the interior of a <location></location> tag into an ODESystem. */
  private def parseODE(node : NodeSeq) = {
    val invariantStr = {
      val invariants = (node \\ "invariant").map(_.text)
      if(invariants.length == 0)
        "true"
      else if(invariants.length == 1)
        invariants.head
      else
        invariants.reduce(_ + "&" + _)
    }

    val flowStr = {
      val flows = (node \\ "flow")
      assert(flows.length == 1)
      flows.last.text
    }

    val odeSystemStr =
      "{" +
      convertFlow(flowStr) + " & " +
      fixChainedInequalities(convertInvariant(invariantStr)) +
      "}"

    odeSystemStr.asProgram
  }

  private def convertFlow(flow: String) = {
    flow.replaceAll("&amp;", ",").replaceAll("&",",").replaceAll("==", "=")
  }

  private def convertInvariant(inv: String) = {
    inv.replaceAll("&amp;", "&")
  }


  //region Absolutely terrible code for handling absolutely terrible chained inequalities.

  /** A horrible way of rewriting a<=x<=b into a<=x & x<=b until no more chained inequalities exist*/
  private def fixChainedInequalities(s: String): String = {
    assert(!s.contains("=="), "Should've removed all double-equals by now.")
    assert(!s.matches(".*&(lt|gt|leq|geq|eq);.*"), "Shouldn't removed all HTML codes by now")

    /* s: the string currently being processed.
     * state: the current state of the parser.
     *   0 => start state
     *   1 => have encountered first <=
     * middleTerm: the middle term.
     * prefix: the prefix to the middle term.
     */
    var prefix = ""
    var state = 0
    var middleTerm = ""
    var toProcess = s

    while(!toProcess.isEmpty) {
      //Find the first inequality and push toProcess into prefix.
      if (state == 0) {
        leadingInequality(toProcess) match {
          case Some(ineq) => {
            state = 1
            prefix = prefix + ineq
            toProcess = stripLeadingInequality(toProcess)
          }
          case None => {
            prefix += toProcess.head
            toProcess = toProcess.tail
          }
        }
      }

      //Read off the LHS of an inequality.
      else if (state == 1) {
        leadingInequality(toProcess) match {
          case Some(ineq) => {
            //found a<=x<=b!
            prefix = prefix + middleTerm + "&" + middleTerm + ineq
            toProcess = stripLeadingInequality(toProcess)
            state = 0
            middleTerm = ""
          }
          case None => {
            //Found "a<=x | ..." or "a<=x & ..."
            if (endOfTerm(toProcess)) {
              prefix = prefix + middleTerm + toProcess.head
              toProcess = toProcess.tail
              state = 0
              middleTerm = ""
            }
            //Still reading the middle term.
            else {
              middleTerm += toProcess.head
              toProcess = toProcess.tail
            }
          }
        }
      }
    }

    prefix + middleTerm
  }

  private def endOfTerm(s: String) = {
    leadingInequality(s).nonEmpty ||
      s.matches("&") ||
      s.matches("|") //@todo anything else?
  }

  private def leadingInequality(s: String): Option[String] = {
    if(s.matches("^<=.*"))
      Some("<=")
    else if(s.matches(">=.*"))
      Some(">=")
    else if(s.matches("^=.*"))
      Some("=")
    else if(s.matches("^<.*"))
      Some("<")
    else if(s.matches(">.*"))
      Some(">")
    else
      None
  }

  private def stripLeadingInequality(s: String) = {
    assert(leadingInequality(s).nonEmpty)
    val ineq = leadingInequality(s).get

    if(ineq.length == 1)
      s.tail
    else if(ineq.length == 2)
      s.tail.tail
    else
      ???
  }

  //endregion
}
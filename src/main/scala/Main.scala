import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._

import scala.xml._

import edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXPrettyPrinter

/**
  * @author Nathan Fulton
  */
object Main {
  def main(argv : Array[String]): Unit = {
    PrettyPrinter.setPrinter(KeYmaeraXPrettyPrinter)
    val result = SogokonConverter(new java.io.File("/home/nfulton/Downloads/benchmarks"))
    println(ProblemSet2JSON(result))
  }
}

/** Good enough pretty-print of a String -> Formula list as a JSON object.
  * @author Nathan Fulton */
object ProblemSet2JSON {
  def apply(map: Map[String, Formula]): String =
    "{\n" + map.map(kvp => {
      assert(!kvp._1.contains('\'') && !KeYmaeraXPrettyPrinter(kvp._2).contains('"'))
      s"""  "${kvp._1}": "${KeYmaeraXPrettyPrinter(kvp._2)}" """
    }).reduce(_ + ",\n" + _) + "\n}"
}

/** Converts all examples from Sogokon et al. 2016 nonlinear systems benchmark.
  * @author Nathan Fulton */
object SogokonConverter {
  /** These are the files we don't support yet. */
  private def ignoreList(file: java.io.File) =
    //Ignored because we dont' support hybrid systems yet.
    file.getName.contains("buck_v1") ||
    file.getName.contains("buck_v2") ||
    file.getName.contains("buck_v3") ||
    file.getName.contains("buck_v4") ||
    file.getName.contains("buck_v5") ||
    file.getName.contains("buck_v6") ||
    file.getName.contains("buck_v7") ||
    file.getName.contains("buck_v8") ||
    file.getName.contains("buckboost_v1") ||
    //ignored because no CFG file
    file.getName.contains("boost_v1") ||
    //to make commenting out items from ignore list easier...
    false

  def apply(directory: java.io.File): Map[String, Formula] = {
    directory
      .listFiles()
      .filter(f => f.getName.endsWith("xml") && !ignoreList(f))
      .map(xmlFile => {
        val modelName = xmlFile.getName.replaceAll("\\.xml$", "")
        assert(!modelName.contains('/'), "Wanted just the base file name, no paths.")

        val cfgFile = new java.io.File(xmlFile.getAbsoluteFile.toString.replaceAll("\\.xml$", ".cfg"))
        assert(cfgFile.exists(), s"Need the CFG file associated with ${xmlFile.getAbsoluteFile} to be named ${cfgFile.getAbsoluteFile.toString}")
        println(s"Loading ${modelName} from:\n\t${xmlFile}\n\t${cfgFile}")

        val xmlContents = scala.io.Source.fromFile(xmlFile).mkString
        val cfgContents = scala.io.Source.fromFile(cfgFile).mkString

        modelName -> Converter(xmlContents, cfgContents)
      })
      .toMap
  }
}

/** Converts SpaceEx into KeYmaera X using raw string transformations on portions of the XML.
  * @todo currrently only works for models that contain a single flow and no transitions.
  * @author Nathan Fulton */
object Converter {
  def apply(xmlContents: String, cfgContents: String): Formula = {
    //Get the ODESystem.
    val system = {
      val xml = XML.loadString(xmlContents)
      val systems = (xml \\ "location").map(parseODE)
      assert(systems.length == 1, "We only handle model files with a single system in them for the moment")
      systems(0)
    }

    //Get the initial/forbidden conditions from the cfg file.
    val initialConditions = {
      val rawFormulaString = cfgContents
        .split('\n')
        .find((p: String) => p.contains("initially"))
        .getOrElse("initially = true")
        .replaceAll(" ", "")
        .replace("initially=", "")
        .replaceAll("\"", "")
        .replaceAll("==","=")

      convertFormula(rawFormulaString).asFormula
    }

    val forbiddenConditions = {
      val rawFormulaString = cfgContents
        .split('\n')
        .find((p: String) => p.contains("forbidden"))
        .getOrElse("forbidden = false")
        .replaceAll(" ", "")
        .replace("forbidden=", "")
        .replaceAll("\"", "")

      convertFormula(rawFormulaString).asFormula
    }

    //Construct model.
    Imply(initialConditions, Box(system, Not(forbiddenConditions)))
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
      convertInvariant(invariantStr) +
      "}"

    odeSystemStr.asProgram
  }

  /** @todo misleading name. Converts a SpaceEx ODE string into a string that should parse as a [[DifferentialProgram]]. */
  private def convertFlow(flow: String) = {
    flow.replaceAll("&amp;", ",").replaceAll("&",",").replaceAll("==", "=")
  } ensuring(parses(_))

  /** @todo misleading name. Converts a SpaceEx <invariant></invariant> into a string that should parse as a [[Formula]]. */
  private def convertInvariant(inv: String) = {
    convertFormula(inv)
  } ensuring(parses(_))

  /** @todo mislieading name. Converts a SpaceEx formula into a string that should parse as a [[Formula]] */
  private def convertFormula(formula: String) = {
    val fixHTML =
      formula.replaceAll("&amp;", "&")
        .replaceAll("&gt;", ">")
        .replaceAll("&geq;", ">=")
        .replaceAll("&lt;", "<")
        .replaceAll("&leq;", ">")
        .replaceAll("<-", "< -")
        .replaceAll("==", "=")

    fixChainedInequalities(fixHTML)
  } ensuring(parses(_))

  /** Trys to parse the string as a formula/program/differentialprogram. Returns true if any of those parses work.*/
  private def parses(s: String) : Boolean = {
    try {s.asFormula; true}
    catch {
      case e : Throwable => {
        try{s.asProgram; true}
        catch {
          case e: Throwable => {
            try{s.asDifferentialProgram; true}
            catch {
              case e: Throwable => {
                false
              }
            }
          }
        }
      }
    }
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
    leadingInequality(s).nonEmpty || s.head == '&' || s.head == '|' //@todo anything else?
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
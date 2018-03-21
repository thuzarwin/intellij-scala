package org.jetbrains.plugins.scala.lang.macros

import org.jetbrains.plugins.scala.extensions.ResolvesTo
import org.jetbrains.plugins.scala.lang.macros.MacroBundleCompanionInjector.isMacroBundle
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScSimpleTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector

/**
  * Nikolay.Tropin
  * 21-Mar-18
  */
class MacroBundleCompanionInjector extends SyntheticMembersInjector {
  override def needsCompanionObject(source: ScTypeDefinition): Boolean =
    isMacroBundle(source)


  /**
    * Since scala 2.11 it's possible to create macro implementations inside classes with a single parameter
    * of type [[scala.reflect.macros.blackbox.Context]] or [[scala.reflect.macros.whitebox.Context]]
    *
    * @see http://docs.scala-lang.org/overviews/macros/bundles.html
    *
    * But this methods are used as if they were defined object, not in a class.
    * So let's create a synthetic companion object which extends macro bundle.
    */
  override def injectSupers(source: ScTypeDefinition): Seq[String] = {
    source match {
      case obj: ScObject =>
        obj.fakeCompanionClassOrCompanionClass match {
          case clazz: ScClass if isMacroBundle(clazz) => Seq(clazz.name)
          case _ => Nil
        }
      case _ => Nil
    }
  }
}

object MacroBundleCompanionInjector {

  private def isMacroBundle(td: ScTypeDefinition): Boolean = td match {
    case c: ScClass => c.parameters.size == 1 && isMacroContext(c.parameters.head)
    case _ => false
  }

  private def isMacroContext(param: ScParameter): Boolean = {
    param.paramType.map(_.typeElement).exists {
      case ScSimpleTypeElement(ResolvesTo(td: ScTypeDefinition)) => isMacroContextFQN(td.qualifiedName)
      case _ => false
    }
  }

  private def isMacroContextFQN(fqn: String): Boolean =
    fqn == "scala.reflect.macros.blackbox.Context" || fqn == "scala.reflect.macros.whitebox.Context"
}

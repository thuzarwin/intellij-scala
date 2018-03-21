package org.jetbrains.plugins.scala.lang.macros

import org.jetbrains.plugins.scala.debugger.{ScalaVersion, Scala_2_11}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScReferenceElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScObject}
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveTestCase
import org.junit.Assert

/**
  * Nikolay.Tropin
  * 21-Mar-18
  */
class MacroBundleCompanionTest extends ScalaResolveTestCase {
  override implicit val version: ScalaVersion = Scala_2_11

  override protected def isIncludeReflectLibrary = true

  override def folderPath = super.folderPath() + "resolve/macroBundle/"

  def testSCL8414a(): Unit = findReferenceAtCaret() match {
    case ref: ScReferenceElement =>
      ref.resolve() match {
        case o: ScObject =>
          Assert.assertTrue("Resolve to a synthetic object expected", o.getNavigationElement.isInstanceOf[ScClass])
        case _ =>
          Assert.fail("Resolve to object expected")
      }
  }

  def testSCL8414b(): Unit = findReferenceAtCaret() match {
    case ref: ScReferenceElement =>
      ref.resolve() match {
        case fun: ScFunction =>
          Assert.assertTrue("Resolve to a function in macro bundle class expected", fun.containingClass.isInstanceOf[ScClass])
        case _ =>
          Assert.fail("Resolve to a function expected")
      }
  }
}

import scala.language.experimental.macros
import scala.reflect.macros._
class Impl(val c: whitebox.Context) {
  def mono = c.literalUnit
  def poly[T: c.WeakTypeTag] = c.literal(c.weakTypeOf[T].toString)
}
object Macros {
  def poly[T] = macro Impl.<caret>poly[T]
}
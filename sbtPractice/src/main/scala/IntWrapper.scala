/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 11/7/12
 * Time: 1:40 PM
 * To change this template use File | Settings | File Templates.
 */
class IntWrapper(val value:Int) extends Arithmetic[IntWrapper]{

  def +(other: IntWrapper) = IntWrapper(value + other.value)
  def -(other: IntWrapper) = IntWrapper(value - other.value)
  def *(other: IntWrapper) = IntWrapper(value * other.value)
  def /(other: IntWrapper) = IntWrapper(value / other.value)
  def %(other: IntWrapper) = IntWrapper(value % other.value)

  override def equals(other: Any): Boolean = {
    (value==other.asInstanceOf[IntWrapper].value)
  }

  def isZero: Boolean = (value==0)

  override def toString = value.toString
}

object IntWrapper{
  def apply(value:Int) = new IntWrapper(value)
}

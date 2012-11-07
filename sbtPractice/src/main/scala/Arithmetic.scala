/**
 * Created with IntelliJ IDEA.
 * User: qiwan
 * Date: 11/7/12
 * Time: 1:41 PM
 * To change this template use File | Settings | File Templates.
 */
abstract class Arithmetic[U]{
  def +(other: U): U

  def -(other: U): U

  def *(other: U): U

  def /(other: U): U

  def %(other: U): U

  override def equals(other: Any): Boolean

  def isZero:Boolean
}
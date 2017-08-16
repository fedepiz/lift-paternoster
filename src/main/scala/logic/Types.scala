package logic

/**
  * Created by federico on 16/08/17.
  */
object Types {
  sealed trait Type


  case class Float() extends Type

  type ArraySize = Int //Will probably change at some point...
  case class Array(elementT:Type, size:ArraySize) extends Type

  def arraySizes(arr:Array):List[ArraySize] = {
    arr.size::(arr.elementT match {
      case nested: Array => arraySizes(nested)
      case _ => List()
    })
  }

  def arrayBottomElementType(arr:Array):Type = {
    arr.elementT match {
      case nested:Array => arrayBottomElementType(nested)
      case other => other
    }
  }
}

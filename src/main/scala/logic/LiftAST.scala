package logic

/**
  * Created by Federico on 18-Aug-17.
  */
object LiftAST {

  sealed trait Type


  case object Float extends Type

  type ArraySize = Int //Will probably change at some point...
  case class Array(elementT:Type, size:ArraySize) extends Type

  case class Function(inputType:Type, outputType:Type) extends Type

  def arraySizes(arr:Array):List[ArraySize] = {
    arr.size::(arr.elementT match {
      case nested: Array => arraySizes(nested)
      case _ => List()
    })
  }

  def arrayBottomElementType(arr:Array):Type = {
    arr.elementT match {
      case nested: Array => arrayBottomElementType(nested)
      case other => other
    }
  }

  sealed trait Operation

  case class Map(fType:Function, size:ArraySize) extends Operation
}

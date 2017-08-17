package logic

/**
  * Created by federico on 17/08/17.
  */
object Operations {
  sealed trait Operation

  case class Map(inputType:Types.Type, outputType:Types.Type, size:Types.ArraySize) extends Operation
}

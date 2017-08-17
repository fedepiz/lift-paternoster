package logic

/**
  * Created by federico on 16/08/17.
  */

object Renderer {
  import Types.Type
  sealed trait TypeRenderer

  case class FloatRenderer() extends TypeRenderer
  sealed trait ArrayTypeRenderer extends TypeRenderer
  case class LinearArrayRenderer(element:TypeRenderer, size:Int) extends ArrayTypeRenderer
  case class GridArrayRenderer(elementType: TypeRenderer, width:Int, height:Int) extends ArrayTypeRenderer

  private def typeRendererWidth(typeRenderer: TypeRenderer):Int = typeRenderer match {
    case FloatRenderer() => 1
    case LinearArrayRenderer(elem, size) => typeRendererWidth(elem) * size
    case GridArrayRenderer(elem, width, _) => typeRendererWidth(elem) * width
  }

  private def typeRendererHeight(typeRenderer: TypeRenderer):Int = typeRenderer match {
    case FloatRenderer() => 1
    case LinearArrayRenderer(elem, size) => typeRendererHeight(elem)
    case GridArrayRenderer(elem, _, height) => typeRendererHeight(elem) * height
  }


  def typeRenderer(t:Type):TypeRenderer = t match {
    case _:Types.Float => FloatRenderer()
    case array:Types.Array =>
      //Get the nested array sizes as an ordered list
      val sizes = flattenArraySizes(array)
      //Group the ordered list of sizes according to the default dimension rules
      val groupedSizes = groupSizesByDimensions(defaultDimensionSplits(sizes.length), sizes)
      //The ultimate non-array element contained in the nested array
      val bottomElement = Types.arrayBottomElementType(array)
      arrayTypeRenderer(bottomElement, groupedSizes)
  }

  private def arrayTypeRenderer(bottomT:Type, sizes:List[List[Types.ArraySize]]):ArrayTypeRenderer = {
    if(sizes.isEmpty) {
      throw new Exception("Array type renderer with empty sizes - impossible!!")
    }
    val(currentSizes::nextSizes) = sizes
    //Build the contained element first...
    val inner = nextSizes match {
      //If we are out of sizes, then this is the end of the array. we contain the bottom element
      case Nil => typeRenderer(bottomT)
      //Otherwise recurse
      case _ => arrayTypeRenderer(bottomT, nextSizes)
    }
    //Now build the current level of array
    currentSizes.length match {
      //1 dimension - linear array. Dimension is length
      case 1 => LinearArrayRenderer(inner, currentSizes.head)
      //2 dimensions - grid. Dimensions are width and height
      case 2 => GridArrayRenderer(inner, currentSizes.head, currentSizes.tail.head)
      //any other - not supported yet!
      case n => throw new Exception(s"Unsupported rendering of $n-dimensional array level. Try another dimension grouping")
    }
  }


  private def flattenArraySizes(array:Types.Array):List[Types.ArraySize] =
    array.size::(array.elementT match {
      case nested:Types.Array => flattenArraySizes(nested)
      case _ => List()
    })

  private def defaultDimensionSplits(n:Int):List[Int] = n match {
    case 0 => Nil
    case 1 => List(1)
    case 2 => List(2)
    case _ => 2::defaultDimensionSplits(n - 2)
  }


  private def groupSizesByDimensions(dimensions:List[Int], sizes:List[Types.ArraySize]):List[List[Int]] = {
    dimensions match {
      case Nil => List()
      case dim::other_dims => sizes.take(dim) ::groupSizesByDimensions(other_dims, sizes.drop(dim))
    }
  }

  sealed trait PrimitiveRenderer
  case class Rectangle(x:Int, y:Int, width:Int, height:Int) extends PrimitiveRenderer
  case class Box(x:Int, y:Int, width:Int, height:Int) extends PrimitiveRenderer

  def makePrimitives(typeRenderer: TypeRenderer):Iterable[PrimitiveRenderer] = {
    typeRenderer match {
      case FloatRenderer() => Seq(Rectangle(0, 0, 1, 1))
      case LinearArrayRenderer(element, size) =>
        val elemWidth = typeRendererWidth(element)
        //compute inner element primitives
        val elementPrims = makePrimitives(element)
        //Repeat each set of primitives up to size times, translating the set by the
        //position
        val sets = (0 until size).map(pos => elementPrims.map(translate(_, dx = elemWidth*pos, dy = 0)))
        //As a final results, flatten the sets and add the container box
        sets.flatten ++ Seq(Box(0, 0, size*elemWidth, 1))
      case GridArrayRenderer(elementType, width, height) =>
        val elemWidth =typeRendererWidth(elementType)
        val elemHeight = typeRendererHeight(elementType)
        //compute inner element primitives
        val elementPrims = makePrimitives(elementType)
        //Compute the positions where the children will go
        val positions = for(x <- 0 until width;
                            y <- 0 until height) yield (x*elemWidth, y*elemHeight)
        //For each position, replicate the elementPrimitives and translate them to that place
        val sets = positions.map{case (x,y) => elementPrims.map(translate(_, dx = x, dy = y))}
        //Flatten the sets and wrap in container box
        sets.flatten ++ Seq(Box(0, 0, width*elemWidth, height*elemHeight))
    }
  }

  private def translate(primitive:PrimitiveRenderer, dx:Int, dy:Int):PrimitiveRenderer = {
    primitive match {
      case r:Rectangle => r.copy(x = r.x + dx, y = r.y + dy)
      case b:Box => b.copy(x = b.x + dx, y = b.y + dy)
    }
  }
}
package gui

import javafx.application.Application
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.layout.Pane
import javafx.scene.paint.Color
import javafx.stage.Stage

import logic.{Graphics, LiftAST, Scene}
import logic.Graphics.{Arrow, Box, Rectangle}



class Main extends Application {
  override def start(stage: Stage): Unit = {
    val mainPane = new MainPane(800, 600)
    stage.setScene(new javafx.scene.Scene(mainPane, mainPane.width, mainPane.height))
    stage.setTitle("lift-paternoster")
    stage.show()

    import LiftAST._
    val operation = LiftAST.Map(Function(Float, Array(Float,2)), 10)

    mainPane.draw(Scene.drawOperation(Scene.operationNode(operation)))
  }
}

object Main {
  def main(args:Array[String]) = {
    Application.launch(classOf[Main], args:_*)
  }
}

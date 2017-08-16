package gui

import javafx.application.Application
import javafx.stage.Stage

/**
  * Created by federico on 16/08/17.
  */
class Main extends Application {
  override def start(stage: Stage): Unit = {
    stage.setTitle("lift-paternoster")
    stage.show()
  }
}

object Main {
  def main(args:Array[String]) = {
    Application.launch(classOf[Main], args:_*)
  }
}

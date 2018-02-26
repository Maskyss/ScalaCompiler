package main

import java.io.FileNotFoundException

import main.Helper._

import scala.collection.mutable._
import scala.io.Source

class LexAnalizer {
  def initMap(namefile: String):Unit = {
    try {
      var tempList =  new ListBuffer[rowToken]
      var lst = new ListBuffer[List[rowToken]]

      var row =1
      var rowInd =1

      for (line <- Source.fromFile(namefile).getLines) {

        var tokens = tokenPat.findAllIn(line)

        rowInd=1
        for(next <- tokens){
          tempList += rowToken(row,rowInd,next)
          rowInd+=1
        }
         row+=1
         lst += tempList.toList

      }
    } catch {
        case ex: FileNotFoundException => Error = true
          println("Missing file exception")
      }
    }
}

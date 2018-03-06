package main

import main.LexicalAnalizer._
import main.Helper._

object Main extends App {

    val an = new LexAnalizer()
    an.initMap(Console.readLine()+".asm")
    //an.printLexic()
}

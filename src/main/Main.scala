package main

import java.io.FileNotFoundException

import main.Helper._

import scala.io.Source
object Main extends App {

    val an = new LexAnalizer()
    print("Please, input filename: ")
    fileName = Console.readLine()
    an.initMap(fileName +".asm") //initialize Lexical Table, in LexiacalList = ListBuffer[List[mainToken]]
    if(!Error){
        Console.out.println(Console.RED +"\t\t\t\t\tLEXICAL TABLE"+ Console.RESET)
        an.lexicalName()
        val sy = new SyntaxAnalizer()
        Console.out.println(Console.RED +"\t\t\t\t\tSYNTAX TABLE"+ Console.RESET)
        //sy.printTable()
       // val sm =  new SymaticAnalize()
        val sm =  new SymanticWriter()
        sm.SearchSegment()


        try {
            Source.fromFile(fileName+".lst").getLines.foreach(println(_))}
        catch {
            case ex: FileNotFoundException => Error = true //if enter file not found
                println("Missing file exception")
        }

    }
}

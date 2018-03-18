package main

object Main extends App {

    val an = new LexAnalizer()
    print("Please, input filename: ")
    an.initMap(Console.readLine()+".asm")
    Console.out.println(Console.RED +"\t\t\t\t\tLEXICAL TABLE"+ Console.RESET)
    an.printLexic()
    val sy = new SyntaxAnalizer(an)
    Console.out.println(Console.RED +"\t\t\t\t\tSYNTAX TABLE"+ Console.RESET)
    sy.printTable()
}

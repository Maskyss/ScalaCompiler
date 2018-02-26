package main
import scala.io.Source
import main.Helper._
package LexicalAnalizer {

  import java.io.FileNotFoundException

  import scala.collection.mutable
  import scala.collection.mutable._


  class LexicalAnalizer(){
      private var file:String = ""
      private var mapWith = scala.collection.mutable.Map[Int, List[String]]()
      private var listings =  List[String]()
    //  val coun
      def initMap(namefile: String):Unit = {
        try {
          file = Source.fromFile(namefile) getLines() mkString "\n"
          var i = 0
          var count = 1
          var word = ""

          var lsforMap = new ListBuffer[String]()
          while (i < file.length) {
            file.apply(i) match {
              case '\n' => {
                //println(word)
                //println("*************************")
                if (!lsforMap.isEmpty) {
                  lsforMap += word

                  mapWith += (count -> lsforMap.toList)
                }
                lsforMap.clear()
                word = ""
                count += 1
                i += 1
              }
              case '\t' => i += 1
              case _ => {
                if (isDiv(file.apply(i))) {
                  lsforMap += word
                  word = ""
                  word += file.apply(i)
                  lsforMap += word
                  word = ""
                  i += 1
                } else if (isNum(file.apply(i)) && (file.apply(i - 1) == '(')) {
                  lsforMap += word
                  word = ""
                  word += file.apply(i)
                  lsforMap += word
                  word = ""
                  i += 1
                }
                else {
                  file.apply(i) match {
                    case ' ' => {
                      i += 1
                      lsforMap += word
                      //println(word)
                      word = ""
                    }
                    case _ => {
                      word += file.apply(i)
                      i += 1
                    }
                  }
                }
              }
            }
            if(i==file.length-1){
              word+=file.charAt(i)
              lsforMap+= word
              mapWith += (count -> lsforMap.toList)}
          }
        } catch {
          case ex: FileNotFoundException => Error = true
            println("Missing file exception")
        }
      }
    def analize():Unit={

      val maxNum = mapWith.keys.max
      var i= 1
      while(i<=maxNum){
        if(!mapWith.contains(i))i+=1
        listings = mapWith.get(i).get
        println(listings.contains(' '))//listings -=(' ')
        var lCoun = listings.length
        var j = 0
        printf("-----------------ROW#%d#-----------------\n",i)
        print("\tLEXEM\t\t\t\tLENGTH\t\t\t\tTYPE\n")
        while(j<lCoun){
         // if(listings.lift(j).getOrElse("No name given") == ' ') j+=1
          println(listings.lift(j).getOrElse("No name given"))
      //    print(listings.lift(j))
          j+=1
        }
        i+=1


      }
    }
    def lexAnalize(str: String) = {

    }
  }

}

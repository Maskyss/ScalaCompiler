package main

import java.io.FileNotFoundException
import main.Helper._
import scala.collection.mutable.ListBuffer
import scala.io.Source

class LexAnalizer {
  def initMap(namefile: String): Unit = {
    try {
      var tempList = new ListBuffer[mainToken] //tempList for string with all tokens

      var row = 1

      var flComent = false
      for (line <- Source.fromFile(namefile).getLines) { //read file
        tempList.clear()                             //with each line tempList clear
        var tokens = tokenPat.findAllIn(line)       //tokenPat regex-string , func findAllIn search token
        flComent = false
        var coment = ""
        for (next <- tokens) {  //tokens is array string
          if(next==";"||(flComent)){
            coment+=next
            flComent=true
          }
          else tempList += mainToken(row,next) //such member List has num of row and tokenname string from line
        }
        if (flComent) tempList+=mainToken(row,coment)
        row += 1
        LexicalList += tempList.toList
      }
    } catch {
      case ex: FileNotFoundException => Error = true //if enter file not found
        println("Missing file exception")
    }
  }
  def lexicalName():Unit = {
    var tempList = new ListBuffer[syntaxToken]
    for(listBuf<- LexicalList){
      tempList.clear()
      if(listBuf.nonEmpty){//LexicalList may be empty, because file has \n - lines
        for(go<-listBuf) {
          var tmp1 = go.token
          var base = checkWhat(tmp1)

          if(base.equals("User ident")&&((tmp1.length>8)|| isNum(tmp1.charAt(0)))) {//check token if length>8 and name has 1st character as number
            ErrorList+= ErrorT(go.row,"Bad identification")
            WarningError+=1
          }
          if(!base.equals("Comment"))
            tempList+=syntaxToken(go.row,go.token,base)
        }
        if(!tempList.isEmpty)
          SyntaxList+=tempList.toList
      }
    }
  }

  def printLexic(): Unit = {
   var si = 0
    var forIdent = new ErrorT(0,"")//variable for demonstrate in lexical table Error`s name
    var err:String = ""
    var flag = false
    var tempList = new ListBuffer[syntaxToken]

    for(listBuf<- LexicalList){
          si = 0
      if(listBuf.nonEmpty){//LexicalList may be empty, because file has \n - lines

        for(go<-listBuf) {

                if (si==0){
                  printf(" ------------------------ROW#%s#------------------------\n", go.row)
                  print("\tLEXEM\t\t\t\tLENGTH\t\t\tTYPE\n")
                  tempList.clear()
                }

                var tmp1 = go.token

                if(tmp1.length<4)printf("\t%s\t\t\t\t\t", tmp1)
                else if(tmp1.length>7) printf("\t%s\t\t\t", tmp1)
                  else printf("\t%s\t\t\t\t", tmp1)

                print(tmp1.length)

                var base = checkWhat(tmp1)

                if(base.equals("User ident")&&((tmp1.length>8)|| isNum(tmp1.charAt(0)))) {//check token if length>8 and name has 1st character as number
                  flag = true
                  ErrorList+= ErrorT(go.row,"Bad identification")
                  forIdent = ErrorT(go.row,"Bad identification")
                  err = tmp1
                  WarningError+=1
                }
                printf("\t\t\t%s\n", base)
                si+=1
                if(!base.equals("Comment"))
                  tempList+=syntaxToken(go.row,go.token,base)
              }
              if(!tempList.isEmpty)
                SyntaxList+=tempList.toList
              if (flag==true) {
                Console.out.println(  Console.RED + "bad identification in " +forIdent.row+" row, name " +err+ Console.RESET )
                flag  = false
              }
      }
    }
    //for(listBuf<- SyntaxList){println(listBuf)}
    //SyntaxList.foreach(println(_))
  }
}

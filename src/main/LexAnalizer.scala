package main

import java.io.FileNotFoundException

import main.Helper._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

class LexAnalizer {
  def initMap(namefile: String): Unit = {
    try {
      var tempList = new ListBuffer[mainToken]

      var row = 1
      var rowInd = 1

      for (line <- Source.fromFile(namefile).getLines) {
        tempList.clear()
        var tokens = tokenPat.findAllIn(line)

        rowInd = 1
        for (next <- tokens) {
          tempList += mainToken(row, rowInd, next)
          rowInd += 1
        }
        row += 1
        LexicalList += tempList.toList
      }
   //   for(listBuf<- lst){println(listBuf)}
    } catch {
      case ex: FileNotFoundException => Error = true
        println("Missing file exception")
    }
  }

  def printLexic(): Unit = {
   var si = 0
    var forIdent = new ErrorT("",0,false)
    var flag = false
    for(listBuf<- LexicalList){
          si = 0
              for(go<-listBuf) {
                if (si==0){
                  printf(" ------------------------ROW#%s#------------------------\n", go.row)
                  print("\tLEXEM\t\t\t\tLENGTH\t\t\tTYPE\n")
                }

                var tmp1 = go.token

                if(tmp1.length<4)printf("\t%s\t\t\t\t\t", tmp1)
                else if(tmp1.length>7) printf("\t%s\t\t\t", tmp1)
                  else printf("\t%s\t\t\t\t", tmp1)

                print(tmp1.length)

                var base = checkWhat(tmp1)
                if(base.equals("User ident")&&((tmp1.length>8)|| isNum(tmp1.charAt(0)))) {
                  flag = true
                  forIdent = new ErrorT(tmp1,go.row, true)
                }
                printf("\t\t\t%s\n", base)
                si+=1
                SyntaxList+=tokenSyntax(go,base)
              }
              if (flag==true) {
                Console.out.println(  Console.RED + "bad identification in " +forIdent.row+" row name " +forIdent.str+ Console.RESET )
                flag  = false
              }
    }
  }
}

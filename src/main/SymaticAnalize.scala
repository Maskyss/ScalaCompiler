package main
import java.io.{File, PrintWriter}

import main.Helper._
import scala.collection.mutable.ListBuffer

class SymaticAnalize {
  var DataBegin = new ListBuffer[String]
  var DataEnd = new ListBuffer[String]
  var DataExist = false
  var nameIdentData = new ListBuffer[nameIdData]
  case class nameIdData(bytes:Int,name:String)
  var bytesInt = 0
  var bytesHex:StringBuilder = new StringBuilder

  def Databegin(listBuf: List[syntaxToken]):Boolean= {
      if(listBuf.length==2)
          for(go<-listBuf){
              if(go.typeOfLexem == "User ident"){
                if((ErrorList.filter(_.row == go.row).isEmpty)&&(listBuf(1).token.toLowerCase()=="segment")){
                  DataBegin += go.token
                  return true
                }
              }
              else {
                printList(listBuf)
                print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
                println("Expected: instruction or directive")
            }
          }
      else {
        ErrorList+= ErrorT(listBuf(0).row,"Expected: instruction or directive")
        printList(listBuf)
        print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
        println("Expected: instruction or directive")
      }
    return false
  }

  def printList(listBuf: List[syntaxToken]) = {
    print("\t\t")
    for(go<-listBuf)print(go.token+" ")
    println()
  }

  def printHex(i:Int): Unit ={
    var k = ""
    bytesHex.clear()
    if(i.toHexString.length==1){
      k= i.toHexString
      bytesHex.append("000"+k)
      print(bytesHex.toString().toUpperCase)
    }
    if(i.toHexString.length==2){
      k= i.toHexString
      bytesHex.append("00"+k)
      print(bytesHex.toString().toUpperCase)
    }
    if(i.toHexString.length==3){
      k= i.toHexString
      bytesHex.append("0"+k)
      print(bytesHex.toString().toUpperCase)
    }
    if(i.toHexString.length==4){
      k= i.toHexString
      bytesHex.append(k)
      print(bytesHex.toString().toUpperCase)
    }
    if(i.toHexString.length>4) println("Memory")
  }

  def DDDQFirst(listBuf: List[syntaxToken]): Unit ={
    if(listBuf(1).token.toLowerCase=="dd"||listBuf(1).token.toLowerCase=="dq"||listBuf(1).typeOfLexem=="User ident"){
      printList(listBuf)
      print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
      println("Symbol not defined: "+listBuf(1).token)
    }
    if(listBuf(2).typeOfLexem=="User ident"&&listBuf(1).typeOfLexem=="Dec constant"){
      printList(listBuf)
      print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
      println("Symbol not defined: "+listBuf(2).token)
    }
    if(listBuf(2).typeOfLexem=="Dec constant"&&listBuf(1).typeOfLexem=="Dec constant") {
      printList(listBuf)
      print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
      println("Operator expected")
    }
  }

  def DataVariables(listBuf: List[syntaxToken]):Int = {
    var flag  = false
    if(listBuf.length==1){
        if(listBuf(0).typeOfLexem=="User ident"){
          printList(listBuf)
          print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
          println("Expected: instruction or directive")
        }
        else if(listBuf(0).token=="DD" ||listBuf(0).token=="DQ"){
          printHex(bytesInt)
          printList(listBuf)
          print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
          println("Operand expected")
        }
        else {
          printList(listBuf)
          print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
          println("Extra characters on line")
        }
    }
    else if(listBuf.length==2){
      if(listBuf(0).typeOfLexem=="User ident"){
        if(listBuf(1).token.toLowerCase=="dd" ||listBuf(1).token.toLowerCase=="dq"){
          printHex(bytesInt)
          printList(listBuf)
          nameIdentData+=nameIdData(bytesInt, listBuf(0).token)
          print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
          println("Operand expected")
        }
        else {
          printList(listBuf)
          print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
          println("Expected: instruction or directive")
        }
      }
      else if(listBuf(0).token.toLowerCase=="dd"){
        printHex(bytesInt)
        printList(listBuf)
        print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
        println("Symbol not defined: "+listBuf(1).token)
        bytesInt+=4
      }
      else if (listBuf(0).token.toLowerCase=="dq"){
        printHex(bytesInt)
        printList(listBuf)
        print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
        println("Symbol not defined: "+listBuf(1).token)
        bytesInt+=8
      }
    }
    else if(listBuf.length>=3){
      if(listBuf(0).token.toLowerCase=="dd"){
        printHex(bytesInt)
        bytesInt+=4
        DDDQFirst(listBuf)
      }
      else if (listBuf(0).token.toLowerCase=="dq"){
        printHex(bytesInt)
        bytesInt+=8
        DDDQFirst(listBuf)
      }
      else if(listBuf(0).typeOfLexem=="Dec constant"){
          printList(listBuf)
        print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
        println("Extra characters on line")
      }
      else if(listBuf(0).typeOfLexem=="User ident"&&(!(listBuf(1).token.toLowerCase=="dq"||listBuf(1).token.toLowerCase=="dd"))){
        printList(listBuf)
        print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
        println("Expected: instruction or directive")
      }
      else if(listBuf(0).typeOfLexem=="User ident"&&(listBuf(2).typeOfLexem=="User ident"||listBuf(2).typeOfLexem=="Directive")&&(listBuf(1).token.toLowerCase=="dq"||listBuf(1).token.toLowerCase=="dd")){
        printHex(bytesInt)
        printList(listBuf)
        print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
        println("Symbol not defined: "+listBuf(2).token)
        if (listBuf(1).token.toLowerCase=="dq")
          bytesInt+=8
        if (listBuf(1).token.toLowerCase=="dd")
          bytesInt+=4
      }
      else {
        if(!ErrorList.filter(_.row==listBuf(0).row).isEmpty) {
          printList(listBuf)
          print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
          println("Extra characters on line")
        }
        else {
          printHex(bytesInt)
          printList(listBuf)
          nameIdentData += nameIdData(bytesInt, listBuf(0).token)
          if (listBuf(1).token.toLowerCase == "dq")
              bytesInt += 8
          if (listBuf(1).token.toLowerCase == "dd")
            bytesInt += 4
          if(listBuf.length!=3){
            print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
            println("Syntax error")
          }
        }
      }
    }
    return 0
  }

  def faddF(listBuf: List[syntaxToken]):Boolean= {
    printHex(bytesInt)
    printList(listBuf)
    if(DataBegin(0).toString.toLowerCase()=="data"){
          if((!listBuf.filter(_.token.toLowerCase=="ss").isEmpty)){
            if(!listBuf.filter(_.token.toLowerCase=="ebp").isEmpty)bytesInt+=6
            else bytesInt+=7
          }
          if((!listBuf.filter(_.token.toLowerCase=="ds").isEmpty)||listBuf(3).typeOfLexem == "User ident"){
            if(nameIdentData.filter(_.name==listBuf(3).token).isEmpty&&(listBuf.length==7)&&(!listBuf.filter(_.token.toLowerCase=="esp").isEmpty)) bytesInt+=7
            else if(nameIdentData.filter(_.name==listBuf(3).token).isEmpty&&(listBuf.length==7)&&(!listBuf.filter(_.token.toLowerCase=="ebp").isEmpty)) bytesInt+=6
            else if(!listBuf.filter(_.token.toLowerCase=="esp").isEmpty)bytesInt+=8
            else if(!listBuf.filter(_.token.toLowerCase=="ebp").isEmpty)bytesInt+=7
            else bytesInt+=6
          }
          if((!listBuf.filter(_.token.toLowerCase=="cs").isEmpty)||
            (!listBuf.filter(_.token.toLowerCase=="es").isEmpty)||
            (!listBuf.filter(_.token.toLowerCase=="gs").isEmpty)||
            (!listBuf.filter(_.token.toLowerCase=="fs").isEmpty))
          {
            if(!listBuf.filter(_.token.toLowerCase=="esp").isEmpty)bytesInt+=8
            else bytesInt+=7
          }
    }
    else{
      if((!listBuf.filter(_.token.toLowerCase=="ss").isEmpty)){
        if(!listBuf.filter(_.token.toLowerCase=="ebp").isEmpty)bytesInt+=3
        else bytesInt+=4
      }
      if((!listBuf.filter(_.token.toLowerCase=="ds").isEmpty)||listBuf(3).typeOfLexem == "User ident"){
        if(nameIdentData.filter(_.name==listBuf(3).token).isEmpty&&(listBuf.length==7)&&(!listBuf.filter(_.token.toLowerCase=="esp").isEmpty)) bytesInt+=7
        else if(nameIdentData.filter(_.name==listBuf(5).token).isEmpty&&(listBuf.length==9)&&(!listBuf.filter(_.token.toLowerCase=="esp").isEmpty)) bytesInt+=8
        else if(nameIdentData.filter(_.name!=listBuf(3).token).isEmpty&&(listBuf.length==7)&&(!listBuf.filter(_.token.toLowerCase=="esp").isEmpty)) bytesInt+=4
        //else if(nameIdentData.filter(_.name==listBuf(3).token).isEmpty&&(listBuf.length==7)&&(!listBuf.filter(_.token.toLowerCase=="ebp").isEmpty)) bytesInt+=6
        else if(!listBuf.filter(_.token.toLowerCase=="esp").isEmpty)bytesInt+=5
        else if(!listBuf.filter(_.token.toLowerCase=="ebp").isEmpty)bytesInt+=4
        else bytesInt+=3
      }
      if((!listBuf.filter(_.token.toLowerCase=="cs").isEmpty)||
        (!listBuf.filter(_.token.toLowerCase=="es").isEmpty)||
        (!listBuf.filter(_.token.toLowerCase=="gs").isEmpty)||
        (!listBuf.filter(_.token.toLowerCase=="fs").isEmpty))
      {
        if(!listBuf.filter(_.token.toLowerCase=="esp").isEmpty)bytesInt+=5
        else bytesInt+=4
      }
    }
    if (!(listBuf(1).typeOfLexem == "Type of ident")) return false
    if (!(listBuf(2).typeOfLexem == "Operator of type def")) return false
    if (!(listBuf(3).typeOfLexem == "Segment reg" || listBuf(3).typeOfLexem == "User ident")) return false

    if (!(listBuf(4).token == ":"||listBuf(4).token == "[")) return false
    if (!(listBuf(5).typeOfLexem == "User ident"||listBuf(5).typeOfLexem == "32b register")) return false

    if (!(listBuf(6).token == "["||listBuf(6).token == "]")) return false

    if(listBuf.length==9){
      if (!(listBuf(7).typeOfLexem == "32b register")) return false
      if (!(listBuf(8).token == "]")) return false
    }
    return true
  }
  def faddpF(listBuf: List[syntaxToken]):Boolean= {
  if(listBuf.length==10) {
    if (!(listBuf(1).token.toLowerCase == "st")) return false
    else {
      printHex(bytesInt)
      bytesInt+=2
    }
    if (!(listBuf(2).token == "(")) return false
    if (!(listBuf(3).typeOfLexem == "Dec constant" && (listBuf(3).token.toInt<8)&&(listBuf(3).token.toInt>=0))) return false
    if (!(listBuf(4).token == ")")) return false
    if (!(listBuf(5).token == ",")) return false
    if (!(listBuf(6).token.toLowerCase == "st")) return false
    if (!(listBuf(7).token == "(")) return false
    if (!(listBuf(8).typeOfLexem == "Dec constant" && (listBuf(8).token.toInt == 0))) return false
    if (!(listBuf(9).token == ")")) return false
  }
  else if(listBuf.length==1){
    return false
  }
  else if(listBuf.filter(_.typeOfLexem=="Coproces reg").isEmpty){
    return false
  }
  else if(listBuf.length!=10){
      printHex(bytesInt)
      bytesInt+=2
    return false
  }
  return true

}
  def fcomF(listBuf: List[syntaxToken]):Boolean= {
    if(listBuf.length==5){
      if((listBuf(1).typeOfLexem == "User ident")) {
        bytesInt+=4
        return false
      }
      if (!(listBuf(1).token.toLowerCase == "st")) return false
      if (!(listBuf(2).token == "(")) return false
      if (!(listBuf(3).typeOfLexem == "Dec constant" && (listBuf(3).token.toInt<8)&&(listBuf(3).token.toInt>=0))) return false
      if (!(listBuf(4).token == ")")) return false
    }
    else if(listBuf.length==1){
      return false
    }
    else if(listBuf.length>1){
      if ((listBuf(1).token.toLowerCase == "st")){
        if(listBuf.filter(_.token==",").isEmpty&&(!listBuf.filter(_.typeOfLexem=="User ident").isEmpty)) {
          bytesInt+=4
        }
        else if(!listBuf.filter(_.token==",").isEmpty&&(!listBuf.filter(_.typeOfLexem=="User ident").isEmpty)) {
         var co=0
          var tmp = 0
          var tmp1=0
          for (it <- listBuf ) {
            if(it.typeOfLexem=="User ident"){
                tmp1 = co}
            else if(it.token==","){
               tmp = co}
            co+=1
          }
          if(tmp1<tmp) bytesInt+=4
        }
      }
      else {
         bytesInt+=4
      }
      return false
    }
    return true
  }

  def CodeVariables(listBuf: List[syntaxToken]) :Unit= {
    listBuf(0).token.toLowerCase match{
      case "finit" =>{
        if(listBuf.length==1){
          printHex(bytesInt)
          printList(listBuf)
          bytesInt+=3
        }
        else if(listBuf(0).token.toLowerCase=="dq"||listBuf(0).token.toLowerCase=="dd"){
          printHex(bytesInt)
          printList(listBuf)
          print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
          println("Reserved word used as symbol: FINIT")
        }
        else {
          printHex(bytesInt)
          printList(listBuf)
          print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
          println("Extra characters on line")
          bytesInt+=7
        }
      }
      case "fadd" =>{
        if(listBuf.length==7 || listBuf.length==9){
          if(faddF(listBuf)){
            if(listBuf(3).typeOfLexem == "User ident"){
                if(nameIdentData.filter(_.name==listBuf(3).token).isEmpty){
                print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
                println("Symbol not defined:"+listBuf(3).token)
              }
            }
            if(listBuf(5).typeOfLexem == "User ident"){
              if(nameIdentData.filter(_.name==listBuf(5).token).isEmpty){
                print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
                println("Symbol not defined:"+listBuf(5).token)
              }
            }
          }
          else{
            print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
            println("Invalid syntax")
          }
        }
        else {
          printHex(bytesInt)
          printList(listBuf)
          print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
          println("Invalid syntax")
          bytesInt+=6
        }

      }
      case "faddp" =>{
        if(!faddpF(listBuf)){
          printList(listBuf)

          print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
          println("Invalid syntax")
        }
        else printList(listBuf)

      }
      case "fcom" =>{
        printHex(bytesInt)
        printList(listBuf)
        bytesInt+=2
        if(!fcomF(listBuf)){
          print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
          println("Invalid syntax")
        }
      }
    }
  }

  def SearchData():Unit={
   val br = LexicalList.iterator
    var bf  = br.next()

    for (listBuf <- SyntaxList) {
                    if (bf==LexicalList.last){}
                    else if(bf.isEmpty) {
                      while (bf.isEmpty){
                        bf = br.next()
                        println()
                      }
                      bf = br.next()
                    }
                    else  if(listBuf(0).row > bf(0).row){
                      for (go <- bf)
                        print(go.token)
                      println()
                      while (listBuf(0).row > bf(0).row) {
                        bf = br.next()
                        if(bf.isEmpty) {
                          while (bf.isEmpty){
                            println()
                            bf = br.next()}
                        }
                      }
                      bf = br.next()
                    }
                    else bf = br.next()


      if(listBuf(0).token.toLowerCase=="end"&&(listBuf==SyntaxList.last)){
        printList(listBuf)
      }
      else if (!listBuf.filter(_.token.toLowerCase.contains("segment")).isEmpty){
          if(Databegin(listBuf)) {
            DataExist=true
            bytesInt = 0
          }
          printHex(bytesInt)
          printList(listBuf)
        }
        else if (!listBuf.filter(_.token.toLowerCase.contains("ends")).isEmpty ) {
        DataExist=false
        DataEnd+=listBuf(0).token
            if((DataBegin.isEmpty||DataBegin!=DataEnd)){
              printList(listBuf)
              print(fileName +".asm ("+listBuf(0).row+")"+ "\t")
              println("Block nesting error")}
            else {
              printHex(bytesInt)
              printList(listBuf)
            }
        }
        else if(listBuf(0).typeOfLexem=="Machine command"){
          CodeVariables(listBuf)
        }
        else {
          DataVariables(listBuf)
        }

      if (!DataExist&&(listBuf.filter(_.token.toLowerCase.contains("ends")).isEmpty )&&(!DataExist&&(listBuf.filter(_.token.toLowerCase.contains("end")).isEmpty )))
        println("Data emitted with no segment")
      }
    if(SyntaxList.last.filter(_.token.toLowerCase=="end").isEmpty) {
      print(fileName +".asm ("+(LexicalList.length+1)+")"+ "\t")
      println("End of file, no END directive")
    }

    if(DataEnd!=DataBegin&&(DataBegin.length>DataEnd.length)){
      println("Open segments")
    }
  }

  def SearchSegment():Unit = {
   SearchData()
 }
}
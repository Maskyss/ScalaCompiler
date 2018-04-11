package main
import main.Helper._

class SyntaxAnalizer{

  def printTable():Unit={

    var beginNumInLine = 1
    var countLexem = 1
    var numOperand = 1
    var firstAppearRow = 0
    var str:String = ""

    for(listBuf<- SyntaxList) {
      firstAppearRow=0
      for(go<-listBuf){
        if(firstAppearRow==0)
          Console.out.print(Console.BLUE +"ROW: "+ Console.RESET+go.row +" ")//print line number from a new line

        if(go.typeOfLexem=="Dec constant"&&countLexem==1) {//separate checking for decemical numbers in data segment
          Console.out.print(Console.BLUE +"OP#" +1+": "+ Console.RESET+str+" ")
          printf("%s ", go.token)
          printf("(%d,%d) ", beginNumInLine, countLexem)
          beginNumInLine = 1
          countLexem = 1
        }

        if(check(go)&&str.isEmpty){
          if(go.typeOfLexem=="User ident"&&countLexem==1)//checking user ident in data segment and code segment
            Console.out.print(Console.BLUE +"LABEL: " + Console.RESET+go.token+" ")

          printf("(%d,%d) ",beginNumInLine,countLexem)
          countLexem=1
          beginNumInLine+=countLexem
        }
        else {
              if(go.token==",") {
                beginNumInLine+=countLexem
                numOperand+=1

              }
              else {
                countLexem+=1
                str +=go.token
                if(go.typeOfLexem=="Type of ident"||go.token.toLowerCase()=="ptr")
                  str+=" "
              }

              if(go.token=="]"||go.token==")"){
                  countLexem-=1
                Console.out.print(Console.BLUE +"OP#" +numOperand+": "+ Console.RESET+str+" ")
                printf("(%d,%d) ",beginNumInLine,countLexem)
                beginNumInLine+=countLexem
                str =""
                countLexem =1
              }
        }
        firstAppearRow+=1
      }
      println()
      numOperand=1
      beginNumInLine=1
      countLexem =1
      str =""
    }
  }
  def check(syn:syntaxToken): Boolean ={
    syn.typeOfLexem match {
      case "Directive" =>{
        Console.out.print(Console.BLUE +"COMM/DIR: " + Console.RESET+syn.token+" ")
        return true
      }
      case "Machine command" =>
      {
        Console.out.print(Console.BLUE +"COMM/DIR: " + Console.RESET+syn.token+" ")
        return true
      }
      case "User ident" =>
      {
        return true
      }
      case _ => return false
    }
  }
}

package main
import main.Helper._

class SyntaxAnalizer(lexAnalizer: LexAnalizer){
  trait SynTable
  def printTable():Unit={

    var tmp = 1
    var count = 1
    var buf = 1
    var si = 0
    var str:String = ""
    for(listBuf<- SyntaxList) {
      si=0
      for(go<-listBuf){
        if(si==0)
          Console.out.print(Console.BLUE +"ROW: "+ Console.RESET+go.row +" ")

        if(go.typeOfLexem=="Dec constant"&&count==1) {
          printf("%s ", go.token)
          printf("(%d,%d) ", tmp, count)
          tmp = 1
          count = 1
        }

        if(check(go)&&str.isEmpty){
          if(go.typeOfLexem=="User ident"&&count==1)
            Console.out.print(Console.BLUE +"LABEL: " + Console.RESET+go.token+" ")

          printf("(%d,%d) ",tmp,count)
          count=1
          tmp+=1
        }
        else {
              if(go.token==",") {
                tmp+=1
                buf+=1
              }
              else {

                count+=1
                str +=go.token
                if(go.typeOfLexem=="Type of ident"||go.token.toLowerCase()=="ptr")
                  str+=" "
              }

              if(go.token=="]"||go.token==")"){
                count-=1
                Console.out.print(Console.BLUE +"OP#" +buf+": "+ Console.RESET+str+" ")
                printf("(%d,%d) ",tmp,count)
                str =""
                tmp=1
                count =1
              }
        }
        si+=1
      }
      println()
      buf=1
      tmp=1
      count =1
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

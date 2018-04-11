package main
import scala.collection.mutable.ListBuffer

object Helper {
  var fileName:String=""
  val numbers = "1234567890"
  val dividers = ":,()[] "
  val commentar = ";"
  def isNum(ch:Char):Boolean = numbers.contains(ch)
  def isDiv(ch:Char):Boolean = dividers.contains(ch)

  val machineComm = List("finit","fcom","fadd","faddp")
  val directives = List("end", "segment", "ends", "dq", "dd")
  val regGeneral32 = List("eax", "ebx", "ecx", "edx", "esi", "edi", "ebp", "esp")
  val typeOfIdent = List("dword","qword")
  val ptr = "ptr"
  val st = "st"
  val regSegment  = List("fs","gs", "cs", "ss", "ds", "es")


  def checkWhat(tempTok:String):String ={
    tempTok match {
      case tempTok if dividers.contains(tempTok.toLowerCase) => baseDelim().setName()
      case tempTok if regSegment.contains(tempTok.toLowerCase) => baseRegSegment().setName()
      case tempTok if typeOfIdent.contains(tempTok.toLowerCase) => baseTypeofIdent().setName()
      case tempTok if regGeneral32.contains(tempTok.toLowerCase) => baseReg32().setName()
      case tempTok if directives.contains(tempTok.toLowerCase) => baseDirect().setName()
      case tempTok if machineComm.contains(tempTok.toLowerCase) => baseMachineCom().setName()
      case tempTok if ptr.contains(tempTok.toLowerCase) => baseOperofDef().setName()
      case tempTok if st.contains(tempTok.toLowerCase) => baseCopReg().setName()
      case tempTok if checkInt(tempTok)=> baseDec().setName()
      case tempTok if tempTok.contains(";") => baseComment().setName()
      case _ => baseUserId().setName()
    }
  }
  def checkInt(t:String):Boolean = t.matches(floatPat)


  //val intPat = """0|(\+|-)?[1-9][0-9]*[a-z]?"""
  val floatPat =  """([^[a-zA-Z]]*(-)?[0-9]*[dD]?\.[0-9]*)|(-)?[0-9][0-9]*[dD]?"""
  val operatorPat = """\[|]|\(|\)|\,|:|;|!="""
  val idPat = """[0-9]*?[a-zA-Z][0-9a-zA-Z]*"""
  val tokenPat = (idPat + "|"  +floatPat+ "|" + operatorPat).r

  var Error = false
  var WarningError = 0
  var ErrorList = new ListBuffer[ErrorT]
  case  class ErrorT(row:Int, typeOfError:String)

  var LexicalList = new ListBuffer[List[mainToken]]
  var SyntaxList = new ListBuffer[List[syntaxToken]]
  case class mainToken(row:Int,  token:String)
  case class syntaxToken(row:Int, token:String,typeOfLexem:String)

trait base
  case class baseDelim() {
     def setName():String = "Single lexem"
  }
  case class baseRegSegment(){
     def setName():String = "Segment reg"
  }
  case class baseTypeofIdent(){
    def setName():String  = "Type of ident"
  }
  case class baseReg32() {
    def setName():String = "32b register"
  }
  case class baseDirect(){
    def setName():String = "Directive"
  }
  case class baseMachineCom(){
    def setName():String = "Machine command"
  }
  case class baseOperofDef() {
    def setName():String ="Operator of type def"
  }
  case class baseDec() {
    def setName():String ="Dec constant"
  }
  case class baseCopReg(){
    def setName():String ="Coproces reg"
  }
  case class baseUserId(){
    def setName():String ="User ident"
  }
  case class baseComment(){
    def setName():String ="Comment"
  }

}

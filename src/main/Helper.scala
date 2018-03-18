package main

import java.io.FileNotFoundException

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Helper {

  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toList
  val numbers = "1234567890".toList
  val dividers = ";:,()[] "
  def isAlph(ch:Char):Boolean = alphabet.contains(ch)
  def isNum(ch:Char):Boolean = numbers.contains(ch)
  def isDiv(ch:Char):Boolean = dividers.contains(ch)

  val machineComm = List("finit","fcom","fadd","faddp")
  val directives = List("end", "segment", "ends", "dq", "dd")
  val regGeneral32 = List("eax", "ebx", "ecx", "edx", "esi", "edi", "ebp", "esp")
  val typeOfIdent = List("dword","qword")
  val ptr = "ptr"
  val st = "st"
  val regSegment  = List("fs"," gs", "cs", "ss", "ds", "es")

  case  class ErrorT(str:String,row:Int, fl:Boolean)
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
      case _ => baseUserId().setName()
    }
  }


  def checkInt(t:String):Boolean = t.matches(floatPat)


  //val intPat = """0|(\+|-)?[1-9][0-9]*[a-z]?"""
  val floatPat =  """([^[a-zA-Z]]*(-)?[0-9]*[dD]?\.[0-9]*)|0|(-)?[1-9][0-9]*[dD]?"""
  val operatorPat = """\[|]|\(|\)|\,|:|!="""
  val idPat = """[0-9]*?[a-zA-Z][0-9a-zA-Z]*"""
  val tokenPat = (idPat + "|"  +floatPat+ "|" + operatorPat).r

  var Error = false
  var LexicalList = new ListBuffer[List[mainToken]]
  var SyntaxList = new ListBuffer[List[syntaxToken]]
  case class mainToken(row:Int,  token:String)
  case class syntaxToken(row:Int, token:String,typeOfLexem:String)
  trait BaseLexem
  case class baseDelim() extends BaseLexem {
     def setName():String = "Single lexem"
  }
  case class baseRegSegment() extends BaseLexem {
     def setName():String = "Segment reg"
  }
  case class baseTypeofIdent() extends BaseLexem {
    def setName():String  = "Type of ident"
  }
  case class baseReg32() extends BaseLexem {
    def setName():String = "32b register"
  }
  case class baseDirect() extends BaseLexem {
    def setName():String = "Directive"
  }
  case class baseMachineCom() extends BaseLexem {
    def setName():String = "Machine command"
  }
  case class baseOperofDef() extends BaseLexem {
    def setName():String ="Operator of type def"
  }
  case class baseDec() extends BaseLexem {
    def setName():String ="Dec constant"
  }
  case class baseCopReg() extends BaseLexem {
    def setName():String ="Coproces reg"
  }
  case class baseUserId() extends BaseLexem {
    def setName():String ="User ident"
  }

}

package main

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
  val directives = List("END", "SEGMENT", "ENDS", "DQ", "DD")
  val regGeneral32 = List("eax", "ebx", "ecx", "edx", "esi", "edi", "ebp", "esp")
  val typeOfIdent = List("dword","qword")
  val ptr = "ptr"
  val regSegment  = List("fs"," gs", "cs", "ss", "ds", "es")
  val lexemType = List( "Machine command","Directive","User identif","Dec const",
                        "32b register","Type of ident","Operator of type def",
                        "Segment reg","Single lexem","Dec const")
  def checkWhat(map:mutable.Map[Int,ListBuffer[String]]):Unit ={

  }
  def printTable(k:Int):Unit={

  }


  //val intPat = """0|(\+|-)?[1-9][0-9]*[a-z]?"""
  val floatPat =  """((-)?[0-9]*[a-z]?\.[0-9]*)|0|(\+|-)?[1-9][0-9]*[a-z]?"""
  val operatorPat = """\[|]|\(|\)|\,|:|!="""
  val idPat = """[a-zA-Z][0-9a-zA-Z]*"""
  val tokenPat = (floatPat + "|" + idPat + "|" + operatorPat).r
  var Error = false


  case class rowToken(row:Int,rowIndex:Int,token:String)
 trait BaseLexem{
   def print()
   def len()
 }
}

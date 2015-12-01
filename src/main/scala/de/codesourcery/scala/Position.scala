package de.codesourcery.scala

case class Position(val x:Int,val y:Int) 
{
  def moveDown() : Position = Position(x,y+1)
  def left() : Position = Position(x-1,y)
	def right() : Position = Position(x+1,y)
}
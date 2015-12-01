package de.codesourcery.scala

import scala.collection.mutable.ListBuffer
import java.awt.Color

class Board(val width:Int,val height:Int) 
{
  // array holds RGB cell colors with 0 == empty cell
  private[this] val grid : Array[Array[Int]] = Array.ofDim[Int](width,height)
  
  private[this] var currentTile:Tile = null
  
  def newTile(t:Tile) : Boolean =
  {
    val toAdd = t.withPosition( Position( width/2 - t.width/2 , 0 ) ) 
    if ( clashes( toAdd ) ) 
    {
      return false
    }
    currentTile = toAdd
    true
  }
  
  def reset() 
  {
    for ( x <- 0 until width ; y <- 0 until height) 
    {
      grid(x)(y) = 0
    }
    currentTile = null
  }
  
  def visitPopulatedCells(visitor: (Int,Int,Int) => Unit  ) 
  {
    for ( x <- 0 until width ; y <- 0 until height) 
    {
       val color = grid(x)(y) match 
       {
         case 0 if ( currentTile != null && currentTile.contains( x , y ) ) => if ( currentTile.isSet( x , y ) ) currentTile.color else 0
         case _ @ x => x
       }
       if ( color != 0 ) {
         visitor(x,y,color)
       }
    }
  }
  
  def moveLeft() : Boolean = 
  {
    if ( currentTile.position.x > 0 && ! clashes( p => p.left() ) ) {
      currentTile = currentTile.withPosition( currentTile.position.left() )
      true
    } else {
      false
    }
  }
  
  def moveRight() : Boolean = 
  {
    if ( currentTile.maxX < width && ! clashes( p => p.right() ) ) {
      currentTile = currentTile.withPosition( currentTile.position.right() )
      true
    } else {
      false
    }
  }  
  
  def rotateCW() : Boolean = mayBeRotate( currentTile.rotateCW )
  
  def rotateCCW() : Boolean = mayBeRotate( currentTile.rotateCCW ) 
  
  private[this] def mayBeRotate( rotated: Tile ) : Boolean = 
  {
    if ( rotated.maxX >= width  || rotated.maxY >= height || clashes( rotated ) ) 
    {
      return false
    }
    currentTile = rotated
    true
  }
  
  private[this] def canMoveDown() : Boolean = 
  {
    if ( currentTile.maxY >= height || clashes( currentTile.withPosition( currentTile.position.moveDown() ) ) ) {
      false
    } else {
      true
    }
  }
  
  /**
   * 
   * @return number of collapsed rows
   */
  def dropTile() : Int =  
  {
     while ( canMoveDown() ) {
       moveTileDown()
     }
     
     commitTile()
     
     var rowsCollapsed = false
     var clearedRows = 0
     do 
     {
       rowsCollapsed = false
       for( y <- height-1 to 1 by -1 )
       {
         while ( isRowOccupied( y ) ) 
         {
        	 clearedRows += 1
           collapseRow( y )
           rowsCollapsed = true
         }
       }
     } while ( rowsCollapsed )
     clearedRows
  }
  
  private[this] def collapseRow(rowY:Int) = 
  {
    for ( y <- rowY until 0 by -1 ; x <- 0  until width ) { 
    	grid(x)(y)=grid(x)(y-1)    
    }
  }  
  
  private[this] def commitTile() 
  {
    for ( x <- 0 until currentTile.width ; y <- 0 until currentTile.height  if ( currentTile.isSetLocal(x,y) ) ) 
    {
    	grid( currentTile.position.x + x )( currentTile.position.y + y ) = currentTile.color
    }
    currentTile = null
  }
  
  def moveTileDown() : Boolean = 
  {
    if ( canMoveDown() ) 
    {
      currentTile = currentTile.withPosition( currentTile.position.moveDown() )
      true  
    } else {
      false
    }
  }
  
  private[this] def isRowOccupied(y:Int) : Boolean = 
  {
    for( x <- 0 until width )
    {
       if ( grid(x)(y) == 0 ) {
         return false
       }
    }
    true    
  }
  
  private[this] def clashes( func : Position => Position ) : Boolean = clashes( currentTile.withPosition( func( currentTile.position ) ) )
  
  private[this] def clashes(t:Tile) : Boolean = 
  {
    for ( x <- 0 until t.width ; y <- 0 until t.height if ( t.isSetLocal(x,y) ) ) 
    {
       if ( grid(t.position.x + x)( t.position.y +y ) != 0) {
         return true
       }
    }
    false
  } 
}
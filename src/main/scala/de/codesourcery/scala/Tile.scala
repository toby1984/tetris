package de.codesourcery.scala

class Tile(val width:Int,val height:Int,val position:Position,val color:Int) {
  
  private val data : Array[Array[Boolean]] = Array.ofDim[Boolean]( width , height )
  
  def this(width:Int,height:Int,color:Int) = this(width,height,Position(0,0),color)
  
  def rotateCW() : Tile = 
  {
    val result = new Tile(height,width,position,color)
    for ( w <- 0.until( width ) ; h <- 0.until(height) ) 
    {    
      result.data(h)(w) = this.data(w)(height-1-h)
    }
    result
  }
  
  def rotateCCW() : Tile = 
  {
    val result = new Tile(height,width,position,color)
    for ( w <- 0.until( width ) ; h <- 0.until(height) ) 
    {    
      result.data(h)(w) = this.data(width-1-w)(h)
    }
    result
  }    
  
  def maxX : Int = position.x + width
  
  def maxY : Int = position.y + height
  
  def set(x:Int,y:Int) : Tile = {
    data(x)(y) = true
    this
  }
  
  def visitPopulatedCells( func : (Int,Int,Int) => Unit ) 
  {
    for ( x <- 0 until width ; y <- 0 until height if data(x)(y) ) 
    {
      func( x , y , color )
    }  
  }
  
  def copy() : Tile =  new Tile( width , height , position, color ).withDataArrayFrom( this )
  
  private def withDataArrayFrom(t : Tile ) : Tile = {
    for ( x <- 0 until width ; y <- 0 until height ) {
      this.data(x)(y) = t.data(x)(y)
    }    
    this
  }
  
  def withPosition( newPos: Position ) : Tile = new Tile( width , height , newPos , color ).withDataArrayFrom( this )
  
  def isSet(x:Int,y:Int) : Boolean = data( x - position.x )( y - position.y )
  
  def isSetLocal(x:Int,y:Int) : Boolean = data( x )( y )
  
  def contains(x:Int,y:Int) : Boolean = x >= position.x && x < position.x + width && y >= position.y && y < position.y + height
}
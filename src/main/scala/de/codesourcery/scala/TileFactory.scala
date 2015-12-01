package de.codesourcery.scala

import scala.collection.mutable.ListBuffer
import java.util.Random
import java.awt.Color
import java.awt.Color

class TileFactory(random:Random) 
{
  def this() {
    this( new Random(0xdeadbeef) )
  }
  
  private[this] var nextTile : Tile = null
  
  def peekNextTile() : Tile = 
  {
    if ( nextTile == null ) {
      nextTile = createRandomTile()
    }
    nextTile
  }
  
  private[this] val tiles =   
  {
    val result = ListBuffer[Tile]();
   
    implicit def colToInt(col:Color) : Int = col.getRGB
    
    /* x
     * x
     * x
     * x
     */
    result += new Tile(1,4,Color.CYAN ).set(0,0).set(0,1).set(0,2).set(0,3)
    
    /* x
     * x
     * xx
     */
    result += new Tile(2,3,Color.ORANGE).set(0,0).set(0,1).set(0,2).set(1,2)
    
    /*  x
     *  x
     * xx
     */
    result += new Tile(2,3,Color.BLUE).set(1,0).set(1,1).set(1,2).set(0,2)
    
    
    /*  x
     * xxx
     */
    result += new Tile(3,2,Color.MAGENTA).set(1,0).set(0,1).set(1,1).set(2,1)
    
    /* xx
     * xx
     */
    result += new Tile(2,2,Color.YELLOW).set(0,0).set(0,1).set(1,0).set(1,1)   
    
    /* x
     * xx
     *  x
     */
    result += new Tile(2,3,Color.GREEN).set(0,0).set(0,1).set(1,1).set(1,2)
    
    /*  x
     * xx
     * x
     */
    result += new Tile(2,3,Color.RED).set(1,0).set(1,1).set(0,1).set(0,2)      
  }
  
  private[this] def createRandomTile() : Tile = tiles( random.nextInt( tiles.size ) ).copy()
  
  def randomTile() : Tile = 
  {
    if ( nextTile == null ) {
      nextTile = createRandomTile()
    }
    val result = nextTile
    nextTile = createRandomTile()
    result
  }
}
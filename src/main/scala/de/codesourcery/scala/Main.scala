package de.codesourcery.scala

import javax.swing.JFrame
import javax.swing.JPanel
import java.awt.Graphics
import java.awt.Color
import java.awt.Dimension
import javax.swing.Timer
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.awt.Font
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.geom.AffineTransform
import java.awt.image.AffineTransformOp
import java.awt.BasicStroke

object Main 
{
  val INITIAL_SPEED = 30
  
  var currentSpeed = INITIAL_SPEED
  var ticksUntilMove = INITIAL_SPEED
  
  var gameOver = false
      
  val tileFactory = new TileFactory  
  
  val board = new Board( 10,15)
  
  val monospaced = new Font( "Monospaced", Font.BOLD , 14 );
  
  var rowsCleared = 0
  	  
  private def round(value:Double) : Int = Math.round( value ).asInstanceOf[Int]
  
  private def restart()
  {
    gameOver = false
    currentSpeed = INITIAL_SPEED
    ticksUntilMove = INITIAL_SPEED
    rowsCleared = 0
    board.reset()
    if ( ! board.newTile( tileFactory.randomTile() ) ) {
      throw new IllegalStateException("Unrechable code reached")
    }
  }
  
  private def dropTile() 
  {
    rowsCleared += board.dropTile()
    if ( ! board.newTile( tileFactory.randomTile() ) ) {
      gameOver = true
    }
  }
  
  final class GridRenderer(g:Graphics,width:Int,height:Int,gridWidth:Int,gridHeight:Int) extends ( (Int,Int,Int) => Unit ) 
  {
    val scaleX = Math.floor( width / gridWidth.asInstanceOf[Float] ).asInstanceOf[Int]
    val scaleY = Math.floor( height / gridHeight.asInstanceOf[Float] ).asInstanceOf[Int]
 
    val maxX = scaleX * gridWidth
    val maxY = scaleY * gridHeight
    
    g.clearRect(0 , 0 , width , height )
    
    g.setColor( Color.BLACK )
    for ( x <- 0 to gridWidth ) {
       g.drawLine( round( x*scaleX ) , 0, round(x*scaleX) , maxY )
    }
    for ( y <- 0 to gridHeight) {
       g.drawLine( 0, round( y*scaleY ) , maxX , round( y*scaleY ) )
    }
        
    def apply(x:Int,y:Int,color:Int) : Unit = 
    {
        val px = round( x*scaleX )+1
        val py = round( y*scaleY )+1
        g.setColor( new Color( color ) )
        g.fillRect( px , py , round( scaleX-1 ) , round( scaleY-1 ) )
    }
  }
  
  private def leftPad(len:Int)(s:String) : String =  len - s.length match 
  { 
      case delta if delta > 0 => ( " " * delta ) +s
      case _ => s
  }
  
  private def rightPad(len:Int)(s:String) : String =  len - s.length match 
  { 
      case delta if delta > 0 => s + ( " " * delta )
      case _ => s
  }
  
  def main(args:Array[String]) 
  {
    val frame = new JFrame("Tetris")
    frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE )
    
    val panel = new JPanel() 
    {
      override def paintComponent(graphics:Graphics) 
      {
        val g = graphics.asInstanceOf[Graphics2D]
        super.paintComponent(g)
        
        val borderWidth = getWidth/4
        
        {
          val xOffset = 10
          val yOffset = 10
          
          val width  = getWidth - xOffset - borderWidth
          val height = getHeight - yOffset - 10
          
          g.setTransform( AffineTransform.getTranslateInstance( xOffset , yOffset ) )
          board.visitPopulatedCells( new GridRenderer( g , width , height , board.width , board.height ) ) 
        }

        val xOffset = getWidth - borderWidth + borderWidth/5
        val yOffset = 10+3
        
        val width = borderWidth/2
        val height = width
                  
        {
          g.setTransform( AffineTransform.getTranslateInstance( xOffset , yOffset ) )
          val nextTile = tileFactory.peekNextTile()
          nextTile.visitPopulatedCells( new GridRenderer( g , width , height , 4 , 4 ) ) 
          g.setTransform( new AffineTransform() )
          g.setColor( Color.BLACK )
          val oldStroke = g.getStroke
          g.setStroke( new BasicStroke(3f) )
          g.drawRect( xOffset -2 , yOffset - 2 , width +3 , height +3 )
          g.setStroke(oldStroke)
        }        

        g.setTransform( new AffineTransform() )        
        g.setColor(Color.BLACK)
        g.setFont( monospaced )
        
        val metrics = g.getFontMetrics().getLineMetrics( "X" , g )
        val lineHeight = (metrics.getAscent + metrics.getDescent ).asInstanceOf[Int]*3/2
        
        val toPrint : Seq[(String,String)] = Array( "Score" -> 1800*rowsCleared , "Lines" -> rowsCleared , "Level" ->  (1+INITIAL_SPEED-currentSpeed) )
                                             .map( p => (p._1 , p._2.toString ) )
        
        val maxKeyLen = toPrint.map( _._1.length ).max
        val maxValueLen  = toPrint.map( _._2.length ).max
        
        val keyPad = rightPad( maxKeyLen ) _
        val valuePad = leftPad( maxValueLen ) _
        
        val y0 = yOffset + height + 2*lineHeight
        val lines = toPrint.map( p => keyPad( p._1 ) + " : " + valuePad( p._2 ) )
        for ( i <- 0 until lines.length ) 
        {
          g.drawString( lines(i) , xOffset , y0 + i*lineHeight )
        }
          
        if ( gameOver ) 
        {
          g.setColor(Color.RED)
          g.setFont( g.getFont().deriveFont( Font.BOLD , 48 ) )
          
          val msg = "Game Over !!!"
          val bounds = g.getFontMetrics.getStringBounds( msg , g )
          g.drawString("Game over!!" , round( getWidth/2 - bounds.getWidth/2 ) , round( getHeight/2 - bounds.getHeight/2 ) )
        }
      }
    }
	  
    val l = new KeyAdapter() 
    {
      override def keyTyped(ev:KeyEvent) 
      {
        val repaintNeeded = ev.getKeyChar match 
        {
          case ' '  if ! gameOver => { dropTile() ; true }
          case '\n' if   gameOver => { restart() ; true }
          case 'a'  if ! gameOver => board.moveLeft()
          case 'd'  if ! gameOver => board.moveRight()
          case 'w'  if ! gameOver => board.rotateCCW() 
          case 's'  if ! gameOver => board.rotateCW() 
          case _   => false
        }
        if ( repaintNeeded ) {
          panel.repaint()
        }
      }
    }

    panel.setBackground( Color.WHITE )
    panel.setPreferredSize( new Dimension(640,640 ) )
    panel.setMinimumSize( new Dimension(640,640 ) )
    
    frame.addKeyListener( l )
    frame.requestFocus()	  

    frame.getContentPane.add( panel )
    frame.pack()
    frame.setVisible(true)
    
    restart()
    
    val timer = new Timer( 16 , ev => 
    {
        if ( gameOver ) 
        {
          return
        }
        ticksUntilMove -= 1
        if ( ticksUntilMove > 0 ) 
        {
          return
        }       
        
        if ( ! board.moveTileDown() ) 
        {
          dropTile()
          if ( ! gameOver && (rowsCleared % 10 ) == 0 && currentSpeed >= 2 ) 
          {
            currentSpeed -= 1
          }
        }
        panel.repaint()
        ticksUntilMove = currentSpeed
    })
    timer.start()
  }  
}
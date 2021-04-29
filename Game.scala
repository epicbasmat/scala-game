package cw

import scala.collection.mutable.ListBuffer;


class Game(wall: List[(Int, Int)], bounty: List[(Int, Int, Int)], initialX: Int, initialY: Int) {
  
  //the current grid, a 10x10 field, where -1=empty, 0=wall, any positive number=bounty
  private var field: Array[Array[Int]] = Array.ofDim[Int](10, 10)

  private var score: Int = 0
  private var positionX: Int = initialX
  private var positionY: Int = initialY
  private var saveX: Int = -1
  private var saveY: Int = -1;
  
  
  //Blacklist bounties once picked up
  private var blacklist : ListBuffer[(Int, Int)] = ListBuffer();
  
  
  for (i <- 0 until 10; k <- 0 until 10) { 
      field(i)(k) = -1
  }
  wall.foreach(w => field(w._1)(w._2) = 0)
  bounty.foreach(w => field(w._1)(w._2) = w._3)

  def rpt(n: Int)(commands: => Unit) {
    for (i <- 1 to n) { commands }
  }

  //** COURSEWORK STARTS HERE **//
  
  //** "Easy" Methods**//
  
  
  def getPlayerPos(): (Int, Int) = {
    return (this.positionX, this.positionY); 
  }
  
  def getScore(): Int = {
    return score;
  }

  //Happy functions
  def arrowLeft() {  
    if (positionX > 0 && field(positionX - 1)(positionY) != 0) {
      this.positionX -= 1;
      checkBounty();
    }
  }
  
  def arrowRight() { 
    if (positionX < 9 && field(positionX + 1)(positionY) != 0) {
      this.positionX += 1;
      checkBounty();
    }
  }

  
  def arrowUp() {
     if (positionY > 0 && field(positionX)(positionY - 1) != 0) {
       positionY -= 1;
       checkBounty();
     }
  }
   
  def arrowDown() {
     if (positionY < 9 && field(positionX)(positionY + 1) != 0) {
       positionY += 1;
       checkBounty();
     }
  }
   
   def arrowLeft(i: Int) {
     rpt(i) (arrowLeft);
   }
   
   def arrowRight(i: Int) {
     rpt(i) (arrowRight);
   }
   
   def arrowUp(i: Int) {
     rpt(i) (arrowUp);
   }
   
   def arrowDown(i: Int) {
     rpt(i) (arrowDown);
   }
   
   def checkBounty() {   
     bounty.foreach( w => {
       if ((w._1, w._2) == getPlayerPos && (blacklist.contains(w._1, w._2) == false)) { 
        score += field(w._1)(w._2);
        field(w._1)(w._2) = -1;
        blacklist += ((w._1, w._2));
       } 
     });
     if (saveX != -1 || saveY != -1) {
       checkBounties();
     }
   }
   
   //** "Advanced" Methods**//
   
   def move(movement: String){
     movement.split("").map( x => {
       x match {
         case "r" => arrowRight();
         case "l" => arrowLeft();
         case "u" => arrowUp();
         case "d" => arrowDown();
       }
     });
   }
   
   def maxBounty(): Int = {
     var totalScore = 0;
     bounty.map( w => {
       totalScore += w._3;
     })
     
     return totalScore
   }
   
   
   def findBounty(bountyX:Int, bountyY:Int, btmLeftX:Int, btmLeftY:Int, topRightX:Int, topRightY:Int):Boolean = {
     if (((bountyX <= topRightX) && (bountyY >= topRightY)) && ((bountyX >= btmLeftX) && (bountyY <= btmLeftY))) {                  
       return true;
     } else {
       return false;
     }
   }
  
   
  // Calculates each corner of a rectangle given two positions              bl           br           tl           tr      area
  def rectangleLogic(saveX:Int, saveY:Int, positionX:Int, positionY:Int):((Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int)) = {
     var area:Int = 0;
     if (saveY == positionY) {
       if (saveX > positionX) {
         area = saveX - positionX;
         return ((positionX, positionY), (saveX, saveY), (positionX, positionY), (saveX, saveY), area);
       } else if (saveX < positionX) {
         area = positionX - saveX;
         return ((saveX, saveY), (positionX, positionY), (saveX, saveY), (positionX, positionY), area);
       }
     } else if (saveX == positionX) {
       if (saveY > positionY) {
         area = saveY - positionY;
         return ((positionX, positionY), (positionX, positionY), (saveX, saveY), (saveX, saveY), area);
       } else if (positionY > saveY) {
         area = positionY - saveY;
         return ((saveX, saveY), (saveX, saveY), (positionX, positionY), (positionX, positionY), area);
       }
     }
     if (saveX > positionX && saveY < positionY) { 
       area = (saveX - positionX + 1) * (positionY - saveY + 1); 
       return ((positionX, positionY), (saveX, positionY), (positionX, saveY), (saveX, saveY), area); 
     } else if (positionX > saveX && positionY < saveY) {
       area = (positionX - saveX + 1) * (saveY - positionY + 1); 
       return ((saveX, saveY), (positionX, saveY), (saveX, positionY), (positionX, positionY), area);
     } else if (saveX > positionX && saveY > positionY) {
       area = (saveX - positionX + 1) * (saveY - positionY + 1); 
       return ((positionX, saveY), (saveX, saveY), (positionX, positionY), (saveX, positionY), area);
     } else if (positionX > saveX && positionY > saveY) {
       area = (positionX - saveX + 1) * (positionY - saveY + 1); 
       return ((saveX, positionY), (positionX, positionY), (saveX, saveY), (positionX, saveY), area);
     }
     return ((-1,-1), (-1,-1), (-1, -1), (-1, -1), -1);
   }
   
   def checkBounties() {       
     var rectangleCoords = rectangleLogic(saveX, saveY, positionX, positionY);
     if (saveX != -1 && saveY != -1) {
       if (rectangleCoords._5 > 9) {
        bounty.map (w => {
          if (findBounty(w._1, w._2, rectangleCoords._1._1, rectangleCoords._1._2, rectangleCoords._4._1, rectangleCoords._4._2)) {
            if (field(w._1)(w._2) > 0) {
              score += field(w._1)(w._2);
              field(w._1)(w._2) = -1;
            }
          }
        })
       saveX = -1;
       saveY = -1;
       } 
     }
   }
   
   def suggestSolution():String = {
     var tempX = positionX
     var tempY = positionY
     var finalString = "";
     bounty.map(w => { 
       var move = suggestMove(w._1, w._2)
       println(move);
       if (move != "") {
          positionX = w._1;
          positionY = w._2;
       }
       finalString += move;
     })
     positionX = tempX;
     positionY = tempY;
     println(finalString);
     println("==");   
     return finalString;
   }  
   
   def suggestMove(suggestionX:Int, suggestionY:Int):String = { //Inefficient but homemade
     var theoreticalMoveX = positionX;
     var theoreticalMoveY = positionY;
     var differenceX = suggestionX - theoreticalMoveX;
     var differenceY = suggestionY - theoreticalMoveY;
     var switch:Boolean = false;
     var limiter:Int = 0;
     var checkXY : List[(Int, String)] = List();
     var finalString:String = "";
     
     if (differenceX < 0) {
       checkXY = checkXY:+((-1, "l"));
     } else if (differenceX > 0) {
       checkXY = checkXY:+((1, "r"));
     } else {
       checkXY = checkXY:+((0, ""));
     }
     
     if (differenceY < 0) {
        checkXY = checkXY:+((-1, "u"));
     } else if (differenceY > 0) {
        checkXY = checkXY:+((1, "d"));
     } else {
       checkXY = checkXY:+((0, ""));
     }
     
     if (suggestionX > 9 || suggestionY > 9) {
       return "";
     }
     
     while (limiter < 255 && differenceX != 0 && differenceY != 0) {    
       while (differenceX != 0 && switch == false) {
         if (field(theoreticalMoveX)(theoreticalMoveY) != 0) {
           theoreticalMoveX += checkXY(0)._1;
           finalString += checkXY(0)._2;
           differenceX = suggestionX - theoreticalMoveX;
           differenceY = suggestionY - theoreticalMoveY;
         }
         else {
           switch = true;
         }
       }
       while (differenceY != 0 && switch == false) {
         if (field(theoreticalMoveX)(theoreticalMoveY) != 0) {
           theoreticalMoveY += checkXY(1)._1;
           finalString += checkXY(1)._2;
           differenceX = suggestionX - theoreticalMoveX;
           differenceY = suggestionY - theoreticalMoveY;
         }
         else {
           switch = true;
         }
       }
       
       if (switch) {
         theoreticalMoveX = positionX;
         theoreticalMoveY = positionY;
         finalString = "";
         
         while (differenceY != 0 && switch == true) {
           if (field(theoreticalMoveX)(theoreticalMoveY) != 0) {
             theoreticalMoveY += checkXY(1)._1;
             finalString += checkXY(1)._2;
             differenceX = suggestionX - theoreticalMoveX;
             differenceY = suggestionY - theoreticalMoveY;
           }
           else {
             return "";
           }
         }
         while (differenceX != 0 && switch == true) {
           if (field(theoreticalMoveX)(theoreticalMoveY) != 0) {
               theoreticalMoveX += checkXY(0)._1;
               finalString += checkXY(0)._2;
               differenceX = suggestionX - theoreticalMoveX;
               differenceY = suggestionY - theoreticalMoveY;
             }
           else {
             return "";
           }
         }
       }
       limiter += 1;
     }
     return finalString;
   }
   
  //** COURSEWORK ENDS HERE**//
  
  /* --- The three save methods below are used by the unit tests to simulate certain conditions, do not change  --- */

  /**
   * Updates saveX and saveY to the current player position.
   */
  def save(): Unit = {
    saveX = positionX
    saveY = positionY
  }

  /**
   * Returns the current save position as a tuple, in (x,y) order.
   */
  def getSavePos(): (Int, Int) = {
    return (saveX, saveY);
  }

  /**
   * Sets the savePos to the values of the parameters.
   */
  def setSavePos(saveX: Int, saveY: Int): Unit = {
    this.saveX = saveX
    this.saveY = saveY
  }

}

object GameBuilder {

  def initialiseGame1(): Game = {
    return new Game(List((3, 0), (3, 1), (3, 2)), List((4, 1, 5), (3, 3, 10)), 0, 0)
  }

  def initialiseGame2(): Game = {
    return new Game(List((3,3) ,(3,4), (3,5), (5,3), (5,4), (5,5)), List((4,4,1), (6,3,1)), 3, 2)
  }

  def initialiseGame3(): Game = {
    return new Game(List((3,0), (3,1), (3,2)), List((4,1,5),(3,3,10)), 4, 1)
  }
}

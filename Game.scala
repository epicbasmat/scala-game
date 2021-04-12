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
  
  private var directionalArray: Array[Int] = Array[Int](4);
  
  
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
   
   
   //Argumentative functions
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
  
   //Back to regular scheduling
   
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
     if (((bountyX < topRightX) && (bountyY > topRightY)) && ((bountyX > btmLeftX) && (bountyY < btmLeftY))) {                  
       return true;
     } else {
       return false;
     }
   }
   
  def rectangleLogic(saveXCheck: Boolean, saveYCheck:Boolean) = {
     var area:Int = 0;
     //true -> saveX|Y is greater than positionX|Y
     //TODO: rewrite cuz lots of repeated code that could be  like findBounty.
     
     if (saveXCheck && saveYCheck) { //top left 
       area = (saveX - positionX + 1) * (saveY - positionY + 1);    
       if (area > 9) {
         bounty.map( w => {
           if (findBounty(w._1, w._2, positionX, saveY, saveX , positionY)) {
             if (field(w._1)(w._2) != -1) {
               score += field(w._1)(w._2);
             }
           } else {
             
           }
         });
         saveX = -1;
         saveY = -1;
       }
     } else if (saveXCheck == false && saveYCheck) { //top right
       area = (positionX - saveX + 1) * (saveY - positionY + 1);    
       if (area > 9) {
         bounty.map( w => {
           if (findBounty(w._1, w._2, saveX, saveY, positionX, positionY)) {
             if (field(w._1)(w._2) != -1) {
               score += field(w._1)(w._2);
             }
           } else {
           
           }
         });
         saveX = -1;
         saveY = -1;
       }
     } else if (saveXCheck && saveYCheck == false) { //bottom left 
       area = (saveX - positionX + 1) * (positionY - saveY + 1);    
       
       if (area > 9) {
         bounty.map( w => {
           if (findBounty(w._1, w._2, positionX, positionY, saveX, saveY)) {
             if (field(w._1)(w._2) != -1) {
               score += field(w._1)(w._2);
             }
           } else {
             
           }
         });
         saveX = -1;
         saveY = -1;
       } 
     } else if (saveXCheck == false && saveYCheck == false) {  // bottom right
       area = (positionX - saveX + 1) * (positionY - saveY + 1);  
       if (area > 9) {
         bounty.map( w => {
           if (findBounty(w._1, w._2, saveX, positionY, positionX, saveY)) {
             if (field(w._1)(w._2) != -1) {
               score += field(w._1)(w._2);
             }
           } else {
             
           }
         });
         saveX = -1;
         saveY = -1;
       }
     }
   }
   
   def checkBounties() {
       var saveXCheck:Boolean = false;
       var saveYCheck:Boolean = false;
       if (saveX > positionX) {
         saveXCheck = true;
       }; if (saveY > positionY) {
         saveYCheck = true;
       }       
       
       rectangleLogic(saveXCheck, saveYCheck);
   }
   
   def suggestSolution() {
     
   }
   
   def blockChecker(moveX:Int, moveY:Int, direction:Int): (Int, Int) = {

     direction match {
       case 0 => { //lr
         for (i <- 0 to 9){
           if (field(moveX)(i) > -1) {
             return (moveX, i);
           }
         }
       } case 1 => {//ud
           for (i <- 0 to 9){
             if (field(i)(moveY) > -1) {
               println(s"field($moveY, $i)");
               return (i, moveY);
             }
           }
        }   
     }
     return (-1, -1);
   }
   
   
   def suggestMove(suggestionX:Int, suggestionY:Int):String = { //Inefficient but homemade
     var theoreticalMoveX = positionX;
     var theoreticalMoveY = positionY;
     var differenceX = suggestionX - theoreticalMoveX;
     var differenceY = suggestionY - theoreticalMoveY;
     var checkXY : List[(Int, String)] = List();
     var limiter:Int = 0;
     var suggestString:String = "";
     
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
     
     println("==DIAGNOSTICS==");
     println(positionX, positionY);
     println(suggestionX, suggestionY);
     println(differenceX, differenceY);
     println(checkXY.mkString(""));
     
          if (checkXY(0)._1 != 0 && checkXY(1)._1 != 0) {
       if (theoreticalMoveX != suggestionX && (theoreticalMoveX < 10 || theoreticalMoveX > 0)) {
         while (theoreticalMoveX != suggestionX && limiter < 255) {
           theoreticalMoveX += checkXY(0)._1          
           if (theoreticalMoveX < 10 && theoreticalMoveX > 0) {
             if (field(theoreticalMoveX)(theoreticalMoveY) != -1) {
               suggestString += suggestString.concat(checkXY(0)._2);
             } else {
               if (blockChecker(theoreticalMoveX, theoreticalMoveY, 0) != (-1, -1)) { 
                 
               } else {
                 return "";
               }
             }
           }
           limiter += 1;
         }
       }
       limiter = 0;
       if (theoreticalMoveY != suggestionY) {
         while (theoreticalMoveY != suggestionX && limiter < 255) {
           theoreticalMoveY += checkXY(1)._1;
           if (theoreticalMoveY < 10 && theoreticalMoveY > 0) {
             if (blockChecker(theoreticalMoveX, theoreticalMoveY, 1) != (-1,-1)) {
               suggestString += suggestString.concat(checkXY(1)._2);
             } else {
               println("Invalid");
               return "";
             }
           }
           limiter += 1;
         }
       }
     }
     
     println(suggestString);

     return suggestString;
     
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

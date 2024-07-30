import scala.collection.mutable.Map
import scala.io.StdIn


object practical_6{
    
    def main(args : Array[String]):Unit ={
        var inventory1 : Map[Int,(String,Int,Double)]=Map()
        var inventory2 : Map[Int,(String,Int,Double)]=Map()
        var inventory3 : Map[Int,(String,Int,Double)]=Map()
        println("Do you want to ad products (0,1)")
        var a:Int=StdIn.readInt()
        var quantity:Int =0
        var price:Double = 0.0
        var product_id :Int = 0
        var name : String = " "
        while(a==1){ 
           
            println("Enter the product ID")
            product_id = StdIn.readInt()
            println("Enter the product Name")
            name = StdIn.readLine()
            println("Enter the product quantity")
            quantity = StdIn.readInt()
            println("Enter the product price for a piece")
            price = StdIn.readDouble()
            println("Do you want to continue (0,1)")
            a = StdIn.readInt()
            if(inventory1.contains(product_id)){
                addExistingProducts(inventory1,product_id,(name,quantity,price))

            }else{
            addProductsinvent1(inventory1,product_id,(name,quantity,price))
            }
           
        }
         println("Do you want to add data forinventory 02")
          var b:Int=StdIn.readInt()
           while(b==1){ 
           
            println("Enter the product ID")
            product_id = StdIn.readInt()
            println("Enter the product Name")
            name = StdIn.readLine()
            println("Enter the product quantity")
            quantity = StdIn.readInt()
            println("Enter the product price for a piece")
            price = StdIn.readDouble()
            println("Do you want to continue (0,1)")
            b = StdIn.readInt()
            if(inventory1.contains(product_id)){
                addExistingProducts(inventory2,product_id,(name,quantity,price))

            }else{
            addProductsinvent1(inventory2,product_id,(name,quantity,price))
            }
           
        }
        println(inventory1)
        println(inventory2)
        retrieveProductList(inventory1)
        retrieveProductList(inventory2)
        println("total value of inventory 01")
        println(totalVal(inventory1))
        println("total value of inventory 02")
        println(totalVal(inventory2))
        inventory3 = mergeInventories(inventory2,inventory1)
        println("total value of inventory 03")
        println(totalVal(inventory3))
        println(inventory3)
        checkInventory(inventory3)
        var c = 0
        println("What is the product ID that you want to check")
        c = StdIn.readInt()
        checkId(c,inventory3)
        
        

    }
    def addProductsinvent1(map: Map[Int,(String,Int,Double)],product_Id:Int,Value:(String,Int,Double)):Map[Int,(String,Int,Double)] = {
        map += (product_Id-> Value)

    }
    def retrieveProductList(map: Map[Int,(String,Int,Double)]):Unit={
        var a = map.values.map(_._1)
        println("the products list")
        a.foreach(println)
    
    }
    def addExistingProducts(map: Map[Int,(String,Int,Double)],product_Id:Int,Value:(String,Int,Double)): Unit = {
        val existingTuple = map.getOrElse(product_Id,("Unknown",0,0.0))
        val update = (Value._1,
                        existingTuple._2 + Value._2,
                        math.max(existingTuple._3,Value._3) )
                        map(product_Id) = update
    }
    def totalVal(map:Map[Int,(String,Int,Double)]):Double = {
        map.values.map{ case(_,quantity,price) => quantity*price }.sum
    }
    def checkInventory(map:Map[Int,(String,Int,Double)]):Unit ={
        if(map.isEmpty){
            println("This inventory is Empty")
        }else{
            println("This Inventory is not Empty")
        }
    }
   
    def mergeInventories(map1: Map[Int, (String, Int, Double)], map2: Map[Int, (String, Int, Double)]): Map[Int, (String, Int, Double)] = {
    val mergedMap = map1.clone() 
    for ((key, value) <- map2) {
      if (mergedMap.contains(key)) {
        val updatedValue = combineValues(mergedMap(key), value)
        mergedMap(key) = updatedValue
      } else {
        mergedMap += (key -> value)
      }
    }
    mergedMap
    }

    def combineValues(v1: (String, Int, Double), v2: (String, Int, Double)): (String, Int, Double) = {
    val (name1, quantity1, price1) = v1
    val (name2, quantity2, price2) = v2
    (name1, quantity1 + quantity2, math.max(price1, price2))
    }
    def checkId(x:Int,map1:Map[Int,(String,Int,Double)]):Unit ={
        if(map1.contains(x)){
            println("your product is avalable")
        }else{
            println("Your product is not available")
        }
    }
}

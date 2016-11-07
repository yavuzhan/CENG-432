object Simple extends App {
	val numbers = new Array[Int](3)
	numbers(0) = 10
	numbers(1) = 25
	numbers(2) = 30
	numbers.foreach( args => {
		if(args % 2 == 0){
		println(args * 2)
		}
		else{
		println(args * 3)
		}
	})
}
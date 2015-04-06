package exercises

object currying {
  def product1(f: Int => Int)(a: Int, b: Int): Int =
    if(a > b) 1
    else f(a) * product1(f)(a + 1, b)             //> product1: (f: Int => Int)(a: Int, b: Int)Int
    

  product1(x => x * x)(3, 4)                      //> res0: Int = 144
  
  def fact(n: Int): Int = product1(x => x)(1, n)  //> fact: (n: Int)Int
  
  fact (4)                                        //> res1: Int = 24
  
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if(a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
                                                  //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:
                                                  //|  Int)Int

 
  def product(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x * y, 1)(a, b)        //> product: (f: Int => Int)(a: Int, b: Int)Int
    
  product(x => x * x)(3, 4)                       //> res2: Int = 144
 
}
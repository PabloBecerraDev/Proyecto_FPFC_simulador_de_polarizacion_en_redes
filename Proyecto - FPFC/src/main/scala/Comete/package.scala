package object Comete {
    //Vector que contiene los valores de una distribución - debe sumar 1
    type DistributionValues = Vector[Double]
    //Vector que contiene las frecuencias
    type Frequency = Vector[Double]
    //Pareja de los datos anteriores
    type Distribution = (Frequency, DistributionValues)
    //Funcion que dada una distribution devuelve un double
    type MedidaPol = Distribution => Double

    //Función para sacar el minimo de la función auxiliar (tenemos que (pi,y) es convexa)
    def min_p(f:Double=>Double, min:Double, max:Double, prec:Double):Double = {
        if ((max-min) < prec) (max+min)/2 //Caso de parada
        else {
            val divideInterval = (max-min)/10
            val points = (0 to 10).map(i => min + i * divideInterval)
            points.map(p => (p, f(p))).minBy(_._2)
            val newMin = Math.max(min, minP - intervalLength / 2.0)
            val newMax = Math.min(max, minP + intervalLength / 2.0)
            min_p(f, newMin, newMax, prec)          
        }
    }

    //Función que devuelve el valor de la medida de polarización de COMETE parametrizada en alpha y beta
    def rhoCMT_Gen(alpha: Double, beta: Double): MedidaPol = {
        // Función que recibe una distribución y calcula la polarización
        (distribution: Distribution) => {
          val (frequencies, values) = distribution
          val tuples = frequencies.zip(values) //Hacemos tuplas de la distribucion (pi,yi)
    
          def rhoAux(p: Double): Double = {
            //Representación de rhoaux
            tuples.map { case (pi, y) => Math.pow(pi, alpha) * Math.pow(Math.abs(y - p), beta) }.sum
          }
          // Función utilizada en min_p para hallar el punto minimo de opinión en la polarización
          val f = (p: Double) => rhoAux(p)
          val min = min_p(f, 0.0, 1.0, 0.001)
          f(min)
        }
      }
      

}

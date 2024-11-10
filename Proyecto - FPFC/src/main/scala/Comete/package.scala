package object Comete {
  type DistributionValues = Vector[Double]
  type Frequency = Vector[Double]
  type Distribution = (Frequency, DistributionValues)
  type PolMeasure = Distribution => Double




  // Entrada:
  //   - f: Double => Double (función que recibe un Double y devuelve un Double; debe ser cóncava en el intervalo [min, max])
  //   - min: Double (límite inferior del intervalo en el que se busca el mínimo)
  //   - max: Double (límite superior del intervalo en el que se busca el mínimo)
  //   - prec: Double (precisión deseada; cuando el intervalo [min, max] es menor o igual a este valor, se detiene la búsqueda)
  // Salida:
  //   - Double (valor aproximado en el intervalo [min, max] donde f alcanza su valor mínimo)
  def min_p(f: Double => Double, min: Double, max: Double, prec: Double): Double = {
    val intervalo = max - min
    val mid = (min + max) / 2
    if (intervalo <= prec) mid
    else {
      val left = mid - prec / 2
      val right = mid + prec / 2
      if (f(left) < f(right)) min_p(f, min, mid, prec)
      else min_p(f, mid, max, prec)
    }
  }
  //Entrada: 
  //  -alpha: Double (dato usado para la ponderación de comete)
  //  -beta: Double (dato usado para la ponderación de comete)
  //Salida:
  //  El valor de la medida de la polarización de la distribución
    def rhoCMT_Gen(alpha: Double, beta: Double): PolMeasure = {
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

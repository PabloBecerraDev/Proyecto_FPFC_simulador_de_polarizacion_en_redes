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
}

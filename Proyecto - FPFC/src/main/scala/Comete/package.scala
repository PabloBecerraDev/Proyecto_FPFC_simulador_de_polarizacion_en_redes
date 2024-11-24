import scala.annotation.tailrec

package object Comete {

    //Vector que contiene los valores de una distribución - debe sumar 1
    type DistributionValues = Vector[Double]
    //Vector que contiene las frecuencias
    type Frequency = Vector[Double]
    //Pareja de los datos anteriores
    type Distribution = (Frequency, DistributionValues)
    //Funcion que dada una distribution devuelve un double
    type PolMeasure = Distribution => Double



  // Entrada:
  //   - f: Double => Double (función que recibe un Double y devuelve un Double; debe ser cóncava en el intervalo [min, max])
  //   - min: Double (límite inferior del intervalo en el que se busca el mínimo)
  //   - max: Double (límite superior del intervalo en el que se busca el mínimo)
  //   - prec: Double (precisión deseada; cuando el intervalo [min, max] es menor o igual a este valor, se detiene la búsqueda)
  // Salida:
  //   - Double (valor aproximado en el intervalo [min, max] donde f alcanza su valor mínimo)
  @tailrec
  def min_p(f: Double => Double, min: Double, max: Double, prec: Double): Double = {
    val intervalo = max - min
    if (intervalo <= prec) {
      ((min + max) / 2 * 1000.0).round / 1000.0
    } else {
      val step = intervalo / 10
      val points = (0 to 10).map(i => min + i * step)
      val evaluations = points.map(p => (p, f(p)))
      val (bestPoint, _) = evaluations.minBy(_._2)
      val bestIndex = points.indexOf(bestPoint)
      val newMin = if (bestIndex == 0) points(0) else points(bestIndex - 1)
      val newMax = if (bestIndex == points.size - 1) points(bestIndex) else points(bestIndex + 1)
      min_p(f, newMin, newMax, prec)
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
        val result = f(min)
        ((result * 1000).round) / 1000.0
      }
  }



  //Entrada:
  //  -m: PolMeasure (medida de polarizacion)
  //Salida:
  //  El valor de la medida de la polarización normalizado
  def normalizar(m: PolMeasure): PolMeasure = {
    //Se crear el peor caso, donde halla 50% de probabilidad de una opinion extrema(0 o 1)
    val peorCaso: Distribution = (Vector(0.5, 0.0, 0.0, 0.0, 0.5), Vector(0.0, 0.25, 0.5, 0.75, 1.0))
    //Se normaliza la medida, dividiendo la medida polarizada de la Distribution dada
    //Por la del peor caso
    (d: Distribution) => {
    val result = m(d)/m(peorCaso)
    ((result * 1000).round) / 1000.0
    }
  }
}

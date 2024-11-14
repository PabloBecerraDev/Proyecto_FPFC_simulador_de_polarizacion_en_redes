//Importamos todo lo de comete
import Comete._

package object Opinion {

//Secuencia de creencias de los agentes entre [0,1]
type SpecificBelief = Vector[Double]

type GenericBelief = Int => SpecificBelief
type AgentsPolMeasure = 
    (SpecificBelief, DistributionValues) => Double

    //Función que devuelve comete parametrizada y normalizada para agentes
def rho(alpha: Double, beta: Double): AgentsPolMeasure = {
  (specificBelief: SpecificBelief, distributionValues: DistributionValues) => {
    val numAgents = specificBelief.length
    val k = distributionValues.length

    // Creación de intervalos considerando el primer y último elemento
    val firstInterval = (0.0, (distributionValues(1) + distributionValues(0)) / 2)
    val middleIntervals = (1 until k - 1).map(i => 
      ((distributionValues(i) + distributionValues(i - 1)) / 2, 
       (distributionValues(i) + distributionValues(i + 1)) / 2))
    val lastInterval = ((distributionValues(k - 2) + distributionValues(k - 1)) / 2, 1.0)

    val intervals = firstInterval +: middleIntervals :+ lastInterval

    // Clasificación de agentes en intervalos
    val classifiedAgents = specificBelief.map { belief =>
      intervals.indexWhere { case (start, end) => start <= belief && belief < end } match {
        case -1 => k - 1  // Asigna al último intervalo si no hay coincidencia
        case idx => idx
      }
    }


    // Cálculo de frecuencias relativas por intervalo
    val frequency = intervals.indices.map(i =>
      classifiedAgents.count(_ == i) / numAgents.toDouble
    ).toVector

    // Calcula la medida de polarización con normalización
    val rhoAux = rhoCMT_Gen(alpha, beta)
    val normalized = normalizar(rhoAux)

    // Calcula la medida de polarización normalizada
    normalized((frequency, distributionValues))
  }
}


}

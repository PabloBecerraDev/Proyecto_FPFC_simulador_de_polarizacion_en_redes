//Importamos todo lo de comete
import Comete._

package object Opinion {

//Secuencia de creencias de los agentes entre [0,1]
type SpecificBelief = Vector[Double]

type GenericBelief = Int => SpecificBeliefConf
type AgentsPolMeasure = 
    (SpecificBelief, DistributionValues) => Double

    //Función que devuelve comete parametrizada y normalizada para agentes
def rho(alpha: Double, beta: Double): AgentsPolMeasure = {
  // rho es la medida de polarización de agentes basada en comete
  (specificBelief: SpecificBelief, distributionValues: DistributionValues) => {
    val numAgents = specificBelief.length
    val k = distributionValues.length

    // Dividir el intervalo [0, 1] en intervalos usando distributionValues
    val intervals = (0 until k - 1).map(i => 
      (distributionValues(i), distributionValues(i + 1))
    )

    // Clasificación de agentes por intervalos
    val classifiedAgents = specificBelief.map { belief =>
      intervals.indexWhere { case (start, end) => start <= belief && belief < end }
    }

    // Calcular frecuencias relativas por intervalo (RECTIFICAR)
    val frequency = intervals.map { case (_, _) =>
      classifiedAgents.count(_ == _) / numAgents.toDouble
    }

    // Calcular la medida de polarización comete
    val rhoAux = rhoCMT_Gen(alpha, beta)
    val normalized = normalizar(rhoAux)

    // Normalizamos y calculamos la medida de polarización
    normalized((frequency, distributionValues))
  }
}


}

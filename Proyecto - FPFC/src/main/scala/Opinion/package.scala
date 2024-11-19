import Comete._

package object Opinion {

//Secuencia de creencias de los agentes entre [0,1]
type SpecificBelief = Vector[Double]

// es una funcion que recive un entero un SpecificBelief generado por alguna
// de las funciones proporcionadas.
type GenericBelief = Int => SpecificBelief

//recive SpecificBelief y DistributionValues y devuelve un double (medida de polarizacion)
type AgentsPolMeasure = (SpecificBelief, DistributionValues) => Double



//types segunda parte Opinion - modelar la evolucion

type WeightedGraph = (Int, Int) => Double
type SpecificWeightedGraph = (WeightedGraph, Int)
type GenericWeightedGraph = Int => SpecificWeightedGraph
type FunctionUpdate = (SpecificBelief, SpecificWeightedGraph) =>SpecificBelief





// Entrada:
//   - alpha: Double
//       Parámetro para ponderar las frecuencias de los agentes en la medida de polarización.
//   - beta: Double
//       Parámetro para ponderar las diferencias entre valores de opinión y el punto central.
// Salida:
//   - AgentsPolMeasure
//       Función que, dada una creencia específica (`SpecificBelief`) y valores de
//       distribución (`DistributionValues`), calcula un valor de polarización para los agentes.
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



// solo pongo esto aca por que si lo pongo en pruebas me manda error, ademas,
// es mas eficiente al ejecutarse desde la consola. Pero igual no deberia de estar aca.


def i1(nags: Int): SpecificWeightedGraph = {
  (
    (i: Int, j: Int) =>
      if (i == j) 1.0                           // Influencia total de un agente sobre sí mismo.
      else if (i < j) 1.0 / (j - i).toDouble    // Influencia decrece con la distancia entre agentes.
      else 0.0,                                 // Sin influencia si i > j.
    nags
  )
}

def i2(nags: Int): SpecificWeightedGraph = {
  (
    (i: Int, j: Int) =>
      if (i == j) 1.0
      else if (i < j) (j - i).toDouble / nags.toDouble
      else (nags - (i - j)).toDouble / nags.toDouble,
    nags
  )
}



// Entrada:
//   - swg: SpecificWeightedGraph
//       Una tupla que contiene:
//       - Una función de influencia (funcionInfluencia: WeightedGraph), que calcula el
//       peso de la influencia entre dos agentes dados sus índices.
//       - El número total de agentes (n: Int) en el grafo.
// Salida:
//   - IndexedSeq[IndexedSeq[Double]]
//       Una matriz explícita de influencias, donde cada celda [i][j] contiene el
//       peso de la influencia del agente i sobre el agente j.
def showWeightedGraph(swg: SpecificWeightedGraph): IndexedSeq[IndexedSeq[Double]] = {
  // se parte swg en (funcionInfluencia, n) siendo funcionInfluencia
  // la funcion de influencia y n la cantidad de agentes
  val (funcionInfluencia, n) = swg
  // tabulate recive el numero de agentes y una funcion, en este caso la funcion de influencia
  IndexedSeq.tabulate(n, n)((i, j) => funcionInfluencia(i, j))
}


}

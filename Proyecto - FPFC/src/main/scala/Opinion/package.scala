import Comete._
import scala.collection.parallel.CollectionConverters._


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

    // Creación de intervalos
    val intervals = (0 until k).map(i => {
      if (i == 0) (0.0, (distributionValues(i) + distributionValues(i + 1)) / 2) // Primer intervalo
      else if (i == k - 1) ((distributionValues(i - 1) + distributionValues(i)) / 2, 1.0) // Último intervalo
      else ((distributionValues(i - 1) + distributionValues(i)) / 2, (distributionValues(i) + distributionValues(i + 1)) / 2) // Intervalos intermedios
    })

    // Clasificación de agentes en intervalos
    val classifiedAgents = specificBelief.map { belief =>
      intervals.indexWhere {
        case (start, end) =>
          if (end == 1.0) start <= belief && belief <= end
          else start <= belief && belief < end
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


// Entrada:
//   - sb: SpecificBelief
//       Vector con los Beliefs o opiniones de n agentes
//   - swg: SpecificWeightedGraph
//       Tupla con una funcion de influencia, fI(j,i), que dados dos agentes devuelve cuanta influencia
//       tiene el agente j sobre i y el numero de agentes en el grafo
// Salida:
//   - SpecificBelief
//       Vector con los nuevos Beliefs o opiniones de los n agentes
def confBiasUpdate(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
  // Desestructuramos el grafo de influencia (wI) y el número de agentes (numAgents)
  val (fI, n) = swg
  //Queden los nuevos Beliefs en un vector, para que concuerde con el type
  Vector.from(for {i <- 0 until sb.length
                   //Se crea el conjunto Ai, con los agentes que tienen una influencia sobre el agente i
                   ai = (0 until sb.length).filter(fI(_,i) > 0.0)

                   //Realiza la multiplicacion dentro de la sumatoria, con la condicion que el agente j exista en Ai
                   //Si j no existe en Ai devuelve 0 para que no afecte en la suma
                   multiplicacion = for {
                     j <- 0 until sb.length
                   }yield{
                     if (ai.exists(_ == j)) {
                       (1 - math.abs(sb(j) - sb(i)))*(fI(j, i)) *(sb(j) - sb(i))
                     }else 0
                   }
    //Sumar el Belief del agente i con la divison de la sumatoria por la cardinalidad de Ai
                   }yield sb(i)+(multiplicacion.toList.sum/ai.size))
}



// Entrada:
//   - fu: FunctionUpdate
//       Función que actualiza las creencias de los agentes con base en las creencias actuales y la red.
//   - swg: SpecificWeightedGraph
//       Tupla con una función de influencia fI(j, i) entre agentes y el número total de agentes en el grafo.
//   - b0: SpecificBelief
//       Vector con las creencias iniciales de n agentes.
//   - t: Int
//       Número de pasos a simular.
// Salida:
//   - IndexedSeq[SpecificBelief]
//       Secuencia de vectores con las creencias de los agentes en cada paso del tiempo.
def simulate(fu: FunctionUpdate, swg: SpecificWeightedGraph, b0: SpecificBelief, t: Int): IndexedSeq[SpecificBelief] = {
  // Genera la secuencia de creencias para cada paso hasta t
  def iterar(pasoActual: Int, creenciasActuales: SpecificBelief, historial: IndexedSeq[SpecificBelief]): IndexedSeq[SpecificBelief] = {
    if (pasoActual >= t) historial
    else {
      // Actualiza las creencias para el siguiente paso
      val siguienteCreencia = fu(creenciasActuales, swg)
      iterar(pasoActual + 1, siguienteCreencia, historial :+ siguienteCreencia)
    }
  }
  iterar(0, b0, IndexedSeq(b0))
}


//verciones paralelas

// en este caso la funcion es la misma algoritmicamente hablando que rho, unicamente agregue par a las
// colecciones usadas.
def rhoPar(alpha: Double, beta: Double): AgentsPolMeasure = {
  (specificBelief: SpecificBelief, distributionValues: DistributionValues) => {
    val numAgents = specificBelief.length
    val k = distributionValues.length

    // Creación de intervalos considerando el primer y último elemento
    val firstInterval = (0.0, (distributionValues(1) + distributionValues(0)) / 2)
    val middleIntervals = (1 until k - 1).par.map(i => // Paraleliza el cálculo de los intervalos intermedios
      ((distributionValues(i) + distributionValues(i - 1)) / 2,
        (distributionValues(i) + distributionValues(i + 1)) / 2)
    )
    val lastInterval = ((distributionValues(k - 2) + distributionValues(k - 1)) / 2, 1.0)

    val intervals = firstInterval +: middleIntervals.toList :+ lastInterval

    // Clasificación de agentes en intervalos
    val classifiedAgents = specificBelief.par.map { belief =>
      intervals.indexWhere { case (start, end) => start <= belief && belief < end } match {
        case -1 => k - 1  // Asigna al último intervalo si no hay coincidencia
        case idx => idx
      }
    }

    // Cálculo de frecuencias relativas por intervalo
    val frequency = intervals.indices.par.map(i => // Paraleliza el cálculo de frecuencias
      classifiedAgents.count(_ == i) / numAgents.toDouble
    ).toVector

    // Calcula la medida de polarización con normalización
    val rhoAux = rhoCMT_Gen(alpha, beta)
    val normalized = normalizar(rhoAux)

    // Calcula la medida de polarización normalizada
    normalized((frequency, distributionValues))
  }
}


def confBiasUpdatePar(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
  val (fI, n) = swg
  // Divide el rango en dos mitades y paraleliza las tareas principales
  val (left, right) = common.parallel(
    (0 until sb.length / 2).par.map { i =>
      val ai = (0 until sb.length).par.filter(fI(_, i) > 0.0).toList
      val multiplicacion = (0 until sb.length).par.map { j =>
        if (ai.contains(j)) {
          (1 - math.abs(sb(j) - sb(i))) * fI(j, i) * (sb(j) - sb(i))
        } else 0.0
      }.sum
      sb(i) + (if (ai.nonEmpty) multiplicacion / ai.size else 0.0)
    }.seq, // Convertimos de nuevo a secuencia estándar
    (sb.length / 2 until sb.length).par.map { i =>
      val ai = (0 until sb.length).par.filter(fI(_, i) > 0.0).toList
      val multiplicacion = (0 until sb.length).par.map { j =>
        if (ai.contains(j)) {
          (1 - math.abs(sb(j) - sb(i))) * fI(j, i) * (sb(j) - sb(i))
        } else 0.0
      }.sum
      sb(i) + (if (ai.nonEmpty) multiplicacion / ai.size else 0.0)
    }.seq // Convertimos de nuevo a secuencia estándar
  )
  Vector.from(left ++ right)
}



}

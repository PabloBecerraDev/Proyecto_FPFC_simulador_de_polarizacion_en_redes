import Comete._
import Opinion._
import Benchmark._
import common._


//pruebas rhoCMT_Gen
val cmt1 = rhoCMT_Gen(1.2, 1.2)

val pi_max = Vector(0.5, 0.0, 0.0, 0.0, 0.5)
val pi_min = Vector(0.0, 0.0, 1.0, 0.0, 0.0)
val pi_der = Vector(0.4, 0.0, 0.0, 0.0, 0.6)
val pi_izq = Vector(0.6, 0.0, 0.0, 0.0, 0.4)
val pi_int1 = Vector(0.0, 0.5, 0.0, 0.5, 0.0)
val pi_int2 = Vector(0.25, 0.0, 0.5, 0.0, 0.25)
val pi_int3 = Vector(0.25, 0.25, 0.0, 0.25, 0.25)
val pi_cons_centro = pi_min
val pi_cons_der = Vector(0.0, 0.0, 0.0, 0.0, 1.0)
val pi_cons_izq = Vector(1.0, 0.0, 0.0, 0.0, 0.0)

val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)

cmt1(pi_max, likert5)
cmt1(pi_min, likert5)
cmt1(pi_der, likert5)
cmt1(pi_izq, likert5)
cmt1(pi_int1, likert5)
cmt1(pi_int2, likert5)
cmt1(pi_int3, likert5)
cmt1(pi_cons_centro, likert5)
cmt1(pi_cons_der, likert5)
cmt1(pi_cons_izq, likert5)


//pruebas normalizar
val cmt1_norm = normalizar(cmt1)

cmt1_norm(pi_max, likert5)
cmt1_norm(pi_min, likert5)
cmt1_norm(pi_der, likert5)
cmt1_norm(pi_izq, likert5)
cmt1_norm(pi_int1, likert5)
cmt1_norm(pi_int2, likert5)
cmt1_norm(pi_int3, likert5)
cmt1_norm(pi_cons_centro, likert5)
cmt1_norm(pi_cons_der, likert5)
cmt1_norm(pi_cons_izq, likert5)



//pruebas paquete Opinion

//Creencias genéricas
def uniformBelief(nags:Int): SpecificBelief= {
    Vector.tabulate(nags)((i:Int) =>
        (i+1).toDouble/nags.toDouble)
}

def midlyBelief(nags:Int):SpecificBelief = {
    val middle= nags/2
    Vector.tabulate(nags)((i:Int) =>
        if (i < middle) math.max(0.25-0.01*(middle-i-1),0)
        else math.min(0.75 - 0.01*(middle-i),1))
}
def allExtremeBelief(nags:Int):SpecificBelief = {
    val middle = nags/2
    Vector.tabulate(nags)((i:Int)=>
        if (i < middle)0.0 else 1.0)
}

def allTripleBelief(nags:Int):SpecificBelief={
    val oneThird = nags/3
    val twoThird = (nags/3)*2
    Vector.tabulate(nags)((i:Int)=>
        if (i < oneThird)0.0
        else if (i>=twoThird)1.0
        else 0.5)
}
def consensusBelief(b:Double)(nags:Int):SpecificBelief={
    Vector.tabulate(nags)((i:Int)=>b)
}

//generamos diferentes creencias especificas de 100 agentes

//pruebas rho
val sb_ext = allExtremeBelief(100)
val sb_cons = consensusBelief(0.2)(100)
val sb_unif = uniformBelief(100)
val sb_triple = allTripleBelief(100)
val sb_midly = midlyBelief(100)

val rho1 = rho(1.2,1.2)
val rho2 = rho(2.0,1.0)

val dist1 = Vector(0.0,0.25,0.5,0.75,1.0)
val dist2 = Vector(0.0,0.2,0.4,0.6,0.8,1.0)
rho1 (sb_ext, dist1)
rho2 (sb_ext, dist1)
rho1 (sb_ext, dist2)
rho2 (sb_ext, dist2)
rho1 (sb_cons, dist1)
rho2 (sb_cons, dist1)
rho1 (sb_cons, dist2)
rho2 (sb_cons, dist2)
rho1 (sb_unif, dist1)
rho2 (sb_unif, dist1)
rho1 (sb_unif, dist2)
rho2 (sb_unif, dist2)
rho1 (sb_triple, dist1)
rho2 (sb_triple, dist1)
rho1 (sb_triple, dist2)
rho2 (sb_triple, dist2)
rho1 (sb_midly, dist1)
rho2 (sb_midly, dist1)
rho1 (sb_midly, dist2)
rho2 (sb_midly, dist2)


//pruebas showWeightedGraph


def i1(nags:Int):SpecificWeightedGraph ={
    ((i:Int, j:Int) => if (i==j) 1.0
    else if (i<j) 1.0/(j-i).toDouble
    else 0.0,nags)
}

def i2(nags:Int):SpecificWeightedGraph ={
    ((i:Int, j:Int) => if (i==j) 1.0
    else if (i<j) (j-i).toDouble/nags.toDouble
    else (nags-(i-j)).toDouble/nags.toDouble, nags)
}
val i1_10=i1(10)
val i2_10=i2(10)
val i1_20=i1(20)
val i2_20=i2(20)
showWeightedGraph(i1_10)
showWeightedGraph(i2_10)

//confBiasUpdate
val sbu_10 = uniformBelief(10)
confBiasUpdate(sbu_10, i1_10)
rho1(sbu_10, dist1)
rho1(confBiasUpdate(sbu_10, i1_10), dist1)

val sbm_10 = midlyBelief(10)
confBiasUpdate(sbm_10, i1_10)
rho1(sbm_10, dist1)
rho1(confBiasUpdate(sbm_10, i1_10), dist1)


//pruebas simulate
for {
    b <- simulate(confBiasUpdate, i1_10, sbu_10, 2)
} yield (b, rho1(b, dist1))

for {
    b <- simulate(confBiasUpdate, i1_10, sbm_10, 2)
} yield (b, rho1(b, dist1))


//pruebas verciones paralelas

confBiasUpdatePar(sbu_10, i1_10)
val rhoPar1 = rhoPar(1.2,1.2)
rhoPar1(sbu_10, dist1)
rhoPar1(confBiasUpdatePar(sbu_10, i1_10), dist1)


//pruebas finales

val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)

// medidas vs
val sbms = for {
    n <- 2 until 16
    nags = math.pow(2, n).toInt
} yield midlyBelief(nags)

val sbes = for {
    n <- 2 until 16
    nags = math.pow(2, n).toInt
} yield allExtremeBelief(nags)

val sbts = for {
    n <- 2 until 16
    nags = math.pow(2, n).toInt
} yield allTripleBelief(nags)

val i1_32768=i1(32768)
val i2_32768=i2(32768)

val polSec = rho(1.2, 1.2)
val polPar = rhoPar(1.2, 1.2)

val cmp1 = compararMedidasPol(sbms, likert5, polSec, polPar)

compararFuncionesAct(sbms.take(sbms.length/2),
    i2_32768, confBiasUpdate, confBiasUpdatePar)

cmp1.map(t => t._6)


//generar las graficas
val evolsSec = for {
    i <- 0 until sbms.length
} yield simEvolucion(
    Seq(sbms(i), sbes(i), sbts(i)),
    i2_32768,
    10,
    polSec,
    confBiasUpdate,
    likert5,
    "Simulacion_Secuencial_" ++ i.toString ++ "-" ++ sbms(i).length.toString
)

//generar las graficas
val evolsPar = for {
    i <- 0 until sbms.length
} yield simEvolucion(
    Seq(sbms(i), sbes(i), sbts(i)),
    i2_32768,
    10,
    polPar,
    confBiasUpdatePar,
    likert5,
    "Simulacion_Paralela" ++ i.toString ++ "-" ++ sbms(i).length.toString
)
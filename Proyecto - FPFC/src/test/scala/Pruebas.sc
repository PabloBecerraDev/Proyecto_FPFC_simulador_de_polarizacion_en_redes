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
rho2 (sb_midly, dist2)
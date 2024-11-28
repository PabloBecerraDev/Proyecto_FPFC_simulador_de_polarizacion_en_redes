# 🌐 Simulador de polarización en redes

Este proyecto implementa un simulador de polarización en redes utilizando programación funcional y concurrente en Scala. Tiene como objetivo analizar cómo evoluciona la polarización en redes sociales mediante modelos computacionales. Incluye la implementación de medidas de polarización y simulaciones tanto en versiones secuenciales como paralelas.

El proyecto forma parte de los requisitos del curso Fundamentos de Programación Funcional y Concurrente y evalúa técnicas como paralelización de datos y tareas para mejorar el rendimiento.




## 👩‍💻 Authors

- Alexandra Marmolejo Gomez - 2241424 
    - 
- Erik Santiago Amariles Solarte - 2242964
    -
- Juan David Pinto Rodríguez - 2240440
    -
- Pablo Esteban Becerra Gomez - 2243506
    -
 



## 🛠️ Technology used

Lenguaje: Scala

Herramientas: Intellij idea, sbt

Librerías: org.scalameter


    
## 📂 Structure
Este proyecto contiene 2 paquetes esenciales que son el paquete *Comete* que contiene las funciones relacionadas con el cálculo de medidas de polarización basadas en distribuciones matemáticas y el paquete *Opinion* que es el responsable de modelar las redes, representar las creencias de los agentes y simular su evolución en el tiempo, además usamos un paquete *Benchmark* para hacer análisis comparativos entre versiones secuenciales y concurrentes.

La estructura del proyecto es la siguiente:

~~~
📁 Proyecto - FPFC/
└── ⚙️build.sbt
├──📁src
    ├── 📁test/scala
        ├── 📄Pruebas.sc
    ├── 📁main/scala
        ├── 📁 Benchmark/
        ├── 📁 Comete/
            ├──📄package.scala
        ├── 📁 Opinion/ 
            ├──📄package.scala
        ├── 📁 common/
~~~


* **📁Funciones en Comete**        
    - min_p: Esta función encuentra los valores mínimos en funciones convexas 
    - rhoCMTGen: Su utilidad es calcular la medida de polarización utilizando _Comete_
    - normalizar: Normaliza los valores de polarización en el rango [0,1] 

* **📁Funciones en Opinion**
    - rho: Calcula la polarización para una red de agentes
    - simulate: Simula la evolución de creencias en una red
    - confBiasUpdate: Aplica el sesgo de confirmación a las creencias de los agentes.
    
## 🚀 How to run
1. [⌨️] Clonar el repositorio
~~~
git clone https://github.com/PabloBecerraDev/Proyecto_FPFC_simulador_de_polarizacion_en_redes.git
~~~

2. [👨‍💻] Abrimos el proyecto en intellij idea

3. [🖱️] Nos dirigimos a src/test/scala y hacemos run al worksheet

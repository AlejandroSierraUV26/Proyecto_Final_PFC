package taller4

object Operacion {
  // Se implementa la función que se pide en el taller
  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean

  def generarCadenas(n: Int): Seq[Seq[Char]] = {
    def generarCombinaciones(tamaño: Int): Seq[Seq[Char]] = {
      if (tamaño == 0) {
        Seq(Seq())
      } else {
        val combinacionesMenores = generarCombinaciones(tamaño - 1)
        for {
          cadena <- combinacionesMenores
          letra <- alfabeto
        } yield letra +: cadena
      }
    }
    generarCombinaciones(n)
  }
  def reconstuirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    val cadenas = generarCadenas(n)

    def buscarCadena(cadenasRestantes: Seq[Seq[Char]]): Seq[Char] = {
      if (cadenasRestantes.isEmpty) {
        Seq()
      } else {
        val actual = cadenasRestantes.head
        if (o(actual)) {
          actual
        } else {
          buscarCadena(cadenasRestantes.tail)
        }
      }
    }

    buscarCadena(cadenas)
  }
  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    def reconstruirRecursivo(actual: Seq[Char], longitudActual: Int): Seq[Char] = {
      if (longitudActual == n && o(actual)) {
        actual
      } else if (longitudActual < n) {
        val siguientes = alfabeto.flatMap(letra => reconstruirRecursivo(actual :+ letra, longitudActual + 1))
        siguientes
      } else {
        Seq()
      }
    }

    reconstruirRecursivo(Seq(), 0)
  }

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {

    var k = 1
    var candidatos = alfabeto.map(Seq(_))

    while (k < n) {
      candidatos = candidatos.flatMap { candidato =>
        alfabeto.map(candidato :+ _)
      }.filter(o) // Filtrar las cadenas candidatas con el oráculo
      if (candidatos.isEmpty) Seq() // No hay cadenas válidas de longitud k

      k += 1 // Duplicar la longitud para la próxima iteración
    }

    candidatos.find(_.length == n).getOrElse(Seq()) // Devolver la cadena de longitud n si existe, de lo contrario, una lista vacía
    candidatos.headOption.getOrElse(Seq()) // Devolver la primera cadena si existe, de lo contrario, una lista vacía
  }


  def main(args: Array[String]): Unit = {
    // Se define el oráculo
    val tamañoDeseado = 16
    val secuencia_buscar = (1 to tamañoDeseado).map(_ => alfabeto(scala.util.Random.nextInt(alfabeto.length))).mkString("")

    val tamaño_secuencia = secuencia_buscar.length
    val o: Oraculo = (s: Seq[Char]) => {
      secuencia_buscar.containsSlice(s)
    }
    // Se llama a la función
    val inicio = System.nanoTime()

    println("Solucion ingenua")
    println(s"Generando cadenas de tamaño: $tamaño_secuencia")
    val cadenas = reconstuirCadenaIngenuo(tamaño_secuencia, o)
    println(s"Cadena encontrada: $cadenas")

    val fin = System.nanoTime()
    println()
    println((fin - inicio) / 1e6)
    println()
    println("Solucion mejorada")

    val inicio2 = System.nanoTime()
    println(s"Generando cadenas de tamaño: $tamaño_secuencia")
    val cadenas2 = reconstruirCadenaMejorado(tamaño_secuencia, o)
    println(s"Cadena encontrada: $cadenas2")
    val fin2 = System.nanoTime()
    println()
    println((fin2 - inicio2) / 1e6)
    println()
    println("Solucion  turbo mejorada")
    val inicio3 = System.nanoTime()
    println(s"Generando cadenas de tamaño: $tamaño_secuencia")
    val cadenas3 = reconstruirCadenaTurbo(tamaño_secuencia, o)
    println(s"Cadena encontrada: $cadenas3")
    val fin3 = System.nanoTime()
    println()
    println((fin3 - inicio3) / 1e6)
    println()
  }
}

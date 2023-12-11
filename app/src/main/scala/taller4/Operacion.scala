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
  def reconstruirCadenaTurbo(n: Int, o: Seq[Char] => Boolean): Seq[Char] = {
      Seq('s')
  }
  def main(args: Array[String]): Unit = {
    // Se define el oráculo
    val secuencia_buscar = Seq('a','c','a','c')

    val o: Oraculo = (s: Seq[Char]) => {
      secuencia_buscar.containsSlice(s)
    }
    // Se llama a la función
    println("Solucion ingenua")
    println(s"Generando cadenas de tamaño 4")
    val cadenas = reconstuirCadenaIngenuo(4, o)
    println(s"Cadena encontrada: $cadenas")
    println()
    println("Solucion mejorada")
    println(s"Generando cadenas de tamaño 4")
    val cadenas2 = reconstruirCadenaMejorado(4, o)
    println(s"Cadena encontrada: $cadenas2")
    /*
    println()
    println("Solucion  turbo mejorada")
    println(s"Generando cadenas de tamaño 4")
    val cadenas3 = reconstruirCadenaTurbo(4, o)
    println(s"Cadena encontrada: $cadenas3")
     */
  }
}

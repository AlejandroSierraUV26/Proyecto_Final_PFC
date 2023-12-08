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
    val totalelementos = cadenas.length
    def buscar_cadena(i: Int): Seq[Char] = {
      if (i == totalelementos) {
        Seq()
      } else {
        val cadena = cadenas(i)
        if (o(cadena)) {
          cadena
        } else {
          buscar_cadena(i + 1)
        }
      }
    }
    buscar_cadena(0)
  }
  def main(args: Array[String]): Unit = {
    // Se define el oráculo
    val secuencia_buscar = Seq('a','c','a','c')

    val o: Oraculo = (s: Seq[Char]) => {
      secuencia_buscar.containsSlice(s)
    }
    // Se llama a la función
    println(s"Generando cadenas de tamaño 4")
    val cadenas = reconstuirCadenaIngenuo(4, o)
    println(s"Cadena encontrada: $cadenas")

  }
}

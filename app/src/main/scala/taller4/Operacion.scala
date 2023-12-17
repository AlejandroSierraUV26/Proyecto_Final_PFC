package taller4

import common.task

import scala.annotation.tailrec
import scala.util.Random

object Operacion {
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
      } yield(letra +: cadena)
    }
  }
  generarCombinaciones(n)
}
  def reconstuirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    val cadenas = generarCadenas(n)
    cadenas.find(o).getOrElse(Seq())
  }
  def reconstuirCadenaIngenuoPar(n: Int, o: Oraculo): Seq[Char] = {
    val cadenas = task(generarCadenas(n)).join()
    cadenas.find(o).getOrElse(Seq())
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
  def reconstruirCadenaMejoradoPar(n: Int, o: Oraculo): Seq[Char] = {
    def reconstruirRecursivo(actual: Seq[Char], longitudActual: Int): Seq[Char] = {
      if (longitudActual == n && o(actual)) {
        actual
      } else if (longitudActual < n) {
        val tareas = alfabeto.map(letra => task(reconstruirRecursivo(actual :+ letra, longitudActual + 1)))
        val siguientes = tareas.flatMap(_.join())
        siguientes
      } else {
        Seq()
      }
    }
    reconstruirRecursivo(Seq(), 0)
  }
  @tailrec
  def buscarCadena(cadenasRestantes: Seq[Seq[Char]], o: Oraculo): Seq[Char] = {
    cadenasRestantes match {
      case Nil => Seq()
      case actual :: rest =>
        if (o(actual)) {
          actual
        } else {
          buscarCadena(rest,o)
        }
    }
  }
  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    def reconstruirCadenaTurboAux(k: Int, candidatos: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k >= n) {
        candidatos
      } else {
        val nuevosCandidatos = candidatos.flatMap { candidato =>
          alfabeto.map(candidato :+ _)
        }
        reconstruirCadenaTurboAux(k + 1, nuevosCandidatos)
        }
      }
    val candidatosIniciales = alfabeto.map(Seq(_))
    buscarCadena(reconstruirCadenaTurboAux(1, candidatosIniciales),o)
  }
  def reconstruirCadenaTurboPar(n: Int, o: Oraculo): Seq[Char] = {
    def reconstruirCadenaTurboAuxPar(k: Int, candidatos: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k >= n) {
        candidatos
      } else {
        val tareas = candidatos.flatMap { candidato =>
          alfabeto.map(letra => task(candidato :+ letra))
        }
        val nuevosCandidatos = tareas.map(_.join())

        if (nuevosCandidatos.isEmpty) Seq()
        else reconstruirCadenaTurboAuxPar(k + 1, nuevosCandidatos)
      }
    }
    val candidatosIniciales = alfabeto.map(Seq(_))
    val candidatosFinales = reconstruirCadenaTurboAuxPar(1, candidatosIniciales)
    buscarCadena(candidatosFinales,o)
  }
  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    @tailrec
    def reconstruirCadena(k: Int, candidatos: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k >= n) {
        candidatos
      } else {
        val nuevosCandidatos = candidatos.flatMap { candidato =>
          alfabeto.map(candidato :+ _)
        }.filter(o)

        if (nuevosCandidatos.isEmpty) Seq()
        else reconstruirCadena(k + 1, nuevosCandidatos)
      }
    }

    val candidatosIniciales = alfabeto.map(Seq(_))
    val candidatosFinales = reconstruirCadena(1, candidatosIniciales)

    candidatosFinales.find(_.length == n).getOrElse(Seq())
  }

  def reconstruirCadenaTurboMejoradaPar(n: Int, o: Oraculo): Seq[Char] = {
    def reconstruirCadena(k: Int, candidatos: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k >= n) {
        candidatos
      } else {
        val tareas = candidatos.flatMap { candidato =>
          alfabeto.map(letra => task(candidato :+ letra))
        }
        val nuevosCandidatos = tareas.map(_.join())

        val candidatosFiltrados = nuevosCandidatos.filter(o)
        if (candidatosFiltrados.isEmpty) Seq()
        else reconstruirCadena(k + 1, candidatosFiltrados)
      }
    }

    val candidatosIniciales = alfabeto.map(Seq(_))
    val candidatosFinales = reconstruirCadena(1, candidatosIniciales)

    candidatosFinales.find(_.length == n).getOrElse(Seq())
  }

  def tiempo_funcion(n: Int, o: Oraculo, f: (Int, Oraculo) => Seq[Char]): Double = {
    val inicio = System.nanoTime()
    f(n, o)
    val fin = System.nanoTime()
    (fin - inicio) / 1.0e9
  }
  def compararSecuencialParalela(funcionSecuencial: (Int, Oraculo) => Seq[Char], funcionParalela: (Int, Oraculo) => Seq[Char])(n: Int, o: Oraculo): (Double, Double, Double) = {
    val tiempoSecuencial = tiempo_funcion(n, o, funcionSecuencial)
    val tiempoParalelo = tiempo_funcion(n, o, funcionParalela)
    val aceleracion = tiempoSecuencial / tiempoParalelo
    (tiempoSecuencial, tiempoParalelo, aceleracion)
  }
  def main(args: Array[String]): Unit = {
    for {
      i <- 1 to 4
      n = math.pow(2, i).toInt
      secuencia = (1 to n).map(_ => alfabeto(Random.nextInt(alfabeto.length)))
    } yield {
      val o: Oraculo = (s: Seq[Char]) => secuencia.containsSlice(s)
      val tiempo1 = tiempo_funcion(n, o, reconstruirCadenaTurboMejorada)
      println(s"Tiempo Turbo Mejorada: $tiempo1")
    }
  }
}

/**
 * Plantilla para pruebas
* @author Carlos Delgado
* @version 1.0
* @note 22 de Noviembre de 2023 
 */
package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import taller4.Operacion.Oraculo

@RunWith(classOf[JUnitRunner])
class TestTaller4 extends AnyFunSuite{
    val alfabeto: Seq[Char] = Seq('a', 'c', 'g', 't')
    test("testCadenaEstatica"){
        val Seq1 = Seq('a','c','g','t')
        val tamaño_Seq1 = Seq1.length
        val o: Oraculo = (s: Seq[Char]) => {Seq1.containsSlice(s)}
        val SolucionIngenua = Operacion.reconstuirCadenaIngenuo(tamaño_Seq1,o)
        assert(SolucionIngenua == Seq1)
        val SolucionMejorada = Operacion.reconstruirCadenaMejorado(tamaño_Seq1,o)
        assert(SolucionMejorada == Seq1)
        val SolucionTurbo = Operacion.reconstruirCadenaTurbo(tamaño_Seq1,o)
        assert(SolucionTurbo == Seq1)
        val SolucionTurboMejorada = Operacion.reconstruirCadenaTurboMejorada(tamaño_Seq1,o)
        assert(SolucionTurboMejorada == Seq1)
    }
    test("SolucionIngenua"){
        for (i <- 2 until 10){
            val tamañoDeseado = i
            val secuencia_buscar = (1 to tamañoDeseado).map(_ => alfabeto(scala.util.Random.nextInt(alfabeto.length)))
            val o: Oraculo = (s: Seq[Char]) => {secuencia_buscar.containsSlice(s)}
            val Solucion = Operacion.reconstuirCadenaIngenuo(tamañoDeseado,o)
            assert(Solucion == secuencia_buscar)
        }
    }
    test("SolucionMejorada"){
        for (i <- 2 until 10) {
            val tamañoDeseado = i
            val secuencia_buscar = (1 to tamañoDeseado).map(_ => alfabeto(scala.util.Random.nextInt(alfabeto.length)))
            val o: Oraculo = (s: Seq[Char]) => {
                secuencia_buscar.containsSlice(s)
            }
            val Solucion = Operacion.reconstruirCadenaMejorado(tamañoDeseado, o)
            assert(Solucion == secuencia_buscar)
        }
    }
    test("SolucionTurbo"){
        for (i <- 2 until 10) {
            val tamañoDeseado = i
            val secuencia_buscar = (1 to tamañoDeseado).map(_ => alfabeto(scala.util.Random.nextInt(alfabeto.length)))
            val o: Oraculo = (s: Seq[Char]) => {
                secuencia_buscar.containsSlice(s)
            }
            val Solucion = Operacion.reconstruirCadenaTurbo(tamañoDeseado, o)
            assert(Solucion == secuencia_buscar)
        }
    }
    test("SolucionTurboMejorada"){
        for (i <- 2 until 10) {
            val tamañoDeseado = i
            val secuencia_buscar = (1 to tamañoDeseado).map(_ => alfabeto(scala.util.Random.nextInt(alfabeto.length)))
            val o: Oraculo = (s: Seq[Char]) => {
                secuencia_buscar.containsSlice(s)
            }
            val Solucion = Operacion.reconstruirCadenaTurboMejorada(tamañoDeseado, o)
            assert(Solucion == secuencia_buscar)
        }
    }
}

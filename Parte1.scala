object Parte1 {

  // Ejercicio 1
  val duplicar: Int => Int  = x => x * 2
  val cuadrado: Int => Int  = x => x * x
  val esPar: Int => Boolean = x => x % 2 == 0

  // Ejercicio 2
  val cubo: Int => Int                     = x => x * x * x
  val promedio: (Double, Double) => Double = (a, b) => (a + b) / 2
  val mayor: (Int, Int) => Int             = (a, b) => if a >= b then a else b
  val esMultiploDeTres: Int => Boolean     = x => x % 3 == 0

  // Ejercicio 3
  def transformar(
      numero: Int,
      operacion: Int => Int
  ): Int = operacion(numero)

  // Ejercicio 4
  def calcular(
      a: Double,
      b: Double,
      operacion: (Double, Double) => Double
  ): Double = operacion(a, b)

  // Ejercicio 5
  def crearMultiplicador(factor: Int): Int => Int = x => x * factor

  val duplicar2: Int => Int = crearMultiplicador(2)
  val triplicar: Int => Int = crearMultiplicador(3)
  val porDiez: Int => Int   = crearMultiplicador(10)

  // Ejercicio 6
  def aplicarDescuento(precio: Double, porcentaje: Double): Double =
    precio - precio * (porcentaje / 100)

  val descuento10: Double => Double = precio => aplicarDescuento(precio, 10)
  val descuento20: Double => Double = precio => aplicarDescuento(precio, 20)
  val descuento50: Double => Double = precio => aplicarDescuento(precio, 50)

  // Ejercicio 7
  def elevar(exponente: Int)(base: Int): Int =
    if exponente == 0 then 1
    else base * elevar(exponente - 1)(base)

  val cuadradoCurried: Int => Int = elevar(2)
  val cuboCurried: Int => Int     = elevar(3)
  val cuartaPotencia: Int => Int  = elevar(4)

  // Ejercicio 8
  val limpiar: String => String             = s => s.trim
  val convertirMinusculas: String => String = s => s.toLowerCase
  val longitud: String => Int               = s => s.length
  val esLongitudValida: Int => Boolean      = n => n >= 5

  val validarTexto: String => Boolean =
    (limpiar andThen convertirMinusculas andThen longitud andThen esLongitudValida)

  // Ejercicio 9
  def sumar(
      desde: Int,
      hasta: Int,
      transformar: Int => Int
  ): Int =
    if desde > hasta then 0
    else transformar(desde) + sumar(desde + 1, hasta, transformar)

  def producto(
      desde: Int,
      hasta: Int,
      transformar: Int => Int
  ): Int =
    if desde > hasta then 1
    else transformar(desde) * producto(desde + 1, hasta, transformar)

  // Ejercicio 10
  def transformarTexto(
      texto: String,
      transformacion: Char => Char
  ): String =
    if texto.isEmpty then ""
    else transformacion(texto.head) + transformarTexto(texto.tail, transformacion)

  def buscarCaracter(
      texto: String,
      condicion: Char => Boolean
  ): Boolean =
    if texto.isEmpty then false
    else if condicion(texto.head) then true
    else buscarCaracter(texto.tail, condicion)

  def main(args: Array[String]): Unit = {

    // Ejercicio 1
    println()
    println(s"duplicar(5)  = ${duplicar(5)}")
    println(s"cuadrado(4)  = ${cuadrado(4)}")
    println(s"esPar(8)     = ${esPar(8)}")

    // Ejercicio 2
    println()
    println(s"cubo(3)              = ${cubo(3)}")
    println(s"promedio(4.0, 6.0)   = ${promedio(4.0, 6.0)}")
    println(s"mayor(7, 3)          = ${mayor(7, 3)}")
    println(s"esMultiploDeTres(9)  = ${esMultiploDeTres(9)}")

    // Ejercicio 3
    println()
    println(s"transformar(5, x => x * 2)  = ${transformar(5, x => x * 2)}")
    println(s"transformar(5, x => x * x)  = ${transformar(5, x => x * x)}")
    println(s"transformar(5, x => x + 10) = ${transformar(5, x => x + 10)}")
    println(s"transformar(5, x => -x)     = ${transformar(5, x => -x)}")

    // Ejercicio 4
    println()
    println(s"suma        = ${calcular(10, 5, (a, b) => a + b)}")
    println(s"resta       = ${calcular(10, 5, (a, b) => a - b)}")
    println(s"multiplicar = ${calcular(10, 5, (a, b) => a * b)}")
    println(s"division    = ${calcular(10, 5, (a, b) => a / b)}")
    println(s"promedio    = ${calcular(10, 5, (a, b) => (a + b) / 2)}")

    // Ejercicio 5
    println()
    println(s"duplicar2(7) = ${duplicar2(7)}")
    println(s"triplicar(7) = ${triplicar(7)}")
    println(s"porDiez(7)   = ${porDiez(7)}")

    // Ejercicio 6
    println()
    println(s"descuento10(500) = ${descuento10(500)}")
    println(s"descuento20(500) = ${descuento20(500)}")
    println(s"descuento50(500) = ${descuento50(500)}")

    // Ejercicio 7
    println()
    println(s"cuadradoCurried(5) = ${cuadradoCurried(5)}")
    println(s"cuboCurried(3)     = ${cuboCurried(3)}")
    println(s"cuartaPotencia(2)  = ${cuartaPotencia(2)}")

    // Ejercicio 8
    println()
    println(s"""validarTexto("  Hola  ") = ${validarTexto("  Hola  ")}""")
    println(s"""validarTexto("  Hi  ")   = ${validarTexto("  Hi  ")}""")
    println(s"""validarTexto("Scala")    = ${validarTexto("Scala")}""")

    // Ejercicio 9
    println()
    println(s"sumar(1, 4, x => x)       = ${sumar(1, 4, x => x)}")
    println(s"sumar(1, 3, x => x * x)   = ${sumar(1, 3, x => x * x)}")
    println(s"sumar(1, 3, x => x*x*x)   = ${sumar(1, 3, x => x * x * x)}")
    println(s"producto(1, 4, x => x)    = ${producto(1, 4, x => x)}")

    // Ejercicio 10
    println()
    println(s"""transformarTexto("scala", c => c.toUpper) = ${transformarTexto("scala", c => c.toUpper)}""")
    println(s"""buscarCaracter("Scala2026", c => c.isDigit) = ${buscarCaracter("Scala2026", c => c.isDigit)}""")
    println(s"""buscarCaracter("Scala", c => c == 'z')      = ${buscarCaracter("Scala", c => c == 'z')}""")
  }
}

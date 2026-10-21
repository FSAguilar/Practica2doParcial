class Pelicula(
    val titulo: String,
    val duracionMinutos: Int,
    val clasificacion: String
)

object Pelicula {
  def apply(
      titulo: String,
      duracionMinutos: Int,
      clasificacion: String
  ): Pelicula =
    new Pelicula(
      titulo,
      if duracionMinutos <= 0 then 90 else duracionMinutos,
      clasificacion
    )
}

class FuncionCine(
    val pelicula: Pelicula,
    val sala: Int,
    val hora: String,
    val precioEntrada: Double,
    val asientosDisponibles: Int
) {

  // 3. puedeReservar
  def puedeReservar(cantidad: Int): Boolean =
    cantidad <= asientosDisponibles

  // 4. costoReserva
  def costoReserva(cantidad: Int): Double =
    cantidad * precioEntrada

  // 5. reservar
  def reservar(cantidad: Int): FuncionCine =
    if puedeReservar(cantidad) then
      new FuncionCine(pelicula, sala, hora, precioEntrada, asientosDisponibles - cantidad)
    else
      new FuncionCine(pelicula, sala, hora, precioEntrada, asientosDisponibles)

  // 6. descripcion
  def descripcion: String =
    s"""|${pelicula.titulo}
        |Sala: $sala
        |Hora: $hora
        |Precio: $precioEntrada Bs
        |Asientos disponibles: $asientosDisponibles""".stripMargin
}

object FuncionCine {
  def apply(
      pelicula: Pelicula,
      sala: Int,
      hora: String,
      precioEntrada: Double,
      asientosDisponibles: Int
  ): FuncionCine =
    new FuncionCine(
      pelicula,
      if sala < 1 then 1 else sala,
      hora,
      if precioEntrada < 0 then 0.0 else precioEntrada,
      if asientosDisponibles < 0 then 0 else asientosDisponibles
    )

  // 7. funcionEstandar
  def funcionEstandar(
      pelicula: Pelicula,
      sala: Int,
      hora: String
  ): FuncionCine =
    FuncionCine(pelicula, sala, hora, 30.0, 50)
}

object Parte2 {
  def main(args: Array[String]): Unit = {

    // 8. Objetos de prueba
    val pelicula1 = Pelicula("Dune", 155, "PG-13")
    val funcion1  = FuncionCine(pelicula1, 3, "20:30", 35.0, 40)
    val funcion2  = FuncionCine.funcionEstandar(pelicula1, 1, "16:00")

    // 9. Pruebas
    println(funcion1.descripcion)
    println(funcion1.puedeReservar(5))
    println(funcion1.costoReserva(5))

    val funcionReservada = funcion1.reservar(5)
    println(funcion1.asientosDisponibles)
    println(funcionReservada.asientosDisponibles)
    println(funcion2.descripcion)
  }
}

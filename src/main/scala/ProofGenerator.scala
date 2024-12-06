class ProofGenerator {
  // _YOUR_CODE_HERE_
  sealed trait Kai

  case object Prash extends Kai

  case class Kevin(r: Ryan, v: Varun) extends Kai

  case class Harsha(v: Varun) extends Kai

  sealed trait Rahul

  case class Ryan() extends Rahul

  case object Kirby extends Rahul

  sealed trait Shuchi extends Rahul

  case class Denzil(t: Tome, r: Rahul) extends Shuchi

  case class Varun() extends Shuchi

  sealed trait Tome extends Kai with Rahul

  case class Spencer(k: Kai) extends Tome

  case class Nour(k1: Kai, d: Denzil, k2: Kai) extends Tome

}

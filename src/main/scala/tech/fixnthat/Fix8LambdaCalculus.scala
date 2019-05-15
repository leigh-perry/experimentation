package tech.fixnthat

object Fix8LambdaCalculus {

  // https://en.wikipedia.org/wiki/Fixed-point_combinator
  //
  //  Y    = λf.(λx.f(xx))(λx.f(xx))
  //  Y g  = (λf. (λx.f(xx)) (λx.f(xx))) g
  //       = (λx.g(xx)) (λx.g(xx))
  //       = g((λx.g(xx)) (λx.g(xx)))
  //       = g (Y g)

}

package tech.fixnthat

object Fix8LambdaCalculus {

  //  loop = (λx.xx)(λx.xx)
  //       = (λx.xx)(λx.xx)
  //  rec f = f (rec f)
  //        = f (f (f (f (rec f))))
  //
  // https://en.wikipedia.org/wiki/Fixed-point_combinator
  //
  //  Y    = λf.(λx.f(xx))(λx.f(xx))
  //  Y g  = (λf. (λx.f(xx)) (λx.f(xx))) g
  //       = (λx.g(xx)) (λx.g(xx))
  //       = g((λx.g(xx)) (λx.g(xx)))
  //       = g (Y g)

}

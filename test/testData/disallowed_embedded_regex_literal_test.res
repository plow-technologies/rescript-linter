let isAnImageRe = %re("/.(jpg|jpeg|png|gif|svg)$/i")

let a = isAnImageRe->Js.Re.test_("some.jpg")

let b = Js.Re.test_(%re("/.(jpg|jpeg|png|gif|svg)$/i"), "some.gif")

Js.log(a || b)

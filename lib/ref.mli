type doc := Doc.t

type ('a,'b) fmt = ('a, doc ref, unit,'b) format4
val printf: doc ref -> ('a, unit) fmt -> 'a
val kprintf: (doc ref -> 'b) -> doc ref -> ('a, 'b) fmt -> 'a

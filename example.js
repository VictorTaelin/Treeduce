var treeduce = require(".");

console.log(treeduce.eval(`
0 = z
1 = (s 0)
2 = (s 1)
3 = (s 2)
4 = (s 3)
5 = (s 4)
6 = (s 5)
7 = (s 6)
8 = (s 7)
9 = (s 8)

(not t) = f
(not f) = t

(pred z)      = z
(pred (s #n)) = #n

(double z      #r) = #r
(double (s #a) #r) = (double #a (s (s #r)))

(add z      #b) = #b
(add (s #a) #b) = (s (add #a #b))

(mul z      #b) = z
(mul (s #a) #b) = (add #b (mul #a #b))

(cmp z      z)      = eql
(cmp (s #n) z)      = gtn
(cmp z      (s #m)) = ltn
(cmp (s #n) (s #m)) = (cmp #n #m)

(cmp.case ltn #a #b #c) = #a
(cmp.case eql #a #b #c) = #b
(cmp.case gtn #a #b #c) = #c

(shift (var #idx)      #inc #dep) = (cmp.case (cmp #idx #dep) (var #idx) (var (add #idx #inc)) (var (add #idx #inc)))
(shift (lam #bod)      #inc #dep) = (lam (shift #bod #inc (s #dep)))
(shift (app #fun #arg) #inc #dep) = (app (shift #fun #inc #dep) (shift #arg #inc #dep)) 

(subst (var #idx)      #val #dep) = (cmp.case (cmp #idx #dep) (var #idx) #val (var (pred #idx)))
(subst (lam #bod)      #val #dep) = (lam (subst #bod (shift #val (s z) z) (s #dep)))
(subst (app #fun #arg) #val #dep) = (app (subst #fun #val #dep) (subst #arg #val #dep))

(reduce (var #idx))               = (var #idx)
(reduce (lam #bod))               = (lam #bod)
(reduce (app #fun #arg))          = (reduce.go (reduce #fun) #arg)

(reduce.go (lam #bod)      #x)    = (reduce (subst #bod #x z))
(reduce.go (app #fun #arg) #x)    = (app (app #fun #arg) #x)
(reduce.go (var #idx)      #x)    = (app (var #idx) #x)

(normal (var #idx))               = (normal.go (reduce (var #idx)))
(normal (lam #bod))               = (normal.go (reduce (lam #bod)))
(normal (app #fun #arg))          = (normal.go (reduce (app #fun #arg)))

(normal.go (var #idx))            = (var #idx)
(normal.go (lam #bod))            = (lam (normal #bod))
(normal.go (app #fun #arg))       = (app (normal #fun) (normal #arg))

c0 = (lam (lam (var 0)))
c1 = (lam (lam (app (var 1) (var 0))))
c2 = (lam (lam (app (var 1) (app (var 1) (var 0)))))
c3 = (lam (lam (app (var 1) (app (var 1) (app (var 1) (var 0))))))
main = (normal (app c2 c3))
`, {debug: true}));


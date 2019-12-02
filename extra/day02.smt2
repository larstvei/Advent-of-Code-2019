(declare-const noun Int)
(declare-const verb Int)

(assert (< noun verb))
(assert (= (+ 1114711 noun (* verb 216000)) 19690720))

(check-sat)
(get-model)

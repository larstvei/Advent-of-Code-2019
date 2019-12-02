(declare-const noun Int)
(declare-const verb Int)

(assert (< verb noun))
(assert (= (+ 1114711 (* noun 216000) verb) 19690720))

(check-sat)
(get-model)

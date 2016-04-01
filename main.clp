(deftemplate destination
	(slot name (type STRING))
	(slot climate (type STRING) (allowed-strings "summer" "winter" "autumn" "spring"))
	(slot englishSpeaking (type STRING) (default "no")(allowed-strings "yes" "no"))
	(slot visitPurpose (type STRING)(allowed-strings "adventure" "pleasure"))
	(slot region (type STRING))
	(slot budget (type INTEGER))
	(slot activityType (type STRING))
	(slot indoorActivity (type STRING)(default "no")(allowed-strings "yes" "no"))
	(slot outdoorActivity (type STRING)(default "no")(allowed-strings "yes" "no")))

; ; The initial facts
; ; i.e. The budget of the user
(deffacts the-facts
(destination (name "Singapore")(climate "summer")(englishSpeaking "yes")(region "asia")(budget 5000)
(activityType "leisure")(indoorActivity "yes")(outdoorActivity "yes")(safety "yes")))

(defrule start
?state <-(state start)
=>
(printout t crlf)(printout t crlf)
(printout t "********************************************************"crlf)
(printout t "                       Welcome to Where2Go              "crlf)
(printout t "********************************************************"crlf)
(printout t crlf)
)

(defrule enquire-budget
?state <- (state budget)
=>
(printout t crlf)
(printout t "Do you have a budget?" crlf)
(printout t "Please specify your budget in the following format, i.e: if $5000 dollars -> 5000" crlf)
(bind ?n (read))
 )
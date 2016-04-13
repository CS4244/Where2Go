;; @Gary i need u to insert an assert at the end of each question
;; like this (assert (checkweather on))
;; as well as other asserts for the other questions
;; this will fire the various destination filters

;;question refers to the question to be asked
;; choice refers to the choices available to the user
;;slotName refers to the actual slot name in the desired template
;;For questionType, radio implies question accepts only one answer and checkbox accepts multiple answers. 
(deftemplate ask
	(slot question (type STRING))
	(multislot choice (type STRING)(default "NULL"))
	(slot slotName (type STRING))
    (slot questionType (type STRING) (allowed-strings "RADIO" "CHECKBOX" "TEXTBOX") (default "RADIO"))
)

;; no pyclips
;;(deftemplate filter
;;	(slot status (type STRING) (default "Off") (allowed-strings ;;"Off" "On"))
;;	(slot targetSlot (type SYMBOL) (default NULL) (allowed-symbols ;;NULL englishSpeaking visitPreference region geography leisure ;;daysReq avgHotel activityType waterActivity outdoorActivity weather))
;;	(slot slotType (type STRING) (default "regular") (allowed-
;;strings "regular" "multi"))
;;)


; ; In destination, NULL is not allowed except in
; ; waterActivity and outdoorActivity.
; ; Here, a NULL means that the destination
; ; has a lack of water/outdoor activity
(deftemplate destination
	(slot name (type STRING))
	(slot englishSpeaking (type STRING) (default "No")(allowed-strings "NULL" "Yes" "No"))
	(multislot visitPreference (type STRING)(allowed-strings "NULL" "Undiscovered" "Romantic" "Peace and Quiet" "Cultural" "Nightlife" "Family Friendly" "Leisure"))
	(slot region (type STRING)(allowed-strings "NULL" "North America" "Europe" "Other"))
	(multislot geography (type STRING)(allowed-strings "NULL" "Beach" "City" "Volcano" "Seaside" "Mountains" "Wilderness" "Island"))
	(multislot leisure (type STRING)(allowed-strings "NULL" "Casino" "Spa" "Shopping" "Theme Park" "Landmarks" "Zoo" "Museum" "Dining"))
	(slot daysReq (type INTEGER))
	(slot avgHotel (type INTEGER))
	(multislot activityType (type STRING)(allowed-strings  "NULL" "Water" "Outdoor"))
	(multislot waterActivity (type STRING)(allowed-strings "NULL" "Surfing" "Snorkelling" "Water Skiing" "Water Park" "Wind Surfing" "Dolphin Encounter"))
	(multislot outdoorActivity (type STRING)(allowed-strings "NULL" "Mountain Biking" "Rock Climbing" "Hiking" "Snow Skiing" "Zip Line" "Horseback Riding"))
	(slot weather (type STRING) (allowed-strings "NULL" "Hot" "Warm" "Mild" "Cold"))
)


; ; desired stores user's desired destination properties
; ; idea is that "NULL" can be used to identify unasked properties
; ; NULL is allowed for all slots here, as it is used to pattern match and fire questions.
(deftemplate desired
	; ;(slot name (type STRING))
	(slot englishSpeaking (type STRING) (default "NULL")(allowed-strings "NULL" "Yes" "No"))
	(multislot visitPreference (type STRING) (default "NULL") (allowed-strings "NULL" "Undiscovered" "Romantic" "Peace and Quiet" "Cultural" "NightLife" "Family Friendly" "Leisure"))
	(slot region (type STRING)(allowed-strings "NULL" "North America" "Europe" "Other"))
	(multislot geography (type STRING) (default "NULL") (allowed-strings "NULL" "Beach" "City" "Volcano" "Seaside" "Mountains" "Wilderness" "Island"))
	(multislot leisure (type STRING) (default "NULL") (allowed-strings "NULL" "Casino" "Spa" "Shopping" "Theme Park" "Landmarks" "Zoo" "Museum" "Dining"))
    (slot budget (type INTEGER) (default -1))
	(slot daysReq (type INTEGER) (default -1))
	(multislot activityType (type STRING) (default "NULL") (allowed-strings "NULL" "Water" "Outdoor"))
	(multislot waterActivity (type STRING) (default "NULL") (allowed-strings "NULL" "Surfing" "Snorkelling" "Water Skiing" "Water Park" "Wind Surfing" "Dolphin Encounter"))
	(multislot outdoorActivity (type STRING) (default "NULL") (allowed-strings "NULL" "Mountain Biking" "Rock Climbing" "Hiking" "Snow Skiing" "Zip Line" "Horseback Riding"))
	(slot weather (type STRING) (allowed-strings "NULL" "Hot" "Warm" "Mild" "Cold"))
)


; ; The initial facts
(deffacts the-facts
		(destination (name "Abu Dhabi")(englishSpeaking "No")(visitPreference "Undiscovered" "Romantic" "Peace and Quiet" "Family Friendly")(region "Other")(geography "Beach" "City" "Seaside" "Mountains" "Island")(leisure "Spa" "Shopping" "Theme Park" "Landmarks" "Zoo" "Museum" "Dining")(daysReq 0)(avgHotel 219)(activityType "Water" "Outdoor")(waterActivity "Surfing" "Snorkelling" "Water Skiing" "Water Park" "Wind Surfing" "Dolphin Encounter")(outdoorActivity "Mountain Biking" "Horseback Riding")(weather "Warm"))

  (count destination undone)
	(desired)
	;;(filter)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;GLOBALS
(defglobal ?*totalDestination* = 0)
(defglobal ?*userBudget* = 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIRING FILTERS (normal slot)
(defrule fireWeatherFilter
(checkweather on)
?desired <- (desired (weather ?desiredW))
?destination <- (destination (weather ?destW))
(test (neq ?desiredW ?destW) )
=>
	(retract ?destination)
  (bind ?*totalDestination* (- ?*totalDestination* 1))
)

(defrule fireEnglishFilter
(checkenglishSpeaking on)
?desired <- (desired (englishSpeaking ?desiredE))
?destination <- (destination (englishSpeaking ?destE))
(test (neq ?desiredE ?destE) )
=>
	(retract ?destination)
  (bind ?*totalDestination* (- ?*totalDestination* 1))
)

(defrule fireRegionFilter
(checkregion on)
?desired <- (desired (region ?desiredR))
?destination <- (destination (region ?destR))
(test (neq ?desiredR ?destR) )
=>
(retract ?destination)
(bind ?*totalDestination* (- ?*totalDestination* 1))
)

(defrule fireBudgetFilter
(desired (budget ?budgetDesired))
(desired (daysReq ?daysReq))
(test (> ?daysReq -1))
(test (> ?daysReq -1))
=>
(do-for-all-facts ((?f destination)) TRUE
      (if(< ?budgetDesired (* ?daysReq ?f:avgHotel))
      then
      (retract ?f)
      (bind ?*totalDestination* (- ?*totalDestination* 1))
      else)
      )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUESTION FIRING

(defrule fireBudgetQuestion
?desired <- (desired (budget -1))
=>
(assert (ask (question "What is your preferred budget? (If $5000.00, please enter 5000) ") (slotName "budget") (questionType "TEXTBOX")))
)

(defrule fireDaysReqQuestion
?desired <- (desired (daysReq -1))
=>
(assert (ask (question "How many days are you planning to spend in the destination? If 5 days, please enter 5") (slotName "daysReq") (questionType "TEXTBOX")))
)

(defrule calculateExpense
(desired (budget ?budget))
(desired (daysReq ?daysReq))
(test (> ?daysReq -1))
(test (> ?budget -1))
=>
(bind ?*userBudget* (* ?budget ?daysReq))
)

(defrule fireGeographyQuestion
?desired <- (desired (geography "NULL"))
=>
(assert (ask (question "What is your geographic preference?") (choice "Beach" "City" "Volcano" "Seaside" "Mountains" "Wilderness" "Island") (slotName "geography") (questionType "RADIO")))
)

(defrule fireWeatherQuestion
?desired <- (desired (weather "NULL"))
=>
(assert (ask (question "What is your ideal weather?") (choice "Hot" "Warm" "Mild" "Cold") (slotName "weather")))
)

(defrule fireEnglishSpeakingQuestion
?desired <- (desired (englishSpeaking "NULL"))
=>
(assert (ask (question "Do you prefer your destination to be an English-speaking destination?") (choice "Yes" "No") (slotName "englishSpeaking")))
)

(defrule fireRegionQuestion
?desired <- (desired (region "NULL"))
=>
(assert (ask (question "What is your ideal destination region?") (choice "North America" "Europe" "Other") (slotName "region")))
)

(defrule fireVisitPreferenceQuestion
?desired <- (desired (visitPreference "NULL"))
=>
(assert (ask (question "Any visit preference?") (choice "Undiscovered" "Romantic" "Peace and Quiet" "Cultural" "Nightlife" "Family Friendly" "Leisure") (slotName "visitPreference") (questionType "RADIO")))
)

;;if leisure chosen previous qn, fire this
(defrule fireLeisureQuestion
(desired (visitPreference "Leisure"))
?desired <- (desired (leisure "NULL"))
=>
(assert (ask (question "What would you like to do for leisure?") (choice "Casino" "Spa" "Shopping" "Theme Park" "Landmarks" "Zoo" "Museum" "Dining") (slotName "leisure") (questionType "CHECKBOX")))
)

(defrule fireActivityTypeQuestion
?desired <- (desired (activityType "NULL"))
=>
(assert (ask (question "What kind of activities do you wish to take part in?") (choice "Outdoor" "Water") (slotName "activityType") (questionType "CHECKBOX")))
)

(defrule fireWaterActivityQuestion
?desired <- (desired (activityType $?x "Water" $?y) (waterActivity "NULL"))
=>
(assert (ask (question "What kind of water activities do you wish to take part in?") (choice "Surfing" "Scuba" "Snorkelling" "Water Skiing" "Water Park" "Wind Surfing" "Dolphin Encounter") (slotName "waterActivity") (questionType "CHECKBOX")))
)

(defrule fireOutdoorActivityQuestion
?desired <- (desired (activityType $?x "Outdoor" $?y) (outdoorActivity "NULL"))
=>
(assert (ask (question "What kind of outdoor activities do you wish to take part in?") (choice "Mountain Biking" "Rock Climbing" "Hiking" "Snow Skiing" "Zip Line" "Horseback Riding") (slotName "outdoorActivity") (questionType "CHECKBOX")))
)

(defrule countAllDestinationsOnce
?undone<-(count destination undone)
(exists (destination))
=>
(retract ?undone)
(do-for-all-facts ((?f destination)) TRUE
      (bind ?*totalDestination* (+ ?*totalDestination* 1)))
(assert (count destination done))
)

(defrule retractAskFactIfLeftOneDestination
(count destination done)
(test (< ?*totalDestination* 2))
(exists (ask))
=>
(do-for-all-facts ((?f ask)) TRUE (retract ?f))
)


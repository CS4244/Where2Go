; ; In destination, NULL is not allowed except in
; ; waterActivity and outdoorActivity.
; ; Here, a NULL means that the destination
; ; has a lack of water/outdoor activity
(deftemplate destination
	(slot name (type STRING))
	(slot englishSpeaking (type STRING) (default "No")(allowed-strings "Yes" "No"))
	(multislot visitPreference (type STRING)(allowed-strings "Off the Beaten Path" "Romantic" "Peace and Quiet" "Local Culture" "Nightlife" "Family Friendly" "Leisure"))
	(slot region (type STRING)(allowed-strings "North America" "Europe" "Other"))
	(multislot geography (type STRING)(allowed-strings "Beach" "City" "Volcano" "Coastal" "Mountains" "Forest" "Island"))
	(multislot leisure (type STRING)(allowed-strings "Casino" "Spa" "Shopping" "Theme Park" "Landmarks" "Zoo" "Museum" "Dining"))
	(slot daysReq (type INTEGER))
	(slot budget (type INTEGER))
	(multislot activityType (type STRING)(allowed-strings "Water" "Outdoor"))
<<<<<<< .merge_file_a06992
	(multislot waterActivity (type STRING)(allowed-strings "NULL" "Surfing" "Scuba" "Snorkelling" "Water Skiing" "Water Park" "Wind Surfing" "Dolphin Encounter"))
=======
	(multislot waterActivity (type STRING)(allowed-strings "NULL" "Surfing" "Snorkelling" "Water Skiing" "Water Park" "Wind Surfing" "Dolphin Encounter"))
>>>>>>> .merge_file_a17316
	(multislot outdoorActivity (type STRING)(allowed-strings "NULL" "Mountain Biking" "Rock Climbing" "Hiking" "Snow Skiing" "Zip Line" "Horseback Riding"))
	(slot weather (type STRING) (allowed-strings "Hot" "Warm" "Mild" "Cold" "Frozen"))
)


; ; desired stores user's desired destination properties
; ; idea is that "NULL" can be used to identify unasked properties
; ; NULL is allowed for all slots here, as it is used to pattern match and fire questions.
(deftemplate desired
	; ;(slot name (type STRING))
	(slot englishSpeaking (type STRING) (default "NULL")(allowed-strings "NULL" "Yes" "No"))
	(multislot visitPreference (type STRING) (default "NULL") (allowed-strings "NULL" "Off the Beaten Path" "Romantic" "Peace and Quiet" "Local Culture" "Nightlife" "Family Friendly" "Leisure"))
	(slot region (type STRING)(allowed-strings "NULL" "North America" "Europe" "Other"))
	(multislot geography (type STRING) (default "NULL") (allowed-strings "NULL" "Beach" "City" "Volcano" "Coastal" "Mountains" "Forest" "Island"))
	(multislot leisure (type STRING) (default "NULL") (allowed-strings "NULL" "Casino" "Spa" "Shopping" "Theme Park" "Landmarks" "Zoo" "Museum" "Dining"))
	(slot daysReq (type INTEGER))
	(slot budget (type INTEGER))
	(multislot activityType (type STRING) (default "NULL") (allowed-strings "NULL" "Water" "Outdoor"))
	(multislot waterActivity (type STRING) (default "NULL") (allowed-strings "NULL" "Surfing" "Snorkelling" "Water Skiing" "Water Park" "Wind Surfing" "Dolphin Encounter"))
	(multislot outdoorActivity (type STRING) (default "NULL") (allowed-strings "NULL" "Mountain Biking" "Rock Climbing" "Hiking" "Snow Skiing" "Zip Line" "Horseback Riding"))
	(slot weather (type STRING) (allowed-strings "NULL" "Hot" "Warm" "Mild" "Cold" "Frozen"))
)

; ; The initial facts
(deffacts the-facts
<<<<<<< .merge_file_a06992
	(destination (name "Ha Long Bay") (englishSpeaking "No") (visitPreference "Off the Beaten Path" "Peace and Quiet" "Local Culture" "Leisure") (region "Other") (geography "Beach" "Mountains" "Forest") (leisure "Casino" "Landmarks") (activityType "Water" "Outdoor") (waterActivity "Surfing" "Scuba") (outdoorActivity "Hiking") (weather "Warm"))
=======
	(destination (name "Ha Long Bay") (englishSpeaking "No") (visitPreference "Off the Beaten Path" "Peace and Quiet" "Local Culture" "Leisure") (region "Other") (geography "Beach" "Mountains" "Forest") (leisure "Casino" "Landmarks") (activityType "Water" "Outdoor") (waterActivity "Surfing" "Snorkelling") (outdoorActivity "Hiking") (weather "Warm"))
>>>>>>> .merge_file_a17316
	(desired)
)

; ; First, check desired and fire off questions to be asked
(defrule fireWeatherQuestion
?desired <- (desired (weather "NULL"))
=>
(printout t "What is your ideal weather? (1.Hot; 2.Warm; 3.Mild; 4.Cold; 5.Frozen) ")
(bind ?n (read))
(switch ?n 
	(case 1 then (modify ?desired (weather "Hot")))
	(case 2 then (modify ?desired (weather "Warm")))
	(case 3 then (modify ?desired (weather "Mild")))
	(case 4 then (modify ?desired (weather "Cold")))
	(case 5 then (modify ?desired (weather "Frozen")))
)
)

; ; Fire english speaking destination preference question
(defrule fireEnglishSpeakingQuestion
?desired <- (desired (englishSpeaking "NULL"))
=>
(printout t "Do you prefer your destination to be an English-speaking destination? (1.Yes; 2.No) ")
(bind ?n (read))
(switch ?n 
	(case 1 then (modify ?desired (englishSpeaking "Yes")))
	(case 2 then (modify ?desired (englishSpeaking "No")))
)
)

; ; Fire region preference question 
(defrule fireRegionQuestion
?desired <- (desired (region "NULL"))
=>
(printout t "What is your ideal destination region? (1.North America; 2.Europe; 3.Other) ")
(bind ?n (read))
(switch ?n 
	(case 1 then (modify ?desired (region "North America")))
	(case 2 then (modify ?desired (region "Europe")))
	(case 3 then (modify ?desired (region "Other")))
)
)


; ; First multislot question, two strings could be assigned to multislot activityType
; ; Fire activityType preference question
(defrule fireActivityTypeQuestion
?desired <- (desired (activityType "NULL"))
=>
(printout t "What kind of activities do you wish to take part in? (1.Outdoor Activities; 2.Water Activities; 3.Both kinds) ")
(bind ?n (read))
(switch ?n 
	(case 1 then (modify ?desired (activityType "Outdoor")))
	(case 2 then (modify ?desired (activityType "Water")))
	(case 3 then (modify ?desired (activityType "Water" "Outdoor")))
)
)

<<<<<<< .merge_file_a06992
=======
; ; Fire waterActivity preference question
(defrule fireWaterActivityQuestion
?desired <- (desired (activityType $?x "Water" $?y) (waterActivity "NULL"))		
=>
(printout t "What kind of water activities are you interested in?")
(bind ?n (read))
(switch ?n
	(case 1 then (modify ?desired (waterActivity "Surfing")))
	(case 2 then (modify ?desired (waterActivity "Snorkelling")))
	(case 3 then (modify ?desired (waterActivity "Water Park")))
	(case 4 then (modify ?desired (waterActivity "Dolphin Encounter")))
	(case 5 then (modify ?desired (waterActivity "Water Skiing")))
	(case 6 then (modify ?desired (waterActivity "Wind Surfing")))
)
)
>>>>>>> .merge_file_a17316
; ; Fire outdoorActivity preference question
(defrule fireOutdoorActivityQuestion
?desired <- (desired (activityType $?x "Outdoor" $?y) (outdoorActivity "NULL"))			;; this ensures the rule is only fired once when it has not been asked yet.
=>
(printout t "What kind of outdoor activities are you interested in? (1.Hiking) ")
(bind ?n (read))
(switch ?n
	(case 1 then (modify ?desired (outdoorActivity "Hiking")))
<<<<<<< .merge_file_a06992
=======
	(case 2 then (modify ?desired (outdoorActivity "Snow Skiing")))
	(case 3 then (modify ?desired (outdoorActivity "Mountain Biking")))
	(case 4 then (modify ?desired (outdoorActivity "Zip Line")))
	(case 5 then (modify ?desired (outdoorActivity "Rock Climbing")))
	(case 6 then (modify ?desired (outdoorActivity "Horseback Riding")))
>>>>>>> .merge_file_a17316
)
)





;;	(destination (name __) (englishSpeaking __) (visitPreference __) (region __) (geography __) (leisure __) (activityType __) (waterActivity __) (outdoorActivity __) (weather __))
;;	(destination (name __) (englishSpeaking __) (visitPreference __) (region __) (geography __) (leisure __) (activityType __) (waterActivity __) (outdoorActivity __) (weather __))
;;	(destination (name __) (englishSpeaking __) (visitPreference __) (region __) (geography __) (leisure __) (activityType __) (waterActivity __) (outdoorActivity __) (weather __))
;;	(destination (name __) (englishSpeaking __) (visitPreference __) (region __) (geography __) (leisure __) (activityType __) (waterActivity __) (outdoorActivity __) (weather __))
;;	(destination (name __) (englishSpeaking __) (visitPreference __) (region __) (geography __) (leisure __) (activityType __) (waterActivity __) (outdoorActivity __) (weather __))


;;) 
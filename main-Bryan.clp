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
	(multislot waterActivity (type STRING)(allowed-strings "NULL" "Surfing" "Scuba" "Snorkelling" "Water Skiing" "Water Park" "Wind Surfing" "Dolphin Encounter"))
	(multislot outdoorActivity (type STRING)(allowed-strings "NULL" "Mountain Biking" "Rock Climbing" "Hiking" "Snow Skiing" "Zip Line" "Horseback Riding"))
	(slot weather (type STRING) (allowed-strings "Hot" "Warm" "Mild" "Cold" "Frozen"))
)


; ; desired stores user's desired destination properties
; ; idea is that "NULL" can be used to identify unasked properties
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
	(destination (name "Ha Long Bay") (englishSpeaking "No") (visitPreference "Off the Beaten Path" "Peace and Quiet" "Local Culture" "Leisure") (region "Other") (geography "Beach" "Mountains" "Forest") (leisure "Casino" "Landmarks") (activityType "Water" "Outdoor") (waterActivity "Surfing" "Scuba") (outdoorActivity "Hiking") (weather "Warm"))
	(desired)
)

; ; First, check desired and fire off questions to be asked
(defrule fireQuestions
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


;;	(destination (name __) (englishSpeaking __) (visitPreference __) (region __) (geography __) (leisure __) (activityType __) (waterActivity __) (outdoorActivity __) (weather __))
;;	(destination (name __) (englishSpeaking __) (visitPreference __) (region __) (geography __) (leisure __) (activityType __) (waterActivity __) (outdoorActivity __) (weather __))
;;	(destination (name __) (englishSpeaking __) (visitPreference __) (region __) (geography __) (leisure __) (activityType __) (waterActivity __) (outdoorActivity __) (weather __))
;;	(destination (name __) (englishSpeaking __) (visitPreference __) (region __) (geography __) (leisure __) (activityType __) (waterActivity __) (outdoorActivity __) (weather __))
;;	(destination (name __) (englishSpeaking __) (visitPreference __) (region __) (geography __) (leisure __) (activityType __) (waterActivity __) (outdoorActivity __) (weather __))


;;) 
;;question refers to the question to be asked
;; choice refers to the choices available to the user
;;slotName refers to the actual slot name in the desired template
;;For questionType, radio implies question accepts only one answer and checkbox accepts multiple answers. 
(deftemplate ask
	(slot question (type STRING))
	(multislot choice (type STRING))
	(slot slotName (type STRING))
  (slot questionType (type STRING) (allowed-strings "RADIO" "CHECKBOX") (default "RADIO"))
	)


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
	(multislot waterActivity (type STRING)(allowed-strings "NULL" "Surfing" "Scuba" "Snorkelling" "Water Skiing" "Water Park" "Wind Surfing" "Dolphin Encounter"))
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
	(destination (name "Ha Long Bay") (englishSpeaking "No") (visitPreference "Off the Beaten Path" "Peace and Quiet" "Local Culture" "Leisure") (region "Other") (geography "Beach" "Mountains" "Forest") (leisure "Casino" "Landmarks") (activityType "Water" "Outdoor") (waterActivity "Surfing" "Scuba") (outdoorActivity "Hiking") (weather "Warm"))
	(desired)
)

(defrule fireWeatherQuestion
?desired <- (desired (weather "NULL"))
=>
(assert (ask (question "What is your ideal weather") (choice "hot" "warm" "mild" "cold") (slotName "weather")))
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




;;	(destination (name __) (englishSpeaking __) (visitPreference __) (region __) (geography __) (leisure __) (activityType __) (waterActivity __) (outdoorActivity __) (weather __))
;;	(destination (name __) (englishSpeaking __) (visitPreference __) (region __) (geography __) (leisure __) (activityType __) (waterActivity __) (outdoorActivity __) (weather __))
;;	(destination (name __) (englishSpeaking __) (visitPreference __) (region __) (geography __) (leisure __) (activityType __) (waterActivity __) (outdoorActivity __) (weather __))
;;	(destination (name __) (englishSpeaking __) (visitPreference __) (region __) (geography __) (leisure __) (activityType __) (waterActivity __) (outdoorActivity __) (weather __))
;;	(destination (name __) (englishSpeaking __) (visitPreference __) (region __) (geography __) (leisure __) (activityType __) (waterActivity __) (outdoorActivity __) (weather __))


;;) 
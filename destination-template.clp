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

(destination (name "Abu Dhabi")(englishSpeaking "No")(visitPreference "Undiscovered" "Romantic" "Peace and Quiet" "Family Friendly")(region "Other")(geography "Beach" "City" "Seaside" "Mountains" "Island")(leisure "Spa" "Shopping" "Theme Park" "Landmarks" "Zoo" "Museum" "Dining")(daysReq 0)(avgHotel 219)(activityType "Water" "Outdoor")(waterActivity "Surfing" "Snorkelling" "Water Skiing" "Water Park" "Wind Surfing" "Dolphin Encounter")(outdoorActivity "Mountain Biking" "Horseback Riding")(weather "Warm"))

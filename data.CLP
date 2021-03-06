(deftemplate destination
	(slot name (type STRING))
	(slot climate (type STRING) (allowed-strings "summer" "winter" "autumn" "spring"))
	(slot visitMth(type INTEGER) (range 1 12))
	(slot englishSpeaking (type STRING) (default "no")(allowed-strings "yes" "no"))
	(slot visitPurpose (type STRING)(allowed-strings "adventure" "pleasure"))
	(slot region (type STRING))
	(slot budget (type INTEGER))
	(slot activityType (type STRING))
	(slot indoorActivity (type STRING)(default "no")(allowed-strings "yes" "no"))
	(slot outdoorActivity (type STRING)(default "no")(allowed-strings "yes" "no"))
	(slot safety (type STRING)(default "no")))
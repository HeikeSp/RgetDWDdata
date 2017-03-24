getDWDdata_monthly <- function(Messstelle, historisch = TRUE){
	if(is.na(suppressWarnings(as.numeric(Messstelle)))) {
		message("Get station ID by station name (to skip this part provide station ID directly)\n")
		# downloade Stationsliste wenn "Messstelle" ein character ist (Stationsname)
		stationen <- getDWDstations()
		Messstelle_nr <- stationen$Stations_id[which(stationen$Stationsname==Messstelle)]
		if(length(Messstelle_nr)==0) stop(paste0('Messstelle "', Messstelle, '" kann nicht gefunden werden'))
		message("\nStation ID is ", Messstelle_nr, "\n")
		Messstelle <- Messstelle_nr
	}
	#ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/monthly/kl/historical/monatswerte_03987_18930101_20151231_hist.zip
	if(historisch){
		message("\ndownload historical dataset\n")
		filenames <- getURL("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/monthly/kl/historical/", ftp.use.epsv=FALSE, dirlistonly=TRUE)
		filenames <- gsub("\r\n", "\n", filenames)
		filenames <- strsplit(filenames, "\n")[[1]]
		ids <- substr(filenames, 13, 17)
		downloadlink <- paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/monthly/kl/historical/", filenames[which(ids==Messstelle)])
	}else{
		message("\ndownload recent dataset\n")
		downloadlink <- paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/monthly/kl/recent/monatswerte_", Messstelle, "_akt.zip")
	}
	
	# Tempfile erzeugen
	zipfile <- tempfile("DWD_download_", fileext=".zip")
	
	# Zip-Datei runterladen
	dexit = 1
	try(dexit <- download.file(downloadlink,zipfile))
	
	# Exit-Status des downloads überprüfen
	if(dexit!=0){
		message("The file at ", downloadlink, " could not be downloaded.")
		return()
	} 
	
	# Dateinamen aus der Zip-Datei auslesen
	files <- unzip(zipfile, exdir=tempdir())
	
	returnData <- NULL
	
	#---- Daten einlesen ----
	datafile <- files[grepl(x=files, pattern="produkt_monat_Monatswerte")]
	datafileCon <- file(datafile, encoding="ISO-8859-1")
	text <- gsub( ",", ";", readLines(datafileCon, warn=FALSE)) # erstmals beobachtet am 2014-05-01 haben die DWD-Daten nicht mehr "," als Trennzeichen, sondern ";". Allerdings sind manche Ueberschriften nich mit ",", daher diese Korrekturzeile
	close(datafileCon)
	
	returnData$Daten <- read.table(textConnection(text), sep=";", header=TRUE, strip.white=TRUE, na.strings=-999, as.is=TRUE, fill=TRUE)
	
	# letzte Zeile loeschen und Spalte loeschen
	returnData$Daten <- returnData$Daten[-nrow(returnData$Daten),-ncol(returnData$Daten)]
	
	# Alle Kopfzeilen in Großbuchstaben
	colnames(returnData$Daten) <- toupper(colnames(returnData$Daten))
	
	# Datumsspalte von character zu Datum umwandeln
	returnData$Daten$MESS_DATUM_BEGINN <- as.Date(as.character(returnData$Daten$MESS_DATUM_BEGINN), format="%Y%m%d")
	returnData$Daten$MESS_DATUM_ENDE <- as.Date(as.character(returnData$Daten$MESS_DATUM_ENDE), format="%Y%m%d")
	
	# Sich wiederholende character Strings in Faktoren umwandeln:
	returnData$Daten$STATIONS_ID <- as.factor(returnData$Daten$STATIONS_ID)
	
	#---- Temp-Datei loeschen: ----
	unlink(c(zipfile,files))

	return(returnData$Daten)
}
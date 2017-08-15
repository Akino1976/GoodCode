#!/usr/bin/env Rscript 
##########################################################################################
# Use for inserting person into table for credit data for api function
##########################################################################################
library(XML)
library(jsonlite)
#options(error = function() traceback(2))

LOGS					<- file.path("../logs")
RUN						<- file.path(LOGS, "run.txt")

file.exists(RUN) &&  file.remove(RUN)
													
RFiles 					<- list.files(path = '.' , include.dirs = TRUE, pattern = "*.R$", 
											recursive = TRUE, full.names = TRUE)
# Load common functions and files , also Meocode and Person table is loaded
source(file = grep("commonfunction", RFiles,  value = TRUE, perl = TRUE, ignore.case = TRUE) )
worker.init		<- function( pkg ){
			for( p in pkg ){
				library( p, character.only = TRUE)
			}
			NULL
		}

#starts with HenkiloTiedot
finlandSourc			<- list( "ostovoima" = list("Purchasing_Power" = 
													c(	"1" = 'Lowest_class',
														"2" = "Lower_medium_class",
														"3" = "Higher_medium_class",
														"4" = "Highest_class")),
								"koulutustaso" = list("Education_level" = 
														c(	"1" = "Basic_education", 
															"2" = "Intermediate_grade",
															"3" = "Higher_eduacation")),
								"elamanvaihe" = list("Life_state" = 
												c("1" = "Young_adults_without_children_aged_18-35",
												"2" = "Family_with_children",
												"3" = "Adults_without_children_aged_35-64",
												"4" =  "Seniors_over_65_years")),
								"asuinaluetyyppi" = list("Type_of_residential_area" = 
												c(	"1" = "Rural", 
													"2" = "Densely_populated_areas",
													"3" = "Towns",
													"4" = "Big_cites",
													"5" = "Metropolitan_area")),
								"asunnonomistussuhde" = list("Ownership_of_housing" = 
													c(	"1" = "Own",
														"2" = "Rented")),
								"asuntoTyyppi"		= list("Housing_type" = 
													c(	"1" = "One_family_houst",
														"2" = "Block_house/aparment")),
								"maksuHairioriski"  = list("Risk_of_payment_defaults" = 					
															c(	"1" = "Small", 					
													"2" = "Minor",
													"3" = "Normal",
													"4" = "High")),
								"SyyKoodi"			= list("Reason_Code" = 
															c("1" = "Credit_application"),
																"2" = "Credit_control",
																"3" = "Debt_collection",
																"6"	= "person_own_approval"),		
								"LuottoLuokka"		= list("Credit_quality"),
								"MerkintojenLkm"	= list("Number_of_remarks")		
						)



args1			<- ifelse( length( commandArgs(TRUE)) > 0, commandArgs(TRUE), "survival")
args1			<- tolower(args1)
		
insertPersonDB		<- function(ids, country, checkDate, sourceT,  sourceData )
{
		date			<- ifelse(missing(checkDate), Sys.Date(), checkDate)
		country			<- ifelse(missing(country), 'SE', country)
		sourceT			<- ifelse(missing(sourceT), 'CREDITCHECK', sourceT)
		if(missing(sourceData) || missing(ids)) stop("Script is stopped, missing source")
		if( !  exists('con'))
		{
				con	 <- try( dbConnect(MySQL(),	user = 'serdara', 	password = 'aiiR1eX5d', 
 										host = '193.182.121.238', dbname = "kriita_survey", port = 3306),
 										silent = TRUE)	
 				if(inherits(con, 'try-error'))
 				{
 					closeConnections()
 					con	 <- try( dbConnect(MySQL(),	user = 'serdara', 	password = 'aiiR1eX5d', 
 										host = '193.182.121.238', dbname = "kriita_survey", port = 3306),
 										silent = TRUE)	
 				}	
 		}									
 	

		insertQuery		<- sprintf("INSERT INTO PaylevoCreditCheck( `personId`, `country`, `checkDate`, `sourceType`, `sourceData` ) 
											VALUES('%s','%s', '%s', '%s', '%s')",  ids , country, date , sourceT , sourceData )		
		insertQuery 	<- gsub("[\t\n\t]", "", insertQuery)
		res				<-  dbSendQuery(con, statement =  insertQuery)	
			
		cat("###############Sucess###############", "\n")
		
		dbClearResult(res)
}		
readXML			<- function(x, ll = finlandSourc)
{
	if(grepl("Error", x[, sourceData]) )
	{
		return(NULL)
	}
	xml_1			<-	xmlParse(  x[, sourceData], useInternal = TRUE)
	.ssn			<- x[, trimws(ssn)]
	.person			<- x[, trimws(personId)]
	.country		<- x[, trimws(country)]
	.checkdate		<- x[, trimws(checkDate)]
	.sourcetype		<- x[, trimws(source)]
	cat("\nPersonId : ", .person	 , "\n")
	if( regexpr( "^(FI)$",.country) > 0 )
	{
		tmpFun			<- function(x)
		{
			Names		<- names(x)
			Step1		<- unlist(x)
			Step2		<- data.table(Names = names(Step1), values = Step1)
			return(Step2)
		}
		xml_1			<- xmlRoot(xml_1)
		xml_2  			<- getNodeSet(xml_1, "//response:Response")[[1]]
		Step1			<-  lapply(xmlToList(xml_2), function(x) return ( tmpFun(x)) )
		GetNames		<-	names(Step1)		
		Source			<- data.table( )
		for( rr in GetNames )
		{
			 step1		<-	Step1[[rr]]
			 step1[, Key := rr]
			 Source 	<- rbind(Source, step1)
			 
		}	 		 
			rmKeys			<- '(KayttajaTunnus|AsiakasTunnus|Versio|KysyttyHenkiloTunnus)'
			Names			<- names(ll)
			reqKey			<- paste0( Names, collapse = "|")
			dataDT1			<-  Source[grepl(reqKey, Names, ignore.case = TRUE), ]
			dataDT1[, c(1:3) := lapply(.SD, trimws), .SDcols = 1:3]			
			dataDT1[, keys1 := gsub(".+\\.(.*)$", "\\1", Names)]
			for( i in Names)
			{
				Names1			<-  ifelse( is.null(names(finlandSourc[[i]])) , unlist( finlandSourc[[i]] ), names(finlandSourc[[i]])) 
				
				dataDT1[grepl(i, keys1, ignore.case = TRUE), keys2 := Names1]
			}
			dataDT1				<- dataDT1[, .(keys = toupper(keys2), values)]
	} else {
		xml_2  			<- getNodeSet(xml_1, "//GETDATA_RESPONSE")[[1]]
		xml_3 			<- xmlSApply(xml_2, xmlValue)
		if(	length(xml_3) < 10 )
		{
			return( message("no values inside xml: ssn ", 	.ssn) )
		}
	
		dataDT 			<- data.table(keys	= names(xml_3), values = xml_3)
		rmKeys			<- '(PNR|FIRST_NAME|GIVEN_NAME|LAST_NAME|CO_ADDRESS|REGISTERED_ADDRESS|ADDRESS|ZIPCODE|TOWN|COMMUNITY|FORSAMLING|FORSAMLINGNO|VAT)'
		dataDT1			<-  dataDT[!keys %like% rmKeys, ]
		dataDT1[values == '',  values := NA]
		dataDT1[, ':=' ( keys 	= toupper(keys),
						values 	= as.character(values)) ]	
	}	
	sourceData		<-	jsonlite::toJSON( dataDT1)
	insertPersonDB(ids = .person, country = .country, checkDate = .checkdate, sourceT = .sourcetype, sourceData = sourceData)
	cat("************* DONE***************")	
}		


##############################################################################
# Funtion used for getting the persons
##############################################################################			
closeConnections 		<- function( )
{
	allCons 	<- 	dbListConnections(MySQL())
	for( cons in allCons) dbDisconnect(cons)
	con	 <- try( dbConnect(MySQL(),	user = 'serdara', 	password = 'aiiR1eX5d', 
 									host = '193.182.121.238', dbname = "kriita_survey", port = 3306),
 									silent = TRUE)
 														
 	tmp	<- dbGetQuery(con, "show processlist")
 	setDT(tmp)
 	tmp1			<- tmp[db == 'kriita_survey' & ! grepl("processlist", Info)]
 	if( NROW(tmp1) > 100) {
 		for( i in 30:NROW(tmp1))
 		{
 			cat("\n==", i, "\n")
 			Kill			<- paste0("KILL ", tmp1[i, Id])
 			dbGetQuery(con, Kill)
 		}
 	} else {
 		return()
 	} 
 	dbDisconnect(con)
									
}			
			




#####################################################################################################
# Load data 
#####################################################################################################
Query 	<- sprintf("SELECT 	ei.*,
							iv.personId,
							iv.siteId,
							iv.ocrNumber,
							iv.ocrNumberReminder,
							iv.dueDate,
							iv.playerId,
							ss.type,
							ss.operatorId, 
							iv.state,
							IF( ei.playingCurrency = 'EUR' 
									AND ei.invoiceCurrency = 'SEK', 
										ei.originalSettlementFee*ei.conversionRate, 
											ei.originalSettlementFee) AS TransactionFee
							FROM ErpInvoice ei 
						INNER JOIN Invoice iv ON (ei.invoiceNumber = iv.invoiceNumber)
						INNER JOIN Site ss ON (iv.siteId = ss.siteId)")


ErpInvoice 				<- getData(query = Query )
ErpInvoice[order(createdAt), Count := 1:.N, by = .(personId)]
Query				<- "SELECT * FROM SsnLookupLog WHERE type = 'CREDITCHECK' AND source REGEXP '(CREDITSAFE|FINLAND)'"
SsnLookupLog 		<- getData(query = Query)
SsnLookupLog 		<- merge(SsnLookupLog , Person[, .(ssn, personId)], by = 'ssn', all.x = TRUE)
SsnLookupLog[, checkDate := as.character(as.Date(createdAt))]
PaylevoCreditCheck	<- getData(query = "select * from kriita_survey.PaylevoCreditCheck")
if(FALSE){
	tmp		<- PaylevoCreditCheck[, .N, by = .(personId, checkDate)][N >1]
	con			<- try( dbConnect(MySQL(),	user = 'serdara', 	password = 'aiiR1eX5d', 
 							host = '193.182.121.238', dbname = "kriita_survey", port = 3306),
 							silent = TRUE)

	for( i in seq_len(NROW(tmp)))
	{
		cat("==========================================================")
	#	PaylevoCreditCheck[, .N, by = .(personId)][ N >1]

		oo		<- tmp[i, personId]
		cat("\n Person: ", oo, "\n")
		dbSendQuery(con, statement =  sprintf("delete  from PaylevoCreditCheck where personId = '%s'", oo))	
		cat("==========================================================")
	}
}

selectionDT			<- merge(	PaylevoCreditCheck[, .(checkDate, personId)], 
								SsnLookupLog[, .( checkDate, personId)], by = c('checkDate', 'personId'), all.x = TRUE) 
idx					<- unique(selectionDT$personId)
idx1				<- ErpInvoice[, unique(personId) ]
setkey(SsnLookupLog, personId)
setkey(ErpInvoice, personId)
#ErpInvoice[J(idx)]
#upDatePerson		<- SsnLookupLog[!J(idx)][!is.na(checkDate) & source == 'CREDITSAFE', unique(personId)]
upDatePerson			<- SsnLookupLog[!J(idx)][!is.na(personId)  & source == 'CREDITSAFE' , personId]
if( length(upDatePerson) == 0)
{
	stop("error script")
} else {
rm(idx, idx1)
cat("Going to update ", length(upDatePerson), "persons", file = RUN , append = TRUE)
SsnLookupLog_1		<- unique(SsnLookupLog[J(upDatePerson)], by = 'personId', fromLast = TRUE)
SsnLookupLog_1 		<- SsnLookupLog_1[!grepl("faultcode|Error", sourceData)]
Nrow				<- NROW(SsnLookupLog_1)
if(Nrow > 100)
{
	logOutput		<- file.path('.', 'out.txt')
	library(doSNOW)
	library(doParallel)
	Tid <- system.time({
			 cores 	<- getOption("mc.cores", detectCores())
			cl		<- makeCluster( cores, outfile = logOutput ) 
			clusterCall( cl, worker.init,  c('data.table', 'XML', 'RMySQL', 'jsonlite'))
			## Används för lägga in webExtrac och data in i environmnet
			clusterExport(cl, c("SsnLookupLog_1", "readXML","logOutput","insertPersonDB","closeConnections"), envir = environment())
			Output	<- parLapply(cl, 1:NROW( SsnLookupLog_1 ), 
				function( x ) readXML( SsnLookupLog_1[x]) )
			stopCluster(cl)
		})[1:3]
} else {
	for( x in 1:Nrow)
	{
		cat(x, "\n")
		readXML( SsnLookupLog_1[x] )
	}
}	
	print("succes", quote= FALSE, file=stderr() )	
} ## ifelse
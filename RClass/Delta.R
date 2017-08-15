#!/usr/bin/env Rscript 
##########################################################################################
# This script is used for all sort of task that aren't to be for 
# reporting purposes, just for solvning different task
##########################################################################################
options(scipen = 999)
options("openxlsx.dateFormat" = "yyyy-mm-dd")

Sys.setenv("R_ZIPCMD" = "/usr/bin/zip") ## makes openxlsx works in centos

scoreScale			<- c('MycketHög' = "c(70,100)",	'Hög' = "c(40, 69)", "Medel" = "c(20,39)", "Låg" = "c(1,19)")
scoreNum 			<- list('1' = c("intervall" = "c(70,100)", 'time' = 120 ),
							'2' = c("intervall" = "c(40, 69)", 'time' = 120 ),
							'3' = c("intervall" = "c(20, 39)", 'time' = 60 ),
							'4' = c("intervall" = "c(1, 19)", 'time' = 60 )
)
scaleOrder			<- c('LågLåg', 'Låg', 'Medel', 'Hög', 'MycketHög')


library(methods)	

"pathSet"			<- function( x )
{
	if( is( x, "character"))
	{
		cmd				<- paste0("echo ", x)
		cmd1			<- pipe( cmd , open = "r")
		path			<- scan(cmd1, what = "character")  
		close(cmd1)
		return(path)
	} else {
	 	stop("Input is ", class( x ), " and not valid in")
	}	
}



FULLPATH		<- pathSet( x = "$PWD" )
.HOME			<- dirname(FULLPATH)
# Set the home diretory 
DIR				<- basename(FULLPATH)
# All R-files 
RFiles 					<- list.files(path = '.' , include.dirs = TRUE, pattern = "*.R$", 
											recursive = TRUE, full.names = TRUE)
# Load common functions and files , also Meocode and Person table is loaded
source(file = grep("commonfunction", RFiles,  value = TRUE, perl = TRUE, ignore.case = TRUE) )


DATA					<- file.path(FULLPATH, "Data")
LOG						<- file.path(FULLPATH, "logs")
GRAF					<- file.path(FULLPATH, "GRAF")

for( dd in c(DATA, LOG, GRAF))
{
	if(! file.exists(dd) )
	{ ## If missing dir
		dir.create(dd, recursive = TRUE)
	}
}

## Further pkg loaded
Packages			 	<- c('reshape2', 'reshape', 'ggthemes', 
							'RColorBrewer','scales', 'ggplot2',  'grid',
							'gridExtra',"Hmisc", "jsonlite")
## Set path and load pkgs
Pack		<- new("startUps", pkgs = Packages, path = dirname(FULLPATH) )
Pack$instant_pkgs()
## Get the lastest rates from ExchangeRate 


Pack$setDirs( )
CurrentDate		<- Sys.Date( )
args1			<- ifelse( length( commandArgs(TRUE)) > 0, commandArgs(TRUE), "deltalist")
args1			<- tolower(args1)



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

Step1 				<- getData(query = Query )



Query 				<- sprintf("SELECT eph.* ,
										ss.type,
										ss.operatorId
								FROM ErpPaymentHistory eph 
								INNER JOIN Invoice ei ON (eph.invoiceNumber = ei.invoiceNumber)
								INNER JOIN Site ss ON (ei.siteId = ss.siteId)" 	 )


Step2 				<- getData(query = Query )




## Estimate the realmoney and get the last date 
moneyDT					<-   Step2[grepl('(m|r)',type) , .(money = SUM(amount), moneyDate =  paymentDate[.N]), by = .(invoiceNumber)]

Step1 					<- merge(Step1, moneyDT, by = 'invoiceNumber', all.x = TRUE)
Step1[is.na(money), money := 0]
rm(moneyDT)


compensationDT			<-  Step2[grepl('c|e|p', type), .(compensation = SUM(amount), compensationDate = paymentDate[.N]), by = .(invoiceNumber)]

Step1 				<- merge(Step1, compensationDT, by = 'invoiceNumber', all.x = TRUE)
Step1[is.na(compensation), compensation := 0]

rm(compensationDT)

ErpInvoice			<- copy(Step1)
ErpInvoice[order(createdAt), Count := 1:.N, by = .(personId)]
if( regexpr("deltalist", args1, ignore.case = TRUE) > 0 )
{
	QueryBuild			<- function(x)
	{
		Query				<- sprintf("select des.personId, erp.country, des.checkDate, des.varData from kriita_survey.%s des
							inner join kriita_db1.Invoice inv on(inv.personId = des.personId)
							inner join kriita_db1.ErpInvoice erp on(inv.invoiceNumber = erp.InvoiceNumber)
							where erp.collectionParty rlike '.*Delta.*'", x)
						
		return(Query)		
	}						
		Query			<-  QueryBuild(x = 'DeltaSafe')				
		checkDT 		<- getData(query = Query)
		checkDT			<- checkDT[, .SD[.N], by = .(personId)] ## get the last ones
		checkDT[, .(personId , checkDate)]
		setnames(checkDT, "varData", "sourceData")
	 	Query		<- sprintf("select des.personId 
	 									, des.checkDate
	 									, des.country
	 									, des.sourceData from kriita_survey.%s des
						inner join kriita_db1.Invoice inv on(inv.personId = des.personId)
						inner join kriita_db1.ErpInvoice erp on(inv.invoiceNumber = erp.InvoiceNumber)
						where erp.collectionParty rlike '.*Delta.*'", "PaylevoCreditCheck")		
		creditData1			<- getData(query = Query)
		creditData1			<- creditData1[, .SD[.N], by = .(personId)]
		creditData2			<- rbind(creditData1[, .( checkDate, personId, country, sourceData )], checkDT[, .(checkDate ,personId, country ,sourceData)])
		creditData2 		<- unique(creditData2[order(checkDate)], by = 'personId', fromLast = TRUE)
		rm(creditData1)
		gc(reset = TRUE)
		require(doSNOW)
		require(doParallel)
		## Function for loading pkg for parallel enviornment
		worker.init		<- function( pkg ){
			for( p in pkg ){
				library( p, character.only = TRUE)
			}
			NULL
		}
	
	f		<- function( x )
	{
		.personId		<- x[, trimws(personId)]
		.checkdate		<- x[, trimws(checkDate)]
		sourceData		<- jsonlite::fromJSON(x[, sourceData])
		setDT(sourceData)
		rmKeys			<- '(ADDRESS|COMMUNITY|ZIPCODE|FORSAMLINGNO|FORSAMLING|TOWN)'
		sourceData1 		<- sourceData[!keys %like% rmKeys]
	
		return( sourceData1[,  .(personId = .personId , keys, values)] )
	}
			
			
	logOutput			<- file.path(LOG, 'log.txt')		
	OutPut				<- list( )		
	Tid <- system.time({
			cores 	<- getOption("mc.cores", detectCores())
			cl		<- makeCluster( cores, outfile = logOutput ) 
			clusterCall( cl, worker.init,  c('data.table', 'jsonlite'))
			## Används för lägga in webExtrac och data in i environmnet
			clusterExport(cl, c("creditData2","OutPut", "f","logOutput"), envir = environment())
			Output	<- parLapply(cl, 1:NROW( creditData2 ), 
				function( x ) f( creditData2[x]) )
			stopCluster(cl)
		})[1:3]

	
	DataSet			<- rbindlist(Output)
	DataSet[, keys := gsub("BODY\\.(.*)", "\\1", keys)]
	DataSet 		<- DataSet[!grepl("\\.", keys), ]
	getKeys			<- '(AGE|FINAL_TAX|INCOME|DEBT|SCORING)'
	DataSet1		<- unique(DataSet[keys %like% getKeys], by = c('personId', 'keys'), fromLast =TRUE)
	CreditData		<- dcast.data.table(  DataSet1,    personId  ~ keys  )
	rm(DataSet, DataSet1); gc(reset = TRUE)
	CreditData[, personId := as.character(personId)]

	## limitDays for using the conditional credit check
	for( i in names(scoreNum) )
	{
		cat(i, "\n")
		step1			<- scoreNum[[i]]
		
		step2			<- parse(text =  step1['intervall'])
		CreditData[ SCORING %between% eval(step2), ':=' ( level = i, limitDays = step1['time'])] 
	} 
	CreditData[SCORING <= 0, ':=' ( level = "5", limitDays = "60")]
	
	for( i in 1:length(scoreScale))
	{
		cat(i, "\n")
		step1			<- scoreScale[i]
		step2			<- parse(text = step1)
		CreditData[SCORING %between% eval(step2), scale := names(step1) ] 
	} 

	CreditData[ is.na(scale),  scale := 'LågLåg']


	rm(step1, step2, i )

	
	DeltaErp		<- ErpInvoice[grepl("Delta", collectionParty), ]
	DeltaErp[, personId  := as.character(personId)]
	setkey(DeltaErp, personId)
	setkey(CreditData, personId)
	### Get the ids 
	idx 			<- DeltaErp[ state == 'WAITING_TO_BE_COLLECTED' & is.na(suspendedAt) & collectionDueDate <= Sys.Date() - days(4) , unique(personId)]
	## Summary statistics 
	HistorDT			<- DeltaErp[J(idx)]
	HistorDT[, debtLeft := totalAmount - (money + compensation) ]

	## aggregeta the historical data
	tmpDT			<- HistorDT[ state == 'WAITING_TO_BE_COLLECTED', .(personId, debtReference, invoiceNumber , createdAt , debtLeft = as.numeric(round(debtLeft)))]					
	tmpDT[order(personId ,createdAt), Count := 1:.N, by = .(personId)]					

	Step1				<- dcast.data.table(tmpDT, personId ~ Count, value.var = "debtLeft")
	Names				<- colnames(Step1)	
	Names1				<- paste0("no", setdiff(Names, c("personId")))
	setnames(Step1, 2:NCOL(Step1), Names1)
	Step1				<- RowStat(Step1, id = 'personId', type = 'sum')	
	
 	nrDays				<- HistorDT[state == 'WAITING_TO_BE_COLLECTED', .SD[.N],  by = .(personId)][, .(personId, nrDaysLastInvoice = as.integer( difftime(Sys.Date(), collectionDate, units = 'days') )) ]
	
	Step1 				<- merge(Step1, nrDays, by = 'personId', all.x = TRUE)
	
	
	DataSet			<-	HistorDT[, .(			Antal 				= .N, 
												noRisk				= SUM(type == 'NORMAL'),
												noPaid				= SUM(grepl("PAID", state)),
												noDeltaCollection	= SUM(grepl("Delta", collectionParty) & grepl('WAITING_TO_BE_COLLECTED', state)),
												noPaidCollection	= SUM(!is.na(collectionDate) & grepl('PAID', state)),
												debtReference		= debtReference[.N]
	), by = .(personId = as.character(personId))]

	

	
	DataSet				<- merge(DataSet, Step1, by = 'personId', all.x = TRUE)
	rm(Step1, tmpDT)
	DataSet				<- merge(DataSet, Person[, .(personId =  as.character(personId), ssn)], by = 'personId', all.x = TRUE)
	DataSet				<- merge(DataSet, CreditData, by = 'personId', all.x = TRUE)

	toNumeric				<- c('SCORING','TOTAL_INCOME','TOTAL_INCOME2','DEBT_SUM','DEBT_AMAL_SUM','DEBT_SUM')
	Names					<- names(DataSet)
	NamesNum				<- which(Names %in% toNumeric )
	DataSet[, c(NamesNum) := lapply(.SD, as.numeric ), .SDcols = NamesNum]
	
	DataSet 				<- merge( DataSet , 
									Person[,.(personId =  as.character(personId), ssn, firstName, lastName, email, phone)],
								by = c('ssn','personId'), all.x = TRUE)	
	
	DataSet[, HyperLink := paste0("https://kriita2.meacode.net/debt/edit/id/", debtReference)]
	names(DataSet$HyperLink)		<- paste("HyperLink", 1:NROW(DataSet))
	class(DataSet$HyperLink)		<- 'hyperlink'
	

	
	
	########################################################################
	# OUTPUT
	########################################################################
	SummaryDT		<- HistorDT[state ==  'WAITING_TO_BE_COLLECTED', 
									.(	NoInvoice			= .N,
										AverageInvoice		= round(.N/uniqueN(personId)),
										Factoring 			= SUM( type == 'NORMAL' ), 
										FactoringMoney		= SUM(ifelse(  type == 'NORMAL', round(debtLeft), 0)),
										AdminMoney			= SUM(ifelse(  type != 'NORMAL', round(debtLeft), 0))
					), by = .( operatorId )][order(NoInvoice)]	

	SummaryDT 		<- rbind(SummaryDT , SummaryDT[, lapply(.SD, tmpSum)])
	SummaryDT[is.na(operatorId), `:=` ( AverageInvoice  = NA, operatorId = 'Total')]
	Seq				<- seq(-30, 100, 10)
	
	DataSet[,	SCORING_x := cut2(SCORING, Seq)]
	########################################################################
	# OUTPUT
	########################################################################
	DataSet_1 	<- DataSet[, .( AntalPersoner 	= .N, 
								DebtOut 		= SUM(Total),
								SnittSkuld 		= round(MEAN(DEBT_SUM)),
								SnittInkomst	= Format(MEAN(TOTAL_INCOME)),
								SnittÅlder		= round(MEAN(as.integer(AGE)))), by = .(SCORING  = SCORING_x)][order(SCORING)]
	DataSet_1	<- rbind(DataSet_1, DataSet_1[, lapply(.SD, tmpSum)])
	
	DataSet_1[.N, `:=` ( SnittSkuld = NA, SnittInkomst = NA, SnittÅlder = NA)]
	DataSet_1[.N, SCORING := 'Total']
	Name1		<- sprintf('%s_%s.xlsx',  args1 , CurrentDate)
	fullNames	<-  file.path(DATA, Name1)			
	wb				<- openxlsx::createWorkbook()	
	name 		<- 'Statistic'
	addWorksheet( wb, name)
	setColWidths(wb, sheet = name, cols = 1:NCOL(SummaryDT), widths = 25)
	Count		<- 1
	for(op in c("SummaryDT", "DataSet_1"))
	{
			
			Step1			<- get(op)
		
			writeData(wb, sheet = name,  x = Step1  , 
							startRow =  Count, headerStyle = hs1)
				
			
			Count			<- Count + NROW(Step1) +2	
			
	}		
	
	
	
	setValue(name = "ScoringOverZero", colum = c(1:NCOL(DataSet)), data = DataSet[SCORING > 0][order(nrDaysLastInvoice)] ) 
	setValue(name = "ScoringUnderZero", colum = c(1:NCOL(DataSet)), data = DataSet[SCORING < 0][order(nrDaysLastInvoice)] ) 
	openxlsx::saveWorkbook(wb, file = fullNames, overwrite = TRUE)
	
	
	#./swData.bash -m -f 'Delta.R' -c 'deltalist'
	if( file.exists(fullNames) )
	{
			print("succes", quote= FALSE, file=stderr() )	
	} else {
			print("error", quote= FALSE, file=stderr() )	
	}
	

}	
		
#!/usr/bin/env Rscript 
##########################################################################################
# This script is used for all sort of task that aren't to be for 
# reporting purposes, just for solvning different task
##########################################################################################
options(scipen = 999)
options("openxlsx.dateFormat" = "yyyy-mm-dd")

Sys.setenv("R_ZIPCMD" = "/usr/bin/zip") ## makes openxlsx works in centos


DeductionRule	<- list( 	'co' = 'COMPENSATION',
							'pc' = 'PAPER_INVOICE_CHARGE',
							'ci' = 'COMPENSATION_INTEREST',
							'cr' = 'COMPENSATION_REMINDER',
							'cf' = 'INVOICING_CHARGE',
							'cl' = 'CANCELLATION',
							'ma' = 'MANUAL_DEDUCTION',
							'fb' = 'fallbackDeliveryCharge',
							'si' = 'Interest_settlement',
							'sr' = 'Reminder_charge_settlement',
							'rh' = 'Reserve_hold',
							'rr' = 'Reserve release')


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



library(methods)	
scoreScale		<- c('MycketHög' = "c(70,100)",	'Hög' = "c(40, 69)", "Medel" = "c(20,39)", "Låg" = "c(1,19)")

worker.init		<- function( pkg ){
			for( p in pkg ){
				library( p, character.only = TRUE)
			}
			NULL
		}

regexp			<- tolower(paste0( names(finlandSourc), collapse = '|'))
paste0(as.character(do.call("rbind",lapply(finlandSourc, function(x) unlist(names(x)[[1]])))), collapse = "|")
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

# All R-files 
RFiles 					<- list.files(path = '.' , include.dirs = TRUE, pattern = "*.R$", 
											recursive = TRUE, full.names = TRUE)
# Load common functions and files , also Meocode and Person table is loaded
source(file = grep("commonfunction", RFiles,  value = TRUE, perl = TRUE, ignore.case = TRUE) )

# pc, ci,cr,cf,ma,fb
DeductionRule 		<- data.table( type = c('co', 'pc', 'ci', 'cr','cf','cl','ma','fb'), 
									what = c('COMPENSATION', 'PAPER_INVOICE_CHARGE', 'COMPENSATION_INTEREST', 
											'COMPENSATION_REMINDER', 'INVOICING_CHARGE', 'CANCELLATION', 
											'MANUAL_DEDUCTION', 'fallbackDeliveryCharge')
)

FULLPATH		<- pathSet( x = "$PWD" )
DIR						<- basename(FULLPATH)

DATA					<- file.path(FULLPATH, "Data")
GRAF					<- file.path(FULLPATH, "GRAF")
LOG						<- file.path(FULLPATH, 'logs')
FULLPATH				<- pathSet( x = "$PWD" )
.HOME			<- dirname(FULLPATH)
CreditDataPath	<-file.path(DATA, "credit.RData")
# Set the home diretory 


RUN			<- file.path(LOG, "run.txt")
file.exists(RUN) && file.remove(RUN)
library(survival)



last_day 				<- function(date) {
  			 ceiling_date(date, "month") - days(1)
}

invlogit <- function( x ) {
  step1 <- 1/(1+exp( - x ))
  return( round( step1, 4))
}

for( i in c(GRAF, DATA , LOG))
{
	if(! file.exists(i) )
	{ ## If missing dir
		dir.create(i, recursive = TRUE)
	}
}
rm(i)

## Further pkg loaded
Packages			 	<- c('reshape2', 'reshape', 'ggthemes', 'bit64','doSNOW','rpart','qgraph',
							'RColorBrewer','scales', 'ggplot2',  'grid','doParallel','gmodels',
							'gridExtra',"Hmisc", "jsonlite", 'ggfortify', 'caret') 
							
## Set path and load pkgs
Pack		<- new("startUps", pkgs = Packages, path = dirname(FULLPATH) )
Pack$instant_pkgs()
## Get the lastest rates from ExchangeRate 

Pack$setDirs( )

CurrentDate		<- Sys.Date( )
args1			<- ifelse( length( commandArgs(TRUE)) > 0, commandArgs(TRUE), "survival")
args1			<- tolower(args1)
print(args1)

Query 	<- sprintf("SELECT * FROM ExchangeRate WHERE 
				date = ( SELECT max(date) FROM ExchangeRate)")

Rates 	<- getData(query = Query)
## Change to swedish currency 
Rates[, Rates := Rates[currency == 'SEK', rate]/rate]


Rates		<- Rates[,.(currency, Rates)]

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
Query 				<- sprintf("SELECT eph.* ,
										ss.siteId,
										ss.type,
										ss.operatorId
								FROM ErpPaymentHistory eph 
								INNER JOIN Invoice ei ON (eph.invoiceNumber = ei.invoiceNumber)
								INNER JOIN Site ss ON (ei.siteId = ss.siteId)"	 )


ErpPaymentHistory 				<- getData(query = Query )


## Estimate the realmoney and get the last date 
moneyDT							<-  ErpPaymentHistory[grepl('(m|r)',type) , .(money = SUM(amount), moneyDate =  paymentDate[.N]), by = .(invoiceNumber)]

ErpInvoice 						<- merge(ErpInvoice, moneyDT, by = 'invoiceNumber', all.x = TRUE)

compensationDT					<-  ErpPaymentHistory[grepl('c|e|p', type), .(compensation = SUM(amount), compensationDate = paymentDate[.N]), by = .(invoiceNumber)]
ErpInvoice 						<- merge(ErpInvoice, compensationDT, by = 'invoiceNumber', all.x = TRUE)

Query		<- sprintf("SELECT 	et.*,
								ss.siteId,
								ss.type,
								ss.operatorId FROM ErpTransaction et 
							INNER JOIN Invoice ei ON (et.invoiceNumber = ei.invoiceNumber)
							INNER JOIN Site ss ON (ei.siteId = ss.siteId)")

ErpTransaction 				<- getData(query = Query )


Query				<- "SELECT * FROM InvoiceStateLog WHERE toState REGEXP '(^INVOICED$|.*SENT)'"
InvoiceStateLog		<- getData(query = Query)
InvoiceStateLog 			<- unique( InvoiceStateLog[order(created), ], by = 'invoiceNumber')[,.( invoiceNumber, sentAt = toState, created = as.Date(created))]
InvoiceStateLog[, sentAt := created][, created := NULL]
ErpInvoice			<- merge( ErpInvoice , InvoiceStateLog, by = 'invoiceNumber', all.x = TRUE)

rm(InvoiceStateLog); gc(reset = TRUE)
PaylevoCreditCheck 			<- getData(query = "select * from kriita_survey.PaylevoCreditCheck where sourceType REGEXP '(CREDITSAFE|BISNODE_FINLAND)'")
PaylevoCreditCheck	<- unique( PaylevoCreditCheck,
								 by = 'personId', formLast = TRUE)

Count			<- 0
f		<- function( x )
{
		cat(strrep("=", 60))
		Count 	<<- Count + 1
		.personId		<- x[, trimws(personId)]
		.checkdate		<- x[, trimws(checkDate)]
		.country		<- x[, trimws(country)]
		cat("\n Running nr nr ",  Count , "\n")
		sourceData		<- jsonlite::fromJSON(x[, sourceData])
		setDT(sourceData)
									
		sourceData[, personId := .personId]							
		cat("\ndone\n")
		cat(strrep("=", 60))
		return( sourceData )
}

	if( file.exists(CreditDataPath) &&  
		as.integer(difftime(Sys.Date() , as.Date( file.info(CreditDataPath)$mtime) )) < 2 )
	{
		load(CreditDataPath)
	} else {
	
		logOutput		<- file.path(LOG, 'outjson.txt')
		file.exists(logOutput) && file.remove(logOutput)
		OutPut				<- list( )		
		Tid <- system.time({
				cores 	<- getOption("mc.cores", detectCores())
				cl		<- makeCluster( cores, outfile = logOutput ) 
				clusterCall( cl, worker.init,  c('data.table', 'jsonlite'))
				## Används för lägga in webExtrac och data in i environmnet
				clusterExport(cl, c("PaylevoCreditCheck","OutPut", "f","logOutput", "Count"), envir = environment())
				Output	<- parLapply(cl, 1:NROW( PaylevoCreditCheck ), 
					function( x ) f( PaylevoCreditCheck[x]) )
				stopCluster(cl)
			})[1:3]
		DataSet		<- rbindlist(Output, fill = TRUE)
		## rm english
		#idx 		<-  DataSet[keys %like% '(CREDITENTRIE|BUSINESSCONNECTIONS|CREDITINFORMATION|PERSON)', unique(personId)]
		#setkey(DataSet, personId)
		#DataSet_1		<- DataSet[!J(idx),]
		#DataSet_1[!is.na(keys), unique(keys)]
		
		DataSet[, keys := gsub("BODY\\.(.*)", "\\1", keys)]

		getKeys			<- '(AGE|FINAL_TAX|INCOME|DEBT|SCORING|Purchasing_Power|Education_level|Life_state|Type_of_residential_area|Ownership_of_housing|Housing_type|Risk_of_payment_defaults|Reason_Code|Number_of_remarks|Credit_quality)'
		DataSet1		<-  DataSet[keys %like% getKeys]
		
		CreditData		<- dcast.data.table(DataSet1, personId  ~ keys , value.var = 'values' )
		CreditData[, personId := as.character(personId)]
	
		save(CreditData, file = file.path(DATA, "credit.RData"))
		rm(getKeys, DataSet1, DataSet, PaylevoCreditCheck, OutPut)
		gc(reset = TRUE)
	}




	

timeToDefault		<- Sys.Date() - months(4)

idx					<- ErpInvoice[, unique(invoiceNumber)]
setkey(ErpPaymentHistory, invoiceNumber)


ErpPayment			<- ErpPaymentHistory[J(idx)]
isNA				<- ErpPayment[, lapply(.SD, function(x) all(is.na(x)))] 
if( any(isNA == TRUE )  )
{
	ErpPayment[, c( which(isNA  == TRUE)) := NULL]
}


ErpPayment1			<-  ErpPayment[, .( Charges = SUM( legalCharge + interest + reminderCharge + collectionCharge + installmentCharge + invoicingCharge),
										overpayment = SUM(overpayment),
										capital		= SUM(capital)
), by = .(invoiceNumber)]

DataDT				<- merge(ErpInvoice, ErpPayment1, by = 'invoiceNumber', all.x = TRUE)
DataDT[, c("debtReference") := NULL]
DataDT				<- merge(DataDT, Rates, by.x = 'invoiceCurrency', by.y = 'currency', all.x = TRUE)

isNumeric			<- which(DataDT[, lapply(.SD, is.numeric)] == TRUE)

for( i in isNumeric)
{
	set(DataDT, i = which(is.na(  DataDT[, c(i), with = FALSE])), j = i, value = 0L )
}
rm( isNumeric , ErpPayment, idx ); gc(reset = TRUE)
## Make all to SEK
DataDT[, `:=` ( money 					= money*Rates, 
				Charges 				= Charges*Rates,
				compensation			= compensation*Rates,
				overpayment				= overpayment*Rates,
				capital					= capital*Rates,
				originalSettlementFee	= originalSettlementFee*Rates,
				playingOriginalAmount	= playingOriginalAmount*Rates
)]
## If sent at is na then use createdAt
DataDT[is.na(sentAt) , sentAt := as.Date(createdAt)]
## Make the selection 
DataDT1				<-  DataDT[as.character(sentAt) >= '2014-10-01' & as.character(sentAt) < timeToDefault,  ]

DataDT_2			<-  DataDT1[, .( 	Total		= .N,
										NrPaid		= SUM(grepl('PAID', state)),
										NrDefault	= SUM(grepl("WAITING_TO_BE_COLLECTED", state)),
										NrDrop		= SUM(grepl("UNNECESSARY|CLOSED", state)),
										MoneyIN		= SUM(money - ifelse( is.na( compensation ), 0, compensation)),
										amountLoss	= SUM( ifelse( grepl("WAITING_TO_BE_COLLECTED", state) , playingOriginalAmount,0)),
										Charges		= SUM(Charges + ifelse(grepl('PAID', state), originalSettlementFee,0)),
										OverPay		= SUM(overpayment),
										Capital		= SUM(capital)
							), by = .(personId)][order(NrPaid)]
## DataDT1 is used below							
rm(DataDT ); gc(reset = TRUE)							
							
if(  
	file.exists( file.path(DATA, "Analysis.RData") ) &&
	 as.integer(difftime(Sys.Date(), as.Date(file.info(DATA, "Analysis.RData")$mtime)[[1]])) < 2
  )
{
	cat("Loading ", "\n")
	 load(file.path(DATA, "Analysis.RData"))
} else {
	DataDT_2[, personId := as.character(personId)]	
	DataDT_2			<- merge( DataDT_2, CreditData, by = 'personId', all.x = TRUE)
	save(DataDT_2, file = file.path(DATA, "Analysis.RData"))
	Summary				<- DataDT1[, .( 	AntalFakturor	= uniqueN(invoiceNumber), 
										AntalPerson 	= uniqueN(personId), 
										startSent 		= min(sentAt), 
										endSent 		= max(sentAt),
										AmountLoss		= DataDT_2[,   SUM(amountLoss)],
										MoneyIN			= DataDT_2[,   SUM(MoneyIN)],
										Charges			= DataDT_2[,   SUM(Charges)],
										CapitalIN		= DataDT_2[,   SUM(Capital)]
		)]
}


		
CreditScore			<- DataDT_2[!is.na(AGE),    ]
CreditScore[, ProfitCustomer := Charges - amountLoss]
CreditScore[, `:=` ( Total 			= Total - NrDrop, # if zero then this customer hasn´t  genreted any invoice
					 Profitably 	= ifelse(ProfitCustomer > 0, 1, 0 )		
 ) ]	

Summary1	<-	CreditScore[, .(	AntalKunder 	= uniqueN(personId),
							Antal			= SUM(Total),
							AntalBetld		= SUM(NrPaid),
							AntalDefault	= SUM(NrDefault),
							AntalDrop		= SUM(NrDrop),
							MoneyIN			= SUM(MoneyIN),
							Loss			= SUM(amountLoss),
							Charges			= SUM(Charges)
							
		)]
## Omit those that are equal to zero		
CreditScore1			<-  CreditScore[Total != 0]
Names					<- names(CreditScore1)
## 
rmNames					<- which(CreditScore1[, lapply(.SD, function(x) all(is.na(x)))] == TRUE)
if( any(rmNames > 0 ) ) 
{
	CreditScore1[, c( rmNames ) := NULL]
	rm(rmNames)
}
Int						<- c("Total", "NrPaid", "NrDefault", "NrDrop", "AGE", "DEBT_NUMBER", "HISTORICAL_DEBT_DETAILS","SCORING")
Num						<- c("MoneyIN", "amountLoss", "Charges","OverPay", "AVERAGE_OWNED_PART_PERCENT", "DEBT_SUM", "FINAL_TAX", "FINAL_TAX2")
Num						<- c(Num,grep("INCOME", Names, value = TRUE))
CreditScore1[, c( Num) := lapply(.SD, as.numeric), .SDcols = Num]
CreditScore1[, c( Int) := lapply(.SD, as.integer), .SDcols = Int]
## rm those that have high amounts of NA, thersehold is that they should be below 5%
rmNA				<- CreditScore1[, lapply(.SD, function(x) SUM(is.na(x)))]/NROW(CreditScore1) > 0.05 
CreditScore1[, c( which(rmNA == TRUE) ) := NULL]
rm(rmNA)

## Choose what to analyze here 
Names			<- names(CreditScore1)
KeepVars		<- setdiff(Names,c("personId", "Total", "NrPaid", "NrDefault", "NrDrop", "MoneyIN","DEBT_PERSON", "amountLoss","Charges","OverPay","Capital","DEBT_AMAL_NUMBER", "DEBT_AMAL_SUM","DEBT_EMAL_NUMBER","DEBT_EMAL_SUM"))

CreditScore2			<-  CreditScore1[, c(KeepVars), with = FALSE]
#CreditScore2[, idx := .I]
CreditScore2[, incTaxIncome := TEXEBLE_INCOME/TEXEBLE_INCOME2]
CreditScore2[incTaxIncome %like% '(Inf|NaN)', incTaxIncome := 0L]
## DEBTS 
CreditScore2[, quantile(HISTORICAL_DEBT_DETAILS, probs = seq(0,1, 0.01), na.rm = TRUE)]

CreditScore2[, ':=' ( Profitably 	= factor(Profitably, levels = c(0,1), labels = c("False", "True")),
						DEBT_NUMBER = ifelse(DEBT_NUMBER < 5, 'Low', 'High' ),
						AgeInt		= as.factor(ifelse(AGE < 30, 'Young', 
											ifelse(AGE >= 30 & AGE < 50, "Middel", "Old")
						)),
					DEBT_DETAILS = as.factor(
								ifelse(HISTORICAL_DEBT_DETAILS == 0, 'None', 
									ifelse( HISTORICAL_DEBT_DETAILS > 0 & HISTORICAL_DEBT_DETAILS < 10, 
										'Medium', 'High') )),
					idx = .I					
) ]
## rm those that have a near zero variance, 
nzv						<- nearZeroVar( CreditScore2)
CreditScore2 			<- CreditScore2[, -c(nzv), with = FALSE]
importantVars			<- c("DEBT_SUM", "FINAL_TAX","INCOME","TOTAL_INCOME")

checkOutliers			<- CreditScore2[, lapply(.SD, 
				function(x) if(is.numeric(x))  quantile(x, probs = seq(0.99,1,0.01), na.rm = TRUE) else NA), .SDcols = importantVars]
if( all((checkOutliers[2, ]		- checkOutliers[1, ] )/checkOutliers[2, ] > 0.9) )
{
	CreditScore2				<-  CreditScore2[ DEBT_SUM  < (checkOutliers[.N, DEBT_SUM] - 1), ]
}

## omit rows that containt NA
idx					<- CreditScore2[, lapply(.SD, function(x) which(is.na(x)== TRUE) ) ]
omitRows			<- as.integer(unique(as.character(unlist(c(idx)))))
CreditScore2		<- CreditScore2[-omitRows]

## Data is cleand
trainIndex				<- createDataPartition(CreditScore2$idx, p = 0.8, list = FALSE )	
rmVars					<- c("HISTORICAL_DEBT_DETAILS", "idx", "AGE","ProfitCustomer")
trainDT					<- CreditScore2[trainIndex , ]
testDT					<- CreditScore2[-trainIndex , ]
trainDT[, prop.table(table(Profitably))]
testDT[, prop.table(table(Profitably))]



########################################################################
# Correlation analysis
########################################################################
# Select the v-variable                           
xVars				<- trainDT[, -c(rmVars), with  = FALSE]  

isNumeric			<- which(xVars[, lapply(.SD, is.numeric)]  == TRUE )

## Make the correlation matrix
corMatri			<-	cor(xVars[, c(isNumeric), with = FALSE])
dissimilarity 		<- 1 - abs(corMatri)
distance 			<- as.dist(dissimilarity) 

cc 					<- hclust(distance, method = 'complete')


pdf( file = file.path( GRAF, paste0('Dendogram.pdf')) ,
     height = unit(8,"cm"), width = unit(9,"cm"),
     pointsize = 10, colormodel = "rgb")                       
plot( cc, main = "Dendogram (Hierarkisk klusteranalys)", xlab = " ", 
	ylab = "Distans (dissimilarity)", sub = "", )
dev.off()
##plot(hclust(distance),     main="Dissimilarity = 1 - abs( Correlation )", xlab="") 

corMatri[corMatri > 0.99]		<- 0
pdf( file = file.path(GRAF, paste0("CorrelationNumeric.pdf")) ,
     height = unit(6,"cm"), width = unit(7,"cm"),
     pointsize = 10, colormodel = "rgb")
	qgraph(corMatri, layout="spring", minimum = 0.1  ,borders = FALSE, vsize = 3,
       	label.cex = 0.6, labels = colnames(corMatri), directed = TRUE, arrows = TRUE,
		label.scale = FALSE)
title(main = 'Correlation of numeric credit data', col.main = "blue", cex.main = 1.3, family = "serif", sub = "Green = positive, red = negative \n(thickness on line = level of correlation)", cex.sub = 0.6, font.sub = 4)  	
dev.off( )	
rm( )

corID		<- findCorrelation(corMatri, cutoff = 0.9)
rmName		<- colnames(corMatri[,corID])
xVars		<- xVars[, -c(rmName), with = FALSE]
rm( cc, distance, dissimilarity, corID, rmName, corMatri)
gc(reset = TRUE)

########################################################################################
xVar			<- xVars[ , -c('Profitably'), with =FALSE]
yVar			<- xVars[ , c('Profitably'), with =FALSE]
fitControl <- trainControl(## 10-fold CV
                           	method = "repeatedcv",
                           	number = 10,
                           	summaryFunction = twoClassSummary,
							classProbs = TRUE,
                           ## repeated 3 times
                           repeats = 3)


bmFit1 <- train(y = yVar$Profitably,
				x =  xVar,
                	 method = "rpart", 
                	 metric = 'ROC',
                 	trControl = fitControl,
                 	tuneLength = 20)

bmFit1 
Importnat			<- varImp(bmFit1)
pdf( file = file.path( GRAF, paste0('VarImportants.pdf')) ,
     height = unit(8,"cm"), width = unit(9,"cm"),
     pointsize = 10, colormodel = "rgb")                       
plot( Importnat, main ="Variable importants", xlab = " ", 
	ylab = "", sub = "", )
dev.off()

pdf( file = file.path( GRAF, paste0('ROCtree.pdf')) ,
     height = unit(8,"cm"), width = unit(9,"cm"),
     pointsize = 10, colormodel = "rgb")     
plot(bmFit1, scales = lst(x = list(log = 10)))
dev.off()
Imp			<- data.table(Importnat$importance, keep.rownames = TRUE)
ImpVar		<- Imp[Overall < 20, rn]

xVar			<- xVars[ , -c('Profitably', ImpVar), with =FALSE]
yVar			<- xVars[ , c('Profitably'), with =FALSE]
tmp				<- cbind(yVar ,xVar)

# xerror = cross validation error
tree_loss_matrix  <- rpart(Profitably ~., method = "class", data= tmp, control = rpart.control(cp = 0.0001, minsplit = 50))

cp_				<- printcp(tree_loss_matrix)
cp_				<- as.data.table(cp_)
cp_[, SERule := xerror + xstd]
SERule			<- cp_[which.min(SERule), CP]
plotcp(tree_loss_matrix)
prundeTree		 	<- prune( tree_loss_matrix, cp = SERule)


pdf( file = file.path( GRAF, paste0('tree1.pdf')) ,
     height = unit(10,"cm"), width = unit(12,"cm"),
     pointsize = 10, colormodel = "rgb")     
rattle::fancyRpartPlot(prundeTree, sub = sprintf("Baserat på SE regel, %s", SERule))
dev.off( )
rm(Imp, ImpVar, tmp , SERule, cp_) 

Tree1		<- CreditScore2[DEBT_SUM >= 226 & DEBT_NUMBER == 'High', 
										 .( Profit 		= SUM(ifelse( Profitably == 'True', ProfitCustomer, 0)),
												CreditLoss	= SUM( ifelse( Profitably == 'False', ProfitCustomer, 0)),
												RatioCL		= round(SUM( Profitably == 'False')/.N ,4)*100
)]
Tree2		<- CreditScore2[DEBT_SUM < 226, 
							 .(	Profit 	= SUM(ifelse( Profitably == 'True', ProfitCustomer, 0)),
								CreditLoss	= SUM( ifelse( Profitably == 'False', ProfitCustomer, 0) ),
								RatioCL		= round(SUM( Profitably == 'False')/.N ,4)*100
								)]
Tree3		<- CreditScore2[DEBT_SUM >= 226 & DEBT_NUMBER != 'High' & TOTAL_INCOME < 88000,  
							 .(	Profit 	= SUM(ifelse( Profitably == 'True', ProfitCustomer, 0)),
								CreditLoss	= SUM( ifelse( Profitably == 'False', ProfitCustomer, 0) ),
								RatioCL		= round(SUM( Profitably == 'False')/.N ,4)*100
								)]

## Forecasting, decide the optimal cutoff 

library(ROCR)
PredictProb			<- predict( prundeTree, newdata = testDT, type = 'prob') 
ROCData				<- list("predictions" = PredictProb[,2], "labels" =  as.character (testDT$Profitably))
Pred				<- prediction(predictions = PredictProb[,2], labels = as.character(testDT$Profitably ))
roc.perf			<- performance(Pred, measure = 'tpr', x.measure = 'fpr' )
aucTree				<- slot(performance(Pred, measure = 'auc'), "y.values")[[1]]
pdf( file = file.path( GRAF, paste0('ROCtree.pdf')) ,
     height = unit(4,"cm"), width = unit(6,"cm"),
     pointsize = 10, colormodel = "rgb")    
	plot(roc.perf, main = 'Performance for decsion tree modell')
 	abline(a=0, b= 1)
dev.off()


roc.acc				<-  performance(Pred, measure = 'acc')
ind 				<- which.max( slot(roc.acc	, "y.values")[[1]] )
acc 				<- slot(roc.acc, "y.values")[[1]][ind]
cutoff 				<- slot(roc.acc, "x.values")[[1]][ind]

perfspec <- performance(Pred, measure="spec", x.measure="cutoff")
perfsens <- performance(Pred, measure="sens", x.measure="cutoff")
dd		<- rbind(
		data.table( 	yname 	= slot(perfspec, "y.name"), 
					xvalue 	=  as.numeric(slot(perfspec, "x.values")[[1]]),
					yvalue	=  as.numeric(slot(perfspec, "y.values")[[1]]) )
,
		data.table( 	yname 	= slot(perfsens, "y.name"), 
					xvalue 	=  as.numeric(slot(perfsens, "x.values")[[1]]),
					yvalue	=  as.numeric(slot(perfsens, "y.values")[[1]]) )
)


		Line <-	ggplot(dd, aes(x = xvalue, y = yvalue, group = yname, colour = yname)) +
					 geom_line(size = 2) +
					scale_y_continuous( expand = c(0.01,0.03), breaks = pretty_breaks(10),labels= percent) +					 
					scale_x_continuous( expand = c(0.01,0.03), breaks = pretty_breaks(10), labels = percent) +
					theme_igray( ) +
					theme(legend.position = 'bottom') +
 					GuideCol(x = 'Measure') +
 					labs(x = 'cutoff', y = 'value', title = 'Optimal cutoff')
				

pdf( file = file.path( GRAF, paste0('rocOpt.pdf')) ,
     height = unit(5,"cm"), width = unit(7,"cm"),
     pointsize = 10, colormodel = "rgb")    
	print(Line)
dev.off()

rm( Line, dd, perfspec, perfsens)

#library(xgboost)


PredictClass		<- predict( prundeTree, newdata = testDT, type = 'class') 
conFTree			<- confusionMatrix( PredictClass, testDT$Profitably, positive = 'True' )
SummaryTest			<- testDT[, .(Antal = .N)]


##################### OUTPUT TO LATEX #########################
save( Summary, Summary1, Tree1, Tree2, Tree3 ,Format , conFTree , aucTree, SummaryTest,file = file.path(DATA, "Data.RData") )
###############################################################




library(statisticalModeling)

CreditScore2[ ]




set.seed(42) ## in order to make the result the same each time
## This contorel makes a 10 fold cross validation and repeat this 3 times, and also estimate the class probabilities
fitControl 			<- trainControl(method = "repeatedcv",  ## K fol
									number = 10, 
									repeats = 3, 
									verboseIter = TRUE,
									summaryFunction = twoClassSummary,
									classProbs = TRUE )
cartGrid 			<- expand.grid(.cp=(1:50)*0.01)
tmp					<-  na.omit(CreditScore2)
rpartGrouped <- train(Profitably ~ ., data =  na.omit(trainDT[,-rmVars, with = FALSE]),
					 method = "rpart",
					 tuneLength = cartGrid,
					 trControl = fitControl, metric = 'ROC')


		 
		
		 



PredictClass		<- predict( prundeTree, newdata = testDT) 
error_1				<- testDT$ProfitCustomer - PredictClass
## prediction with class
PredictClass		<- predict( prundeTree, newdata = testDT, type = 'class') 
conFTree			<- confusionMatrix( PredictClass, testDT$Profitably )
## Accuracy
sum(diag(t)/sum(t))
library(pROC)
## Prediction with probabitlise



aucModel			<- auc( testDT$Profitably, PredictProb[,2])				
plot(roc(testDT$Profitably, PredictProb[,2]))
## Checking on the tree debtsum seems to be a good predictior, in this 18% is profitable customer
## to reference this is to check for those with scoring under < 0 is 25%
rattle::fancyRpartPlot(prundeTree, sub = "")

fmodel(prundeTree, ~ DEBT_SUM)
ggplot( CreditScore2, aes(x = DEBT_SUM, y = TEXEBLE_INCOME, colour = Profitably) ) + 
geom_point() +
coord_cartesian(xlim = c(0,30000), ylim = c(0,200000)) +
geom_hline( yintercept = 120000) +
geom_vline(xintercept = 606)
fmodel(prundeTree, ~ DEBT_SUM)


## To decide on a cutoff that is optimla use ifelse( p > 0.99, "M","F") in a loop and 
## view the confusionMatrix in that context and looking for the accuracy,
## this is best done using ROC
library(caTool)

## Also add the cost, invoice cost, credit loss, add aggregate. discount by some intreset rate 0.02 to make 
## the money into present value
log_model_logit <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                       family = binomial(link = logit), data = training_set)

# Make predictions for all models using the test set
predictions_logit <- predict(log_model_logit, newdata = test_set, type = "response")


## decision tree 
# printcp show the tree pruning with nsplit 
# prune show the min cp value
# use prp to plot rpart.plot, extra = 1, show 

tree_loss_matrix  <- rpart(loan_status ~ ., method = "class", data = training_set,
                           parms = list(loss=matrix(c(0, 10, 1, 0), ncol = 2)),
                           control = rpart.control(cp = 0.001))
# give the optimal cutoff value and reports the binary value
predict( tree_loss_matrix, newdata = <whatever>, type = 'class') 
predict( tree_loss_matrix, newdata = <whatever>, type = 'prob') # give the probabailit, see statistical ch4 p.8 
# if type response then a vector of pod is given
#confusionmatrix table()
# sensitify and specifity impornat measure when evaluating models, accuracy and cutoff is related
# ROC, x = 1 - specifity, y = sensitify, start of ROC equals cut off = 1, cut off = 0 at end 
# AUC is importint when comparing models
roc(test_set$loan_status, predictions_logit)
# if caluclate you by your self the omit the type class
## statiscial modelling
#use mosaic
# fmodel library(statisticalModeling)
# Install devtools if necessary
install.packages("devtools")
#model_2 <- rpart(net ~ age + sex, data = Runners)
 #fmodel(model_2, ~ age + sex)
# Install statisticalModeling
devtools::install_github("dtkaplan/statisticalModeling")
#evaluate_model(model_1)

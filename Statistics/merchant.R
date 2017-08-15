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
CreditDataPath	<-file.path(DATA, "merchant.RData")
# Set the home diretory 


RUN			<- file.path(LOG, "run.txt")
file.exists(RUN) && file.remove(RUN)
library(survival)


last_day 				<- function(date) {
  			 ceiling_date(date, "month") - days(1)
}

y_date_format <- function()
{
   function(x)
   {
       y <- format(x,"%Y")
       m <- format(x,"%b")
       d	<- format(x, "%d")
       ifelse(!duplicated(y), y ,paste0(m, "\n", d))
   }
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
args1			<- ifelse( length( commandArgs(TRUE)) > 0, commandArgs(TRUE), "sbg")
if( regexpr("^sbg$", args1 ) > 0 )
{
	args1			<- "messenio|southbaygroup"
}
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
						INNER JOIN Site ss ON (iv.siteId = ss.siteId)
						WHERE ss.operatorId RLIKE '%s'", args1 )


ErpInvoice 				<- getData(query = Query )

ErpInvoice[, personId := as.character(personId)]
ErpInvoice[order(createdAt), Count := 1:.N, by = .(personId)]
Query 				<- sprintf("SELECT eph.* ,
										ss.siteId,
										ss.type,
										ss.operatorId
								FROM ErpPaymentHistory eph 
								INNER JOIN Invoice ei ON (eph.invoiceNumber = ei.invoiceNumber)
								INNER JOIN Site ss ON (ei.siteId = ss.siteId)
								WHERE ss.operatorId RLIKE '%s'", args1	 )


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
							INNER JOIN Site ss ON (ei.siteId = ss.siteId)
							WHERE ss.operatorId RLIKE '%s'", args1)

ErpTransaction 				<- getData(query = Query )


PaylevoCreditCheck 			<- getData(query = "select * from kriita_survey.PaylevoCreditCheck where sourceType REGEXP '(CREDITSAFE|BISNODE_FINLAND)'")
PaylevoCreditCheck[, personId := as.character(personId)]

setkey(PaylevoCreditCheck, personId)
PaylevoCreditCheck	<- unique( PaylevoCreditCheck[J( as.character( unique(ErpInvoice$personId))) ],
								 by = 'personId', formLast = TRUE)

PaylevoCreditCheck1		<-  PaylevoCreditCheck[!is.na(sourceType)]

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
				clusterExport(cl, c("PaylevoCreditCheck1","OutPut", "f","logOutput", "Count"), envir = environment())
				Output	<- parLapply(cl, 1:NROW( PaylevoCreditCheck1 ), 
					function( x ) f( PaylevoCreditCheck1[x]) )
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
	
		save(CreditData, file = CreditDataPath )
		rm(getKeys, DataSet1, DataSet, PaylevoCreditCheck, OutPut)
		gc(reset = TRUE)
	}

ErpInvoice[, `:=` (Bill_date = as.Date(createdAt))]
ErpInvoice[, `:=` (Weekly = cut(Bill_date, breaks = 'weeks' ))]
ErpInvoice[, `:=` (Yearly 		= format(Bill_date, "%Y"), 
					Monthly 	= format(Bill_date, "%b")
)]


Months 			<- ErpInvoice[, .(	DateRange 	= unique(format(Bill_date, "%b")),
									tmpRange 	= unique(format(Bill_date, "%m")))][order(tmpRange), DateRange]
										


Daily			<- ErpInvoice[state != 'CLOSED',  .(	Antal 	= .N ), by = .(Yearly , Monthly , country)][order(Monthly, country)]
Daily$Monthly	<- factor(Daily$Monthly, levels = Months)

Bar	<-		ggplot(Daily, aes(x = Monthly, y = Antal, colour = Yearly, group = Yearly)) +
				geom_line( size = 0.95) +
				geom_point(size = 1.3) + 
				scale_y_continuous( breaks = pretty_breaks(6), labels = Format )  +	
				theme_igray() + 
				scale_colour_tableau("tableau10") +
				theme(legend.position = 'bottom') +
				GuideCol("Creation year", nrow = 1)	+
				facet_wrap( ~ country, ncol = 2, scales = 'free_y') +
				labs(x = "Månadsvis" ,y = 'Antal', title = sprintf('Antal skapade fakturor (%s)', args1))
	
rm(Daily)	

	barN		<- file.path( GRAF, "Line.pdf")
	pdf( file = barN,
	     height = unit(6,"cm"), width = unit(10,"cm"),
	     pointsize = 10, colormodel = "rgb", bg = "white")  
		print( Bar )		
	dev.off()			
		
Daily			<- ErpInvoice[state != 'CLOSED',  .(	Antal 	= .N ), by = .(Weekly  =as.Date(Weekly) , country)][order(Weekly, country)]
Week	<-	ggplot(Daily, aes(x = Weekly, y = Antal, colour = country, group = 1)) +
				geom_line( size = 0.95) +
				geom_point(size = 1.3) + 
				scale_y_continuous( breaks = pretty_breaks(6), labels = Format )  +	
				scale_x_date(expand = c(0.01,0.01),
							breaks = date_breaks("8 week"),
							labels = y_date_format()) +
				theme_igray() + 
				scale_colour_tableau("tableau10") +
				theme(legend.position = 'bottom') +
				GuideCol("Creation year", nrow = 1)	+
				facet_wrap( ~ country, ncol = 2, scales = 'free_y') +
				labs(x = "Veckovis" ,y = 'Antal', title = sprintf('Antal skapade fakturor (%s)', args1))
	
	barN		<- file.path( GRAF, "weekLine.pdf")
	pdf( file = barN,
	     height = unit(6,"cm"), width = unit(10,"cm"),
	     pointsize = 10, colormodel = "rgb", bg = "white")  
		print( Week )		
	dev.off()	
							
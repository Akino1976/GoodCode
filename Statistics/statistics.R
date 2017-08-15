#!/usr/bin/env Rscript 
##########################################################################################
# This script is used for all sort of task that aren't to be for 
# reporting purposes, just for solvning different task
##########################################################################################
options(scipen = 999)
options("openxlsx.dateFormat" = "yyyy-mm-dd")

Sys.setenv("R_ZIPCMD" = "/usr/bin/zip") ## makes openxlsx works in centos

## Better view with list
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
DeductionRule 			<- data.table(type = names(DeductionRule), what  = DeductionRule)

library(methods)	
scoreScale		<- c('MycketHög' = "c(70,100)",	'Hög' = "c(40, 69)", "Medel" = "c(20,39)", "Låg" = "c(1,19)")

worker.init		<- function( pkg ){
			for( p in pkg ){
				library( p, character.only = TRUE)
			}
			NULL
		}

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



## Further pkg loaded
Packages			 	<- c('reshape2', 'reshape', 'ggthemes', 
							'RColorBrewer','scales', 'ggplot2',  'grid',
							'gridExtra',"Hmisc", "jsonlite")
## Set path and load pkgs
Pack		<- new("startUps", pkgs = Packages, path = dirname(FULLPATH) )
Pack$instant_pkgs()




Pack$setDirs( )
CurrentDate		<- Sys.Date( )
args1			<- ifelse( length( commandArgs(TRUE)) > 0, commandArgs(TRUE), "stats")
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
						INNER JOIN Site ss ON (iv.siteId = ss.siteId)"	)
						
						
ErpInvoice 				<- getData(query = Query )						




## Payments
Query 				<- sprintf("SELECT eph.* ,
										ss.type,
										ss.operatorId
								FROM ErpPaymentHistory eph 
								INNER JOIN Invoice ei ON (eph.invoiceNumber = ei.invoiceNumber)
								INNER JOIN Site ss ON (ei.siteId = ss.siteId)" )


Step2 				<- getData(query = Query )


moneyDT				<-  Step2[grepl('(m|r)',type) ]
rowS				<- c("amount", "capital", "interest", "reminderCharge","collectionCharge","installmentCharge","invoicingCharge","legalCharge","overpayment")
Step2[grepl('(m|r)',type), lapply(.SD, SUM), by = .(invoiceNumber), .SDcols = rowS][invoiceNumber == '14359101011']
Step2[grepl('(m|r)',type), lapply(.SD, SUM), by = .(invoiceNumber), .SDcols = rowS][invoiceNumber == '14359100988']

compensationDT			<-  
ErpPaymentHistory[grepl('c|e|p', type), .(compensation = SUM(amount), compensationDate = paymentDate[.N]), by = .(invoiceNumber)]
Step2[grepl('c|e|p', type), lapply(.SD, SUM), by = .(invoiceNumber), .SDcols = rowS]


Query		<- sprintf("SELECT 	et.*,
								ss.siteId,
								ss.type,
								ss.operatorId FROM ErpTransaction et 
							INNER JOIN Invoice ei ON (et.invoiceNumber = ei.invoiceNumber)
							INNER JOIN Site ss ON (ei.siteId = ss.siteId)")

ErpTransaction 				<- getData(query = Query )



Query				<- sprintf(" SELECT sd.transactionId,	
 										sd.paidAt,
 										sd.amount,
 										sd.type,
										erp.invoiceNumber,
										erp.settlementRule ,
										ss.type AS 'Risk',
										ss.operatorId  FROM SettlementDeduction sd
								 INNER JOIN ErpTransaction erp ON(erp.transactionId = sd.transactionId)
								 INNER JOIN Invoice iv ON (iv.invoiceNumber = erp.invoiceNumber)
								 INNER JOIN Site ss ON(iv.siteId = ss.siteId)")

SettlementDeduction			<- getData(query = Query)
dcast.data.table(SettlementDeduction, invoiceNumber ~ type, value.var = 'amount', SUM)



BankAccount		<- list('780764498' = c(Typ = 'KlientMedel', land = 'UK', Valuta = 'SEK'),
						'50509489'	= c(Typ = 'KlientMedel', land = 'SE', Valuta = 'EUR'),
						'50179489'	= c(Typ = 'KlientMedel', land = 'UK', Valuta = 'EUR'),
						'50756869'	= c(Typ = 'KlientMedel', land = 'SE', Valuta = 'NOK'),
						'722424248' = c(Typ = 'Transaktion', land = 'SE', Valuta = 'SEK'),
						'50757059'	= c(Typ = 'KlientMedel', land = 'UK', Valuta = 'NOK'),
						'840009658'	= c(Typ = 'KlientMedel', land = 'SE', Valuta = 'SEK'),
						'90461249298'	= c(Typ = 'KlientMedel', land = 'SE', Valuta = 'NOK'),
						'8901112224'	= c(Typ = 'KlientMedel', land = 'SE', Valuta = 'DKK'),
						'31311001520968' = c(Typ = 'KlientMedel', land = 'FI', Valuta = 'EUR'),
						'903816288'		= c(Typ = 'KlientMedel', land = 'SE', Valuta = 'SEK'),
						'875604218'		= c(Typ = 'KlientMedel', land = 'SE', Valuta = 'SEK'),
						'790636298'		= c(Typ = 'Transaktion', land = 'SE', Valuta = 'SEK')
						)	


	Account			<- list('166-0232' 					= c(konto = "722424248", valuta = 'SEK', typ = 'Transaktion'),
							'514-5420' 					= c(konto = "780764498", valuta = 'SEK', typ = 'Klientmedel'),
							'576-8353' 					= c(konto = "840009658", valuta = 'SEK', typ = 'Klientmedel'),
							'DE07514206000013482005'		= c(konto = "13482005", valuta = 'DE', typ = 'Transaktion'),
							'FI1131311001520968'			= c(konto = "31311001520968", valuta = 'EUR', typ = 'Transaktion'),
							'NO5190461249298'			= c(konto = "90461249298", valuta = 'NOK', typ = 'Transaktion'),
							'SE2860000000000050509489' 	= c(konto = "50509489", valuta = 'EUR', typ = 'Klientmedel'),
							'SE9160000000000050756869'  = c(konto = "50756869", valuta = 'NOK', typ = 'Klientmedel'),
							'SE9360000000000050179489'	= c(konto = "50179489", valuta = 'EUR', typ = 'Klientmedel')
)
	
	Payments		<- getData(query = 'select * from kriita_survey.Payment')
	for( i in 1:length(BankAccount))
	{
		Step1 	<- BankAccount[i]
		Payments[grepl(names(Step1), AccountId), Valuta := Step1[[1]][[3]]]	
	}
	


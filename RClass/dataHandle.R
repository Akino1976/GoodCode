##########################################################################################
# In this file the data manipulation will be handled, data will be sent
# to 
##########################################################################################
options(scipen = 999)


library(openxlsx)
"pathSet"			<- function( x )
{
	if( is( x, "character"))
	{
		cmd				<- paste0("echo ", x)
		cmd1			<-  pipe( cmd , open = "r")
		path			<-   scan(cmd1, what = "character")  
		close(cmd1)
		return(path)
	} else {
	 	stop("Input is ", class( x ), " and not valid in")
	}	
}



settlmentRules 	<- list( 	'cl' = 'cancelled',
							'da' = 'AfterSettlementPeriod',
							'dp' = 'AfterSettlementPeriodWhenPaid',
							'ia' = 'Immediately' ,
							'ip' = 'ImmediatelyWhenPaid'
	)
	
PaymentRule		<- list(	'c' = 'compensation',
							'r' = 'reference',
							'm' = 'manual_payment',
							'e' = 'creditor_compensation',
							'p' = 'past_payment'
)

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

if(FALSE)
{
rule			<- data.table()
for( i in c("settlmentRules", "DeductionRule", "PaymentRule"))
{
	
	rules 		<- get(i)
	for( z in 1:length(rules) )
	{
		step1		<- rules[z]
		rule		<- rbind(rule, data.table(rules = gsub("(.*)R.*", "\\1", i), shortcut = names(step1), translation = step1))
	}


}
	
rule[, translation := toupper(translation)]	
write.csv(rule, file = 'rule.csv', row.names = FALSE )

}



FULLPATH		<- pathSet( x = "$PWD" )
.HOME		<- dirname(FULLPATH)
# Set the home diretory 


# All R-files 
RFiles 					<- list.files(path = .HOME, include.dirs = TRUE, pattern = "*.R$", 
											recursive = TRUE, full.names = TRUE)
# Load common functions and files , DATA dir is where to send all data
source(file = grep(".*RClass.*(?<=tion.R)", RFiles ,value = TRUE, perl = TRUE, ignore.case = TRUE) )



hs1		<- createStyle(fgFill = "#DCE6F1", halign = "center", textDecoration = "Italic", border = "Bottom")

hs2	<- createStyle(	
		fgFill = "black", 
		fontSize = 12,
		halign = "center", 
		textDecoration = "bold", 
			border = "Bottom")

setValue 	<- function(name, colum, data, startRow = 2)
{
	addWorksheet( wb, name)
	setColWidths(wb, sheet = name, cols = colum, widths = 22)
	writeData(wb, sheet = name, data, startRow = startRow, headerStyle = hs1)
}

#system(Code)

# Pkgs needed 


Packages			 	<- c("lubridate", 'data.table','reshape2', 'reshape',
							'ggthemes', 'stargazer', 'RColorBrewer',
							'extrafont', 'quantmod','openxlsx',
							'ggplot2', 'scales', 'grid', 'gridExtra',"Hmisc",
							 'dynlm', 'dplyr', 'RMySQL')
## Set path and load pkgs
Pack		<- new("startUps", pkgs = Packages, path = dirname(FULLPATH) )
Pack$instant_pkgs()
## Get the lastest rates from ExchangeRate 
Query 	<- sprintf("SELECT * FROM ExchangeRate WHERE 
				date = ( SELECT max(date) FROM ExchangeRate)")

Rates 	<- getData(query = Query)
## Change to swedish currency 
Rates[, Rates := Rates[currency == 'SEK', rate]/rate]


Rates		<- Rates[,.(Currency = currency, Rates)]
#####################################################################################
# Load in conditions  
#####################################################################################
query					<- "SELECT mm.*, 
									ss.type AS TYPE, 
									ss.operatorId,
									mcs.invoiceHoldPeriodRecurring,
									mcs.invoicableStatuses FROM MeacodeMerchant mm 
						INNER JOIN Site ss ON (mm.merchantId = ss.siteId)
						INNER JOIN MerchantCountrySettings mcs ON (mm.merchantId = mcs.merchantId AND 
																	mm.country = mcs.country)"

Meacode					<- getData(query = query)
setnames(Meacode, c('invoiceFee', 'reminderFee'), c('CondinvoiceFee', 'CondreminderFee'))



Meacode[, TYPE := ifelse(grepl("RISK", TYPE), 'NoRisk', 
							ifelse(grepl("NORMAL", TYPE), "Risk", TYPE) )]
							
Meacode[grepl("Risk", TYPE), Risk := 1]			
Meacode[grepl("NoRisk", TYPE), Risk := 0]

Meacode[grepl("store" , storeId), operatorId := 'Store']


Meacode[, c('createdAt','expiresAt','invoiceCurrency') := NULL]
Company	<- list( 'UK' = c(	"comeon","bertil", "vinnarum", 
							"casinoroom","redkings","svenskalottoportalen",
							"3hholdings","garantispelet","videoslots",
							"europaspelet","garantispelet","supervinsten",
							"superspelet") )
	




#################################################################################### 			
# MySQL cmds 
# dbListTables(con) = list all tables in above connection
# dbListFields(con, DBtable) = list of all columns in table
####################################################################################	
## This is by each transaction
StartDate		<- "2013-05-14"
CurrentDate		<- Sys.Date( )
			
Query	<- sprintf("SELECT CONCAT(inv.siteId, '.', LOWER(ei.country)) AS siteId ,
							ei.invoiceCurrency AS Currency,
							inv.ocrNumber, 
							inv.playerId,
							ei.clientStatus AS status,
							inv.toAccount,
							et.* FROM ErpTransaction et 
						INNER JOIN ErpInvoice ei ON (ei.invoiceNumber = et.invoiceNumber)
						INNER JOIN Invoice inv ON(inv.invoiceNumber = et.invoiceNumber)",
					 	StartDate);

Step1 		<- getData(query = Query )

								
Step1		<- Step1[!siteId %like% "(kriita|3h)"]



for( i in 1:length(settlmentRules))
{
	first_		<- settlmentRules[i]
	Step1[grepl(names(first_), settlementRule), settlementRuleString :=  first_]
}

Step1		<- merge(Step1, Rates, by.x = "currency", by.y = 'Currency', all = TRUE)
Step1 		<- Step1[!is.na(siteId)]

tmpMea		<- unique(Meacode[, .(storeId , TYPE , operatorId, country, settlementInterval, 
								settlementPeriod, transactionFeePercentage)], by = 'storeId')

Step1		<- merge(Step1, tmpMea, by.x = "siteId", by.y = 'storeId', all.x = TRUE )
rm(tmpMea)

ErpTransaction		<- copy(Step1)
ErpTransaction 		<- ErpTransaction[!is.na(invoiceNumber)]
rm(Step1)
######## Merge with SettlementDeduction
query 							<- 'select * from SettlementDeduction'
SettlementDeduction				<- getData(query = query)
setnames(SettlementDeduction, 'amount', 'DeductedAmount')
SettlementDeduction[, tmpVal := paste0(DeductedAmount, " (",  paidAt,")")]
SettlementDeduction[order(paidAt), Count := 1:.N, by = .(transactionId)]

Step2				<- dcast.data.table(SettlementDeduction[order(paidAt)] , transactionId ~ Count, value.var = 'tmpVal')

Names				<- colnames(Step2)

Names1				<- paste0("DeductionNr_", setdiff(Names, "transactionId"))
setnames(Step2, 2:NCOL(Step2), Names1)

ErpTransaction 		<- merge(ErpTransaction, Step2, by = 'transactionId', all.x = TRUE)
SettlementDeduction[, tmpVal := NULL]

rm(Step2)
ErpTransaction 					<- ErpTransaction[!is.na(transactionId)]
rm( query); gc(reset = TRUE)
## Get each transaction, mulitple invoiceNr
Query			<- sprintf("SELECT * FROM ErpPaymentHistory where paymentDate >= '%s'", StartDate)
Step1 			<- getData(query = Query )
Step1[, paymentDate := as.Date(paymentDate)]
ErpPaymentHistory 	<- copy(Step1)
rm(Step1)


## ErpInvoice with Invoice 
Query 	<- sprintf("SELECT 	ei.*,
							iv.state,
							iv.siteId AS merchantId,
							iv.toAccount,
							iv.personId,
							iv.playerId,
							iv.dueDate,
							iv.ocrNumber,
							iv.ocrNumberReminder,
							IF( ei.playingCurrency = 'EUR' 
									AND ei.invoiceCurrency = 'SEK', 
										ei.originalSettlementFee*ei.conversionRate, 
											ei.originalSettlementFee) AS TransactionFee
							FROM ErpInvoice ei 
						INNER JOIN Invoice iv ON (ei.invoiceNumber = iv.invoiceNumber)
						WHERE createdAt >= '%s'",  StartDate)			
			
Step1 				<- getData(query = Query )
Step1[, siteId := paste0(merchantId, '.', tolower(country))]
Step1		<- Step1[!siteId %like% "(kriita|3h)"]



Step1[, ':=' ( Bill_date 			= as.Date(createdAt),
				dueDate 			= as.Date(dueDate),
				Payment_date		= as.Date(closureDate),
				firstReminderDate 	= as.Date(firstReminderDate),
				collectionDate		= as.Date(collectionDate)
			 )]




Step1[is.na(TransactionFee), TransactionFee := settlementFee]



Step1[, ':=' ( 
			Bill_month		= format(Bill_date, "%Y-%b"),
			Payment_month	= format(Payment_date, "%Y-%b")
)]

Step1			<- merge(Step1, Rates, by.x = 'invoiceCurrency', by.y = "Currency", all = TRUE)

Meacode1		<- Meacode[, -c('invoiceCustomerSupport','senderEmail','storeContact','storeLogo','brand','storeStreet', 'storeWww','transactionDateFormat'), with = FALSE]


ErpInvoice				<- merge(Step1, Meacode1, by.x = c('merchantId',"siteId","country"), by.y = c('merchantId','storeId',"country"), all.x = TRUE)					
ErpInvoice 				<- ErpInvoice[!is.na(personId)]

## Estimate the realmoney and get the last date 
moneyDT					<-   ErpPaymentHistory[grepl('(m|r)',type) , .(money = SUM(amount), moneyDate =  paymentDate[.N]), by = .(invoiceNumber)]

ErpInvoice 				<- merge(ErpInvoice, moneyDT, by = 'invoiceNumber', all.x = TRUE)
ErpInvoice[is.na(money), money := 0]
rm(moneyDT)


compensationDT			<-  ErpPaymentHistory[grepl('c|e|p', type), .(compensation = SUM(amount), compensationDate = paymentDate[.N]), by = .(invoiceNumber)]
ErpInvoice 				<- merge(ErpInvoice, compensationDT, by = 'invoiceNumber', all.x = TRUE)
ErpInvoice[is.na(compensation), compensation := 0]
ErpInvoice 		<- ErpInvoice[!personId %like% '\\b(311|1115|70594)\\b', ]
rm(compensationDT)


ErpPaymentHistory 		<- merge(ErpPaymentHistory, 
									ErpInvoice[,.(invoiceNumber, TYPE , operatorId, siteId, merchantId, country)],
								 by = 'invoiceNumber', all.x = TRUE)

ErpPaymentHistory		<- ErpPaymentHistory[!is.na(siteId)]





setkey(ErpInvoice, personId)
ErpInvoice[, InvoiceNr := .N, by = .(personId)]
ErpInvoice[, tmp := 1L]
## Add up each by unique customer, based on personId
ErpInvoice[order(Bill_date), Count:= cumsum(tmp), by = .(personId)]
	
		  	
## Divided each of the revenues
ErpInvoice[, ':=' ( Revenue  = invoiceFee + TransactionFee + reminderFee + interest ) ]

	

Regex		<- paste0(Company$UK, collapse = "|")
ErpInvoice[siteId %like% Regex, Company := "UK"]	
ErpInvoice[is.na(Company), Company := 'AB']	

ErpTransaction		<- merge(ErpTransaction, ErpInvoice[, .(invoiceNumber, playerId, 
															Erp_createdAt = createdAt,
															Company,	Risk)], by = c('invoiceNumber', 'playerId'),
												all.x = TRUE)


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


Account			<- list(	'166-0232' 					= c(konto = "722424248", valuta = 'SEK', typ = 'Transaktion'),
							'514-5420' 					= c(konto = "780764498", valuta = 'SEK', typ = 'Klientmedel'),
							'576-8353' 					= c(konto = "840009658", valuta = 'SEK', typ = 'Klientmedel'),
							'DE07514206000013482005'	= c(konto = "13482005", valuta = 'DE', typ = 'Transaktion'),
							'FI1131311001520968'		= c(konto = "31311001520968", valuta = 'EUR', typ = 'Transaktion'),
							'NO5190461249298'			= c(konto = "90461249298", valuta = 'NOK', typ = 'Transaktion'),
							'SE2860000000000050509489' 	= c(konto = "50509489", valuta = 'EUR', typ = 'Klientmedel'),
							'SE9160000000000050756869'  = c(konto = "50756869", valuta = 'NOK', typ = 'Klientmedel'),
							'SE9360000000000050179489'	= c(konto = "50179489", valuta = 'EUR', typ = 'Klientmedel')
)


TransType 		<- list('NMSC' = 'Miscellaneous',
						'NTRF' = 'Transfers (I/O local payments)',
						'NCRO' = 'Cross-border (I/O payments)',
						'NLOC' = 'Local payment (I/O)',
						'NCHG' = 'Transaction type')						

Query		<- sprintf("SELECT * FROM kriita_survey.Payment");

Payments	<- getData(query = Query)


for( i in 1:length(TransType))
{
	Step1 	<- TransType[i]
	Payments[grepl(names(Step1), TransaktionRest), Type := Step1]	
}

for( i in 1:length(BankAccount))
{
	Step1 	<- BankAccount[i]
	Payments[grepl(names(Step1), AccountId), Valuta := Step1[[1]][[3]]]	
}



Payments 	<- merge(Payments, Rates, by.x = 'Valuta', by.y = 'Currency', all = TRUE)			
			
Payments[is.na(Rates) , Rates := 1]
Query		<- sprintf("SELECT * FROM Balance");
Balance		<- getData(query = Query ,dbname = "kriita_survey")
Balance[, CloseBalance := as.numeric(gsub("([0-9\\.]+).*", "\\1", CloseBalance))]


Months 			<- ErpInvoice[, .(	DateRange 	= unique(format(Bill_date, "%Y-%B")),
								tmpRange 	= unique(format(Bill_date, "%Y-%m")))][order(tmpRange), DateRange]
										
MonthsLow 			<- ErpInvoice[, .(	DateRange 	= unique(format(Bill_date, "%Y-%b")),
							tmpRange 	= unique(format(Bill_date, "%Y-%m")))][order(tmpRange), DateRange]



Comp	<- list('europaspelet' = c('europaspelet|superspelet|supervinsten|nordiska'),
				'casinoroom' = c('casinoroom'),
				'comeon' = c('comeon'),
				'videoslots' = c('videoslots'),
				'southbaygroup' = c('southbaygroup|testoplus|superdietten|sbg'),
				'healthpills' = c('healthpills'),
				'redkings' = c('redkings'),
				'novus' = c('novus'),
				'campus' = c('campus'),
				'protectme' = c('protectme'),
				'hem24'		= c('hem24'))

			
ErpInvoice[, SiteId := ifelse(grepl("garantispelet.se", siteId), "europaspelet.se", 
							ifelse(grepl("garantispelet2.se", siteId), "europaspelet2.se", siteId ))]
							
query		<- "SELECT * FROM Person"
Person		<- getData(query = query)							
							

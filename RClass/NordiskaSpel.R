#!/usr/bin/env Rscript 
##########################################################################################
# This script is used for all sort of task that aren't to be for 
# reporting purposes, just for solvning different task
##########################################################################################
options(scipen = 999)
options("openxlsx.dateFormat" = "yyyy-mm-dd")

Sys.setenv("R_ZIPCMD" = "/usr/bin/zip") ## makes openxlsx works in centos

scoreScale		<- c('MycketHög' = "c(70,100)",	'Hög' = "c(40, 69)", "Medel" = "c(20,39)", "Låg" = "c(1,19)")
scoreNum 		<- list('1' = c("intervall" = "c(70,100)", 'time' = 120 ),
						'2' = c("intervall" = "c(40, 69)", 'time' = 120 ),
						'3' = c("intervall" = "c(20, 39)", 'time' = 60 ),
						'4' = c("intervall" = "c(1, 19)", 'time' = 60 )
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


podNum			<- list("0"		= c("intervall" = "c(-50,0)", "pod" = 1 ),
						"1"		= c("intervall" = "c(0,5)", "pod" = 0.727),
						"2" 	= c("intervall" = "c(6,10)", "pod" = 0.714),
						"3" 	= c("intervall" = "c(11,15)", "pod" = 0.652),
						"4" 	= c("intervall" = "c(16,20)", "pod" = 0.589),
						"5" 	= c("intervall" = "c(21,25)", "pod" = 0.476),
						"6" 	= c("intervall" = "c(26,30)", "pod" = 0.152),
						"7" 	= c("intervall" = "c(31,35)", "pod" = 0.048),
						"8" 	= c("intervall" = "c(36,40)", "pod" = 0.017),
						"9" 	= c("intervall" = "c(41,45)", "pod" = 0.011),
						"10" 	= c("intervall" = "c(46,50)", "pod" = 0.009),
						"11" 	= c("intervall" = "c(51,55)", "pod" = 0.006),
						"12" 	= c("intervall" = "c(56,60)", "pod" = 0.004),
						"13" 	= c("intervall" = "c(61,65)", "pod" = 0.003),
						"14" 	= c("intervall" = "c(66,75)", "pod" = 0.002),
						"15" 	= c("intervall" = "c(76,100)", "pod" = 0.001)
									
)


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

scaleOrder			<- c('LågLåg', 'Låg', 'Medel', 'Hög', 'MycketHög')

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
args1			<- ifelse( length( commandArgs(TRUE)) > 0, commandArgs(TRUE), "invoice")
args1			<- tolower(args1)
.operatorId		<- 'nordiskaspel'

## Set Meacode according to specification rules 

# Meacode			<- Meacode[grepl( .operatorId , operatorId)]

Regexp			<- paste0(Meacode[, unique(merchantId)], collapse = "|")


####################################################################################
## Load data for Nordiska
####################################################################################


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
						WHERE ss.operatorId RLIKE  '%s'",  .operatorId	)

Step1 				<- getData(query = Query )



Query 				<- sprintf("SELECT eph.* ,
										ss.type,
										ss.operatorId
								FROM ErpPaymentHistory eph 
								INNER JOIN Invoice ei ON (eph.invoiceNumber = ei.invoiceNumber)
								INNER JOIN Site ss ON (ei.siteId = ss.siteId)
									WHERE ss.operatorId RLIKE  '%s'", .operatorId	 )


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


rm(Step2, Step1)
gc(reset = TRUE)

Query 				<- sprintf("SELECT eph.* ,
										ss.type,
										ss.operatorId
								FROM ErpPaymentHistory eph 
								INNER JOIN Invoice ei ON (eph.invoiceNumber = ei.invoiceNumber)
								INNER JOIN Site ss ON (ei.siteId = ss.siteId)
									WHERE ss.operatorId RLIKE  '%s'", .operatorId	 )
									
									
Query		<- sprintf("SELECT et.* FROM ErpTransaction et 
							INNER JOIN Invoice ei ON (et.invoiceNumber = ei.invoiceNumber)
							INNER JOIN Site ss ON (ei.siteId = ss.siteId)
								WHERE ss.operatorId RLIKE  '%s'", .operatorId	 )

Step1 				<- getData(query = Query )
idx					<- unique(Step1[,  transactionId])



SettlementDeduction				<- getData(query = 'SELECT * FROM SettlementDeduction')
setnames(SettlementDeduction, 'amount', 'DeductedAmount')
setkey(SettlementDeduction, transactionId)
SettlementDeduction 			<- SettlementDeduction[J(idx)][!is.na(DeductedAmount)]
SettlementDeduction[order(paidAt), Count := 1:.N, by = .(transactionId)]
SettlementDeduction[, ':=' ( DeductedAmount = DeductedAmount*-1 ,
							type = ifelse(is.na(type), '', type)
)]
SettlementDeduction[, tmp := paste0(DeductedAmount, ' ', type)]
SettlementDeduction1		<-  dcast.data.table(SettlementDeduction, transactionId ~ Count, value.var = 'tmp')
Names						<- colnames(SettlementDeduction1)
Names						<- setdiff(Names,'transactionId')
Names1						<- paste0('Nr_',Names)
setnames(SettlementDeduction1, 2:NCOL(SettlementDeduction1), Names1)

Step1					<- merge(Step1, SettlementDeduction1, by = 'transactionId', all.x = TRUE)
rm(SettlementDeduction1, Names, Names1)






ErpTransaction			<- copy(Step1)

rm(Step1)
gc(reset = TRUE)


Query				<- sprintf("SELECT * FROM InvoiceStateLog");

InvoiceStateLog 	<- getData(query = Query )

Query				<- sprintf("SELECT sc.*, 
										ss.type, 
										ss.operatorId FROM Subscription sc
								INNER JOIN Site ss ON(ss.siteId = sc.merchantId)
								WHERE ss.operatorId  RLIKE '%s'", .operatorId	 );

Subscription 				<- getData(query = Query )


Query				<- sprintf("SELECT sc.merchantId,
								 sl.* FROM SubscriptionLog sl
								INNER JOIN Subscription sc ON (sl.subscriptionId = sc.subscriptionId)
								INNER JOIN Site ss ON(ss.siteId = sc.merchantId)
								WHERE ss.operatorId  RLIKE '%s'", .operatorId);

SubscriptionLog 	 <- getData(query = Query )


Query				<- sprintf("SELECT rr.*,
								 		 ss.type, 
										ss.operatorId FROM Reservation rr
								INNER JOIN Site ss ON(rr.merchantId = ss.siteId)
								WHERE ss.operatorId RLIKE '%s'", .operatorId );
Reservation			<- getData(query = Query)

Query				<- sprintf("SELECT *  FROM Transaction")
						
Transaction			<- getData(query = Query)
Transaction			<- merge(Transaction, Reservation[, .(invoiceNumber ,reservationId, merchantId, type, operatorId)], by = 'reservationId', all.x = TRUE)

Player 				<- getData(query = "SELECT * FROM Player")
##########################################################################################
# All payments
##########################################################################################
if( FALSE )
{
	.month	<- "2016-08"
	ErpInv				<- ErpInvoice[grepl(.month, closureDate)]
	ErpInv				<- merge(ErpInv, unique(Transaction[,.(invoiceNumber, merchantItemId)], by = 'invoiceNumber', fromLast = TRUE) , by = 'invoiceNumber', all.x = TRUE)

	ErpInv1				<-  ErpInv[,.(personId,invoiceNumber, status, createdAt, Amount = playingOriginalAmount - invoiceFee, merchantItemId, closureDate)]

	ErpInv1				<- merge(ErpInv1, Person[,.( personId, ssn)] , by = 'personId', all.x = TRUE)
		Name1		<- sprintf('NordiskaPaid_%s.xlsx', .month)
		wb			<- openxlsx::createWorkbook()	

	setValue(name = .month, colum = c(1:NCOL(ErpInv1)), data = ErpInv1[order(closureDate)])  

	openxlsx::saveWorkbook(wb, file = file.path(DirName, Name1), overwrite = TRUE)
}

################################################################################
# 
################################################################################
Transaction1			<-   Transaction[!is.na(invoiceNumber), .(  merchantItemId = merchantItemId[.N], noitemId = uniqueN(merchantItemId),
										 amount = SUM(amountPerItem)), by = .(invoiceNumber) ]
ErpInvoice 				<- merge(ErpInvoice, Transaction1 , by = 'invoiceNumber', all.x = TRUE)
ErpInvoice 				<- ErpInvoice[!is.na(invoiceNumber)]

ErpTransaction			<- merge(ErpTransaction, ErpInvoice[,.( invoiceNumber, siteId, closureDate , state ,operatorId, playerId, merchantItemId)], 
								by = 'invoiceNumber', all.x = TRUE)

Revenue					<- ErpTransaction[!is.na(settlementPaidAt), .(settledAmount = SUM(settlementAmount - settlementAmountDeducted),
																	settlementPaidAt = settlementPaidAt[.N]
				), by = .(invoiceNumber)]

ErpInvoice			<- merge(ErpInvoice, Revenue,
							by = 'invoiceNumber', all = TRUE)
									
rm(Transaction1)
######### Invoice profile for nordiska spel
if( regexpr("invoice", args1, ignore.case = TRUE) > 0 )
{
	################################### NotPaid ##################
	notPaidDT				<- ErpInvoice[!grepl("UNNECESSARY|PAID|CLOSED", state)]
	notPaidDT				<- merge(notPaidDT, 
									InvoiceStateLog[(grepl('WAITING_TO_BE_INVOICED|INVOICE_CREATED', fromState)) & grepl('INVOICE_SENT| INVOICED', toState) , 
											.( invoiceNumber , sentAt = as.Date(created) )], 
									by = 'invoiceNumber', all.x = TRUE)

	notPaidDT[!is.na(sentAt), noDaysOpen :=  as.integer( difftime( Sys.Date(), sentAt, units = 'days'))]

	maxDay			<- round_any( notPaidDT[, max(noDaysOpen, na.rm = TRUE)], 10, ceiling)
	Seq				<- c( seq(0,70 ,14 ), maxDay)
	notPaidDT[, Bins := cut2(noDaysOpen, Seq)]
	notPaidDT[is.na(sentAt), Bins := 'NotSent']
	summaryNotPaid			<- notPaidDT[, .(Antal = .N, amount = SUM(amount)), by = .(Bins)][order(Bins)]


	######### Paid #############
	PaidDT			<- ErpInvoice[state == 'PAID' & closureDate >= Sys.Date() - months(3), 
						.( invoiceNumber, state, siteId, playerId, ocrNumber, createdAt, dueDate, 
							collectionDate, closureDate, merchantItemId ,amount)]
							
	
	PaidDT 			<- merge(PaidDT,	
							ErpTransaction[, .( settlementAt = settlementAt[.N], settlementPaidAt = settlementPaidAt[.N]), by = .(invoiceNumber)],
						 by = 'invoiceNumber', all.x = TRUE)
	
	PaidDT[, paidMonth	:= as.character( format(as.Date(closureDate), "%Y-%m"))]
	PaidDT			<- PaidDT[order(settlementPaidAt)]
	Payments		<- getData(query = 'SELECT * FROM kriita_survey.Payment')
	
	Payed			<- Payments[TransaktionType == 'D' & grepl("(Pure Sales|Nordiska|ng media)", OrderPart1, ignore.case = TRUE), .(OrderPart1 ,ValueDate, EntryDate, TransaktionAmount)]
	Payed[, OrderPart1 := gsub("(.*)\\sSL.*", "\\1", OrderPart1)]
	Payed1 			<- Payed[, .(TransaktionAmount  = SUM(TransaktionAmount)), by = .(ValueDate, OrderPart1)][order(ValueDate)]
	Payed1[, Month := format(as.Date(ValueDate), "%Y-%m")]
	PayedOut			<- Payed1[ValueDate >= '2016-01-01', .(TransaktionAmount = SUM(TransaktionAmount)), by = .(Month)]
	
	
	idx				<- ErpTransaction[, transactionId]
	setkey(SettlementDeduction, transactionId)
	DeductionDT			<- SettlementDeduction[J(idx), ][!is.na(DeductedAmount), ]
	DeductionDT[, paidMonth := format(as.Date(paidAt), "%Y-%m")]
	Deduction 			<- DeductionDT[,.( deduction = round(SUM(DeductedAmount*-1))), by = .(paidAt)][order(paidAt)]
	DeductionDT_1		<- DeductionDT[,.( deduction = round(SUM(DeductedAmount*-1))), by = .(paidMonth, type)][order(paidMonth)]
	for( i in 1:length(DeductionRule) )
	{
		Step1		<- DeductionRule[i]
		
		DeductionDT_1[grepl(names(Step1), type), typeOf := Step1]
	}
	
	
	DeductionDT_1 		<- dcast.data.table(DeductionDT_1, paidMonth~ typeOf, value.var = 'deduction')[order(paidMonth)]
	DeductionDT_1 		<- RowStat(DeductionDT_1,id = 'paidMonth',type = 'sum')
	
	DeductionDT_1 		<- DeductionDT_1[grepl('201[6|7].*', paidMonth)]
	DeductionDT_1 		<- rbind(DeductionDT_1 , DeductionDT_1[, lapply(.SD, tmpSum)], fill = TRUE)
	DeductionDT_1[is.na(paidMonth), paidMonth := 'Total']
	rm(Step1, i )
	
	
	
	ForecastDT 			<- ErpTransaction[state == 'PAID' & as.Date(settlementAt) >= Sys.Date(), ]
	
	ForecastDT[, ':=' ( settlmentMonth	= format(as.Date(settlementAt), "%Y-%m"),
						closureMonth 	= format(as.Date(closureDate), "%Y-%m"))]
	ClosedWhen			<- ForecastDT[, .(noClosed = .N), by = .(settlementAt ,closureMonth)][order(settlementAt, closureMonth)]
	Forecast			<- ForecastDT[ , .(	settlementAmount  = SUM(settlementAmount), 
											settlementAmountDeducted  = SUM(settlementAmountDeducted), 
											settlementAmountDeductible  = SUM(settlementAmountDeductible) ), by = .(settlementAt)]
											
	Forecast[, expected_settlementAmount := settlementAmount - (settlementAmountDeducted + settlementAmountDeductible) ]	
	
	Settled		<- ErpTransaction[!is.na(settlementPaidAt), .( settlement = SUM(settlementAmount)), by = .(settlementPaidAt)][order(settlementPaidAt)]
	Settled 	<- merge(Settled, Deduction, by.x = 'settlementPaidAt', by.y = 'paidAt', all = TRUE)
	Settled 	<- Settled[settlementPaidAt >= '2016-01-01' & !is.na(settlement)]
	Settled[, settlementPaidMonth := format( as.Date(settlementPaidAt), "%Y-%m")]
	Settled1	<- Settled[, .(settlement = SUM(settlement), deduction = SUM(deduction)),by =.(settlementPaidMonth)]
	Settled1 	<- merge(Settled1, PayedOut, by.x = 'settlementPaidMonth', by.y = 'Month', all = TRUE)
													
	######### Create excel file 
		Name1		<- paste0('Nordiskaspel_', args1 ,'_', CurrentDate ,'.xlsx')
		wb			<- openxlsx::createWorkbook( )
		name		<- "Settlment"
		addWorksheet( wb, name)
	
		df		<- data.frame(	expected_settlementAmount 	= 'Den förväntade summan som ska settlas till nordiskaspel, kan förändras pga deduction och supportärenden',
								closureMonth				= 'Månad när fakturan anses betald och klar',
								noClosed					= 'Antal som är betalda o klara baserad på settlementAt datum och closureMonth',
								NOT							= 'settlementAt : 2017-01-16 och closureMonth:2016-11, noClosed: 3, betyder att för de fakturor som ska settlas 2017-01-16 är 3 betalda i 2016-11 månad',
								TransaktionAmount			= 'Summa registrerad hos banken för settlement till Nordiska' 
		)
		
		df1		<- data.frame( What = colnames(df), Explanation = t(df))
		setColWidths(wb, sheet = name, cols = 1, widths = 25)
		setColWidths(wb, sheet = name, cols = 2, widths = 45)
		writeData(wb, sheet = name,  x = df1, startRow = 1, startCol = 1,	borders = "surrounding", 
								headerStyle = hs1)
		setColWidths(wb, sheet = name, cols = 3:NCOL(Forecast), widths = 25)
		Count		<- NROW(df1) + 3
		
		
		for( z in c("Forecast", "ClosedWhen", "Settled1"))
		{
			Step1		<- get(z)
			if( regexpr("Settled1", z, ignore.case = TRUE) > 0 && exists("DeductionDT_1") )
			{
				writeData(wb, sheet = name,  x = Step1, startRow = Count, startCol = 1,  headerStyle = hs1)	
				writeData(wb, sheet = name,  x = DeductionDT_1, startRow = Count, startCol = NCOL(Step1) + 2,  headerStyle = hs1)	
			} else {
				writeData(wb, sheet = name,  x = Step1, startRow = Count, startCol = 1,  headerStyle = hs1)	
			}	
			Count		<- Count + NROW(Step1) + 2
		}
		
		uniqBins		<- as.character( summaryNotPaid[, unique( Bins)]) 
		uniqBins1 		<- setdiff(uniqBins,"NotSent" )
		uniqBins		<- c("NotSent", uniqBins1)
		setkey(notPaidDT, Bins)
		for( i in uniqBins)
		{
			cat("\n**************************************\n")
			name 		<- 	gsub("\\s{2,}", "", i)
			cat("## Running ", name, "\n##")
			addWorksheet( wb, name)
			setColWidths(wb, sheet = name, cols = 1, widths = 35)
			setColWidths(wb, sheet = name, cols = 2:3, widths = 25)

			Step1			<- notPaidDT[J(i), .(personId, invoiceNumber, state , createdAt,  sentAt , dueDate, noDaysOpen,  merchantId = siteId, merchantItemId, amount) ]
			Step1			<- Step1[order( createdAt)][!grepl("WAITING_TO_BE_COLLECTED",state)]
			Step1 			<- merge( Step1, 
									Person[, .(personId, firstName, lastName , ssn, phone , street )],
									by = 'personId', all.x = TRUE)
			SummaryDT		<- Step1[, .(Antal = .N, amount = SUM(amount)), by = .(merchantItemId)][order(Antal)]
			Summary1		<- SummaryDT[, lapply(.SD, tmpSum)]
			Summary1[is.na(merchantItemId), merchantItemId := 'Total']
			SummaryDT 		<- rbind(SummaryDT, Summary1)
			writeData(wb, sheet = name,  x = SummaryDT, startRow = 1, startCol = 1,	borders = "surrounding", 
						headerStyle = hs1)
			
			Count			<- NROW(SummaryDT) + 3 		
			setColWidths(wb, sheet = name, cols = 4:NCOL(Step1), widths = 25)
			writeDataTable(wb, sheet = name,  x = Step1  ,  startRow =  Count , 
							 tableStyle = 'TableStyleMedium21',	headerStyle = hs2) 
			cat("\n**************************************\n")
		}
		rm(i, Step1, Summary1, SummaryDT)
		uniqPaid			<- sort( PaidDT[, unique(paidMonth)])
		setkey(PaidDT, paidMonth)
		setkey(ErpTransaction, invoiceNumber)
		for( z in uniqPaid)
		{
			Step1			<- PaidDT[J(z)][order(closureDate)]		
			idx				<- Step1[, invoiceNumber]
			Step2			<- ErpTransaction[J(idx), .( merchantId = siteId, playerId, settlementAmount = settlementAmount - settlementAmountDeducted, settlementAt, settlementPaidAt)]
			Step3			<- Step2[, .( settlementAmount = SUM(settlementAmount)), by = .(settlementPaidAt)][order(settlementPaidAt)]
			Step3[is.na(settlementPaidAt), settlementPaidAt := 'NotSettled']
			Step4			<- Step2[is.na(settlementPaidAt), .( NotSettled = SUM(settlementAmount)), by = .(settlementAt)][order(settlementAt)]
			Step4			<- rbind(Step4, Step4[, lapply(.SD, tmpSum)], fill = TRUE)
		
			SumSettle		<- Step3[, lapply(.SD, tmpSum)]
			SumSettle[is.na(settlementPaidAt), settlementPaidAt := 'Total']
			Step3			<- rbind(Step3, SumSettle)
			SumPaid			<- Step1[, .( AmountIn		= SUM(amount)), by = .( siteId )][order(AmountIn)]
			AggPaid			<- SumPaid[, lapply(.SD, tmpSum)]
			AggPaid[is.na(siteId), siteId := 'Total']
			Step2			<- rbind(SumPaid,AggPaid)
			cat("\n**************************************\n")
			name 		<- 	paste0("Paid_", z)
			cat("## Running ", name, "\n##")
			addWorksheet( wb, name)
			setColWidths(wb, sheet = name, cols = 1:NCOL(Step1), widths = 20)

			writeData(wb, sheet = name,  x = Step2, startRow = 1, startCol = 1,	borders = "surrounding", 
					headerStyle = hs1)
			Nrow		<- NCOL(Step2) + 2
			
			writeData(wb, sheet = name,  x = Step3, startRow = 1, startCol = Nrow,	borders = "surrounding", 
					headerStyle = hs1)
			
			Nrow		<- Nrow + NCOL(Step3) + 1 	
			if( NROW(Step4[!is.na(settlementAt)]) > 0 )
			{	
				writeData(wb, sheet = name,  x = Step4, startRow = 1, startCol = Nrow,	borders = "surrounding", 
						headerStyle = hs1)		
			}		
			Max_		<- max( c( NROW(Step3)	,NROW(Step2), NROW(Step4)))
				
			Count	<- Max_ + 3
			writeDataTable(wb, sheet = name,  x = Step1  ,  startRow =  Count , 
							 tableStyle = 'TableStyleMedium21',	headerStyle = hs2) 
			
			cat("\n**************************************\n")
		}

	
		
		
		
		fileName		<- file.path(DATA, Name1)	
	
	
		openxlsx::saveWorkbook(wb, file = fileName, overwrite = TRUE)	
		## End of if		
		if( file.exists(fileName) )
		{
			print("succes", quote= FALSE, file=stderr() )	
		} else {
			print("error", quote= FALSE, file=stderr() )	
		}
			
}
	
### Data that is in debt collection for nordiska spel
if( regexpr("inkasso", args1, ignore.case = TRUE) > 0 )
{
	BinaryFile			<- file.path(DATA, "score.RData")
	if( file.exists(BinaryFile) )
	{
		load(BinaryFile) # DataSetDT
	}
	
	DateRange		<- DataSetDT[, range(checkDate, na.rm = TRUE)]
	
	
	
	DeltaErp 		<- ErpInvoice[grepl("Delta", collectionParty) & grepl("WAITING_TO_BE_COLLECTED", state)]
	DeltaErp[, Count := NULL]
		
	DeltaErp[order(collectionDate), Count := 1:.N, by = .(personId)]

		
	
	DeltaErp[, total_remain	:= paste0( totalAmount, " [", invoiceRemainingAmount,"]")]
	DeltaErp[, nrDayCollection := as.integer(difftime(Sys.Date(), collectionDate, units = 'days'))]
	noInvoice			<- DeltaErp[, .(noInvoice = .N), by = .(personId)]
	oneInvoice			<- noInvoice[noInvoice == 1,personId]
	moreInvoice			<- noInvoice[noInvoice > 1,personId]
	
	Name1		<- paste0('Nordiskaspel_', args1 ,'_', CurrentDate ,'.xlsx')
	wb			<- openxlsx::createWorkbook( )

	
	setkey(DeltaErp, personId)
	for( z in c('oneInvoice', 'moreInvoice'))
	{
		idx				<- get(z)
		DataSet			<- DeltaErp[J(idx) ]
		Step1			<- dcast.data.table( DataSet, personId ~ Count, value.var = c('ocrNumberReminder','total_remain'))
		for(i in 2:NCOL(Step1))
		{
			set(Step1, i = which(is.na(Step1[, c(i), with = FALSE])), j = i, value = 0L)
		}
	
		PaidDT			<- DataSet[, .( totalAmount = SUM(totalAmount), paidAmount = SUM(money)), by = .(personId)]
	
		
		timeDT			<- DataSet[, .(	FirstInvoice = collectionDate[1], 
										LastInvoice	=  collectionDate[.N]), by = .(personId)]
	
		timeDT[, nrDayCollection := as.integer(difftime(Sys.Date(), LastInvoice, units = 'days'))]

		DeltaDT 		<- Reduce(function(x,y) merge(x,y, by = 'personId', all.x = TRUE), 
										list( 	Step1, 
												PaidDT, 
												timeDT,
												DataSetDT[, .( personId, DEBT_SUM , TOTAL_INCOME, TOTAL_INCOME2, SCORING , scale)], 
												Person[, .( personId, firstName, lastName, email, phone, ssn)]
						))

		
		Step1			<- DeltaDT[nrDayCollection <= 40][order(nrDayCollection, scale)]
		Step2			<- DeltaDT[nrDayCollection > 40][order(nrDayCollection, scale)]
		name			<- paste0(z, '_less40Days')			
		name1			<- paste0(z, '_more40Days')	
		if(NROW(Step1) > 0)
		{
			what			<- gsub("(.*)Invoice", "\\1", z)
			addWorksheet( wb, name)
			df		<- data.frame(	"total_remain_*" = 'Total invoiced amount [invoice remaining amount]',
									totalAmount		= "Total debt the person has to Delta inkasso",
									paidAmount		= "Amount the person has paid to paylevo/Delta",
									nrDayCollection = 'Days in collection from the LastInvoice',
									Not				= sprintf('Customer has %s invoice and the last in within 40 days', what))

			df1		<- data.frame( What = colnames(df), Explanation = t(df))
			
			setColWidths(wb, sheet = name, cols = 1, widths = 25)
			setColWidths(wb, sheet = name, cols = 2, widths = 45)
			writeData(wb, sheet = name,  x = df1, startRow = 1, startCol = 1,	borders = "surrounding", 
							borderStyle = 'double' , headerStyle = hs1)

			setColWidths(wb, sheet = name, cols = 3:NCOL(Step1), widths = 15)
			
			Step_1			<- Step1[, .(totalAmount  = SUM(totalAmount), paidAmount = SUM(paidAmount)), by = .(scale) ]
			Step_1$scale	<- factor(Step_1$scale, scaleOrder)
			Step_1			<- Step_1[order(scale)]
			Step_1			<- rbind(Step_1, Step_1[, lapply(.SD, tmpSum)])
			Step_1[.N, scale := 'Total']
			Step_1[, ':=' ( totalAmount = Format(totalAmount), paidAmount = Format(paidAmount) )]
			
			writeData(wb, sheet = name,  x = Step_1, startRow = 1, startCol = NCOL(df1) + 2,	borders = "surrounding", 
							borderStyle = 'double' , headerStyle = hs1)
			
			Nrow			<- ifelse( NROW(Step_1) >= NROW(df1), NROW(Step_1), NROW(df1) )				
			Count 	<-  Nrow + 3				
			
			writeDataTable(wb, sheet = name,  x = Step1,  startRow =  Count , 
							 tableStyle = 'TableStyleMedium21',	headerStyle = hs2) 
			rm(Step1, what, name, Step_1)				 
		}	
		
		if(NROW(Step2) > 0)
		{
			what			<- gsub("(.*)Invoice", "\\1", z)
			addWorksheet( wb, name1)
			df		<- data.frame(	"total_remain_*" = 'Total invoiced amount [invoice remaining amount]',
									totalAmount		= "Total debt the person has to Delta inkasso",
									paidAmount		= "Amount the person has paid to paylevo/Delta",
									nrDayCollection = 'Days in collection from the LastInvoice',
									Not				= sprintf('Customer has %s invoice and the last in after 40 days', what))

			df1		<- data.frame( What = colnames(df), Explanation = t(df))
			Count 	<-  NROW(df1) + 3
			setColWidths(wb, sheet = name1, cols = 1, widths = 25)
			setColWidths(wb, sheet = name1, cols = 2, widths = 45)
			writeData(wb, sheet = name1,  x = df1, startRow = 1, startCol = 1,	borders = "surrounding", 
							borderStyle = 'double' , headerStyle = hs1)

			setColWidths(wb, sheet = name1, cols = 3:NCOL(Step2), widths = 15)
			
			Step_2		<- Step2[, .(totalAmount  = SUM(totalAmount), paidAmount = SUM(paidAmount)), by = .(scale) ]
			Step_2$scale	<- factor(Step_2$scale, scaleOrder)
			Step_2			<- Step_2[order(scale)]
			Step_2			<- rbind(Step_2, Step_2[, lapply(.SD, tmpSum)])
			Step_2[.N, scale := 'Total']
			Step_2[, ':=' ( totalAmount = Format(totalAmount), paidAmount = Format(paidAmount) )]
			
			writeData(wb, sheet = name1,  x = Step_2, startRow = 1, startCol = NCOL(df1) + 2,	borders = "surrounding", 
							borderStyle = 'double' , headerStyle = hs1)
			
			Nrow			<- ifelse( NROW(Step_2) >= NROW(df1), NROW(Step_2), NROW(df1) )				
			Count 	<-  Nrow + 3	
			
			
			writeDataTable(wb, sheet = name1,  x = Step2,  startRow =  Count , 
							 tableStyle = 'TableStyleMedium21',	headerStyle = hs2) 
		
			rm(Step2, what, name1, Nrow, Step_2)	
		}	
		
	
	}## end of forLoop

				
		
											
					
		fileName		<- file.path(DATA, Name1)	
	
	
		openxlsx::saveWorkbook(wb, file = fileName, overwrite = TRUE)	
		## End of if		
		if( file.exists(fileName) )
		{
			print("succes", quote= FALSE, file=stderr() )	
		} else {
			print("error", quote= FALSE, file=stderr() )	
		}
			
	
}



if( regexpr("stat", args1, ignore.case = TRUE) > 0 )
{
	SRC					<- grep('insertperson', RFiles, ignore.case = TRUE, value = TRUE)
	BinaryFile			<- file.path(DATA, "score.RData")
	DEBUG				<- file.path(LOG, "debug.txt")
	file.exists(DEBUG) && file.remove(DEBUG)
	Mtime				<- as.Date(file.info(BinaryFile)$mtime) 
	
	if(file.exists(BinaryFile))
	{
		load(BinaryFile) # DataSetDT
	}
	
	
	
	
	DateRange		<- DataSetDT[, range(checkDate, na.rm = TRUE)]
	
	
	if( length(SRC) == 1 & CondDate > 1 )
	{
		logOutput		<- file.path(LOG, 'outjson.txt')
		source(SRC, echo = TRUE) # creditData
		OutPut 	<- list()
		setkey(creditData, personId)
		creditData1			<- getData(query = "select * from kriita_survey.PaylevoCreditCheck")
		creditData1			<- unique(creditData1, by = 'personId', fromLast = TRUE)
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
			
			
			
	OutPut				<- list( )		
	Tid <- system.time({
			cores 	<- getOption("mc.cores", detectCores())
			cl		<- makeCluster( cores, outfile = logOutput ) 
			clusterCall( cl, worker.init,  c('data.table', 'jsonlite'))
			## Används för lägga in webExtrac och data in i environmnet
			clusterExport(cl, c("creditData1","OutPut", "f","logOutput"), envir = environment())
			Output	<- parLapply(cl, 1:NROW( creditData1 ), 
				function( x ) f( creditData1[x]) )
			stopCluster(cl)
		})[1:3]

	
	DataSet		<- rbindlist(Output)
	DataSet[, keys := gsub("BODY\\.(.*)", "\\1", keys)]
	DataSet 		<- DataSet[!grepl("\\.", keys), ]
	getKeys			<- '(AGE|FINAL_TAX|INCOME|DEBT|SCORING)'
	DataSet1		<- unique(DataSet[keys %like% getKeys], by = c('personId', 'keys'), fromLast =TRUE)
	CreditData		<- dcast.data.table(  DataSet1,    personId  ~ keys  )
	rm(DataSet, DataSet1); gc(reset = TRUE)
	CreditData[, personId := as.character(personId)]


		

	CreditData[, SCORING := as.integer(SCORING)]
	for( i in 1:length(scoreScale))
	{
		cat(i, "\n")
		step1			<- scoreScale[i]
		step2			<- parse(text = step1)
		CreditData[SCORING %between% eval(step2), scale := names(step1) ] 
	} 
	CreditData[ is.na(scale),  scale := 'LågLåg']
	rm(step2, step1,i)
	# save(DataSetDT, file = file.path(DATA, "score.RData"))
	toNumeric				<- c('SCORING','TOTAL_INCOME','TOTAL_INCOME2','DEBT_SUM','DEBT_AMAL_SUM')
	CreditData[, c(toNumeric) := lapply(.SD, as.numeric), .SDcols = toNumeric]
	
		
	for(i in 1:length(podNum))
	{
		step1		<- unlist(podNum[i])
		CreditData[SCORING %between% eval(parse(text = step1[[1]])),  POD := as.numeric(step1[2]) ]
	}
		
	## settledAmount is amount actual paid outd
	ErpInv			<- ErpInvoice[country == 'SE', .( invoiceNumber, 
									personId, 
									playerId,
									createdAt, 
									merchantId  = siteId,
									state ,
									OriginalAmount = playingOriginalAmount - invoiceFee ,
									settledAmount,
									settlementPaidAt)]
	
	ErpInv_1		<- ErpInv[, .(	Antal			= .N,
									dropOut			= SUM(grepl("UNNECESSARY|CLOSED", state)),
									open			= SUM(grepl("INVOICE|REMINDED|COLLECTED", state)),
									OriginalAmount 	= SUM(OriginalAmount),
									settledAmount	= SUM(settledAmount)
	 ), by = .(personId = as.character(personId))]
	
	ErpInv_1		<- merge(ErpInv_1 , CreditData, by = 'personId', all.x = TRUE )
	ErpInv_1 		<- ErpInv_1[order(settledAmount)][, idx := .I]

	
ErpInv_1[order(settledAmount)]
	Line		<-	ggplot(ErpInv_1[order(settledAmount)], aes(personId, y = settledAmount))  + 
						geom_bar(size = 1.2, stat="identity") +
						facet_wrap( ~ variable, scales = 'free', ncol = 1 ) +
						scale_y_continuous(expand = c(0.1,0.1),
										breaks = pretty_breaks(10), labels = Format) +
						scale_x_discrete(expand = c(0.01,0.01), breaks = pretty_breaks(20))	+							
						theme_dark() +
						theme( legend.position = "bottom", 
						title = element_text(size = 11, vjust = 1.5)) +
						GuideCol(x = 'Settlment') +
						ggtitle("Settlement for year 2016 and onwards")
		


ErpInv_1[, SUM(settledAmount)]
		
		Sub_1					<- Subscription[, .(startsAt , subState = state, playerId = merchantCustomerId, subscriptionId ,merchantId, amount, merchantItemId)]
		Sub_1					<- merge(Sub_1, Player[, .(playerId, personId)], by = 'playerId', all.x = TRUE)
		Sub_1 					<- unique(Sub_1, by = c('subState', 'subscriptionId'))
		
		ErpInv					<- merge(ErpInv, Sub_1, by = c('playerId', 'personId', 'merchantId'), all.x = TRUE)
	  	file.exists(BinaryFile) && file.remove( BinaryFile) 
		
		save(ErpInv_1, file = file.path(DATA, "score.RData"))

	

	}
	
	gc(reset = TRUE)

		
	
	KreditChecks		<- data.frame(  Creditcheck[, ftable(decision1 ~ RISK + status)])
	KreditChecks 		<- setDT(KreditChecks)[Freq > 0]
	KreditChecks1		<- dcast(KreditChecks, RISK + decision1 ~ status, SUM , margins = TRUE)
	KreditChecks1 		<- setDT(KreditChecks1)
	
	Creditcheck[, c('invoicableStatuses','invoicableStatuses1','test') := NULL ]
	
	tt 				<-  tableFun(KreditChecks1[ RISK == 'Admin'], plot = FALSE)
	tt1 			<-  tableFun(KreditChecks1[ RISK != 'Admin' & RISK != '(all)'], plot = FALSE)
	#################### OUTPUT ####################
	AdminChart		<- file.path(GRAF, "AdminCreditTable.pdf")
	pdf( file = AdminChart,
	     height = unit(5,"cm"), width = unit(18,"cm"),
	     pointsize = 10, colormodel = "rgb", bg = "white")  
		
	 grid.arrange(tt1, tt, nrow = 2, as.table = TRUE, heights = c(10,9))
	dev.off()
	############################################################
	rm(tt,tt1, KreditChecks1, KreditChecks)
	

							
	CurrentDate			<- Sys.Date()
	date4Months			<- CurrentDate - months(4) ## credit loss if not paid
	
	idx					<- ErpInvoice[, unique(personId)]
	setkey(Person, personId)
	PersonDT			<- Person[J(idx)][,.( personId, country, email, phone, ssn, street, zip, city)]	
		DataSetDT_1		<- merge( DataSetDT[!is.na(checkDate )], Player[,.(personId, playerId)], by = 'personId', all.x = TRUE)
	## Take the sample, estimate the cost
	Subscription[merchantCustomerId == '150323-24']
	 ErpInvoice[playerId =='150323-24']
	Sub_1					<- Subscription[, .(startsAt , state,merchantCustomerId, subscriptionId ,merchantId, amount, merchantItemId)]
	Sub_1					<- merge(Sub_1, Player[,.(personId, merchantCustomerId  = playerId)], by = 'merchantCustomerId', all.x = TRUE)
	Sub_1					<- unique(Sub_1, by = c('merchantCustomerId', 'personId','subscriptionId'))
	
	Subscription1 		<- merge( Sub_1,
								DataSetDT, by = 'personId',all.x = TRUE)
	
	
	###### Cost ##########
	Subscription1[, Cost := ifelse( SCORING <= 0, amount*0.5, amount*0.95)]
	Subscription1[is.na(Cost) , Cost := amount*0.95]
	
	
	
	SubSummay			<- dcast.data.table( data.table( Subscription1[state == 'RUNNING', table( scale , merchantItemId)]), 
			scale  ~ merchantItemId 
	)
	
	SubSummay$scale		<- factor(SubSummay$scale, scaleOrder)
	SubSummay 			<- SubSummay[order(scale)]
	SubSummay			<- RowStat(SubSummay, id = 'scale', type = 'sum')
	SubSummay			<- rbind(SubSummay, SubSummay[, lapply(.SD, tmpSum)])
	#################### OUTPUT ####################
	SubSummay[is.na(scale) , scale := 'Total']
	############################################################
	Subscription1[, c('amount','state','merchantItemId') := NULL]


	ErpInvoice1 			<- merge(ErpInvoice,  Subscription1, 
									by.x = c('playerId', 'personId'),
									by.y = c('merchantCustomerId', 'personId'), all.x = TRUE)
	
	ErpInvoice1			<- merge(ErpInvoice1, Person[, .(personId, ssn)], by = 'personId', all.x = TRUE)
	ErpInvoice1[, Age :=  as.integer(format(Sys.Date(), "%Y")) - as.integer(gsub("(\\w{4}).*", "\\1", ssn))]
	ErpInvoice1			<- unique(ErpInvoice1, by = 'invoiceNumber')
	ErpInvoice1[, Count := 1:.N, by = .(subscriptionId)]
	ErpInvoice1[Count > 1, Cost := 0L]
		
	ErpInvoice1[, ':=' ( INCOME = as.numeric(INCOME), TEXEBLE_INCOME = as.numeric(TEXEBLE_INCOME))]
	
	IncomeRange			<- ErpInvoice1[, quantile(INCOME, na.rm = TRUE, probs = seq(0,1,0.1))]
	Seq2				<- as.numeric(sapply(IncomeRange, function(x) round_any(x, 1000, ceiling)))
	
	ErpInvoice1[, IncomeIntervall := cut2(INCOME, Seq2)]
	
	
	ErpInvoice1[, IncomeIntervall := gsub("\\s+", "", IncomeIntervall)]
	ErpInvoice1[, settleMonth := format(as.Date(settlementPaidAt), "%Y-%m")]
	
	

	
	Settlment			<- ErpInvoice1[settlementPaidAt  >= '2016-01-01' , .(settledAmount = SUM(settledAmount) - SUM(Cost)), by = .(IncomeIntervall)][order(IncomeIntervall)]	
	Settlment[, firstInt := as.numeric(gsub("\\[([0-9]+)\\,.*","\\1", IncomeIntervall))]
	Settlment			<- Settlment[order(firstInt)]
	Settlment[, Kumulative_Settled := paste0(round(cumsum(settledAmount)/SUM(settledAmount),3)*100, "%")]
	Settlment			<- rbind(Settlment, Settlment[, lapply(.SD, tmpSum)])
	Settlment[, settledAmount := Format(as.numeric(settledAmount))]
	Settlment[, firstInt := NULL]
	#################### OUTPUT ####################
	Settlment[.N, IncomeIntervall := 'Total']
	############################################################
	Scale			<- ErpInvoice1[settlementPaidAt  >= '2016-01-01', .(settledAmount = SUM(settledAmount) - SUM(Cost) ), by = .(scale)][order(scale)]	
	Scale$scale		<- factor(Scale$scale, levels = scaleOrder)
	Scale 			<- Scale[order(scale)]
	Scale[, Kumulative_Settled := paste0(round(cumsum(settledAmount)/SUM(settledAmount),3)*100, "%")]
	Scale			<- rbind(Scale, Scale[, lapply(.SD, tmpSum)])
	Scale[, settledAmount := Format(as.numeric(settledAmount))]
	#################### OUTPUT ####################
	Scale[.N, scale := 'Total']
	tt 				<-  tableFun(Scale, plot = FALSE)
	tt1				<-  tableFun(Settlment, plot = FALSE)
	SettleChart		<- file.path(GRAF, "Settlmentable.pdf")
	pdf( file = SettleChart,
	     height = unit(5,"cm"), width = unit(10,"cm"),
	     pointsize = 10, colormodel = "rgb", bg = "white")  
		
	 grid.arrange(tt1, tt, nrow = 1, as.table = TRUE)
	dev.off()
	
	
	############################################################
	
	AGE			<- ErpInvoice1[settlementPaidAt  >= '2016-01-01', .(settledAmount = SUM(settledAmount) - SUM(Cost)), by = .(Age)][order(Age)][!is.na(Age)]	
	AGE[order(Age), kumulativeSettlment := cumsum(settledAmount)]
	AGE[, proportionSettlement := round(kumulativeSettlment/SUM(settledAmount),3)]
	AGE1		<- melt.data.table(AGE[,c(1:3), with = FALSE], id.vars = 'Age')
	AGE1[, Age := as.character(Age)]				
	
	
	Line		<-	ggplot(AGE1, aes(Age, y = value, group = variable, colour = variable))  + 
						geom_line(size = 1.2) +
						facet_wrap( ~ variable, scales = 'free', ncol = 1 ) +
						scale_y_continuous(expand = c(0.1,0.1),
										breaks = pretty_breaks(10), labels = Format) +
						scale_x_discrete(expand = c(0.01,0.01), breaks = pretty_breaks(20))	+							
						theme_dark() +
						theme( legend.position = "bottom", 
						title = element_text(size = 11, vjust = 1.5)) +
						GuideCol(x = 'Settlment') +
						ggtitle("Settlement for year 2016 and onwards")
	#################### OUTPUT ####################
	LineName		<- file.path( GRAF, "LineFlow.pdf")
	pdf( file = LineName,
	     height = unit(8,"cm"), width = unit(9,"cm"),
	     pointsize = 10, colormodel = "rgb", bg = "white")  
		print( Line )		
	dev.off()	
	############################################################	


	SubInvoice		<- ErpInvoice1[!is.na(subscriptionId) & as.Date(createdAt) <= date4Months, .(	noPaid			= SUM(state == 'PAID'),
																noOpen			= SUM(grepl("INVOICE|REMINDED_1|PARTLY_PAID", state)),
																noCollection	= SUM(state == 'WAITING_TO_BE_COLLECTED'),
																noDropOut		= SUM(grepl("UNNECESSARY|CLOSED", state)),
																moneyIn 		= SUM(settledAmount), 
																Cost 			= SUM(Cost),
																Antal			= Count[.N]
	), by = .(personId)]
	SubInvoice[, profit := moneyIn - Cost]
	
	tmpErp			<- unique(ErpInvoice1, by = 'personId')
	tmpErp[, c('Cost','sessionId','invoiceNumber', 'createdAt', 'settledAmount', 'settlementAmount') := NULL]
	SubInvoice 			<- merge(tmpErp , SubInvoice, by = 'personId')
	SubInvoice[, INCOME := as.integer(INCOME)]
	library(rpart)
	library(rpart.plot)
	library(partykit)
	library(party)
	library(caret)
	
	
	
	DataSet			<- SubInvoice[, .(	personId, profit, scale, Age ,SCORING, ZIPCODE, 
										INCOME, INCOME2 , DEBT_SUM, DEBT_NUMBER, 
										ASSESSED_VALUE_TOTAL, TEXEBLE_INCOME, TEXEBLE_INCOME2, 
										TOTAL_INCOME, TOTAL_INCOME2)]
	DataSet 		<- DataSet[!is.na(SCORING)]
	split			<- 0.80
	
	
	numCols			<- which(! names(DataSet) %like% '(ZIPCODE|scale|personId)') 
	## Make them as numeric
	DataSet[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]
	DataSet[, Customer := ifelse(profit  > 0, 'Good', 'Bad')]
	#DataSet[, summary(lm(log(profit) ~ SCORING + INCOME + INCOME2 +DEBT_SUM + ASSESSED_VALUE_TOTAL + DEBT_NUMBER))]
	#DataSet[, cor(profit, SCORING)]
		
	trainIndex		<- createDataPartition(DataSet$personId, p = split, list = FALSE )	
	Data_train		<- DataSet[trainIndex,  ][!is.na(INCOME)]
	Data_test		<- DataSet[-trainIndex, ]

	cartGrid 	<- expand.grid(.cp=(1:50)*0.01)
	fitControl 	<- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	modFormula				<- formula(Customer ~ SCORING + INCOME + DEBT_SUM)
	main_model				<- train( modFormula, data = Data_train, 	tuneLength = 30, 
									method = 'rpart', trControl = fitControl, tuneGrid = cartGrid)
	plot( varImp(main_model, scale = FALSE)			)					
	print(main_model)								
	
 	main_tree 		<- rpart(modFormula, method = 'class', data = Data_train, control = rpart.control( cp = 0.001, minbucket =50 ))
	main_predict 	<- predict(main_tree, Data_test, type = 'class' )
	confusionMatrix(main_predict, Data_test$Customer)
	
	split.fun 	<- function(x,labs, digits, varlen, faclen)
	{
		labs1	<- strsplit(labs, "\\s")
		labs 	<-  do.call('c', lapply(labs1, function(x) unlist(paste0(x[1], " ",  x[2], " ", as.integer(x[3])))))
		labs
			
	} 
	
	printcp(main_tree)
	plotcp(main_tree)

	#################### OUTPUT ####################
	DecisionName		<- file.path( GRAF, "DecisionPlot.pdf")
	pdf( file = DecisionName,
	     height = unit(8,"cm"), width = unit(9,"cm"),
	     pointsize = 10, colormodel = "rgb", bg = "white")  
		rpart.plot(main_tree, type = 4, extra = 101, tweak = 0.9, round = 0, space = 1,
									split.fun  = split.fun , leaf.round = 0, varlen = 8 , yesno.yshift = -1,
									nn = TRUE, faclen = 0, branch.lty = 3,shadow.col = 'gray')	
	dev.off()	
	############################################################	
	partykit:::.list.rules.party(as.party(main_tree))
	
	
	###########################################################################
	# Survival analysis
	###########################################################################
	library(ggfortify) 
	
	
	InvoiceState		<- unique( InvoiceStateLog[grepl("\\b(INVOICE_SENT|INVOICED)\\b", toState), .(invoiceNumber, toState, created)],
								 by = 'invoiceNumber', fromLast = TRUE)
	ErpInv				<- ErpInvoice1[!is.na(subscriptionId) & as.Date(createdAt) <= date4Months,]	
	ErpInv				<- merge(ErpInv, InvoiceState, by = 'invoiceNumber', all.x = TRUE)
	ErpInv[, ':=' (  sentAt = as.Date(created),
					closedAt 	= as.Date(closedAt),
					Bill_date	= as.Date(createdAt)
	)]
	ErpInv[, Done := ifelse(grepl("PAID", state), 1, 0)]
	ErpInv[grepl('PAID', state) & is.na(closureDate), closureDate := as.character(moneyDate )]	
	ErpInv[is.na(sentAt), sentAt := Bill_date]
	
	ErpInv[grepl("PAID", state), time := as.integer(difftime(closureDate , sentAt, units = 'days'))]
	ErpInv[grepl("UNNECESSARY", state), time := as.integer(difftime(closedAt , Bill_date, units = 'days'))]	
	ErpInv[grepl("CLOSED", state), time := as.integer(difftime(closedAt , Bill_date, units = 'days'))]
	ErpInv[grepl("INVOICE_SENT|REMINDED_1|WAITING_TO_BE_COLLECTED", state), time := as.integer(difftime(Sys.Date() , sentAt, units = 'days'))]
	ErpInv[time < 0, time := time*-1]
	fit 	<- survival::survfit(
  				survival::Surv(time = time, event = Done == 1, type = "right") ~ scale ,  ErpInv)	
		
	res 	<- fortify(fit)
	setDT(res)
	res[, scale := gsub(".*=(.*)", "\\1", strata)]
	ErpInv[, ':=' (	Daycollection 	= as.integer(difftime( collectionDate, sentAt, units = 'days')),
					DayDue			= as.integer(difftime( dueDate, sentAt, units = 'days'))
	)]
	
	
	Vline		<- ErpInv[, .(MeanCollection = round(MEAN(Daycollection)), MeanDue = round(MEAN(DayDue)))]
	

	res$scale		<- factor(res$scale, scaleOrder)
	res				<- res[order(scale)]
	Line			<- ggplot(res, aes(x = time, y = 1- surv, group = scale, colour = scale, order = scale )) +
						geom_ribbon(aes(ymin = 1- lower, ymax = 1- upper), fill = 'steelblue', color = 'black') +
						geom_line( size = 0.9) +
						coord_cartesian(xlim = c(0,200), ylim = c(0,1) ) +
						geom_vline(xintercept =  Vline$MeanCollection, colour = 'red') +
						theme_dark( ) +
						theme(legend.position = 'bottom') +
						scale_y_continuous(expand = c(0.01,0.01),
								breaks = pretty_breaks(10), labels = percent) +
						scale_x_continuous(expand = c(0.01,0.01), breaks = pretty_breaks(10))	+	
						labs(x = 'Dagar till betalning', y = 'Andel') +
						GuideCol('Score Scale') +
						ggtitle("Sannolikhet till att en person betalar sin faktura [rödlinje: efter inkasso]")
			
	
	#################### OUTPUT ####################
	SurvName		<- file.path( GRAF, "SurvivalPlot.pdf")
	pdf( file = SurvName,
	     height = unit(8,"cm"), width = unit(11,"cm"),
	     pointsize = 10, colormodel = "rgb", bg = "white")  
		print(Line)
	dev.off()	
	############################################################	

} ## end of stat



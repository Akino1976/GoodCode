#!/usr/bin/env Rscript 
##########################################################################################
# This script is used for all sort of task that aren't to be for 
# reporting purposes, just for solvning different task
##########################################################################################
options(scipen = 999)
options("openxlsx.dateFormat" = "yyyy-mm-dd")

Sys.setenv("R_ZIPCMD" = "/usr/bin/zip") ## makes openxlsx works in centos



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
args1			<- ifelse( length( commandArgs(TRUE)) > 0, commandArgs(TRUE), "sbg")
args1			<- tolower(args1)




#' RETURN Piechart, data has to be in a specific format
PieChart	<- function( data) {
	
		TotalValue		<-  data[, SUM(CreditIn)]						

		BAR		<- ggplot(data, aes( fill = country, ymax = ymax, ymin = ymin,
						 		xmax = 5, xmin = 2))	+
				geom_rect( )+
				coord_polar( theta = "y") +
				xlim(c(0,5)) + theme_tufte() + 
				theme( 
					axis.text.x	= element_blank( ), 
					axis.text.y	= element_blank( ),
					axis.title.y = element_blank(),
					legend.position = "bottom",
						title	= element_text(size = 9),
					axis.ticks	= element_blank()
					)+
			scale_fill_tableau("tableau20") +
		 annotate("text", x = 0, y = 25, 
				 	label = sprintf("Total settlement \n%s\nSEK", Format(TotalValue)),
					 	size = 4) +
				guides(	fill = guide_legend(
					  title = sprintf("Contributed country"), 
					   title.position = "top", 
					  nrow = 1, 
					 title.hjust = 0.5,
					title.theme = element_text(size = 10, angle = 0),
					keywidth 	= unit(15, "mm"), 
					keyheight	= unit(4,"mm"),
					label.position = "bottom",
					label.hjust 	= 0.5,
					label.theme 	=  element_text(size = 9, angle = 0) 
					)) +
				 labs(x = NULL, y = NULL) 
		return(BAR)		 
				 
	}
	

y_date_format <- function()
{
   function(x)
   {
       m <- format(x,"%b")
       d <- format(x,"%d")
       ifelse(duplicated(m), d ,paste0(d, "\n", m))
   }
}

last_day 				<- function(date) {
  			 ceiling_date(date, "month") - days(1)
}


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



			
Query	<- sprintf("SELECT CONCAT(inv.siteId, '.', LOWER(ei.country)) AS siteId, 
							inv.siteId AS merchantId,
							inv.ocrNumber, 
							inv.playerId,
							ei.clientStatus,
							inv.state,
							ei.country,
							et.* FROM ErpTransaction et 
						INNER JOIN ErpInvoice ei ON(ei.invoiceNumber = et.invoiceNumber)
						INNER JOIN Invoice inv ON(inv.invoiceNumber = ei.invoiceNumber)
						WHERE settlementAt >= '%s'",
					 	StartDate);

Step1 		<- getData(query = Query )

Step1		<- Step1[!siteId %like% "(kriita|3h)"]


Step1		<- merge(Step1, Rates, by.x = "currency", by.y = 'Currency', all = TRUE)
Step1 		<- Step1[!is.na(siteId)]

ErpTransaction		<- copy(Step1)
ErpTransaction 		<- ErpTransaction[!is.na(invoiceNumber)]


rm(Step1)
gc(reset = TRUE)

	Payments			<- getData(query = "SELECT * FROM kriita_survey.Payment")

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

	for( i in 1:length(BankAccount))
	{
		Step1 	<- BankAccount[i]
		Payments[grepl(names(Step1), AccountId), Valuta := Step1[[1]][[3]]]	
	}



	Payments 	<- merge(Payments, Rates, by.x = 'Valuta', by.y = 'Currency', all = TRUE)	
	Payments 	<- Payments[!is.na(File)]

	gc(reset = TRUE)
	Query		<- sprintf("SELECT * FROM Site");
	Site		<- getData(query = Query)
	
	ErpTransaction		<- merge(ErpTransaction, Site[, .( siteId, operatorId)],
							 by.x = c('merchantId'), by.y = c('siteId'), all.x = TRUE) 



	
	
	Reservation			<- getData(query = "SELECT * FROM Reservation")
	Reservation			<- merge(Reservation, Site[, .( siteId, operatorId)],
							 by.x = c('merchantId'), by.y = c('siteId'), all.x = TRUE) 

	Transaction			<- getData(query = "SELECT * FROM Transaction")
	Transaction		<- merge(Transaction, Reservation[, .(reservationId, merchantId, operatorId)],
						by = c('reservationId'), all.x = TRUE) 

	SettlementDeduction			<- getData(query = "SELECT * FROM SettlementDeduction")
	setnames(SettlementDeduction, 'amount', 'DeductedAmount')

	
	SettlementDeduction[, tmpVal := paste0(DeductedAmount, " (", paidAt, ")")]
	SettlementDeduction[, Count := 1:.N, by = .(transactionId)]

	Step2				<- dcast.data.table(SettlementDeduction[order(paidAt)] , transactionId ~ Count, value.var = 'tmpVal')
	Names1				<- setdiff( names(Step2), "transactionId")
	Names2				<- paste0("Deduction", Names1)
	setnames(Step2, Names1, Names2)
	ErpTransaction 		<- merge(ErpTransaction, Step2, by = 'transactionId', all.x = TRUE)
	SettlementDeduction[, tmpVal := NULL]

	rm(Step2)
	gc(reset = TRUE)
	
	Query			<- sprintf("SELECT * FROM ErpPaymentHistory where paymentDate >= '%s'", StartDate)
	Step1 			<- getData(query = Query )
	Step1[, paymentDate := as.Date(paymentDate)]
	ErpPaymentHistory 	<- copy(Step1)
	rm(Step1)
	
	CurrentDate		<- Sys.Date( )

if( regexpr("^(sbg)$", args1, ignore.case = TRUE) > 0 )
{
	
	ErpInv			<- ErpInvoice[operatorId %like% "(southbaygroup|messenio)",  ]
	ErpInv[, ':=' ( Invoiced_deposit_amount  = invoiceOriginalAmount - invoiceFee, 
					Bill_date				= as.Date(createdAt)
		)]
	
	ErpTrans		<- ErpTransaction[operatorId %like% "(southbaygroup|messenio)",  ]

	########################################################################
	# SBG  
	########################################################################				

	Date			<- Sys.Date() - days(7)
	
	Months			<- ifelse( month(CurrentDate) >= 10, month(CurrentDate), paste0("0",month(CurrentDate) ))
	
	regexpMonth			<- paste0(year(CurrentDate) , '-', Months )
	LastDay				<- CurrentDate - days(2)
	LastDate			<- CurrentDate - days(30)
	YearRegexp			<- ifelse( year(CurrentDate) != year(Date), paste0(year(CurrentDate) , '|', year(Date)), year(Date))
		
		
	Payed			<- Payments[grepl('Messenio|sbg', OrderPart1) & grepl(YearRegexp, ValueDate)]

	Payed_1 		<- Payed[ValueDate >= Date, .(TransaktionAmount	= SUM(TransaktionAmount)), by = .(Valuta, ValueDate, EntryDate)]

	ErpInv[, InvoiceState := ifelse(grepl('UNNECESSARY|CLOSED', state), 0, 1)]
	
	
	Line1		<- ErpInv[, .(Total = .N), by = .(Bill_date, InvoiceState)][order(Bill_date)]

	
	
	setkey(ErpInvoice , operatorId)
	Stats			<- 			ErpInv[, .(	Total  				= .N,
											NrPaid				= SUM(grepl('PAID', state)),
											NrCollection 		= SUM(state == 'WAITING_TO_BE_COLLECTED'),
											PaidInCollection 	= SUM(!is.na(collectionDate) & grepl('PAID', state)),
											CreditIN				= SUM(Invoiced_deposit_amount) - SUM(TransactionFee)), 
										by = .(siteId, invoiceCurrency)][order(Total, decreasing = TRUE)]


		Line1		<- ErpInv[, .(Total = .N), by = .(Bill_date, InvoiceState)][order(Bill_date)]
		Line_1		<- Line1[Bill_date >= LastDate]
		Line_1$InvoiceState		<- factor(Line_1$InvoiceState, levels = c(0,1), labels = c('DropOut', 'Invoiced'))
		Line_1[, TotalPeriod := SUM(Total), by = .(InvoiceState)]
		Line_1[, Facet := paste0(InvoiceState , ' nr: ' ,TotalPeriod)]
		Text		<- sprintf("Nr of invoice total difference by dropout from %s", LastDate)
		
		LineChart		<- ggplot(Line_1, aes(Bill_date, Total, group = 1)) +
									geom_line( size = 1.2, colour = 'white') +
									geom_point(size = 2.2, colour = 'white') +
									scale_x_date(expand = c(0.01,0.01),
													breaks = date_breaks("2 days"),
  													labels = date_format("%b\n%d"))  +
  									facet_wrap( ~ Facet, scales = 'free')		+		
  									scale_y_continuous( breaks = pretty_breaks(6), labels = Format) +	
  									theme_dark( ) + 
  									ggtitle(Text)	
  									
	LineName		<- file.path( GRAF, "LineFlow.pdf")
	pdf( file = LineName,
	     height = unit(8,"cm"), width = unit(9,"cm"),
	     pointsize = 10, colormodel = "rgb", bg = "white")  
		print( LineChart )		
	dev.off()	



	Data1		<- ErpTrans[!is.na(settlementPaidAt) , .(CreditIn 	= SUM(settlementAmount*Rates)) ,by = .(country, Rates)]

	Data2		<- Data1[,.SD[order(CreditIn) ] ]


	Data2[, c("ymax", "ymin") := list( 
					tmp 	<- cumsum(CreditIn),
					tmp - CreditIn
			)]


	
	BAR 	<- PieChart( data  = Data2 )
	
	BarName		<- file.path( GRAF, "Flow.pdf")
	pdf( file = BarName,
	     height = unit(8,"cm"), width = unit(9,"cm"),
	     pointsize = 10, colormodel = "rgb", bg = "white")  
		print( BAR )		
	dev.off()	

	setkey(SettlementDeduction, transactionId)
	Deduction		<- merge(ErpTrans[,.(siteId ,invoiceNumber, settlementPaidAt , transactionId, currency)], SettlementDeduction, by = 'transactionId', all.x = TRUE)
	Deduction_1	 	<- Deduction[!is.na(paidAt)]
	Deduction_1[, DeductedAmount := DeductedAmount*-1 ]


	Deduction_2 	<- Deduction_1[, .(DeductedAmount = SUM(DeductedAmount)), by = .(paidAt, currency)]
	
	Deduction3		<- Deduction_1[paidAt >= Date, ]

	for( i in 1:length(deductionRule))
	{
		step1		<- deductionRule[i]
		Deduction3[grepl(names(step1), type), typeOf := step1 ]
	}

	Deduction3 			<- merge( Deduction3 , ErpInv[,.(createdAt , invoiceNumber, clientStatus, money)], by = 'invoiceNumber', all.x = TRUE)

	Deduction4	 		<- Deduction3[, .(DeductedAmount = SUM(DeductedAmount)), by = .(typeOf, paidAt, currency)][order(paidAt)]


	DT_1			<- ErpTrans[settlementAt >= Date, ]
	DT_2			<- DT_1[!is.na(settlementPaidAt),
							 .(	settlementAmount 			= SUM(settlementAmount),
								settlementAmountDeductible	= SUM(settlementAmountDeductible),
								settlementAmountDeducted 	= SUM(settlementAmountDeducted)), by = .( settlementPaidAt, currency)][order(settlementPaidAt)]

	Payed			<- Payments[grepl('Messenio|sbg', OrderPart1) & grepl(YearRegexp, ValueDate)]

	Payed_1 		<- Payed[ValueDate >= Date, .(TransaktionAmount	= SUM(TransaktionAmount)), by = .(Valuta, ValueDate, EntryDate)]


	DT_2 			<- merge(DT_2, Payed_1, by.x = c('settlementPaidAt', 'currency'), by.y = c('ValueDate', 'Valuta'), all.x = TRUE )
	DT_2 			<- merge(DT_2, Deduction_2, by.x = c('settlementPaidAt', 'currency'), by.y = c('paidAt', 'currency'), all.x = TRUE )
	DT_2[, c('settlementAmountDeductible','settlementAmountDeducted') := NULL]
	for( i in 2:NCOL(DT_2))
	{
		set(DT_2, i = which(is.na(DT_2[, c(i), with = FALSE])), j = i, value = 0L)
	}
	DT_2[ EntryDate == 0, ':=' ( EntryDate = NA, TransaktionAmount = NA)]
	
	DT_2[, AmountToSBG := settlementAmount - DeductedAmount]
	
	
	
	
		Name1		<- paste0('SBG', CurrentDate , '.xlsx')
		wb			<- openxlsx::createWorkbook( )


		## Insert to excel 
		name 		<- 'Settlement'
		df		<- data.frame(	Start				= DT_2[, min(settlementPaidAt)],
								End					= DT_2[, max(settlementPaidAt)],
								settlementAmount	 = "Original amount that should be settled",
								EntryDate			= 'Transaction date from SHB %m-%d',
								TransaktionAmount	= "Amount registred from SHB",
								DeductedAmount		= "Amount deducted on that date",
								AmountToSBG			= 'Should be equalt to TransaktionAmount')
		
		df1		<- data.frame( What = colnames(df), Explanation = t(df))
		addWorksheet( wb, name)
			
		setColWidths(wb, sheet = name, cols = 1, widths = 25)
		setColWidths(wb, sheet = name, cols = 2, widths = 45)
		writeData(wb, sheet = name,  x = df1, startRow = 1, startCol = 1,	borders = "surrounding", 
								headerStyle = hs1)
		setColWidths(wb, sheet = name, cols = 3:NCOL(DT_2), widths = 25)
		
		Count		<- NROW(df1) + 3
		UniqueCurr	<- DT_2[, unique(currency)]
		for( i in UniqueCurr)
		{
			Step1		<- DT_2[currency == i]
			writeDataTable(wb, sheet = name,  x = Step1  ,  startRow =  Count , 
							 tableStyle = 'TableStyleMedium21',	headerStyle = hs2) 
			Count		<- Count + NROW(Step1) + 3
			
		}		
				
		Count		<- Count + 1	
		insertImage(wb, sheet = name, file = BarName, width = 8, height = 8, startRow = Count, startCol= 1 ) 	
		
		Deduction5 		<- dcast.data.table(Deduction4, paidAt + currency ~ typeOf, SUM)[order(paidAt, currency)]
		
		writeData(wb, sheet = name,  x = Deduction5, startRow = 6, startCol = NCOL(Step1) + 2,  headerStyle = hs1)	
			
			
		ErpSett		<- ErpTrans[grepl(regexpMonth, settlementPaidAt), .(settlementAmount  = SUM(settlementAmount)), by = .(siteId, currency)][order(settlementAmount)]	
		ErpSett1 	<- dcast.data.table(ErpSett, siteId ~ currency, SUM)
		ErpSett1	<- rbind(ErpSett1, ErpSett1[, lapply(.SD, tmpSum)], fill = TRUE)
		ErpSett1[is.na(siteId), siteId := 'Total']
		writeData(wb, sheet = name,  x = ErpSett1, startRow = Count, startCol = NCOL(Step1) - 1,  headerStyle = hs1)	


		########################################################################################################
		# Deduction
		########################################################################################################
		
		## Insert to excel 
		name 		<- 'Deduction data'
		df		<- data.frame(	settlementPaidAt 	= 'When an settlement to merchant has happend', 
								paidAt				= 'When a deduction from merchant has happend',
								typeOf				= 'The resason for the deduction',
								status				= 'The status of the invoice',
								createdAt			= 'When the invoice/transaction were created'	)
		
		df1		<- data.frame( What = colnames(df), Explanation = t(df))
		addWorksheet( wb, name)
			
		setColWidths(wb, sheet = name, cols = 1, widths = 25)
		setColWidths(wb, sheet = name, cols = 2, widths = 45)
		writeData(wb, sheet = name,  x = df1, startRow = 1, startCol = 1,	borders = "surrounding", 
								headerStyle = hs1)
		setColWidths(wb, sheet = name, cols = 3:NCOL(Deduction3), widths = 25)
		
		Count		<- NROW(df1) + 3
		UniqueCurr	<- Deduction3[, unique(currency)]
		for( i in UniqueCurr)
		{
			Step1		<- Deduction3[currency == i]
			Step2		<- Step1[, .(DeductedAmount  = SUM(DeductedAmount)), by = .(paidAt, currency)]
			writeData(wb, sheet = name,  x = Step2, startRow = Count, startCol = 1,	borders = "surrounding", 
								headerStyle = hs1)
			Count		<- Count + NROW(Step2) + 2 					
								
			writeDataTable(wb, sheet = name,  x = Step1  ,  startRow =  Count , 
							 tableStyle = 'TableStyleMedium21',	headerStyle = hs2) 
			Count		<- Count + NROW(Step1) + 3
			
		}		
				
		Count		<- Count + 1	
		
		
		

				
				
		name 		<- 'InvoiceProfile'
		df			<- data.frame(	Start				= ErpInv[, min(Bill_date)],
									End					= ErpInv[, max(Bill_date)],
									Total	 			= "Nr of invoice created",
									NrPaid				= 'Nr of invoices where customer has paid their invoice to Paylevo',
									NrCollection		= "Invoices that currently are in collection",
									PaidInCollection	= "Nr of invoices that has been in collection and then paid",
									CreditIN			= "Potential settlment to southbaygroup (depends on risk|norisk)")

		
		df1		<- data.frame( What = colnames(df), Explanation = t(df))
		addWorksheet( wb, name)
			
		setColWidths(wb, sheet = name, cols = 1, widths = 25)
		setColWidths(wb, sheet = name, cols = 2, widths = 45)
		writeData(wb, sheet = name,  x = df1, startRow = 1, startCol = 1,	borders = "surrounding", 
								headerStyle = hs1)
		setColWidths(wb, sheet = name, cols = 3:NCOL(Stats), widths = 25)		
		
		Count		<- NROW(df1) + 4
		
		writeData(wb, sheet = name,  x = Stats, startRow = Count, headerStyle = hs1)	
		insertImage(wb, sheet = name, file = LineName, width = 8, height = 8, startRow = Count, startCol= NCOL(Stats) + 2 ) 	

	
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

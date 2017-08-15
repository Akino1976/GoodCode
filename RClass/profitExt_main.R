##########################################################################################
# This script is used for all sort of task that aren't to be for 
# reporting purposes, just for solvning different task
##########################################################################################
options(scipen = 999)
options("openxlsx.dateFormat" = "yyyy-mm-dd")


# Function used for setting the path so that files can be loaded

	

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
CurrentDir		<- "jobs"



# All R-files 
RFiles 					<- list.files(path = .HOME, include.dirs = TRUE, pattern = "*.R$", 
											recursive = TRUE, full.names = TRUE)
# Load common functions and files 
.HOME
source(file =  grep(".*/RClass/commonFunction.R", RFiles ,value = TRUE) )

DataFile		<- grep(".*RClass.*datahandle", RFiles, value = TRUE , ignore.case = TRUE)

## Load data into this envirment, path and everything is set by this file
if( length(DataFile) == 1 )
{
	source( file = DataFile, echo = TRUE)
}

Summary			<- 		quote("list(Antal 	= .N ,
									invoiceFee 		= SUM(invoiceFee),
									TransactionFee 	= SUM(TransactionFee),
									interest 		= SUM(interest),
									reminderFee 	= SUM(reminderFee),
									compensation	= SUM(compensation),
									OverPaid		= SUM(ifelse(invoiceRemainingAmount < 0, invoiceRemainingAmount*-1, 0)),
									UnderPaid		= SUM(ifelse(invoiceRemainingAmount > 0, invoiceRemainingAmount, 0))
				)")
				
Summary 		<- gsub("\\t|\\n", "", Summary)


## Set path and load pkgs, set path one level up
Pack		<- new("startUps", pkgs = Packages, Input = c("GRAF", "DATA", 'VinstUtBET'), 
				 path = .HOME )

#Pack$instant_pkgs()

Pack$setDirs( )

		hs2	<- createStyle(	
					fgFill = "black", 
					fontSize = 12,
					halign = "center", 
					textDecoration = "bold", 
					border = "Bottom")
					
	
################################################################################################
## Main capital 
################################################################################################
			
################################################################################################
## All 
################################################################################################
		
		
		

		Files					<- list.files(VINSTUTBET, pattern = 'xlsx', full.names = TRUE)
		TO						<- Sys.Date()
		Merchant				<- 'Nordiskaspel'	# First character as uppercase, match the files in VINSTUTBET
		Regexp					<-  sprintf("InvoiceDone%s.xlsx", Merchant)
		InvociceDone			<- grep(Regexp, Files, value = TRUE, ignore.case = TRUE)
		Name1					<- sprintf("%sVinstUtagg_%s.xlsx", Merchant, TO)
		## Get the invoices
		if(! NROW(InvociceDone) > 0 )
		{
			wbInv			<- openxlsx::createWorkbook( )
			InvociceDone	<- file.path(VINSTUTBET, Regexp)
		} else {
			wbInv			<- openxlsx::loadWorkbook(InvociceDone)
			Sheets			<- sheets(wbInv)
			InvoiceNr		<- data.table()
			## Decide here how many sheet to import
			for( sheetNr in Sheets )
			{	## Get by all sheets
				cat("************************************************\n")
				cat("Sheet nr ", sheetNr, "\n")
				Step1			<- setDT(openxlsx::read.xlsx(xlsxFile = wbInv,  sheet =  sheetNr ))
			
				InvoiceNr		<- rbind(InvoiceNr, Step1)
			}
				InvoiceChar		<- InvoiceNr[,invoiceNumber]
			
	} ## End of else
	
	if( regexpr('slp', Merchant, ignore.case = TRUE) > 0 )
	{
		Merchant	<- "svenskalottoportalen"
	} else if( regexpr('sbg', Merchant, ignore.case = TRUE) > 0 )
	{
		Merchant	<- "(southbaygroup|messenio)"
	} else if( regexpr('gambling', Merchant, ignore.case = TRUE) > 0 )
	{
		Merchant	<- "(comeon|casinoroom)"
	}
	Data 			<- ErpInvoice[grepl(Merchant, operatorId, ignore.case = TRUE)]
	setkey(Data, invoiceNumber)
	wb					<- openxlsx::createWorkbook( )
	
	
	if( exists('InvoiceChar') )
	{
		## Start of those where profit is not taken
		Data2 		<- Data[!J(InvoiceChar) ]
		
	} else {
		Data2	<- Data
	}	
	
#117160.3 + 10234.1

		

	account 	<- parse(text = 'grepl("576-8353", toAccount)')
	
	byCurrency	<- parse(text = "state == 'PAID' & is.na(closedReason)  & invoiceCurrency == 'SEK'")
	byAll		<- parse(text = "state == 'PAID' & is.na(closedReason)")
	## Paid invoice, based on PaymentDate to
	PaidInvoice 		<- Data2[eval(byCurrency)][Payment_date <= '2017-06-28']
	PaidInvoice[is.na(Payment_date), Payment_date := moneyDate ]
	
	

	
	PaidInvoice[, Payment_month := NULL]
	PaidInvoice[, Payment_month := format(Payment_date, "%Y-%m")]
	
	PaidInvoice_1 		<- PaidInvoice[, eval(parse(text = Summary)),by = .(Payment_month, invoiceCurrency, siteId, toAccount)][order(Payment_month)]
	
	PaidInvoice_1[, ProfitTransfer := SUM(invoiceFee) + SUM(TransactionFee) + SUM(interest) + SUM(reminderFee) , by = .(siteId, invoiceCurrency, Payment_month)]
	PaidInvoice_1[, SUM(ProfitTransfer), by = .(invoiceCurrency)]

	
	Currency		<- PaidInvoice_1[, unique(invoiceCurrency)]
	Count			<- 1


	name 		<- 'ProfitExtractionPaid'

	addWorksheet( wb, name)
			
	setColWidths(wb, sheet = name, cols = 1:NCOL(PaidInvoice_1), widths = 25)	

	
	for( curr in Currency)
	{
		Step1		<- PaidInvoice_1[invoiceCurrency == curr]
		Step1[is.na(Payment_month), Payment_month := 'NoDates']	
		Step1 		<- rbind(Step1, Step1[, lapply(.SD, tmpSum)]	)
		Step1[is.na(Payment_month), Payment_month := 'Total']	
		
		writeData(wb, sheet = name,  x = Step1, startRow = Count, startCol = 1,
								headerStyle = hs1)
								
		Count		<- Count + NROW(Step1) + 3				 
						 	
	}		
				



		## Save invoiceNr to file
		
		tmpInvNr	<- rbind(PaidInvoice[, .(invoiceNumber)])
								
			
								
		sheetName 	<- sprintf("invoiceNr_%s", TO)
		addWorksheet( wbInv, sheetName)
		setColWidths(wbInv, sheet = sheetName, cols = 1, widths = 22)
		writeData(wbInv, sheet = sheetName, x = tmpInvNr, startRow = 1, headerStyle = hs1)
		## Save to invoiceNr collection 
		openxlsx::saveWorkbook(wbInv, file = InvociceDone, overwrite = TRUE)
		
		openxlsx::saveWorkbook(wb, file = file.path(VINSTUTBET, Name1), overwrite = TRUE)					
					
					
					
					
##########################################################################################
# Campus|Novus
##########################################################################################
		Files					<- list.files(VINSTUTBET, pattern = 'xlsx', full.names = TRUE)
		TO						<- Sys.Date()
		Merchant				<- 'campus'	# First character as uppercase, match the files in VINSTUTBET
		Regexp					<-  sprintf("InvoiceDone%s.xlsx", Merchant)
		InvociceDone			<- grep(Regexp, Files, value = TRUE, ignore.case = TRUE)
		Name1					<- sprintf("%sVinstUtagg_%s.xlsx", Merchant, TO)
		## Get the invoices
		if(! NROW(InvociceDone) > 0 )
		{
			wbInv			<- openxlsx::createWorkbook( )
			InvociceDone	<- file.path(VINSTUTBET, Regexp)
		} else {
			wbInv			<- openxlsx::loadWorkbook(InvociceDone)
			Sheets			<- sheets(wbInv)
			InvoiceNr		<- data.table()
			## Decide here how many sheet to import
			for( sheetNr in Sheets )
			{	## Get by all sheets
				cat("************************************************\n")
				cat("Sheet nr ", sheetNr, "\n")
				Step1			<- setDT(openxlsx::read.xlsx(xlsxFile = wbInv,  sheet =  sheetNr ))
			
				InvoiceNr		<- rbind(InvoiceNr, Step1)
			}
				InvoiceChar		<- InvoiceNr[,invoiceNumber]
			
	} ## End of else

	
	Data 			<- ErpInvoice[grepl(Merchant, SiteId, ignore.case = TRUE)]
	setkey(Data, invoiceNumber)
	wb					<- openxlsx::createWorkbook( )
	
	
	if( exists('InvoiceChar') )
	{
		## Start of those where profit is not taken
		Data2 		<- Data[!J(InvoiceChar) ]
		
	} else {
		Data2	<- Data
	}	
	
	FROM			<- '2015-10-01'
	idx 			<- Data2[!is.na(moneyDate), invoiceNumber ]
	setkey(ErpTransaction, invoiceNumber )
	Account			<- quote("toAccount %like% '(SE2860000000000050509489|FI7913513000139383)'")
	tmpData			<- Data2[toAccount %like% '(SE2860000000000050509489|FI7913513000139383)',]
	tmpData1		<- tmpData[ moneyDate <= FROM, .(MoneyIn = SUM(money)) , by = .(moneyDate  = format(moneyDate, "%Y-%m"), invoiceCurrency, toAccount)][order(moneyDate)]


	setValue(name = 'PaidInvoiceData', colum = 1:NCOL(tmpData1), data = tmpData1)

	



		## Save invoiceNr to file
		
		tmpInvNr	<- rbind(tmpData[, .(invoiceNumber)])
								
			
								
		sheetName 	<- sprintf("invoiceNr_%s", Sys.Date( ) )
		addWorksheet( wbInv, sheetName)
		setColWidths(wbInv, sheet = sheetName, cols = 1, widths = 22)
		writeData(wbInv, sheet = sheetName, x = tmpInvNr, startRow = 1, headerStyle = hs1)
		## Save to invoiceNr collection 
		openxlsx::saveWorkbook(wbInv, file = InvociceDone, overwrite = TRUE)
		
		
		openxlsx::saveWorkbook(wb, file = file.path(VINSTUTBET, Name1), overwrite = TRUE)					
					

	

################################################################################################
## Lexbase
################################################################################################

		TO						<- '2016-04-19'
	

		InvoiceDoneLex			<- file.path(VINSTUTBET, "InvoiceDoneLexbase.xlsx")
		if(! file.exists(InvoiceDoneLex) )
		{
			wbInv			<- openxlsx::createWorkbook( )
		} else {
			wbInv			<- openxlsx::loadWorkbook(InvoiceDoneLex)
			Sheets			<- sheets(wbInv)
			InvoiceNr		<- data.table()
			## Decide here how many sheet to import
			for( sheetNr in Sheets )
			{	## Get by all sheets
				cat("************************************************\n")
				cat("Sheet nr ", sheetNr, "\n")
				Step1			<- setDT(openxlsx::read.xlsx(xlsxFile = wbInv,  sheet =  sheetNr ))
			
				InvoiceNr		<- rbind(InvoiceNr, Step1)
			}
				InvoiceChar		<- InvoiceNr[,invoiceNumber]
			
	} ## End of else
	
	ErpInvoice[J(InvoiceChar)][grep("lexbase.se", SiteId, value = FALSE, invert = TRUE)]
	
	LexBase 		<- ErpInvoice[grepl('lexbase', operatorId)]
	setkey(LexBase, invoiceNumber)
	

	LexBase2 					<- LexBase[J(InvoiceChar) ]
	LexBase2 $Bill_month		<- factor(LexBase2$Bill_month, levels = MonthsLow)
	LexBase2 $Payment_month	<- factor(LexBase2$Payment_month, levels = MonthsLow)
	## Analyze taken profits 
		InvoiceProfileTaken	<- setDT(	
							dcast( 
								data.table( LexBase2[, ftable(Bill_month ~ status)])[Freq > 0], Bill_month ~ status, margins = TRUE, 
								SUM)
							)
	

		EarlProfit		<- LexBase2[, eval(parse(text = Summary)),by = .(Payment_month)][order(Payment_month)]
		EarlProfit[is.na(Payment_month), Payment_month := 'NoDates']		
		EarlProfit 		<- rbind(EarlProfit , EarlProfit[, lapply(.SD, tmpSum)]	)		
		EarlProfit[is.na(Payment_month), Payment_month := 'Total']			
	
		Name1				<- paste0('LexbasevinstUttag_', '_' , TO , '.xlsx')
		wb					<- openxlsx::createWorkbook( )
		## Insert to excel 
		name 		<- 'EarlierProfitExtraction'
		df		<- data.frame(	Start		= LexBase2[, min(Bill_date)],
								End			= LexBase2[, max(Bill_date)],
								NoDate	 	= "Invoice hasn't a payment date but consider paid",
								OverPaid	= 'Person has paid to mush',
								UnderPaid	= "Person hasn't paid total amount"	)
		
		df1		<- data.frame( What = colnames(df), Explanation = t(df))
		addWorksheet( wb, name)
			
		setColWidths(wb, sheet = name, cols = 1, widths = 25)
		setColWidths(wb, sheet = name, cols = 2, widths = 45)
		writeData(wb, sheet = name,  x = df1, startRow = 1, startCol = 1,	borders = "surrounding", 
								headerStyle = hs1)
		setColWidths(wb, sheet = name, cols = 3:NCOL(EarlProfit), widths = 25)	
		writeDataTable(wb, sheet = name,  x = EarlProfit  ,  startRow =  NROW(df1) + 4 , 
						 tableStyle = 'TableStyleMedium21',	headerStyle = hs2) 
				
		writeData(wb, sheet = name,  x = InvoiceProfileTaken, startRow = NROW(df1) + 6 + NROW(EarlProfit), startCol = 1,	borders = "surrounding", 
								headerStyle = hs1)

		rm(EarlProfit, InvoiceProfileTaken, Nordiska_2 )
		gc(reset = TRUE)
	
	## Start of those where profit is not taken


	LexBase3 		<- LexBase[!J(InvoiceChar) ][Bill_date <= TO]


	

	
	## Set the current invoice profile
	InvoiceProfile	<- setDT(	
						dcast( 
							data.table( LexBase3[, ftable(Bill_month ~ status)])[Freq > 0], Bill_month ~ status, margins = TRUE, 
							SUM)
							)
	InvoiceProfile$Bill_month		<- factor(InvoiceProfile$Bill_month, levels = MonthsLow)
	InvoiceProfile 					<- InvoiceProfile[order(Bill_month)]
	
	

	## Paid invoice, based on PaymentDate to
	PaidInvoice 		<- LexBase3[status == 'PAID' & state == 'PAID' & is.na(closedReason) ]
	PaidInvoice[is.na(Payment_date), Payment_date := moneyDate ]

	PaidInvoice			<- PaidInvoice[Payment_date <= TO]

	
	PaidInvoice[, Payment_month := NULL]
	PaidInvoice[, Payment_month := format(Payment_date, "%Y-%m")]
	


	PaidInvoice_1 		<- PaidInvoice[, eval(parse(text = Summary)),by = .(Payment_month)][order(Payment_month)]
	
	PaidInvoice_1[, ProfitTransfer := SUM(invoiceFee) + SUM(TransactionFee) + SUM(interest) + SUM(reminderFee) - SUM(compensation), by = .(Payment_month)]
	
	PaidInvoice_1 		<- rbind(PaidInvoice_1 ,PaidInvoice_1[,lapply(.SD, tmpSum)])
	PaidInvoice_1[is.na(Payment_month), Payment_month := 'Total']
	setValue(name = 'PaidInvoice', colum = 1:NCOL(PaidInvoice_1), data = PaidInvoice_1)
	setValue(name = 'PaidInvoiceData', colum = 1:NCOL(PaidInvoice), data = PaidInvoice)

				

		PartlyInvoice			<- LexBase[status == 'PAID' & state == 'PARTLY_PAID'  & is.na(closedReason), ]
		QuanitlePartly			<- PartlyInvoice[, quantile(invoiceRemainingAmount, probs = seq(0,1, 0.1))]
		## Create tmp PaymentDate
		PartlyInvoice[is.na(Payment_date), Payment_date := moneyDate]
		PartlyInvoice 		<- PartlyInvoice[Payment_date <= TO, ]
		PartlyInvoice[, Payment_month := NULL]
		PartlyInvoice[, Payment_month := format(Payment_date, "%Y-%m")]
	#	PartlyInvoice1 		<- PartlyInvoice[Payment_date <= TO, ]

		PartlyInvoice_1 		<- PartlyInvoice[, eval(parse(text = Summary)),by = .(Payment_month)][order(Payment_month)]
		PartlyInvoice_1[, ProfitTransfer := SUM(invoiceFee) + SUM(TransactionFee) + SUM(interest) + SUM(reminderFee) - SUM(UnderPaid), by = .(Payment_month)]

		PartlyInvoice_1 		<- rbind(PartlyInvoice_1 , PartlyInvoice_1[,lapply(.SD, tmpSum)])

		setValue(name = 'PartlyPaidInvoice', colum = 1:NCOL(PartlyInvoice_1), data = PartlyInvoice_1)
		setValue(name = 'PartlyPaidInvoiceData', colum = 1:NCOL(PartlyInvoice), data = PartlyInvoice)

		## Save invoiceNr to file
		
		tmpInvNr	<- rbind(PaidInvoice[, .(invoiceNumber)],	PartlyInvoice[, .(invoiceNumber)])
								
		sheetName 	<- sprintf("invoiceNr_%s", TO)
		addWorksheet( wbInv, sheetName)
		setColWidths(wbInv, sheet = sheetName, cols = 1, widths = 22)
		writeData(wbInv, sheet = sheetName, x = tmpInvNr, startRow = 1, headerStyle = hs1)
		## Save to invoiceNr collection 
		openxlsx::saveWorkbook(wbInv, file = InvoiceDoneLex, overwrite = TRUE)
		
		
		openxlsx::saveWorkbook(wb, file = file.path(VINSTUTBET, Name1), overwrite = TRUE)

################################################################################################
## Utbetalningar för comeone och casinoroom
################################################################################################		

		if( FALSE) 
		{
		
			File			<-	list.files(path = VINSTUTBET, pattern = 'överföring.*xlsx', full.names = TRUE)
			initialDT		<- setDT(read.xlsx(xlsxFile = File, detectDates = TRUE, startRow = 7))	
			initialDT1		<- initialDT[!is.na(Creditor.number), .(Invoice.number = as.character(Invoice.number), Collection.state, Capital.paid.on, 
																	Capital, Invoicing.fee, Reminder.fee, Interest)]
		
			initialDT1 		<- merge(ErpInvoice, initialDT1, by.x = 'invoiceNumber', by.y = 'Invoice.number', all.y = TRUE)															

			intialInvoiceNr		<- 	initialDT1[, invoiceNumber]
			initialDT1[, tmpinvoiceRemainingAmount :=  totalAmount - money]
		}	
			
		## Here is where all the invoiceNr is saved
		InvoiceDoneGambling			<- file.path(VINSTUTBET, "InvoiceDoneGambling.xlsx")
		if(! file.exists(InvoiceDoneGambling) )
		{
			wbInv			<- openxlsx::createWorkbook( )
		} else {
			wbInv			<- openxlsx::loadWorkbook(InvoiceDoneGambling)
			Sheets			<- sheets(wbInv)
			InvoiceNr		<- data.table()
			## Decide here how many sheet to import
			for( sheetNr in Sheets )
			{	## Get by all sheets
				cat("************************************************\n")
				cat("Sheet nr ", sheetNr, "\n")
				Step1			<- setDT(openxlsx::read.xlsx(xlsxFile = wbInv,  sheet =  sheetNr ))
			
				InvoiceNr		<- rbind(InvoiceNr, Step1)
			}
				InvoiceChar		<- InvoiceNr[,invoiceNumber]
			
		} ## End of else
		
		
	
		TO				<-  Sys.Date( )
		setkey(ErpInvoice, invoiceNumber)
		
		OldProfit		<- ErpInvoice[J(InvoiceChar)]
		
		OldProfit_1 	<- OldProfit[!is.na(siteId), eval(parse(text = Summary)), by = .(SiteId)]
		
		NewProfit		<- ErpInvoice[!J(InvoiceChar)][grepl("casinoroom|comeon", siteId)]
		
		
		
		initialDT1 		<- NewProfit[grepl('PAID', status)]
		initialDT1[is.na(Payment_date) & !is.na(moneyDate), Payment_date := moneyDate]
		
		initialDT1[, Payment_month := NULL]
		initialDT1[, Payment_month := format(Payment_date, "%Y-%m")]
		initialDT1[is.na(invoiceRemainingAmount), invoiceRemainingAmount := 0]
		initialDT1 		<- initialDT1[invoiceCurrency == 'SEK',]
		initialDT2		<-  initialDT1[!is.na(siteId) , eval(parse(text = Summary)),by = .(Payment_month,siteId,invoiceCurrency)][order(Payment_month)]	
				
		initialDT2[, ProfitTransfer := SUM(invoiceFee) + SUM(TransactionFee) + SUM(interest) + SUM(reminderFee) - SUM(UnderPaid) - SUM(Compensation), by = .(Payment_month, siteId, invoiceCurrency)]
		
		## Name of the report 
		Name1				<- paste0('GamblingVinstUttag_',   TO , '.xlsx')
		wb					<- openxlsx::createWorkbook( )										
		uniqSite			<- initialDT2[, unique(siteId)]
		setkey(initialDT2, siteId)
		
		name		<- 'EarlierProfitExtraction'
	
		
		setValue(name = name, colum = c(1:NCOL(OldProfit_1)), 
					data = OldProfit_1 )
		
		name		<- 'ProfitTransfer_SEK'
		
		addWorksheet( wb, name)	
		setColWidths(wb, sheet = name, cols = 1:NCOL(initialDT2), widths = 25)	
		
		initialDT2_1		<- initialDT2[invoiceCurrency == 'SEK']
		setkey(initialDT2_1, siteId)
		uniqSite 	<- initialDT2_1[, unique(siteId)]
		Count 		<- 1
		for( site in uniqSite)
		{
			Step		<- initialDT2_1[J(site)]			
			Step		<- Step[order(Payment_month)]
			Step		<- rbind(Step, Step[, lapply(.SD, tmpSum)])
			Step[is.na(Payment_month) , Payment_month := 'Total']
			writeData(wb, sheet = name, x =  Step, startRow = Count, headerStyle = hs1)	
	
			Count 		<- Count + NROW(Step) + 3
		
		}
		
		if( any(initialDT2[, invoiceCurrency] == 'EUR'))
		{
				name		<- 'ProfitTransfer_EUR'
		
				addWorksheet( wb, name)	
				setColWidths(wb, sheet = name, cols = 1:NCOL(initialDT2), widths = 25)	
		
				initialDT2_1		<- initialDT2[invoiceCurrency == 'EUR']
				setkey(initialDT2_1, siteId)
				uniqSite 	<- initialDT2_1[, unique(siteId)]
				Count 		<- 1
				for( site in uniqSite)
				{
					Step		<- initialDT2_1[J(site)]			
					Step		<- Step[order(Payment_month)]
					Step		<- rbind(Step, Step[, lapply(.SD, tmpSum)])
					Step[is.na(Payment_month) , Payment_month := 'Total']
					writeData(wb, sheet = name, x =  Step, startRow = Count, headerStyle = hs1)	
	
					Count 		<- Count + NROW(Step) + 3
		
				}
			
		}
		
		
		
	

	
			openxlsx::saveWorkbook(wb, file = file.path(VINSTUTBET, Name1), overwrite = TRUE)
		
			
			invoiceChar		<- initialDT1[, .(invoiceNumber)]
			name <- sprintf("InvoiceDone_%s", TO)	
			addWorksheet( wbInv, name)
			setColWidths(wbInv, sheet = name, cols = 1, widths = 22)
			
			writeData(wbInv, sheet = name, x =  invoiceChar, startRow = 1, headerStyle = hs1)		
		
			openxlsx::saveWorkbook(wbInv, file = InvoiceDoneGambling , overwrite = TRUE)	
										
			rm(initialDT1, initialDT2_1)								
										
												
##########################################################################################
# SvenskaLottoPortalen
##########################################################################################										
		TO					<- Sys.Date()
		InvoiceDonefile 	<- list.files(path = VINSTUTBET, pattern = 'InvoiceDoneSLP', full.names = TRUE, ignore.case = TRUE)
		if(  length(InvoiceDonefile) == 0 )
		{
			wbInv			<- openxlsx::createWorkbook( )
		} else {
			wbInv			<- openxlsx::loadWorkbook(InvoiceDonefile)
			Sheets			<- sheets(wbInv)
			InvoiceNr		<- data.table()
			## Decide here how many sheet to import
			for( sheetNr in Sheets )
			{	## Get by all sheets
				cat("************************************************\n")
				cat("Sheet nr ", sheetNr, "\n")
				Step1			<- setDT(openxlsx::read.xlsx(xlsxFile = wbInv,  sheet =  sheetNr ))
			
				InvoiceNr		<- rbind(InvoiceNr, Step1)
			}
				InvoiceChar		<- InvoiceNr[,invoiceNumber]
			
		} ## End of else		
		
		
		
		DataSet			<- ErpInvoice[grepl('svenskalotto', siteId)]
		
		PaidSLP			<- DataSet[status == 'PAID' & state == 'PAID']
		PaidSLP[, Payment_month := NULL ]
		PaidSLP[, Payment_month := format(Payment_date, "%Y-%m")]
		PaidSLP_1		<-  PaidSLP[, eval(parse(text = Summary)), by = .(Payment_month)][order(Payment_month)]	
		PaidSLP_1[, ProfitTransfer := SUM(invoiceFee) + SUM(TransactionFee) + SUM(interest) + SUM(reminderFee) - SUM(UnderPaid) - SUM(compensation), by = .(Payment_month)]

		PaidSLP_1 	<- rbind(PaidSLP_1 , PaidSLP_1[, lapply(.SD, tmpSum)])
		
		
		Name1				<- paste0('SLPVinstUttag_', '_' , TO , '.xlsx')
		wb					<- openxlsx::createWorkbook( )
		setValue(name = 'ProfitExtraction', colum = 1:NCOL(PaidSLP_1), data = PaidSLP_1)		
		setValue(name = 'Data', colum = 1:NCOL(PaidSLP), data = PaidSLP)	
				
		openxlsx::saveWorkbook(wb, file = file.path(VINSTUTBET, Name1), overwrite = TRUE)
				
				
		invoiceChar		<- PaidSLP[,.(invoiceNumber)]	
															
		## Save all invoiceNr
		name <- paste0('InvoiceDone_', TO)	
			addWorksheet( wbInv, name)
			setColWidths(wbInv, sheet = name, cols = 1, widths = 22)
			writeData(wbInv, sheet = name, x = invoiceChar , startRow = 1, headerStyle = hs1)		
		openxlsx::saveWorkbook(wbInv, file = file.path(VINSTUTBET, "InvoiceDoneSLP.xlsx") , overwrite = TRUE)									
										
							
		
	
##########################################################################################
# Search all invoice
##########################################################################################			
InvoiceNr		<- data.table()
All				<- list.files(path = VINSTUTBET, pattern = 'Done', full.names = TRUE)			
for( z in All)
{			
	cat(z, "********", "\n")
			wbInv			<- openxlsx::loadWorkbook(z)
			Sheets			<- sheets(wbInv)
			## Decide here how many sheet to import
			for( sheetNr in Sheets )
			{	## Get by all sheets
				cat("************************************************\n")
				cat("Sheet nr ", sheetNr, "\n")
				Step1			<- setDT(openxlsx::read.xlsx(xlsxFile = wbInv,  sheet =  sheetNr ))
			
				InvoiceNr		<- rbind(InvoiceNr, Step1)
			}

}
idx		<-  InvoiceNr[, invoiceNumber]
setkey(ErpInvoice, invoiceNumber)
DataSet		<- ErpInvoice[!J(idx),][state == 'PAID',  eval(parse(text = Summary)),  by = .(operatorId, toAccount)]
DataSet[,  profit := SUM(invoiceFee) + SUM(TransactionFee) + SUM(interest) + SUM(reminderFee) - SUM(compensation), by = .(operatorId, toAccount) ]
DataSet1		<- DataSet[ , .(SUM(invoiceFee) + SUM(TransactionFee) + SUM(interest) + SUM(reminderFee) - SUM(compensation) - SUM(UnderPaid)) , by = .(operatorId, toAccount)][order(V1)]
for( i in names(Account))
{
	DataSet1[grepl(i, toAccount), ':=' (valuta =	Account[[i]][['valuta']] , konto = Account[[i]][['konto']], typ = Account[[i]][['typ']] )]
}



ErpInvoice[!J(idx)][operatorId == 'nordiskapspel']

Date			<- '2016-07-01'
ErpTransaction[settlementAt ==  Date & status == 'PAID', SUM(settlementAmount), by = .( currency, operatorId)]


WhenPaid	<- dcast.data.table( ErpTransaction[settlementAt == Date &settlementRuleString  != 'AfterSettlementPeriod' & status == 'PAID', SUM(settlementAmount), by = .(siteId, currency, toAccount)], siteId + toAccount ~ currency) 
AfterPerid <- dcast.data.table(ErpTransaction[settlementAt == Date & settlementRuleString  == 'AfterSettlementPeriod' , SUM(settlementAmount), by = .(siteId, currency, toAccount)],  siteId + toAccount ~ currency) 
	Name1		<- paste0('Settlment.xlsx')
	wb			<- openxlsx::createWorkbook( )
	setValue(name = "PaidInvoice", colum = c(1:NCOL(WhenPaid)), 
						data = WhenPaid)		
	setValue(name = "AfterSettlementPeriod", colum = c(1:NCOL(AfterPerid)), 
						data = AfterPerid)
		
			
	openxlsx::saveWorkbook(wb, file = file.path(VINSTUTBET, Name1), overwrite = TRUE)

		Name1				<- paste0('PotentialVinst.xlsx')
		wb					<- openxlsx::createWorkbook( )
		setValue(name = 'Vinst', colum = 1:NCOL(DataSet), data = DataSet[order(profit)])		
	
		openxlsx::saveWorkbook(wb, file = file.path(VINSTUTBET, Name1) , overwrite = TRUE)	
		
		
##########################################################################################
# Aptic profit: NOT here we take money from old comeon and casionRoom
# due to that transactionFee wasn't included 
##########################################################################################	

File			<- grep('InvoiceDoneGambling.xlsx', All, value = TRUE)
Fees			<- list("casinoroom.com" = 2,
						"comeon.se"	= 2,
						"vinnarrum.se"	= 2,
						"bertil.se" = 2
)

			Gamling			<- data.table( )
			wbInv			<- openxlsx::loadWorkbook(File)
			Sheets			<- sheets(wbInv)
			## Decide here how many sheet to import
			for( sheetNr in Sheets )
			{	## Get by all sheets
				cat("************************************************\n")
				cat("Sheet nr ", sheetNr, "\n")
				Step1			<- setDT(openxlsx::read.xlsx(xlsxFile = wbInv,  sheet =  sheetNr ))
			
				Gamling		<- rbind(Gamling, Step1)
			}
			
setkey(ErpInvoice, invoiceNumber)			
Data_1			<- ErpInvoice[J(Gamling[, invoiceNumber])][is.na(TransactionFee), ]		
Data_1[, TransactionFee := Invoiced_deposit_amount*0.02]			
Agg			<- Data_1[, .(TransactionFee = SUM(TransactionFee)), by = .(siteId, Currency)]

		
		Name1 	<- 	paste0('TransactionFeeComeOnCasinoRoom.xlsx')
		wb			<- openxlsx::createWorkbook( )
		name 		<- 'Aggregate_Date'
		df		<- data.frame(	Start			= Data_1[, min(Bill_date)],
								End				= Data_1[, max(Bill_date)],
								FeePercentage	= '2% from deposite amount (check from Aptic.xlsx',
								Not 			= 'Earlier profit extraction where invociceFee is equal to zero')
		
		df1		<- data.frame( What = colnames(df), Explanation = t(df))
		addWorksheet( wb, name)
			
		setColWidths(wb, sheet = name, cols = 1, widths = 25)
		setColWidths(wb, sheet = name, cols = 2, widths = 45)
		writeData(wb, sheet = name,  x = df1, startRow = 1, startCol = 1,	borders = "surrounding", 
								headerStyle = hs1)
								
								
		writeData(wb, sheet = name,  x = Agg, startRow = NROW(df1) + 4, startCol = 1,	borders = "surrounding", 
								headerStyle = hs1)							
								
		setColWidths(wb, sheet = name, cols = 3:NCOL(Data_1), widths = 25)	
		
		writeDataTable(wb, sheet = name,  x = Data_1[,.(invoiceNumber, status , Invoiced_deposit_amount, TransactionFee)]  ,  startRow =  NROW(df1) + 4 , startCol = NCOL(Agg) + 3,
						 tableStyle = 'TableStyleMedium21',	headerStyle = hs2) 
	
		openxlsx::saveWorkbook(wb, file = file.path(VINSTUTBET, Name1) , overwrite = TRUE)	


########################################################################################
# Vinnarum and bertil
########################################################################################
	
		TO					<- Sys.Date()
		InvoiceDonefile 	<- list.files(path = VINSTUTBET, pattern = 'InvoiceDoneAptic', full.names = TRUE, ignore.case = TRUE)
		fileName			<- file.path(VINSTUTBET,'InvoiceDoneAptic.xlsx')
		if( length(InvoiceDonefile) == 0 )
		{
			wbInv			<- openxlsx::createWorkbook( )
		} else {
			wbInv			<- openxlsx::loadWorkbook(InvoiceDonefile)
			Sheets			<- sheets(wbInv)
			InvoiceNr		<- data.table()
			## Decide here how many sheet to import
			for( sheetNr in Sheets )
			{	## Get by all sheets
				cat("************************************************\n")
				cat("Sheet nr ", sheetNr, "\n")
				Step1			<- setDT(openxlsx::read.xlsx(xlsxFile = wbInv,  sheet =  sheetNr ))
			
				InvoiceNr		<- rbind(InvoiceNr, Step1)
			}
				InvoiceChar		<- InvoiceNr[,invoiceNumber]
			
		} ## End of else
		
		
		
		sheetName			<- paste0('invoiceNr_', TO)
		Name1				<- paste0('ApticVinstUttag_', '_' , TO , '.xlsx')
		
		## Common formula 
		Summary			<- 		quote("list(Antal 	= .N ,
									invoiceFee 		= SUM(invoiceFee),
									TransactionFee 	= SUM(TransactionFee),
									interest 		= SUM(interest),
									reminderFee 	= SUM(reminderFee),
									CreditIn		= SUM(money),
									Compensation	= SUM(compensation),
									OverPaid		= SUM(ifelse(invoiceRemainingAmount < 0, invoiceRemainingAmount*-1, 0)),
									UnderPaid		= SUM(ifelse(invoiceRemainingAmount > 0, invoiceRemainingAmount, 0))
				)")
				
		Summary 		<- gsub("\\t|\\n", "", Summary)

		Bonnier 		<- ErpInvoice[grepl('bonnier', operatorId)]
		Bonnier[grepl("PAID", status) & is.na(TransactionFee), TransactionFee := Invoiced_deposit_amount*0.02]
		Bonnier[, Bill_month := NULL]
		Bonnier[, Bill_month := format(Bill_date, "%Y-%m")]
		Bonnier1 		<- Bonnier[grepl("PAID", status)]
		Bonnier2		<- Bonnier1[grepl("PAID", status), eval(parse(text = Summary)), by = .(Bill_month, Currency)][order(Bill_month)]
		

		Bonnier2[, ProfitTransfer := SUM(invoiceFee) + SUM(TransactionFee) + SUM(interest) + SUM(reminderFee) - SUM(UnderPaid) - SUM(Compensation), by = .(Bill_month, Currency)]

		Bonnier2 	<- rbind(Bonnier2, Bonnier2[, lapply(.SD, tmpSum)])
		
			invoiceChar 		<- Bonnier1[, .(invoiceNumber) ]
			name <- sprintf("InvoiceDone_%s", TO)	
			addWorksheet( wbInv, name)
			setColWidths(wbInv, sheet = name, cols = 1, widths = 22)
			
			writeData(wbInv, sheet = name, x =  invoiceChar, startRow = 1, headerStyle = hs1)		
		
			openxlsx::saveWorkbook(wbInv, file = fileName , overwrite = TRUE)	


		Name1				<- paste0('VinnarRumBertilVinstUttag_', '_' , TO , '.xlsx')
		wb					<- openxlsx::createWorkbook( )
		name 		<- 'ProfitExtraction'
		df		<- data.frame(	Start		= Bonnier1[, min(Bill_date)],
								End			= Bonnier1[, max(Bill_date)],
								OverPaid	= 'Person has paid to mush',
								UnderPaid	= "Person hasn't paid total amount",
								Not			= 'When TransactionFee is NULL then we have Invoiced_deposit_amount*0.02')
		
		df1		<- data.frame( What = colnames(df), Explanation = t(df))
		addWorksheet( wb, name)
			
		setColWidths(wb, sheet = name, cols = 1, widths = 25)
		setColWidths(wb, sheet = name, cols = 2, widths = 45)
		writeData(wb, sheet = name,  x = df1, startRow = 1, startCol = 1,	borders = "surrounding", 
								headerStyle = hs1)
								
		setColWidths(wb, sheet = name, cols = 3:NCOL(Bonnier2), widths = 25)	
		writeDataTable(wb, sheet = name,  x = Bonnier2  ,  startRow =  NROW(df1) + 4 , 
						 tableStyle = 'TableStyleMedium21',	headerStyle = hs2) 
			
		setValue(name = 'PaidInvoice', colum = 1:NCOL(Bonnier1), data = Bonnier1)		
	
		openxlsx::saveWorkbook(wb, file = file.path(VINSTUTBET, Name1) , overwrite = TRUE)	

########################################################################################
# Campus|Novus
########################################################################################
Data1		<- Payments[grepl("(31311001520968)", AccountId) & TransaktionType == 'C', ocrNumber ]

## All the invoice forom Campus
idx					<- ErpInvoice[grepl("novus", siteId), invoiceNumber] 

setkey(ErpPaymentHistory, invoiceNumber)
MoneyData			<- ErpPaymentHistory[J(idx)][type == 'r', .(Amount = SUM(amount)), by = .(invoiceNumber)]
## Just to check the validate of Money column
DataSet				<- merge(ErpInvoice, MoneyData, by = 'invoiceNumber', all.y = TRUE)
DataSet[, PaidDate := format(moneyDate, "%Y-%m")]
DataSet_1			<- DataSet[money > 0,  ]
DataSet_1[, .(MoneyIn = SUM(money)), by = .(PaidDate)]
DataSet_2			<-  DataSet_1[, .(MoneyIn = SUM(money)), by = .(PaidDate)][order(PaidDate)]
DataSet_2[, Agg := cumsum(MoneyIn)]

ErpInvoice[grepl("campus", siteId), SUM(money), by = .(toAccount)]
Payments[grepl("(31311001520968)", AccountId) & TransaktionType == 'C' & ocrNumber == '6214']
ErpInvoice[ocrNumberReminder == '621402210']
ErpInvoice[ocrNumber == '12396100470']

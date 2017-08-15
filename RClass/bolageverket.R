#!/usr/bin/env Rscript 
library(curl)
library(XML)
library(openxlsx)
library(data.table)
library(RMySQL)
options(scipen = 999)
options("openxlsx.dateFormat" = "yyyy-mm-dd")
if(regexpr( "Linux", Sys.info()['sysname'] ) > 0 )
{
	setwd("/home/serdara/RJobs/")
} else {
	setwd(getwd())	
}
### XLSX Options
hs1		<- createStyle(fgFill = "#DCE6F1", halign = "center", textDecoration = "Italic", border = "Bottom")
Sys.setenv("R_ZIPCMD" = "/usr/bin/zip") ## makes openxlsx works in centos
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

negStyle	<- createStyle(fontColour = '#9C0006', bgFill = "#FFC6CE")
posStyle	<- createStyle(fontColour = '#006100', bgFill = "#C6EFCE")	

getData		<- function( x, query = NULL, dbname = "kriita_db1" )
{
	con			<- try( dbConnect(MySQL(),	user = 'serdara', 	password = 'aiiR1eX5d', 
 							host = '193.182.121.238', dbname = dbname, port = 3306),
 							silent = TRUE)
 	if( ! inherits(con, "try-error") )
	{						
		if(is.null(query))
		{
			Query	<- sprintf("SELECT * FROM %s", x)
		} else {
			Query <- query
		}	
		res		<- dbSendQuery(con, statement = Query)
		data1	<- data.table( dbFetch(res, n = -1) )
	}	
	dbClearResult(res)
	dbDisconnect(con)
	return( data1 )
}



query							<- "SELECT * FROM Person"
Person							<- getData(query = query)
setkey(Person, ssn)

Query 	<- sprintf("SELECT 	ei.*,
							iv.state,
							IF( ei.playingCurrency = 'EUR' 
									AND ei.invoiceCurrency = 'SEK', 
										ei.originalSettlementFee*ei.conversionRate, 
											ei.originalSettlementFee) AS TransactionFee
							FROM ErpInvoice ei 
						INNER JOIN Invoice iv ON (ei.invoiceNumber = iv.invoiceNumber)
						WHERE createdAt >= '%s'", '2014-01-01')
			
ErpInvoice 				<- getData(query = Query )

h <- new_handle(copypostfields = "moo=moomooo")
handle_setheaders(h,
  "Content-Type" = "text/moo",
  "Cache-Control" = "no-cache",
  "User-Agent" = "A cow"
)


DATA			<- file.path(  getwd(), 'Data')
if(!file.exists(DATA))
{
	dir.create(DATA, recursive = TRUE)
}


x	 			<- 'https://poit.bolagsverket.se/poit/PublikStart.do?method=sokAmnesomrade&id=5'
req 			<- curl_fetch_memory(x, handle = h)
parse_headers(req$headers)

x1				<- sprintf('https://poit.bolagsverket.se/poit/PublikSokKungorelse.do?method=gotoPageTop&gotopageTop=%i', 2:7)
x				<- c(x,x1)
DataSet		<- data.table( )
count		<- 1
for( files in x)	
{
	cat("***********************************\n")
	cat("run url ", files, "\n")
	req <- curl_fetch_memory(files, handle = h)
	parse_headers(req$headers)
	w1		<- htmlTreeParse(rawToChar(req$content), error = function( ... ){ },
								useInternalNodes = TRUE )
	print(w1)
	count	<- count + 1 	
	tb 			<- readHTMLTable(w1)
	Step1		<- data.table(tb[['NULL']])
	DataSet		<- rbind(DataSet ,Step1)
				
}								

setnames(DataSet, 'Personnr/Orgnr', 'ssn')

		Name1		<- file.path(DATA, 'SkuldTotal.xlsx')
	

		if(! NROW(Name1) > 0 )
		{
			wbInv			<- openxlsx::createWorkbook( )
			InvociceDone	<- file.path(VINSTUTBET, Regexp)
		} else {
			wbInv			<- openxlsx::loadWorkbook(Name1)
			Sheets			<- sheets(wbInv)
		
			cat("************************************************\n")
			OldData			<- setDT(openxlsx::read.xlsx(xlsxFile = wbInv,  sheet =  'Data' ))
			
		} ## End of else


	DataSet			<- rbind(OldData,  DataSet)
	allSSN			<- DataSet[,ssn]
	
	
	AtPaylevo		<- Person[J(allSSN)][!is.na(personId),]
	ssnNr			<- AtPaylevo[,ssn]
	## Get the profile form Invoice's table
	
	ErpData 		<- merge(AtPaylevo[, .( personId, firstName, lastName, email, ssn)], ErpInvoice, by = 'personId', all.x = TRUE)

	ErpData 		<- ErpData[order(collectionParty)]

	
	wb				<- openxlsx::createWorkbook( )
	setValue(name = "Data", colum = c(1:NCOL(DataSet)), 
						data = DataSet)		
	setValue(name = "InPayleov", colum = c(1:NCOL(AtPaylevo)), 
						data = AtPaylevo)		
	setValue(name = "ErpInvoice", colum = c(1:NCOL(ErpData)), 
						data = ErpData)		
		## Which are the ones that are on ErpInvoice					
	for( ss in  ssnNr )
	{
		conditionalFormatting(wb, sheet = 'Data', cols = 4, rows= 1:NROW(DataSet) , type = 'contains', rule= ss, style = posStyle)
		conditionalFormatting(wb, sheet = 'ErpInvoice', cols = which(regexpr("collectionParty", names(ErpData)) > 0),
					 rows= 1:NROW(ErpData) , type = 'contains', rule= 'Delta Inkasso', style = posStyle)

	}			
		
	openxlsx::saveWorkbook(wb, file =  Name1, overwrite = TRUE)
								


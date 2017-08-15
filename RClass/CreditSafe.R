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
		cmd1			<-  pipe( cmd , open = "r")
		path			<-   scan(cmd1, what = "character")  
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
# Load common functions and files , DATA dir is where to send all data
source(file = grep("commonfunction", RFiles ,value = TRUE, perl = TRUE, ignore.case = TRUE) )


xmlName					<- file.path(FULLPATH, "XML")
dirName					<- file.path(FULLPATH, "Data")
if(! file.exists(xmlName) )
{ ## If missing dir
	dir.create(xmlName, recursive = TRUE)
}



Packages			 	<- c("lubridate",'reshape2', 'reshape', 'XML', 'openxlsx',
								'ggthemes', 'stargazer', 'RColorBrewer',
								'RCurl', 'XML', 'scales','openxlsx','jsonlite',
								'ggplot2', 'scales', 'grid', 'gridExtra',"Hmisc",
							 	'dplyr')
## Set path and load pkgs
Pack		<- new("startUps", pkgs = Packages, path = dirname(FULLPATH) )
Pack$instant_pkgs()



Pack$setDirs( )

hs2	<- createStyle(	
			fgFill 		= "black", 
			fontSize 	= 12,
			halign 		= "center", 
			textDecoration = "bold", 
			border = "Bottom")
## Get the info about parameters

infoXLS			<- list.files(path = 'InputData', pattern = '.*Parametrar_Person_Credit.xlsx$', full.names = TRUE)
infoDT			<- read.xlsx(infoXLS, sheet = 1, rows = 20:180)
setDT(infoDT)
infoDT 			<- infoDT[!is.na(XMLTAG)]



################################################################################
#  ErpInvoice data from Delta
################################################################################
Query 	<- sprintf("SELECT 	ei.*,
							iv.state,
							IF( ei.playingCurrency = 'EUR' 
									AND ei.invoiceCurrency = 'SEK', 
										ei.originalSettlementFee*ei.conversionRate, 
											ei.originalSettlementFee) AS TransactionFee
							FROM ErpInvoice ei 
						INNER JOIN Invoice iv ON (ei.invoiceNumber = iv.invoiceNumber)
						WHERE ei.personId IS NOT NULL 
						AND ei.status <> 'UNNECESSARY'" )

Step1 				<- getData(query = Query )

Step1[, ':=' (	collectionDate 		=  as.Date(collectionDate),
				collectionDueDate	= as.Date(collectionDueDate)
)]
 
Step1[is.na(collectionDueDate), collectionDueDate := collectionDate + days(10)]
Step1			<- merge(Step1, Meacode[, .(merchantId, Risk, country )], 	
								by = c('merchantId','country'), all.x = TRUE)



Query 				<- sprintf("SELECT eph.*
								FROM ErpPaymentHistory eph")

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
## Have at least one invoice in collection
idx				<- ErpInvoice[grepl("Delta", collectionParty) & grepl('WAITING_TO_BE_COLLECTED', status) , unique(personId)]
setkey(ErpInvoice, personId)

Data_1				<- ErpInvoice[J(idx),]

Step0				<- data.table()
for( i in c(0,1))
{
	Step1				<- dcast.data.table(Data_1[order(createdAt)][grepl('WAITING_TO_BE_COLLECTED', status) & Risk == i], personId + Risk~ rowid(personId), value.var = "merchantId")
	Names				<- colnames(Step1)	
	Names1				<- paste0("no", setdiff(Names, c("Risk","personId")))
	setnames(Step1, 3:NCOL(Step1), Names1)
	Step0				<- rbind(Step0,Step1, fill = TRUE)
}

tmp					<- Data_1[order(collectionDate)][status == 'WAITING_TO_BE_COLLECTED',  .(lastCollectionDate = collectionDate[.N]), by = .(personId)] 

DataSet				<-	Data_1[, .(				Antal 				= .N, 
												noInvoiceKH			= SUM(grepl("Kredithanterarna", collectionParty)),
												noPaid				= SUM(grepl("PAID", status)),
												noDeltaCollection	= SUM(grepl("Delta", collectionParty) & grepl('WAITING_TO_BE_COLLECTED', status)),
												noPaidCollection	= SUM(!is.na(collectionDate) & grepl('PAID', status)),
												debtNotPaid			= SUM(totalAmount - paidAmount)
					), by = .(personId, Risk)]
Order1  			<- names(DataSet)					
DataSet				<- merge(DataSet, Step0, by = c('Risk','personId'), all.x = TRUE)
Order2  			<- names(DataSet)
setdiff(Order1 , Order2)
DataSet				<- merge(DataSet, tmp, by = c('personId'), all.x = TRUE)


		Name1		<- paste0('DeltaData.xlsx')
		wb			<- openxlsx::createWorkbook()	
		setValue(name = "Data", colum = c(1:NCOL(DataSet)), data = DataSet[order(Antal, decreasing = TRUE)])  

		openxlsx::saveWorkbook(wb, file = file.path(dirName, Name1), overwrite = TRUE)
rm(Step2, Step1)
gc(reset = TRUE)



getData

headerFields <- c(Accept = "text/xml",
    			Accept = "multipart/*",
    			'Content-Type' = "text/xml; charset=utf-8",
    			SOAPAction = "https://webservice.creditsafe.se/getdata/GetDataBySecure")

openXL(infoXLS)


#' Function that creates the body that is going to be sent
#'@param id = debtReference or PersonId from Paylevo
#' @param 
requestBody		<- function(id, ssn)
{
	creditUrl			<- "http://testwebservice.creditsafe.se/GetData/getdata.asmx"
	transId 			<- id
	transSsn			<- ssn
	body = sprintf('<?xml version="1.0" encoding="utf-8"?>
 			 	<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  					<soap:Body>
  						<GetDataBySecure xmlns="https://webservice.creditsafe.se/getdata/">
						 	<GetData_Request>
				 				<account>
									<UserName>DELTINKTESTIN</UserName>
          							<Password>p3LJe74F</Password>
          							<TransactionId>%s</TransactionId>
         							<Language>SWE</Language>
       	 						</account>
			 					<Block_Name>PERSONCREDIT</Block_Name>
        						<SearchNumber>%s</SearchNumber>
        						<FormattedOutput>ALLANNA JOHAMMA</FormattedOutput>
      					</GetData_Request>
    				</GetDataBySecure>
  				</soap:Body>
			</soap:Envelope>', transId, transSsn)
	## Clean the output		
	body		<- gsub("[\\\t]", "",body)
	
	out		<- tryCatch({ 
				message("Running ssn ", ssn)
				#h	<- basicTextGatherer( )
				#h$reset()
				filename 	<- file.path( xmlName, paste0( ssn, ".xml"))
				 assign("filename", filename, envir = .GlobalEnv)

				
				if(exists("filename"))
				{
					file.exists(filename) && stop("Filename is already present", filename)
				}
				f			<- CFILE(filename , 'wb' )
				curlPerform(	url 				= creditUrl,
							writedata = f@ref, 
							.encoding = 'UTF-8',
						 	.opts = list( ssl.verifypeer = TRUE, connecttimeout = 30),
							followlocation = TRUE,
							httpheader 		= headerFields,
							postfields 		= body ,	
							maxredirs = 5)
				close( f )	
				#h$value()		
			}, 
			error = function(cond){
				 	message("Ssn does not seem to exist:", ssn)
           		 	message("Here's the original error message:")
            			message(cond)
            			# Choose a return value in case of error
           			return(NA)
	
			},
			warning = function(cond){
				
			}, 
			finally = {
				   	message("Processed ssn:", ssn)
           			

			}
		
		) ## end of trycatch
	return(out)	


}

		personId		<- 14
				
		## Save data to xml file, make's the filename global inside requestBody( )
		requestBody(id = 14, ssn = '191603018597')   
		## read the created xml file and make it to JSON
		dataXML 		<- xmlParse(filename)
		dataXML1  		<- getNodeSet(dataXML, "//GETDATA_RESPONSE")[[1]]
		dataXML2 		<- xmlSApply(dataXML1,xmlValue)
		dataDT 			<- data.table(keys	= names(dataXML2), values = dataXML2)
	dataDT[keys == "", keys := NA]
	dataJSON		<- jsonlite::toJSON(dataDT)

Query				<- sprintf("INSERT INTO kriita_survey.DeltaSafe( `personId`, `checkDate`, `varData` ) 
									VALUES('%s', '%s', '%s')",  personId , Sys.Date(), dataJSON )
Query 				<- gsub("[\t\n\t]", "", Query)

con			<- try( dbConnect(MySQL(),	user = 'serdara', 	password = 'aiiR1eX5d', 
 							host = '193.182.121.238', dbname = "kriita_survey", port = 3306),
 							silent = TRUE)
res			<- dbSendQuery(con, statement = Query)			

dbClearResult(res)	
 
 
 

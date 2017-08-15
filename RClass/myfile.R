# set up the file in /etc/nginx/sites-available by using proxypass
# sudo /etc/init.d/nginx restart
# myfile.R
options(scipen = 999)
options("openxlsx.dateFormat" = "yyyy-mm-dd")
	
#' x the table name
#' USAGE getData( x = "Invoice")
closeConnections 		<- function( )
{
	allCons 	<- 	dbListConnections(MySQL())
	for( cons in allCons) dbDisconnect(cons)
	con	<- setupCon()
 														
 	tmp	<- dbGetQuery(con, "show processlist")
 	setDT(tmp)
 	tmp1			<- tmp[db == 'api_lead' & ! grepl("processlist", Info)]
 	if( NROW(tmp1) > 100) {
 		for( i in 30:NROW(tmp1))
 		{
 			cat("\n==", i, "\n")
 			Kill			<- paste0("KILL ", tmp1[i, Id])
 			dbGetQuery(con, Kill)
 		}
 	} else {
 		return()
 	} 
 	dbDisconnect(con)
}

setupCon	<- function( sys =  Sys.info()["sysname"] )
{
	
	if( regexpr("darwin", sys, ignore.case = TRUE) > 0 )
	{
			con			<- try( dbConnect(MySQL(),	user = 'Akino', 	password = 'MoonShine', 
 							host = '192.168.10.10', dbname = 'api_lead', port = 3306),
 						silent = TRUE)
 	} else {
 			con			<- try( dbConnect(MySQL(),	user = 'hackad', 	password = 'txPZVu8sizBgW6sJ', 
 							host = 'localhost', dbname = 'api_lead', port = 3306),
 						silent = TRUE)
 	}
 	if( inherits(con, "try-error") ){
 		if( grepl("many", con[1]))
 		{
 			closeConnections()
 			if( regexpr("darwin", sys, ignore.case = TRUE) > 0 )
			{
				con			<- try( dbConnect(MySQL(),	user = 'Akino', 	password = 'MoonShine', 
 								host = '192.168.10.10', dbname = 'api_lead', port = 3306),
 								silent = TRUE)
 			} else {
 				con			<- try( dbConnect(MySQL(),	user = 'hackad', 	password = 'txPZVu8sizBgW6sJ', 
 									host = 'localhost', dbname = 'api_lead', port = 3306),
 								silent = TRUE)
 			}
 		}
 	}
 	
 	return(con)
	
	
}


getProtecme		<- function( )
{
	if( regexpr("darwin", Sys.info()["sysname"], ignore.case = TRUE) > 0 ) 
	{	
		con			<- try( dbConnect(MySQL(),	user = 'Akino', 	password = 'MoonShine', 
 								host = '192.168.10.10', dbname = 'api_lead', port = 3306),
 								silent = TRUE)
		Query		<- "select status, ssn, phone, email from prod_Protecme"
			
	} else {
		con			<- try( dbConnect(MySQL(),	user = 'hackad', 	password = 'txPZVu8sizBgW6sJ', 
 								host = '195.62.77.50', dbname = 'protectme_main', port = 3306),
 								silent = TRUE)
 		Query		<- "select status, ssn, phone, email from accounts"
			
	}
	
 	res			<- dbSendQuery(con, statement = Query)
 	data1		<- data.table( dbFetch(res, n = -1) )	
 	dbClearResult(res)
	dbDisconnect(con)
	return( data1 )							
}

getDataPart		<- function( x, query = NULL )
{
	
	con 		<- 	setupCon( )	
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



			


#* @get /rundata
normalMean <- function(samples=10){
  data <- rnorm(samples)
  mean(data)
}

#* @post /checkdata
fetch <- function(filename){
	suppressPackageStartupMessages(require( methods ))
	suppressPackageStartupMessages(require( dplyr ))
	suppressPackageStartupMessages(require( dtplyr ))
	suppressPackageStartupMessages(require( RMySQL ))
	suppressPackageStartupMessages(require( tibble ))
	suppressPackageStartupMessages(require( data.table ))
	suppressPackageStartupMessages(require( openxlsx ))
	
	
	Sys.setenv("R_ZIPCMD" = "/usr/bin/zip")



hs1=createStyle(fgFill = "#DCE6F1", halign = "center", textDecoration = "Italic", border = "Bottom")

hs2	= createStyle(	
		fgFill = "black", 
		fontSize = 12,
		halign = "center", 
		textDecoration = "bold", 
			border = "Bottom")
			
	# storage = list.files(path ='../../../Desktop/', pattern = '.*08.*xlsx', full.names = TRUE)
	validColnames	= c("dateuploaded", "firstname", "lastname", "ssn", "email", "zip", "city", "phone", "surveyprice", "source", "adress")
	optionalNames	= c('firstname', 'lastname', 'ssn', 'phone')
	fileInput		= ifelse( ! exists("filename"), "2017-07-13_14_34_52_person11.csv", filename)
	
	hackad= dirname(getwd())
	extended="public/storage/leads"
	#path			 "/Users/akinoosx/Sites/hackad/public/storage/leads" ## enter the full path 
	import=file.path(hackad, extended, 'import')
	export=file.path(hackad, extended, 'export')
	storage=file.path(import, fileInput)
	print(storage)
	if( file.exists( storage ) )
	{
		nrOfDuplicatedPhone= as.integer()
		nrAlreadyExists=as.integer()
	
		if( grepl("(?=\\.csv$)", storage, perl = TRUE) )
		{
			data1						= fread(storage)	
		} else if(  grepl("(?=\\.xlsx$)", storage, perl = TRUE) ){
			data1						= read.xlsx(storage)
		}
		data1=as_tibble( data1 )
		########################################################################################
		# check names first 
		########################################################################################
		Names=tolower(colnames(data1))
		nameId=which(!optionalNames %in% Names)
		if(length( nameId) > 0)
		{
			ErrorNames=Names[nameId]
			ValidName= optionalNames[nameId]
			x=list("status"="ERROR", 
					"code"= "500", 
					"ErrorName"=as.character(na.omit(ErrorNames)) ,
					"Valid"= ValidName)
			output=jsonlite::toJSON(x)
			return(output)
		}
		## check for phonenr
		checkPhone=data1 %>% group_by(phone) %>% summarize( count=n() ) %>% filter(count > 1 )
		if(NROW(checkPhone ) > 0)
		{	# nr of duplicated
			nrOfDuplicatedPhone=checkPhone %>% summarize(n()) %>% as.integer
		}
		# rm those duplicated
		
		data2=as_tibble(  unique(data1, by = 'phone', fromLast = TRUE) )
		checkDupSSN= data2 %>% group_by(ssn) %>% summarize( count=n() ) %>% filter(count > 1 )
		if(NROW(checkDupSSN ) > 0)
		{	# nr of duplicated
			nrOfDuplicatedSSN= checkDupSSN %>% summarize(n()) %>% as.integer
		}
		data2 =anti_join(data2 , checkDupSSN, by = 'ssn')
		
		missingNames=validColnames[ which(validColnames	%in% Names == FALSE)]
		
		for( i in missingNames)
		{
			data2[, i]		<- 'NULL'
		}
		Today=Sys.Date()
		data2=data2 %>%  mutate( 	dateuploaded = ifelse( dateuploaded == 'NULL', as.character(Today), dateuploaded),
									phone		= as.character(gsub("^(0)(.*)", "\\2",phone)),
									ssn1 		= gsub("(\\w{4})(\\w{2})(\\w{2}).*", "\\1-\\2-\\3", ssn),
									zip			= as.numeric(gsub("\\s+","",zip))
									)
	
		rm(data1); gc(reset = TRUE)
		
		MaxAge=72*52
		oldPersons=data2 %>% 
					mutate( weeks = as.integer(difftime(Sys.Date() , ssn1, units = 'weeks') ) ) %>%
					filter( weeks > MaxAge)
		
		data2=data2 %>% 
				mutate( weeks = as.integer(difftime(Sys.Date() , ssn1, units = 'weeks') ) ) %>%
				filter( weeks < MaxAge) %>%
				select(-c(weeks,ssn1))

		phoneKeys=data2 %>% select(phone ) %>% mutate(phone = as.character(phone))

		
		compDT=getDataPart(x = 'prod_Person') 
		compDT=as_tibble( compDT )
		## can only compare ssn that is 12 characters long 				
		## first by phone 
		compDT1=right_join(compDT, phoneKeys, by = 'phone')
		# those who already exists
		alreadyExists=compDT1 %>% filter(!is.na(personId))
		
		if( NROW(alreadyExists) > 0 )
		{
			nrAlreadyExists=NROW(alreadyExists) 		
		}	
			
		keys_2=alreadyExists %>% select(phone)  %>% mutate(phone = as.character(phone))
		## update dataset by rm those that already exists by phone
		data3=anti_join(data2 , keys_2, by = 'phone')
		rm( keys_2)
			

		####################################################################################################
		# Check by ssn, must be 12 characters long when excluding - 	
		####################################################################################################
		ssnKeys=data3 %>%  # 12
				mutate(ssn = gsub("(-|\\.)","",ssn) )  %>% 
				filter(ssn != 'NULL'  & nchar(ssn) == 12 ) %>%  
				select(ssn ) 
												
		
		
		
		ProtecmeDT=as_tibble ( getProtecme( ) )
		
		selectSSN=function( x = data3, y = ssnKeys, comps = compDT )
		{
					tmpData=x %>% 
							mutate(ssn  = gsub("(-|\\.)", "", ssn)) 
									

					compDT1=comps %>% 
							mutate(ssn  = gsub("(-|\\.)", "", ssn)) %>% 
							filter( nchar(ssn) == 12 )	
					
					# if ssnKeys1 > 0 then there is ssn in the prudentia db
					ssnKeys1=right_join(compDT1, y, by = 'ssn')  %>%
								filter(!is.na(personId) ) %>% NROW
			
					# if ssnKeys1 > 0 then there is ssn in the protectme db
										
					ssnKeys2=right_join(ProtecmeDT, y, by = 'ssn')  %>%
												filter(! is.na(phone) )
					
					DBname=names(which(c( prudentia = ssnKeys1, protecme = ssnKeys2 %>% NROW) > 0	))
					if(length(DBname) > 0 )
					{
						dataset1=anti_join(tmpData, ssnKeys2, by = 'ssn')
						if(NROW(dataset1) == 0)
						{
							dataset1=0
						} 
						ll=list("dataset"=dataset1,
								"Omitted"= ssnKeys2 %>% select( ssn, email))
											
					} else {
						ll		<- list("dataset"=tmpData 	,
										"Omitted"=0)
					} 	
				
					return( ll )	
} ## end of function
		
		if(NROW(ssnKeys) > 0 )	
		{
			data4=selectSSN(x = data3, y = ssnKeys)		
			DataDT=data4$dataset		
			omittedSSN=data4$Omitted														
		} else {
			DataDT=data3
			omittedSSN=0
		}	
		
		
		tolowerChar=c('firstname','lastname', 'adress', 'city')
		if( NROW(DataDT) > 0 )
		{
			DataDT=DataDT %>% mutate(firstname = tolower(firstname), 
								lastname=tolower(lastname),
								adress=tolower(adress),
								city=tolower(city))
		} 
		
		
		if( NROW(DataDT) > 0 && NCOL(DataDT) > 1)
		{
			fileName=gsub("(.*)\\.(.*)", "\\1", basename(storage))
			ExportFile=file.path(export, sprintf("%s.xlsx", fileName, gsub("(\\s|:)","_", fileName) ))
			wb=openxlsx::createWorkbook( )
			name='upload'

			addWorksheet( wb, name)
			writeData(wb, sheet = name,  x = DataDT  , 
							startRow =  1, headerStyle = hs1)
			openxlsx::saveWorkbook(wb, file = ExportFile, overwrite = TRUE)
		}
		


		
		
		
		
		Name=file.path(export, sprintf("Notvalidpersons_%s.xlsx",  gsub("(\\s|:)", "_", as.character(Sys.time( ))) ))
		wb=openxlsx::createWorkbook( )
		
		if( exists( "nrOfDuplicatedPhone") &&  NROW(nrOfDuplicatedPhone) > 0 )
		{
			name 		<- 'Duplicated persons'
			addWorksheet( wb, name)
			setColWidths(wb, sheet = name, cols = 1:NCOL(checkPhone), widths = 15)
			writeData(wb, sheet = name,  x = checkPhone  , 
							startRow =  1, headerStyle = hs1)
			
		}
		
	
		
		if( exists( "alreadyExists") &&  NROW(nrAlreadyExists) > 0 )
		{
			name 		<- 'Already exist by phone'
			addWorksheet( wb, name)
			setColWidths(wb, sheet = name, cols = 1:NCOL(2), widths = 15)
			writeData(wb, sheet = name,  x = alreadyExists %>% select(personId ,phone)  , 
							startRow =  1, headerStyle = hs1)
			
		}
		
		
		if( exists( "checkDupSSN") &&  nrOfDuplicatedSSN > 0  )
		{
			name 		<- 'Duplicated ssn'
			addWorksheet( wb, name)
			setColWidths(wb, sheet = name, cols = 1:NCOL(checkDupSSN), widths = 15)
			writeData(wb, sheet = name,  x = checkDupSSN  , 
							startRow =  1, headerStyle = hs1)
		}
		
		if( NROW(omittedSSN) > 1 )
		{
			name 		<- 'Ssn in system'
			addWorksheet( wb, name)
			setColWidths(wb, sheet = name, cols = 1:NCOL(omittedSSN), widths = 15)
			writeData(wb, sheet = name,  x = omittedSSN  , 
							startRow =  1, headerStyle = hs1)
		}
		
		if( exists( "oldPersons") && NROW(oldPersons) > 0  )
		{
			name 		<- 'Old persons'
			addWorksheet( wb, name)
			setColWidths(wb, sheet = name, cols = 1:NCOL(oldPersons), widths = 15)
			writeData(wb, sheet = name,  x = oldPersons %>% select(ssn)  , 
							startRow =  1, headerStyle = hs1)
		}
		
		openxlsx::saveWorkbook(wb, file = Name, overwrite = TRUE)


		if( file.exists(Name ) )
		{
			outputfile = Name
		} else {
			outputfile = "No export file"
		}
		
		
		x=list(status = "SUCCESS", code = "200", 
						duplicatedPerson 	= ifelse( exists("nrOfDuplicatedPhone"), nrOfDuplicatedPhone, "NULL" ), 
						duplicateSSN 		= ifelse( exists("nrOfDuplicatedSSN"), nrOfDuplicatedSSN, "NULL" ), 
						oldPerson			= ifelse( exists("oldPersons"), NROW(oldPersons), "NULL" ), 
						omittedPerson 		= ifelse( exists("nrAlreadyExists"), nrAlreadyExists, "NULL" ) , 
						ommittedSsn			= ifelse( exists("omittedSSN"), NROW(omittedSSN) , "NULL" ) ,
						notValidFile 		= ifelse(exists("Name"), Name, "NULL" ), 
						valid 				= NROW(DataDT), 
						exportFile 			= ifelse(exists("ExportFile"), ExportFile, "NULL" ))
		output	<-jsonlite::toJSON(x)
		return(output)
	
	} 
  
  
  
  
} ## end of fetch

#curl https://api.hackad.se/checkcond/rundata
### curl --data '{"a":4,"b":3}'  http://api.myprotectme.isvorg.com/checkcond/sum
# curl --data '{"filename": "2017-07-10_08_54_43_person10.csv"}'  http://localhost:8080/normalMean
### curl http://api.myprotectme.isvorg.com/checkcond/mean
#!/usr/bin/env Rscript 
##########################################################################################
# This script is used for all sort of task that aren't to be for 
# reporting purposes, just for solvning different task
##########################################################################################
options(scipen = 999)
options("openxlsx.dateFormat" = "yyyy-mm-dd")

Sys.setenv("R_ZIPCMD" = "/usr/bin/zip") ## makes openxlsx works in centos

library(methods)	

### XLSX Options
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
testDATA		<- file.path(getwd(), 'testData')
outPUT			<- file.path(getwd(), 'outData')
GRAF			<- file.path(getwd(), 'GRAF')
if( ! file.exists(outPUT ))
{
	dir.create(outPUT, recursive = TRUE)
}
# All R-files 
RFiles 					<- list.files(path = '.' , include.dirs = TRUE, pattern = "*.R$", 
											recursive = TRUE, full.names = TRUE)
# Load common functions and files , DATA dir is where to send all data
source(file = grep(".*RClass.*(?<=tion.R)", RFiles ,value = TRUE, perl = TRUE, ignore.case = TRUE) )

Packages			 	<- c("lubridate",'reshape2', 'reshape', 'RCurl', 'data.table',
								'ggthemes', 'stargazer', 'RColorBrewer',
								'RCurl' , 'scales','openxlsx','jsonlite',
								'ggplot2', 'scales', 'grid', 'gridExtra',"Hmisc",
							 	'dplyr')
## Set path and load pkgs
Pack		<- new("startUps", pkgs = Packages, path = dirname(FULLPATH) )
Pack$instant_pkgs()

library(httr)

configHeader			<-  add_headers (
        "Content-Type" = "application/json",
        "Authorization" = paste("Token ", '216e20cab4afad68c7ea1860bc17fc29424af399')
    )
    
    
    
    
    
xlsFile			<- list.files(path = testDATA, pattern = 'Riks.*xlsx', full.names = TRUE)

emails				<- read.xlsx(xlsFile)
setDT(emails)
Names				<- names(emails)
Names1				<- gsub(":","", Names)
setnames(emails, Names, Names1)

emailsCheck			<- c(emails$Riksdagen.mejl, na.omit(emails$Regeringen.mejl))   
emailsCheck 		<- emailsCheck[!duplicated(emailsCheck)]
leaksList			<- list( )
emailList			<- list( )
notLeak				<- data.table( )
Count				<- 1
for( i in 1:length( emailsCheck) )
{
	email			<-  emailsCheck[ i ]
	cat(strrep("#", 60), "\n")
	cat("Running email ", email, "\n")
	req <- GET(
    		url = sprintf("https://api.defentry.com/v1/account/%s", email),
    		config = configHeader
	)

	req_1 		<- content(req, as = 'text', encoding = 'utf8')
	cat("Status code ",  req$status_code, "\n")
	dat 		<-  fromJSON(req_1)
	## handle leaks 
	step1	 	<-  try( data.frame(Reduce(rbind, dat$leaks)), silent = TRUE)
	if( inherits(step1,'try-error') )
	{	
		S_1			<- data.frame(obs = unlist(dat$leaks))
		Names1		<- row.names(S_1)
		Names2		<- gsub(".*\\.(.*)","\\1",Names1)
		S_2			<- data.frame(keys = Names2, values = S_1$obs)
		S_3			<- cast(S_2, ~ keys, value = 'values')
		setDT(S_3)
		S_3[, value := NULL]
		step1		<- S_3
		rm(S_1, S_2, Names1, Names2, S_3)
	}
	if( NROW(step1) > 0 )
	{
		setDT(step1)
		step1[, `:=` ( email = email, status = req$status_code)]
		leaksList[[Count]]		<- step1		
		#leaksDT		<- rbind(leaksDT , step1)
		email_1		<- dat$emails[[email]]
		setDT(email_1)
		email_1[, `:=` ( email = email, status = req$status_code)]
		
		emailList[[ Count ]]		<- email_1
	
		cat("Nr or leaks ", NROW(step1), "\n")
		cat(strrep("#", 60), "\n")
		sink(file = file.path(outPUT, paste0(email, '.json')) )
			toJSON(dat)
		sink()
		Count	 <- Count + 1
	} else {
		step1		<- data.table(email = email, status = req$status_code, checked = Sys.time( ), isLeaked = 0)
		notLeak		<- rbind(notLeak, step1)
	}
}

do.call("rbind",lapply(leaksList, function(x) names(x) ))
 do.call ,leaksList)
	
	
	emailDT			<-  rbindlist(emailList)
	
	leakDT			<-  rbindlist(leaksList[-c(23,31)], fill = TRUE)
	leakDT1			<- 	rbindlist (leaksList[c(23,31)])
				<- rbind(leakDT, leakDT1, fill = TRUE)
	emailDT[, tmp := gsub("(.*)@.*","\\1", email)]
	d				<- emailDT[, .(freq = .N), by = .(tmp)][order(freq)]
	pal 			<- brewer.pal(8, "Dark2")
	
	library(wordcloud)
	png(filename = file.path(GRAF ,"leaked.png"), width=1280,height=800)
		wordcloud(d$tmp,d$freq, scale=c(4.5,.1), min.freq=1, 
		max.words=Inf, random.order=FALSE, rot.per=.35, colors= pal)
		 grid.text(label = sprintf("[Läckta email], max antal %s",max(d$freq))  , x= 0.15, y = 0.1, gp=gpar(cex = 2))
	dev.off( )	
	
	
	Websites		<- leakDT[, .(freq = .N), by = .(website)][order(freq, decreasing = TRUE)]
	Websites[, website := as.character(website)]
	save(Websites, file = file.path(testDATA, "Data.RData"))
		
	
	
	
	emailDT[ , c('tmp', 'id_number','last_login_ip','signup_ip','fullname','status') := NULL]
	leakDT[, status := NULL]
	
	Name1			<- paste0('Protectme', Sys.Date() , '.xlsx')
		wb			<- openxlsx::createWorkbook( )


		## Insert to excel 
		name 		<- 'Sammanställning'
		df		<- data.frame(	website = 'Vilka websidor som varit föremål för intrång',
								freq 	= "Antal intrång" 
				)
		
		df1		<- data.frame( What = colnames(df), Explanation = t(df))
		addWorksheet( wb, name)
			
		setColWidths(wb, sheet = name, cols = 1, widths = 25)
		setColWidths(wb, sheet = name, cols = 2, widths = 45)
		writeData(wb, sheet = name,  x = df1, startRow = 1, startCol = 1,	borders = "surrounding", 
								headerStyle = hs1)
		setColWidths(wb, sheet = name, cols = 3:NCOL(Websites), widths = 25)
		Count		<- NROW(df1) + 3

		writeData(wb, sheet = name,  x = Websites , startRow = Count, startCol = 1,	borders = "surrounding", 
								headerStyle = hs1)
		
		Count		<- Count + NROW(Websites) + 3
		
		
		insertImage(wb, sheet = name, file = file.path(GRAF ,"leaked.png"), width = 5, height = 6, startRow = Count, startCol= 4 ) 	


		setValue('EmailData', colum = 1:NCOL(emailDT), data = emailDT)
		setValue('intrångData', colum = 1:NCOL(leakDT), data = leakDT)
		
		openxlsx::saveWorkbook(wb, file = Name1, overwrite = TRUE)		
		
		Count		<- Count + 1	
			
			
		ErpSett		<- ErpTrans[grepl(regexpMonth, settlementPaidAt), .(settlementAmount  = SUM(settlementAmount)), by = .(siteId, currency)][order(settlementAmount)]	
		ErpSett1 	<- dcast.data.table(ErpSett, siteId ~ currency, SUM)
		ErpSett1	<- rbind(ErpSett1, ErpSett1[, lapply(.SD, tmpSum)], fill = TRUE)
		ErpSett1[is.na(siteId), siteId := 'Total']
		writeData(wb, sheet = name,  x = ErpSett1, startRow = Count, startCol = NCOL(Step1) - 1,  headerStyle = hs1)	

		



	
	
	
	
req_2 <- content(req, "text", encoding = 'utf8')

reg2 = fromJSON(req_1)
reg2$'leak'$`89`$`website` <-  "myspace.com"
fromJSON(req_2)

####### Bulk report 


step1				<- paste0(emailsCheck, collapse = ",")
step2				<- toJSON(paste0('{ "emails" : ', toJSON(step1) ,'}'))
#https://api.defentry.com/v1/account/


req_post	<- GET(  	url 		= sprintf("https://api.defentry.com/v1/account/%s", 'test@test.com'),
						config 		= configHeader)
		
req3 <- content(req_post, as = 'text', encoding = 'utf8')
statusCode 		<- fromJSON(req3)
statusCode$job_id		
		
resutls  <- GET(
    url = sprintf("https://api.defentry.com/v1/bulk?job_id=%s", statusCode$job_id),
    add_headers (
        "Content-Type" = "application/json",
        "Authorization" = paste("Token ", '216e20cab4afad68c7ea1860bc17fc29424af399')
    )
)

req4 <- content(resutls, as = 'text', encoding = 'utf8')
fromJSON(req4)

#############################################################################
## Load pkg and script
#############################################################################
## Load pkg and script
library( methods )
library( dplyr )
library( openxlsx )
library( RMySQL )
library( data.table )
library( lubridate )

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


RowStat		<- function( data, id, 
					type = c('mean', 'var', 'sum','sd', 'min','max')) 
{				
	type    <- match.arg( type )

	FUN     <- switch( type,
                mean = function(x) mean(x, na.rm = TRUE),
                sum	 = function(x) sum(x, na.rm = TRUE),
                var  = function(x) var(x, na.rm = TRUE),
                sd   = function(x) sd(x, na.rm = TRUE),
                min  = function(x) min(x, na.rm = TRUE),
                max  = function(x) max(x, na.rm = TRUE)
            )
            
      Data		<- melt.data.table(data, id.vars = id)
      Data1		<- Data[, FUN(value), by = id]
      Data1		<- merge(data, Data1, by = id, all.x = TRUE)
      setnames(Data1, "V1", "Total")
      return(Data1)
}


tmpSum		<- function(x)
{
	if(is(x,'character') ||is(x, 'factor') ||is(x, 'Date'))
	{
		return(NA)
	} else {
		return(SUM(x))
	}
}

#' x Fill in the title of legend
Guide 	<- function(x, ...)
{
	x1 <- guides(	fill = guide_legend(
					  title = x, 
					   title.position = "top", 
					 title.hjust = 0.5,
					title.theme = element_text(size = 10, angle = 0),
					keywidth 	= unit(15, "mm"), 
					keyheight	= unit(4,"mm"),
					label.position = "bottom",
					label.hjust 	= 0.5,
					label.theme 	=  element_text(size = 9, angle = 0),
								... 
					))
	return(x1 )				
}


setCredit	<- function(from)
{
	if(missing(from) )
	{
		print(match.call())
		Query				<- "SELECT * FROM kriita_survey.PaylevoCreditCheck"	
		QuerySend			<- "SELECT * FROM kriita_survey.DeltaSafe"
	} else {
		Query				<- sprintf("SELECT * FROM kriita_survey.PaylevoCreditCheck WHERE checkDate >= '%s'", from)
		QuerySend			<- sprintf("SELECT * FROM kriita_survey.DeltaSafe WHERE checkDate >= '%s'", from)
	}
		if(exists('DeltaSafe',envir = .GlobalEnv) || exists('creditData',envir = .GlobalEnv))
		{
			rm(DeltaSafe, creditData); gc(reset = TRUE)
		}
		con				<- try( dbConnect(MySQL(),	user = 'serdara', 	password = 'aiiR1eX5d', 
 										host = '193.182.121.238', dbname = "kriita_survey", port = 3306),
 										silent = TRUE)	  
 										
		creditData			<- getData(query = Query)		
		#idx1				<- creditData[, unique(personId)]
				
		DeltaSafe 			<- data.table( dbFetch( dbSendQuery(con, statement = QuerySend), n = -1)  )
		DeltaSafe[, sourceType := 'CREDITSAFE']
		setnames(DeltaSafe, "varData","sourceData")
		
		
		creditData 			<- rbind(creditData, DeltaSafe, fill = TRUE)	
		creditData[, noDaysSinceLastCheck := as.integer(difftime(Sys.Date(), checkDate, units = 'days'))]
		assign('creditData', creditData, envir = .GlobalEnv)
		
}

#' x Fill in the title of legend
GuideCol 	<- function(x, ...)
{
	x1 <- guides(	colour = guide_legend(
					  title = x, 
					   title.position = "top", 
					 title.hjust = 0.5,
					title.theme = element_text(size = 10, angle = 0),
					keywidth 	= unit(15, "mm"), 
					keyheight	= unit(4,"mm"),
					label.position = "bottom",
					label.hjust 	= 0.5,
					label.theme 	=  element_text(size = 9, angle = 0),
								... 
					))
	return(x1 )				
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



char_to_num		<- function(x) as.numeric(as.character(x))


#' USAGE: Con  <- fileCon$new( name = <filePath>, content = <character[|Vector]> )
#' 		Con$openCon()  will open connection and write <content> param to <name>
#'    	Con$testCon() will test if there is any connection
#' TODO: 	update testCon() to suppress warning message, 
#'			Build openCon() to handel read or write and reading content into 
#'			data.table object 
#' Returns: File to destionation in name
fileCon <- setRefClass("fileCon", 
				fields = list( 	name 	= "character",
								content = "character" ),
				methods = list(
					testCon	=  function ( n = name)
					{
						Test 	<- try(isOpen( name ), silent = TRUE)
						if( inherits(Test, 'try-error') )
						{
							return(FALSE)
						} else {
							return( TRUE )	
						}
					},
					openCon	= function( )
					{
						if(! testCon( ) )
						{
							con	<- file	(description = name, open = "w", encoding = "UTF8")
							writeLines(content, con = con )
							close( con )
							message(basename(name), " done and closed connection")
						}	
					}	
				)
		
)



#' x the table name
#' USAGE getData( x = "Invoice")
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



#' USAGE: Object <- new("startUps", pkgs = .PACK, Input = c("data", "graf") )
#' CALL: Object$instant_pkgs( ), will update and install pkgs 
#' CALL: Object$setDirs( Extra ), will create and set path 
#'		to <Input> and if nessecary to the <Extra> character
startUps <- setRefClass("startUps",
			fields 	= list( pkgs = "character", Input = "character", path = "character" ),
			methods	= list(
				instant_pkgs = function( )
				{
					pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
    				if (length(pkgs_miss) > 0)
    				{
        				install.packages(pkgs_miss)
    				}
    
    				if (length( pkgs_miss) == 0)
    				{
        				message("\n ...Packages were already installed!\n")
    				}
   	     			attached <- search()
    				attached_pkgs <- attached[grepl("package", attached)]
    				need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
    
    				if (length(need_to_attach) > 0)
    				{
        				for (i in 1:length(need_to_attach))
							require(need_to_attach[i], character.only = TRUE)
   	}
    
					if (length(need_to_attach) == 0)
					{
        				message("\n ...Packages were already loaded!\n")
					}

				}, # End of function  
				setDirs		= function( )
				{
					if( length(Input) > 0)
					{
						.HOME	<- path
						Input	<<- c(Input)
						if( inherits(Input, "character") )
						{
							Output	<- paste0(toupper(Input), " <- file.path('", .HOME, "','", Input, "')")
						
							for( d in Output)
							{
								cat("************************************************\n")
								String	<- gsub(".*\'(.*)\'.*", "\\1", toupper(d))
								cat("Path for", String , "completed\n")
								Step1 	<- parse(text = d)
								cat("************************************************\n")
								assign(String , eval(Step1), globalenv() ) 
								!file.exists(get( String )) && dir.create( get(String) ,
											 recursive = TRUE)
							} # ForLoop ends here
						} else {
							stop("Need to input character inside ", deparse(substitute(Input)))
						}
					}	
					}	 ## End of function setDirs
			) # End of methodsList	
				
) # End of setRefClass






Format <- function( x, n, by = 0, ... ) {
    Fun <- function( x, n,...) {
        Char 	<- prettyNum(	round(x, digits = by ), big.mark = " ",
                             nsmall = 0,scientific = FALSE,...)
        if( missing( n ) ){
            n <- nchar(Char)
            }
        R  <- sprintf( paste("%.", n, "s", sep = "") ,Char)
        return(R)
    }
    if( length( x ) > 1 ) {
        List <- sapply(x, Fun)
        return( List )
        } else {
            return( Fun( x , n,... ) )
            }
    }
		  

     
 # USAGE: Will split data|vector into distinct breaks
 # with the <by> options, use ... inside cut
 "Segment" <- function(x, by = 0.15 , ...) {
 	S		<- seq(0,1, by )
 	quantile <- cut(x, breaks = quantile(x, probs = S, na.rm = TRUE), ..., 
        						include.lowest = TRUE, labels =  names(S))
    					
    return ( quantile ) 
}   


pal <- function(col, border = "light gray", ...){
    n <- length(col)
    plot(0,0, type = "n", xlim = c(0,1), ylim = c(0,1),
    axes = FALSE,  xlab = "", ylab = "", ...)
    rect(0:(n-1)/n,0, 1:n/n, 1, col = col, border = border)
}

#' tableFun( data, columCol , fontFace)
#' columCol is only for header and defautl is white text 
#' fontFace is the 1) Plain, 2) bold, 3) italic, 4) both italic and bold
tableFun	<- function( data, columCol = "azure3", plot = FALSE, fontFace = 3)
{
	tt3		<- ttheme_minimal(
		core=list(	bg_params = list(fill='white', col=NA),
					fg_params = list(fontface = fontFace)),
		colhead=list(bg_params = list(fill= columCol, col=NA),
					fg_params = list(fontface = 2, col = "white"))			
	)
	
	step1		<- tableGrob(data, rows = NULL, theme = tt3)
	
	separators 	<- replicate(ncol(step1) - 1,
						segmentsGrob(x1 = unit(0, "npc")),
						simplify = FALSE)
	
	step1	<- gtable::gtable_add_grob( step1, grobs = separators,
							t = 1, b = nrow(step1), l = seq_len(ncol(step1) - 1) + 1)
								
	if( plot ) {
		grid.draw( step1 )
	} else {
		return( step1 )
	}								
												

}


SUM		<- function(x) sum(x, na.rm = TRUE)
MEAN		<- function(x) mean(x, na.rm = TRUE)
LENGTH	<- function(x) length(x, na.rm =TRUE)

##########################################################################################
# Color options and font 
##########################################################################################
col_blue		 		<- rgb( 0, 68, 91, maxColorValue = 255 )
col_blueGreen			<- rgb( 89, 155, 161, maxColorValue=255 )
col_green				<- rgb( 44, 171, 102, maxColorValue=255 )
col_red 				<- rgb( 237, 47, 36, maxColorValue=255 )
col_grey		 		<- rgb( 76, 76, 76, maxColorValue=255 )
col_orange		 		<- rgb( 242, 101, 34, maxColorValue=255 )
col_yellow				<- rgb( 251, 173, 29, maxColorValue=255 )
col_neut 				<- rgb( 229, 229, 229, maxColorValue=255 )
col_purple				<- rgb( 147, 112, 229, maxColorValue=255)
col_gold				<- rgb(184, 134, 11, maxColorValue=255 )
col_khaki				<- rgb(189, 183, 107,  maxColorValue=255)
colPro			<- c(	"col_blue" = col_blue, "col_blueGreen" = col_blueGreen,
					"col_green" = col_green, "col_red" = col_red,
					"col_orange" = col_orange, "col_khaki" = col_khaki,
					"col_yellow" = col_yellow, "col_purple" = col_purple,
					"col_gold" = col_gold,"col_grey" = col_grey )
					
					
Company	<- list( 'UK' = c(	"comeon","bertil", "vinnarum", 
							"casinoroom","redkings","svenskalottoportalen",
							"3hholdings","garantispelet","videoslots",
							"europaspelet","garantispelet","supervinsten",
							"superspelet") )					





## Load in common table 
Query 	<- sprintf("SELECT * FROM ExchangeRate WHERE 
					date = ( SELECT max(date) FROM ExchangeRate)")

Rates 	<- getData(query = Query)
## Change to swedish currency 
Rates[, Rates := Rates[currency == 'SEK', rate]/rate]
Rates		<- Rates[,.(Currency = currency, Rates)]


## This is by each transaction
StartDate		<- "2014-10-01"
CurrentDate		<- Sys.Date( )
LastDay			<- CurrentDate - days(2)
LastDate		<- CurrentDate - months(1)

## Procedure is that to first check Reservation, if null then look at MerchantCountrySettings, if null check OperatorCountrySettings
## if null then use default which is MeacodeMerchant
Query					<- "SELECT 	mm.merchantId, 
									mm.country,
									mm.invoiceFee,
									mm.invoiceFee,
									mm.storeId, 
									mm.settlementInterval,
									mm.settlementPeriod,
									mm.invoiceHoldPeriod,
									mm.invoiceFeeVatRate,
									mm.invoiceLateInterestPercentage,
									ss.type AS TYPE, 
									ss.operatorId,
									COALESCE(mcs.invoiceHoldPeriod, ocs.invoiceHoldPeriod, mm.invoiceHoldPeriod) AS invoiceHoldPeriod,
									mm.collectionDelay,
									mm.invoicePaymentTerms,
									mm.reminderDelay, 
									mcs.invoiceHoldPeriodRecurring,
									COALESCE(mcs.invoicableStatuses,  ocs.invoicableStatuses) AS invoicableStatuses
							FROM MeacodeMerchant mm 
						INNER JOIN Site ss ON (mm.merchantId = ss.siteId)
						INNER JOIN MerchantCountrySettings mcs ON (mm.merchantId 	= mcs.merchantId AND 
																	mm.country 		= mcs.country) 
						INNER JOIN OperatorCountrySettings ocs ON (ocs.operatorId 	= ss.operatorId AND 
																	ocs.country 	= mm.country)"

Meacode					<- getData(query = Query)

## rm så that it will be unique sites, can be multiple settlementCurrency 




Meacode[, TYPE := ifelse(grepl("RISK", TYPE), 'NoRisk', 
						ifelse(grepl("NORMAL", TYPE), "Risk", TYPE) )]
							
Meacode[grepl("Risk", TYPE), Risk := 1]			
Meacode[grepl("NoRisk", TYPE), Risk := 0]


query							<- "SELECT * FROM Person"
Person							<- getData(query = query)


						
						
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

deductionRule	<- list( 	'co' = 'COMPENSATION',
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
				

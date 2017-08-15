DataSet

clean1			<- DataSet[grepl(regexp, keys, ignore.case = TRUE), ]
clean1[, keys1 := gsub("(?:\\w+)\\.(.*)", "\\1", keys)]
clean1			<- clean1[,.(keys = keys1 , personId, values )]


Names		<- names(finlandSourc)
for( i in seq_len(length(Names)) )
{

	clean1[grepl(Names[i], keys, ignore.case = TRUE), keys1 :=  unlist(finlandSourc[[Names[i]]])]		
}

clean2			<- DataSet[grepl(regexp, Names, ignore.case = TRUE), ]
clean2 			<- clean2[, .(personId,  keys = gsub(".+\\.(.*)$", "\\1", Names) , values )]

names(finlandSourc)
names(finlandSourc[['maksuHairioriski']])

for( i in Names)
{
	
	Names1			<-  ifelse( is.null(names(finlandSourc[[i]])) , unlist( finlandSourc[[i]] ), names(finlandSourc[[i]])) 

	clean2[grepl(i, keys, ignore.case = TRUE), keys1 := Names1]

} 

cleanFI		<- rbind( clean2[, .( personId, key = keys1, values)],
						clean1[, .( personId, key = keys1, values)])

cleanFI[, personId := as.character(personId)]
creditFI[, personId :=  as.character(personId)]
cleanFI 	<- merge(cleanFI , creditFI[, .( personId, country, checkDate,sourceType)], by = 'personId', all.x = TRUE)
uniqPer		<- cleanFI[, unique(personId)]
setkey(cleanFI, personId)
finalClean		<- data.table()
for( i in uniqPer)
{
	cat(strrep("=", 60), "\n")
	cat("Running personId", i, "\n")
	Step1			<- cleanFI[J(i)]
	json			<- toJSON(Step1[, .(key, values)])
	Step2			<- unique(Step1[, .( personId, country, checkDate, sourceType )], by = 'personId')
	Step2[, sourceData := json]
	
	finalClean		<- rbind(finalClean, Step2)
	cat(strrep("=", 60), "\n")
}
finalClean[, sourceData := gsub("key", "keys", sourceData)]
tmp			<- PaylevoCreditCheck[country == 'SE', .(personId, country, checkDate, sourceType, sourceData)]
sapply(tmp, class)
tmp[, `:=` (personId = as.character(personId))]
finalClean[, sourceData := as.character(sourceData)]
sapply(finalClean, class)
Final		<- rbind(tmp, finalClean)
Final[, ordinal := .I]		
setcolorder(Final, c("ordinal" , "personId","country","checkDate" ,"sourceType" ,"sourceData" ))
fwrite(Final, file = "finlandCredit.csv")



print(DataSet_1[is.na(keys) & grepl("", keys, ignore.case = TRUE),  ], nrow = Inf)
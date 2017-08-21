#if (!require(DT)) install.packages("DT")
if (!require(shiny)) install.packages("shiny")
if (!require(RCurl)) install.packages("RCurl")
if (!require(httr)) install.packages("httr")
if (!require(jsonlite)) install.packages("jsonlite")
if (!require(XML)) install.packages("XML")
if (!require(xml2)) install.packages("xml2")
if (!require(rvest)) install.packages("xml2")
	
## constants
## to get all relevant ID's
max_id=100

pre_link_getid="http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&retmode=json&retmax=" ##100&term="
pre_link_getAbs="http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&retmode=json&rettype=abstract&id="
pre_link_getSum="http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&retmode=text&rettype=abstract&id="
pubmed_click="http://www.ncbi.nlm.nih.gov/pubmed/?term="
values=c("uid", "title", "lastauthor", "pubdate", "pubtype", "volume", "issue", "pages", "issn", "lang")
upper_values=c("ID", "Title", "Last Author", "Pubdate", "Pubtype", "Volume", "Issue", "Pages", "ISSN", "LANG")
button2_click=0
output_file="data/pubmed.csv"
output_bin="data/tmp.Rdata"
num_checks=10
num_saves=0
num_saves2=0
save_message=NULL
#################################################################################################
## functions
getAbstracts=function(df.display){

  ids=df.display[,1]
  
   sum.link=paste(pubmed_click, ids, sep="")
   sum.page=lapply(sum.link, read_html)
  ## get data for all ID's	
   sumpage.parse= lapply(1:length(sum.page), function(indx){
	x2=df.display[indx,1]
## get staus -like from pubmed etc
    x3=try(html_nodes(sum.page[[indx]], ".status"))
      if (( class(x3)=="try-error")|| (length(x3)==0)) {
      x3="No Data"
      }else{
      x3= x3%>%html_text
      }
    x4=try(html_nodes(sum.page[[indx]], ".abstr"))
      if (( class(x4)=="try-error")|| (length(x4)==0)){
      x4="No Data"
      }else{
      x4= x4%>%html_text
      }
    x5=try(html_nodes(sum.page[[indx]], ".keywords p"))
      if (( class(x5)=="try-error")|| (length(x5)==0)) {
      x5="No Data"
      }else{
     x5= x5%>%html_text
    }
 return(do.call(rbind,list(c(x2, x3,x4, x5))))
})
res.table=as.data.frame(do.call(rbind, sumpage.parse))
colnames(res.table)=c("ID", "Pubmed Status", "Abstract", "Keywords" )
return(as.data.frame(res.table))
}
############################################################################
## get text of abstract from the api
getSmryFromAPI=function(ids=123456){
abstractURL=paste(pre_link_getSum,ids, sep="" )
pre_abstract=try(GET(abstractURL))
if (class(pre_abstract)== "try-error"){
 return ("error")
}
return (content(pre_abstract, "text"))

}
######################################################################
### function returns words hat are found
getSearchWord=function(ids ,words, gap=20){  
   smry=getSmryFromAPI(ids)
   strLen=nchar(smry)
   cat ("words are --", paste(words), length(words), "\n")
   position=sapply(1:length(words),function(x){ gregexpr(words[[x]],smry, ignore.case=T)})   
   createStmt= sapply(1:length(position), function(x){       
	   if (position[[x]][1] == -1) return (NA)	   	   
	    numMatches=length(position[[x]])
##	select all matches and eventually paste them
		selectPhrase =sapply(1:numMatches, function(y){
		startLine= position[[x]][y]-gap
			endLine=position[[x]][y] + gap
       		if (startLine<0) startLine=0
			if (endLine>strLen) endLine=strLen
			retStmt=substr(smry, startLine, endLine +gap )
            #retStmt2= substr(retStmt, 0, gap)
			#retStmt3=substr(retStmt, gap+1, gap+1+nchar(words[x]))
			#retStmt4=substr(retStmt, gap+1+nchar(words[x]), nchar(retStmt) )
            #cat("words are ", words[x])
			retStmt5=gsub(words[x], paste('<b><span style="background-color: #FFFF00">', words[x], "</span></b>"), retStmt, ignore.case=T  )
			#retStmt_final=paste( retStmt2, '<b><span style="background-color: #FFFF00">', retStmt3, "</span></b>", retStmt4 )
			#cat("Final Statement: ", retStmt_final, "\n")
			return(c(paste(retStmt5, "<br>"), retStmt))
		  })
		return(c(paste(selectPhrase[1], collapse=";"), paste(selectPhrase[2], collapse=";") ))		
	})
## check if all matches were not found


	if(length(which(!is.na(createStmt[1])))==0) return (NA)
   
    retStmt= sapply(1:length(words), function(z){
	  paste ('<b><span style="background-color: #FFFF00">',words[[z]], "--</span></b>", createStmt[[z]][1])
	  })   	
    
	retStmt2= sapply(1:length(words), function(z){
	  paste (words[[z]], "--",createStmt[[z]][2])
	  })   	

	  x=NULL
	  for (y in retStmt){
	  x=paste(x,y)
	  }
	  
	  x2=NULL
	  for (y2 in retStmt2 ){
	  x2=paste(x2,y2)
	  }
	  
	  
	  
	    
    return(c(x, gsub("\n", "", smry)))	   
    }

########

	
################################################################################
## search for words in the abstract

 server = function(input, output, session) {

 # dataInput1 <- observeEvent(input$button,{
 #  x1=input$mtext1

  # })

   link_1= eventReactive(input$button,{ input$file1 })
   link_2=eventReactive(input$button,{ input$file2 })
   link_3=eventReactive(input$button,{ input$searchTerms })
   link_4=eventReactive(input$button, {input$lenSearch})
   link_5=eventReactive(input$button,{input$showRows})
   link_6=eventReactive(input$button,{input$showCols})

##   tolerance=eventReactive(input$button,,{ input$tolerance})
##  save files function
#########################################################################################
     savMessFunc<- function(save_message)({
	save_message
	})
	
###########################################################################################	

 saved_files=eventReactive(input$saveToFile, {
	cat("num_saves is ", num_saves, "\n")
    assign("num_saves2", num_saves, envir=.GlobalEnv)
	addColNam=ifelse (file.exists(output_file), FALSE,TRUE)
	write.table(check_files()$df_print, file=output_file,append=T, row.names=F, col.names=addColNam, sep=";")
    if (get("num_saves", envir=.GlobalEnv)==1){
    assign("save_message", "<h5>saved file</h5>")
	#assign("num_saves", 0, .GlobalEnv)
	#return ("<h5>saved file</h5>")}
	return (list(mesg1=NULL, num2=num_saves2))
	}else {
	assign("num_saves", 0, .GlobalEnv)
	
	return (list(mesg1=NULL, num2=num_saves2))
	}
	})
####################################################################################
   ### function comparinf files
   check_files=eventReactive(input$button,{
    ## track difft submit clicks
    num_saves=1
    assign("num_saves", num_saves, .GlobalEnv)
	assign("save_message", "save file", .GlobalEnv)
	cat("num_saves 0 is ", num_saves, "\n")
    
   
    ##getID= paste(pre_link_getid,  , URLencode(link_1()), sep="") 
    inp=try(as.integer(link_2()))
##	inp.class=try(as.integer(link_2()))
	if (is.na(inp) || class(inp)=="try-error"|| (inp>999) ){
	  num_checks=10
	} else{
	num_checks=inp
	}
   getID= paste(pre_link_getid, num_checks, "&term=" , URLencode(link_1()), sep="") 

   print (getID)
   res_first=GET(getID)
   res1_parsed=content(res_first, "parsed")
   ret_list1= res1_parsed[["esearchresult"]][["idlist"]]
	a=0
    b=0
	res_df=NULL
	res_df2=NULL
	gap=as.numeric(link_4())
	cat("gap is ", gap, "\n")
	if (length(gap)==0 || (is.na(gap)) || (gap>900) || (gap<1)) gap=600 
	cat("gap is ", link_5(), "\n")
	
   if (length (ret_list1)==0) {
   	return( list(messg=(paste("<b>No results obtained for ", link_1(), "</b>")),dfRes=res_df, df_print=NULL))
   
   } else {
   if (length(ret_list1)< num_checks) num_checks=length(ret_list1)
  
   wordList=try(unlist(strsplit(link_3(),",")))
   cat ("link 3 is ", wordList="","\n")
   if (length(wordList)==0) wordList=NA
   cat("worsList is ", paste(wordList),"\n")
   #cat("Word List is", paste(wordList), length(wordList), "\n" )
   for (i in 1:num_checks){
    b = b+1
	ret_fullLink=paste(pre_link_getAbs, ret_list1[i], sep="")
##   get abstract here
#    ret_abstract= getAbstractFromAPI
	getAbst=GET(ret_fullLink) 
    if (!is.na(wordList)){
	getSmry= getSearchWord(ret_list1[i], words=wordList, gap=gap)[1]
	getSmry2=getSearchWord(ret_list1[i], words=wordList, gap=gap)[2]
	}else{
	getSmry="No Search Term"
	getSmry2="No Search Term"
	}

	 abstJson=content(getAbst,"parsed")
     results_dat=sapply(values, function(x){ 
        res=try(if(TRUE){

       if (is.na(abstJson[[2]][[2]][[x]])) {
	   return ("None")}
       if (is.null (abstJson[[2]][[2]][[x]])) return("None")
	   if(length(abstJson[[2]][[2]][[x]])==0) return ("None")
        abstJson[[2]][[2]][[x]]})
       if (class(res)=="try-error") return ("None")
	   return (res)
	   })
	 
	 click_link=paste(pubmed_click,ret_list1[i], sep="")
	 results_dat1= c(results_dat, click_link, paste(link_1(), i), getSmry)
	 results_dat2= c(results_dat, click_link, paste(link_1(), i), getSmry2)
     results_row=do.call(cbind,as.list(results_dat1))
     results_row2=do.call(cbind,as.list(results_dat2))
     if (ncol(results_row) != 13) next
      if (nrow(results_row)>1) results_row=results_row[1,]
     res_df=rbind (res_df,results_row) 
     res_df2=rbind (res_df,results_row2)
	 }
	colnames(res_df)= c(upper_values, "Clickable Link", "Search Query Rank", "Summary")
#	res2_df=subset(res_df, select=-Summary)
	#colnames(res_df2)= c(upper_values, "Clickable Link", "Search Query Rank", "Summary")
	##res2_df=  res_df2 ##
	 res2_df=res_df
	res_df[,1]= try(unlist(sapply(res_df[,1],function(x)
	    {paste('<a href=','"http://www.ncbi.nlm.nih.gov/pubmed/?term=',x , '">',x, "</a>", sep="")})))
		if (class(res_df[,1])=="try-error") res_df=res2_df
    res_df=as.data.frame(res_df) 
		res_df[,10]=toupper(res_df[,10])
	res_df=res_df[,-c(11,12)]
	res_df=cbind(res_df,res2_df[,1] )
	### new dev
	dfx= data.frame(rep("test",10))
#    res_df=cbind(res_df,dfx)
    #res_df=cbind(res_df,placeH)
	print (head(res_df))
	colnames(res_df)= c(upper_values, "Search Results", "raw IDS")
	addColNam=ifelse (file.exists(output_file), FALSE,TRUE)
	## show rows or not-- which rows to show    
res_df=as.data.frame(res_df)
res2_df=as.data.frame(res2_df)
   ##
	if (link_5()==1&&(nrow(res_df)>0)){

         if(is.null(res_df$"Search Results")){
            removeRows=1:nrow(res_df)
         }else{
             removeRows=union(which(is.na(res_df$"Search Results")), which(is.na(res_df$"Search Results"))) }
	cat("lenght of remove rows is ", length(removeRows),"\n")

     	if (length(removeRows)>0){
		res_df=res_df[-removeRows,]
		res2_df=res2_df[-removeRows,]
		
		
		
#if (nrow (res_df)==0) res_df=="No Results"
#if (nrow (res_df)==0) res2_df=="No Results"

		}
		
		
	}
	
		if (link_6()==1){
     res_df=res_df[,c(1:4,11)]
   }

	
print ("res2_df")
print (names(res2_df))
#res2_df=res2_df[,1:12]	
	
#	write.table(res2_df, file=output_file,append=T, row.names=F, col.names=addColNam, sep=";")
    
	list(messg=(paste("<b>The Search for the term", link_1(), "is completed </b>")),dfRes=res_df, df_print=matrix(res2_df[,1:12]), mesg2=num_saves)
    }

   })
  
  ####### tab 2 show file
  display_file=eventReactive(input$button2,{
	if (!file.exists(output_file)) {
	    return (messg="No prior file exists", out_df=NULL)
    } else {
	dfout=read.table(output_file, header=T,sep=";",fill=T)
	return (list(messg="File Displayed Below", out_df=dfout)) 	 
	 }
 })
 
############tab 3 
    mine_text=eventReactive(input$button3,{
   ret.df=getAbstracts(check_files()$df_print)
   return(ret.df)
   })
  
  
  
#x3= function(){paste( "<pre>",x2(), x1(),"</pre>", sep= "\n")} 
   output$welcome = renderText({"<h3>Welcome</h3>"})
   output$test<- renderText({
    HTML(check_files()$messg)})
   output$dftable<- renderDataTable({check_files()$dfRes }, options=list(pageLength=10),escape = FALSE)
   output$savedFileMessage <- renderText({saved_files()$mesg1})

 ### navbar for tab 2
output$current_data=renderText({display_file()$messg})
output$current_file=renderDataTable({display_file()$out_df }, options=list(pageLength=50),escape = FALSE)
  
  ## navbar for tab 3
  output$cur_abst=renderDataTable({mine_text()}, options=list(pageLength=10),escape = FALSE)
 # output$saved =renderText(save_files())
  ## stop session
  if (FALSE){
     session$onSessionEnded(function() {
        stopApp()})}
  }
  

  
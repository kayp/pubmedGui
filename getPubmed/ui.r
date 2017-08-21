if (!require(shiny)) install.packages("shiny")
if (!require(RCurl)) install.packages("RCurl")
if (!require(httr)) install.packages("httr")
if (!require(jsonlite)) install.packages("jsonlite")
if (!require(XML)) install.packages("XML")
if (!require(xml2)) install.packages("xml2")
if (!require(rvest)) install.packages("rvest")
#if (!require(DT)) install.packages("DT")

###



# Define UI for application that draws a histogram
shinyUI(navbarPage("Pubmed Search",

  # Application title

  tabPanel("Submit Query",
  fluidRow(
  
  column(width=6,
  HTML("<b><h3 style=color:blue;>Please enter your query below</h3></b>"))),
 br(),

   fluidRow(
    column(width=2,	
	textInput("file1", "Enter Query", value = 'Cancer AND Prevalance')),

     
	column(width=1,	
	textInput("file2", "# results", value = "200")),
	
		column(width=3,	
	textInput("searchTerms", "Search Upto Five Comma Sep Terms", value = "Prevalance, Incidence")),
	
    column(width=2,
	textInput("lenSearch", "# Characters on each side", value="600")),
	
	 column(width=2, selectInput("showRows", label = "Display rows", 
    choices = list("Show Matches Only" = 1, "Show all Returns" = 2), 
    selected = 2)),
	
		 column(width=2, selectInput("showCols", label = "Display Columns", 
    choices = list("Selected" = 1, "All" = 2), 
    selected = 1))
	
	),
	fluidRow(	
	column(width=1,	
    actionButton("button", "Submit",
    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
	)),

	column(width=5,
		  h4("Results of the Query will be shown below")),
		  

    conditionalPanel(
		condition="output.dftable",
	column(width=1,
	actionButton("saveToFile","Save File"),
	
	htmlOutput("savedFileMessage")
	))

	  
	)			  
    ,
    mainPanel(

      htmlOutput("test"),
	  dataTableOutput("dftable"), width=12
        )
	  ),
	  
	  tabPanel("Stored Data",
	  fluidRow(	
	    column(width=4,	
		HTML('<h3>To view current file "View file" button </h3>')),
		  column( width=4,
          actionButton("button2", "View File", style='padding: 25px 40px;font-size:20px;color:#fff;background-color: #33b79c'))),
	  
	 #   fluidRow(column(
	   mainPanel(
	    htmlOutput("current_data"),
	  dataTableOutput("current_file"), width=12
	  )),
	  
	  tabPanel("Text Mining",
	  fluidRow(
        column(width=4,	
		  HTML('<h3>To mine abstracts page click "Mine Data" button </h3>')),
		  column( width=4,
          actionButton("button3", "Mine Data", style='padding: 25px 40px;font-size:20px;color:#fff;background-color:#9133b7'))),		  		  
	  mainPanel(
	    dataTableOutput("cur_abst"), width=12)
	  )  
	  
	  
	))


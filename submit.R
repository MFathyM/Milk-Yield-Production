library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)
library(rgdal)
library(forecast)
library(fUnitRoots)

#Define UI for application that draws a histogram
ui <- dashboardPage(
  
  #header
  dashboardHeader(title = "Milk Forecasting App"),
  
  #side bar
  dashboardSidebar (
	tags$style(".shiny-input-container  {line-height: 15px; height : 40px}"),
 	tags$style(".form-control  {height: 20px;}"),
  	dateInput("start_time", "Start Date",value = "2018-01-01",min = "2018-01-01", max = "2018-06-30", startview = "year"),
	dateInput("end_time", "End Date",value = "2018-06-30",min = "2018-01-01", max = "2018-06-30", startview = "year"),
    	dateInput("forecasting_time", "Forecasting Date",value = "2018-06-30",min = "2018-06-30", max = "2018-12-30", startview = "year"),
	dateInput("start_actual", "Start Date (Actual)",value = "2018-07-01",min = "2018-07-01", max = "2018-12-30", startview = "year"),
	dateInput("end_actual", "End Date (Actual)",value = "2018-12-30",min = "2018-07-01", max = "2018-12-30", startview = "year"),
	dateInput("start_change", "Start Date (Modified)",value = "2018-06-30",min = "2018-06-30", max = "2018-12-30", startview = "year"),
	dateInput("end_change", "End Date (Modified)",value = "2018-06-30",min = "2018-06-30", max = "2018-12-30", startview = "year"),
	textInput("reason", "Type the reason of change", value = "Type the reason of change"),	
	numericInput("new_my", "Amount of Change in %", value = 0),
	actionButton("add", "Apply Change")
),
  #body
  dashboardBody(
tabsetPanel(type="tab",
    tabPanel("Plots",
			fluidRow(
					box(plotlyOutput("hist")),
					box(plotOutput("plot")),
					box(width = 9, verbatimTextOutput("selected")),
					box(width = 3, uiOutput("go_back"))
				   )
		),
    tabPanel("Map",
			fluidRow(
					box(leafletOutput("mymap"))
				   )
		),
    tabPanel("Actual Data Modifications",
		      fluidRow(
				box(width = 12, verbatimTextOutput("mod"))	
				   )
		),
    tabPanel("Actual Data Evaluation",
		      fluidRow(
				box(width = 12, dataTableOutput("eval"))	
				   )
		)
	)
  )
)

set.seed(135)

#Define server logic required to draw a histogram
server <- function(input, output,session) {

data4 = read.csv("data4.csv")
data5 = read.csv("data4.csv")
data_ts <- ts(data5 , start = c(2018, 1), end = c(2018, 12), frequency = 12)


Dist <- data.frame(Selangor = colnames(data4[10:11]), Perak = colnames(data4[12:13]))
Far <- data.frame(Puchong = colnames(data4[2:3]), Putragaya = colnames(data4[4:5]),Ipoh = colnames(data4[6:7]), Seri.Iskander = colnames(data4[8:9]))
Dates <- as.character(data4[,"Date"])

#map
Malaysia = readOGR(dsn = ".", layer = "Cities")
Malaysia2 <- Malaysia[c(45,54,33,38,20,22,6,10),]
row.names(Malaysia2) <- colnames(data5[2:9])

  drillDown <- reactiveValues(level="State",
                              level_id = 1,
					previous_selection = "Malaysia",
                              event_data = NULL
					)

  levels <- c("State", "City", "District", "Farm")
  observe({
    drillDown[["level"]] <- levels[drillDown[["level_id"]]]
  })

sd <- reactive({(as.POSIXlt(input$start_time)$mon)+1})
ed <- reactive({(as.POSIXlt(input$end_time)$mon)+1})
fd <- reactive({(as.POSIXlt(input$forecasting_time)$mon)+1})
sa <- reactive({(as.POSIXlt(input$start_actual)$mon)+1})
ea <- reactive({(as.POSIXlt(input$end_actual)$mon)+1})
sc <- reactive({
input$add
isolate({
(as.POSIXlt(input$start_change)$mon)+1})})
ec <- reactive({
input$add
isolate({
(as.POSIXlt(input$end_change)$mon)+1})})


sel <- reactive({
if (drillDown[["level_id"]] == 1){
se<- "Malaysia"
} else {
se<- names(data5[match(drillDown[["selected"]],colnames(data5))])
}
se
})



chartData <- reactive({
if (drillDown[["level_id"]]==1){
chartDat <- data_ts[sd():ed(),14:15]
} else if (drillDown[["level_id"]]==2){
chartDat <- data_ts[sd():ed(),match(as.character(Dist[,match(drillDown[["selected"]],colnames(Dist))]),colnames(data_ts))]
} else if (drillDown[["level_id"]]==3){
chartDat <- data_ts[sd():ed(),match(as.character(Far[,match(drillDown[["selected"]],colnames(Far))]),colnames(data_ts))]
}else {
chartDat <- data_ts[sd():ed(),match(drillDown[["selected"]],colnames(data_ts))]
}
chartDat 
})

  
output$hist <- renderPlotly({
  if (drillDown[["level_id"]]==4){
halawaa <-
plot_ly(
  x = names(data_ts[1,match(drillDown[["selected"]],colnames(data_ts))]),
  y = sum(chartData()),
  type = "bar") %>% 
layout(title = "Total Milk Yield", xaxis = list(title = "Location"), yaxis = list(title = "Total Milk Yield"))

} else {
halawaa <-
plot_ly(
  x = colnames(chartData()),
  y = colSums(chartData()),
  type = "bar")%>% 
layout(title = "Total Milk Yield", xaxis = list(title = "Location"), yaxis = list(title = "Total Milk Yield"))

  }
halawaa
})

plotData_1 <- reactive({
if (drillDown[["level"]] == "State"){
   plotData_11 <- data_ts[sd():ed(),"Malaysia"]
} else {
plotData_11 <-data_ts[sd():ed(),match(drillDown[["selected"]],colnames(data5))]
}
plotData_11 
})

plotData_2 <- reactive({
if (drillDown[["level"]] == "State"){
   plotData_22 <- data_ts[sa():ea(),"Malaysia"]
} else {
plotData_22 <-data_ts[sa():ea(),match(drillDown[["selected"]],colnames(data5))]
}
plotData_22 
})

plotData1 <- reactive({ts(plotData_1(), start = c(2018,sd()), end = c(2018,ed()), frequency = 12)})
plotData2 <- reactive({ts(plotData_2(), start = c(2018,sa()), end = c(2018,ea()), frequency = 12)})



f_plot <- reactive({if (fd()>6){
f_plo <-forecast(auto.arima(plotData1()), h=fd()-6)
}else {f_plo <-NULL}
f_plo 
})

output$plot <- renderPlot({
if (fd()>6){
shadow <- plot(f_plot(), main = "The Change of Milk Yield With Time",xlab = "Time", ylab = "Milk Yield")
lines(plotData2(), col = "red")
legend("topleft", c("Training Data","Testing Data","Forecasted Mean"), 
	col=c("black", "red", "blue"), lty=1, cex=1)
}else{
shadow <- ts.plot(plotData1(), plotData2(), gpars=list(main = "The Change of Milk Yield With Time",
	xlab = "Time",
	ylab = "Milk Yield",
	col = c("black","red")))
legend("topleft", c("Training Data","Testing Data"), col=c("black","red"), lty=1, cex=1)
}
shadow
})


v1 <- 0
v1s <- reactive({
input$add
isolate({
if (drillDown[["level_id"]]==1){
v1 <<- rbind(v1,input$new_my)
}else{
v1 <<- rbind(v1,0)
}
v1
})
})

v2 <- 0
v2s <- reactive({
input$add
isolate({
if (drillDown[["level_id"]]==2){
v2 <<- rbind(v2,input$new_my)
}else{
v2 <<- rbind(v2,0)
}
v2
})
})


v3 <- 0
v3s <- reactive({
input$add
isolate({
if (drillDown[["level_id"]]==3){
v3 <<- rbind(v3,input$new_my)
}else{
v3 <<- rbind(v3,0)
}
v3
})
})


v4 <- 0
v4s <- reactive({
input$add
isolate({
if (drillDown[["level_id"]]==4){
v4 <<- rbind(v4,input$new_my)
}else{
v4 <<- rbind(v4,0)
}
v4
})
})

v<- reactive({cbind(v1s(),v2s(),v3s(),v4s())})

num <- reactive({
v()[length(v1s()),drillDown[["level_id"]]]})

new_data <- NULL
new <- reactive({
input$add
isolate({
new_data <<- append(new_data,input$new_my)
})
})
new_hamo <- reactive({new()[-1]})


selecteds <- reactive({
		if (sc()>6 && ec()<12 && drillDown[["level_id"]] == 1){
			selected<-
			c(data4[1:sc()-1,"Malaysia"], 
					 data4[sc():ec(),"Malaysia"]-num()*data4[sc():ec(),"Malaysia"]/100, 
					 data4[(ec()+1):12,"Malaysia"])
			data4<<- cbind(data4[1:15],data.frame("Malaysia" = selected))
		}else if (sc()>6 && ec()==12 && drillDown[["level_id"]] == 1){
			selected<-
			c(data4[1:sc()-1,"Malaysia"], 
					 data4[sc():ec(),"Malaysia"]-num()*data4[sc():ec(),"Malaysia"]/100)
			data4<<- cbind(data4[1:15],data.frame("Malaysia" = selected))
		}else if (sc()>6 && ec()<12 && drillDown[["level_id"]] != 1){
			selected<-
			c(data4[1:sc()-1,match(sel(),names(data5))], 
					 data4[sc():ec(),match(sel(),names(data5))]-num()*data4[sc():ec() ,match(sel(),names(data5))]/100, 
					 data4[(ec()+1):12,match(sel(),names(data5))])
			data4<<- cbind(data4[1:match(sel(),names(data5))-1],
				 selected,
				 data4[(match(sel(),names(data5))+1):16])
			names(data4) <- names(data5)
		}else if (sc()>6 && ec()==12 && drillDown[["level_id"]] != 1){
			selected<-
			c(data4[1:sc()-1,match(sel(),names(data5))], 
					 data4[sc():ec(),match(sel(),names(data5))]-num()*data4[sc():ec(),match(sel(),names(data5))]/100)
			data4<<- cbind(data4[1:match(drillDown[["selected"]],colnames(data5))-1],
				 selected,
				 data4[(match(sel(),names(data5))+1):16])
			names(data4) <- names(data5)
		}
selected
})

data4_update<- reactive({
if (length(new_hamo()) == 0){
data4_up <- data4
}else if (length(new_hamo()) != 0 && drillDown[["level_id"]] == 1){
data4_up <-cbind(data4[1:15],data.frame("Malaysia" = selecteds()))
}else if (length(new_hamo()) != 0 && drillDown[["level_id"]] != 1){
data4_up <-cbind(data4[1:match(sel(),names(data5))-1],
				 selecteds(),
				 data4[(match(sel(),names(data5))+1):16])
}
data4_up
})


yl3n<- c("July","August","September","October","November","December")


evalData <- function(buseyabasha, halawa, shadow, haliba){
buseyabasha1 <- buseyabasha[1:6,haliba]
buseyabasha2 <- buseyabasha[halawa:shadow,haliba]
khara <- buseyabasha1 %>% ts(start=c(2018,halawa),frequency=12)%>% auto.arima()%>% forecast(h=6)
Up<- khara$upper
Lo<- khara$lower
Up1bb <- as.numeric(Up[,1])
Up2bb <- as.numeric(Up[,2])
Lo1bb <- as.numeric(Lo[,1])
Lo2bb <- as.numeric(Lo[,2])
Up1b <- Up1bb[(halawa-6):(shadow-6)]
Up2b <- Up2bb[(halawa-6):(shadow-6)]
Lo1b <- Lo1bb[(halawa-6):(shadow-6)]
Lo2b <- Lo2bb[(halawa-6):(shadow-6)]
n <- intersect(which(buseyabasha2 <Up1b), which(buseyabasha2 >Lo1b))
o <- intersect(which(buseyabasha2 <Up2b), which(buseyabasha2 >Up1b))
eo<- which(buseyabasha2 >Up2b)
u <- intersect(which(buseyabasha2 <Lo1b), which(buseyabasha2 >Lo2b))
eu <- which(buseyabasha2 <Lo2b)
zbon<- c("July","August","September","October","November","December")
mn <- ifelse(length(n)==0,"No Observations",paste(zbon[n+halawa-7],collapse=", "))
mo <- ifelse(length(o)==0,"No Observations",paste(zbon[o+halawa-7],collapse=", "))
meo <- ifelse(length(eo)==0,"No Observations",paste(zbon[eo+halawa-7],collapse=", "))
mu <- ifelse(length(u)==0,"No Observations",paste(zbon[u+halawa-7],collapse=", "))
meu <- ifelse(length(eu)==0,"No Observations",paste(zbon[eu+halawa-7],collapse=", "))
total <- length(Up1bb)
walid <- data.frame(sums=c(length(n),length(o),length(eo),length(u),length(eu)), months = c(mn,mo,meo,mu,meu), 
     	     perc = c(length(n)/total,length(o)/total,length(eo)/total,length(u)/total ,length(eu)/total))
paste(walid$sums,"[",round(walid$perc,2)*100,"%]\n",walid$months)
}

disha <- function(buseyabasha, halawa, shadow){
dro <- cbind(1:(ncol(buseyabasha)-1))
for (i in 2:ncol(buseyabasha)){
dro[i-1] <- data.frame(evalData(buseyabasha, halawa, shadow,i))
}
names(dro)<-names(buseyabasha[-1])
dro<-data.frame(dro)
dro
}


sef <- reactive({data.frame(Location=names(data4[2:ncol(data4)]), Total = rep(ea()-sa()+1,each=ncol(data4)-1),
			Normal = as.character(unlist(disha(data4_update(),sa(),ea())[1,])),
			Above.Mean = as.character(unlist(disha(data4_update(),sa(),ea())[2,])),
			Extremely.Above.Mean = as.character(unlist(disha(data4_update(),sa(),ea())[3,])),
			Below.Mean = as.character(unlist(disha(data4_update(),sa(),ea())[4,])),
			Extremely.Below.Mean = as.character(unlist(disha(data4_update(),sa(),ea())[5,]))
)})

change <- NULL
changes <- reactive({
input$add
isolate({
change <<- c(change,"The milk yield in ",sel()," has been changed by ",input$new_my,"% during the period from ",yl3n[sc()-6], ",2018 to",
yl3n[ec()-6], ",2018\n   The reason is: ",input$reason,"\n   Date Modified:",as.character(Sys.time()),"\n ")
})
})

stss <- reactive ({
if (length(new_hamo()) == 0){
sts<-"There are no changes to the actual data"
}else{
sts<-changes()
}
sts
})



output$mod <- renderPrint({cat(stss())})
output$eval <- renderDataTable({sef()}, 
        options = list(scrollX = TRUE))

output$mymap <- renderLeaflet({

if (drillDown[["level_id"]]==1){
fasya<-
   leaflet(Malaysia2) %>%
          addTiles() %>%
          #fitBounds(lng1=100.18, lat1 = 2.30, lng2=102, lat2=6) %>%
	    #addMarkers(data = map[match(colnames(chartData()),colnames(data5))-1,], lng = as.numeric(map[match(colnames(chartData()),colnames(data5))-1,2]), lat=as.numeric(map[match(colnames(chartData()),colnames(data5))-1,3]))%>%
  	    addPolygons()

	} else if (drillDown[["level_id"]]==2){
fasya<-
leaflet(Malaysia2[c(match(as.character(Far[1:2,match(names(chartData()[1,1]),colnames(Far))]),row.names(Malaysia2)),match(as.character(Far[1:2,match(names(chartData()[1,2]),colnames(Far))]),row.names(Malaysia2))),]) %>%
          addTiles() %>%
          #fitBounds(lng1=100.18, lat1 = 2.30, lng2=102, lat2=6) %>%
	    #addMarkers(data = map[match(colnames(chartData()),colnames(data_ts()))-1,], lng = as.numeric(map[match(colnames(chartData()),colnames(data_ts()))-1,2]), lat=as.numeric(map[match(colnames(chartData()),colnames(data_ts()))-1,3]))%>%
  	    addPolygons()
		
	} else if (drillDown[["level_id"]]==3) {
fasya<-
 leaflet(Malaysia2[match(colnames(chartData()), row.names(Malaysia2)),]) %>%
          addTiles() %>%
          #fitBounds(lng1=100.18, lat1 = 2.30, lng2=102, lat2=6) %>%
	    #addMarkers(data = map[match(colnames(chartData()),colnames(data_ts()))-1,], lng = as.numeric(map[match(colnames(chartData()),colnames(data_ts()))-1,2]), lat=as.numeric(map[match(colnames(chartData()),colnames(data_ts()))-1,3]))%>%
  	    addPolygons()

	} else {
fasya<-
 leaflet(Malaysia2[match(drillDown[["selected"]], row.names(Malaysia2)),]) %>%
          addTiles() %>%
          #fitBounds(lng1=100.18, lat1 = 2.30, lng2=102, lat2=6) %>%
	    #addMarkers(data = map[match(colnames(chartData()),colnames(data_ts()))-1,], lng = as.numeric(map[match(colnames(chartData()),colnames(data_ts()))-1,2]), lat=as.numeric(map[match(colnames(chartData()),colnames(data_ts()))-1,3]))%>%
  	    addPolygons()

}
fasya
 })
 
output$go_back <- renderUI({
  if(drillDown[["level_id"]] != 1){
    actionLink("go_back", "Go Back",  icon("angle-double-left"))
  }
})

prv <- function (x,y,d,f){
if(x==2){
return("Malaysia")
}else if(x==3){
return(names(d[round((which(d==as.characetr(y))+0.5)/2,0)]))
}else{
return(names(f[round((which(f==as.characetr(y))+0.5)/2,0)]))
}
}

observeEvent(input$go_back, {
  drillDown[["level_id"]] <- drillDown[["level_id"]] - 1
  drillDown[["selected"]] <- ifelse(drillDown[["level_id"]]==1,"Malaysia",
							c(names(Dist[round(which(Dist==as.character(drillDown[["selected"]]))/2+0.4,0)]),
							  names(Far[round(which(Far==as.character(drillDown[["selected"]]))/2+0.4,0)])))
})

observeEvent(event_data("plotly_click"),{
  if(drillDown[["level_id"]] == length(levels)) return(NULL)
  drillDown[["level_id"]] <- drillDown[["level_id"]] + 1 
  drillDown[["selected"]] <- event_data("plotly_click")[3]
})



output$selected <- renderPrint({
if (drillDown[["level_id"]] == 1){
return(
cat(
"Selected:",names(data5["Malaysia"]),
"\tLevel:",drillDown[["level"]],
"\tTotal Milk Yield:",round(sum(data5[sd():ed(),"Malaysia"]),2)
))
} 
else {
return(cat("selected:", names(data5[match(drillDown[["selected"]],colnames(data5))]),
"\tLevel:",drillDown[["level"]],
"\tTotal Milk Yield:",round(sum(data5[sd():ed(),match(drillDown[["selected"]],colnames(data5))]),2))
)}
})
}

# Run the application 
shinyApp(ui = ui, server = server)


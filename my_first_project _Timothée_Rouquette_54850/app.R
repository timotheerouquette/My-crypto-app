
# I) Data

#read data
btc.data=read.csv("https://www.cryptodatadownload.com/cdd/gemini_BTCUSD_1hr.csv",header=T,skip = 1)
eth.data=read.csv("https://www.cryptodatadownload.com/cdd/gemini_ETHUSD_1hr.csv",header=T,skip = 1)
ltc.data=read.csv("https://www.cryptodatadownload.com/cdd/gemini_LTCUSD_1hr.csv",header=T,skip = 1)
zec.data=read.csv("https://www.cryptodatadownload.com/cdd/gemini_ZECUSD_1hr.csv",header=T,skip = 1)
# These data bases represent daily updated prices of four cryptocurrencies

# We convert dates
btc.data$Date=strptime(btc.data$Date,format = "%Y-%m-%d %H:%M:%S")
eth.data$Date=strptime(eth.data$Date,format = "%Y-%m-%d %H:%M:%S")
ltc.data$Date=strptime(ltc.data$Date,format = "%Y-%m-%d %H:%M:%S")
zec.data$Date=strptime(zec.data$Date,format = "%Y-%m-%d %H:%M:%S")

#We round values
btc.data$Close=round(btc.data$Close,2)
eth.data$Close=round(eth.data$Close,2)
ltc.data$Close=round(ltc.data$Close,2)
zec.data$Close=round(zec.data$Close,2)

#We count the minimum of rows
min.rows=min(nrow(btc.data),nrow(eth.data),nrow(ltc.data),nrow(zec.data))

# We create new lists
my.cryptos.names=c("Bitcoin","Ethereum","Litecoin","Zcash")
my.cryptos.symbol=c("BTC","ETH","LTC","ZEC")
my.cryptos.new=paste(my.cryptos.names," (",my.cryptos.symbol,")",sep="")
my.periods=list("Last 24h","Last week","Last month","Last year","Overall period")
my.prices=cbind(btc.data$Close[1:min.rows], eth.data$Close[1:min.rows],ltc.data$Close[1:min.rows],zec.data$Close[1:min.rows])
colnames(my.prices) <- my.cryptos.symbol

# We write descriptions
btc.desc="The world’s first cryptocurrency, Bitcoin is stored and exchanged securely on the internet through a digital ledger known as a blockchain. Bitcoins are divisible into smaller units known as satoshis — each satoshi is worth 0.00000001 bitcoin."
eth.desc="Ethereum is both a cryptocurrency and a decentralized computing platform. Developers can use the platform to create decentralized applications and issue new crypto assets, known as Ethereum tokens."
ltc.desc="Litecoin is a cryptocurrency that uses a faster payment confirmation schedule and a different cryptographic algorithm than Bitcoin."
zec.desc="Zcash is a digital currency with strong privacy features. Transact efficiently and safely, with low fees, while ensuring digital transactions remain private."

#We define 3 functions to display the sign and unit of calculations
sign=function(x) {
  s <- paste0(ifelse(x >= 0, "+", ""), round(x,2), "$")
  s
  }
signpc=function(x) {
  s <- paste0(ifelse(x >= 0, "+", ""), round(x,2), "%")
  s
  }
dol=function(x) {
  s <- paste0(round(x,2), "$")
  s
}

# II) analysis and rendering

library(shiny)
library(png)

ui <- fluidPage(
  headerPanel('My Crypto App'),
  tabsetPanel(            
    tabPanel(title = "Markets", #This is the first panel
      sidebarPanel(
        selectInput('crypto', h3('Select cryptocurrency'), my.cryptos.new), #This input is made to select the cryptocurrency we want to see 
        fluidRow(
          column(4,
            imageOutput("image")), #This output displays the logo of the selected cryptocurrency
          column(7,
            h4("About :"),
            textOutput("desc")) #This output displays the logo of the selected cryptocurrency
        )
      ),
      mainPanel(
        selectInput('period', h4('Select period'), my.periods, selected=my.periods[3]), #This input is made to select the period over which we want to see the evolution (Last month by default)
        plotOutput('plot', click = "plot_click"), #This output displays the plot of crypto prices and you can click on it to have the coordinates
        verbatimTextOutput("info"), #This output displays the coordinates of where you clicked
        tableOutput("corr")
      )       
    ),
    tabPanel(title = "My portfolio", #This is the second panel
      sidebarPanel(
        h4("Enter your assets"), #Following are the numeric inputs to generate your portfolio
        numericInput("nbtc", 
                    h3(my.cryptos.new[1]), 
                    value = 0, step=0.1),
        numericInput("neth", 
                    h3(my.cryptos.new[2]), 
                    value = 0, step=0.1),
        numericInput("nltc", 
                     h3(my.cryptos.new[3]), 
                     value = 0, step=0.1),
        numericInput("nzec", 
                     h3(my.cryptos.new[4]), 
                     value = 0, step=0.1)
      ),
      mainPanel(
        selectInput('period2', h4('Select period'), my.periods, selected=my.periods[3]),
        tableOutput("portfolio") #Here we display the data about your portfolio
      )
    )
  )
)

server <- function(input, output) {
  my.crypto.data=reactive({  #Choose the data base
    if(input$crypto==my.cryptos.new[1]) btc.data
    else if(input$crypto==my.cryptos.new[2]) eth.data
    else if(input$crypto==my.cryptos.new[3]) ltc.data
    else zec.data
  })
  my.desc=reactive({   #display the description of the chosen asset
    if(input$crypto==my.cryptos.new[1]) btc.desc
    else if(input$crypto==my.cryptos.new[2]) eth.desc
    else if(input$crypto==my.cryptos.new[3]) ltc.desc
    else zec.desc
  })
  my.image=reactive({  #display the image of the chosen asset
    if(input$crypto==my.cryptos.new[1]) "img/Bitcoin.png"
    else if(input$crypto==my.cryptos.new[2]) "img/Ethereum.png"
    else if(input$crypto==my.cryptos.new[3]) "img/Litecoin.png"
    else "img/Zcash.png"
  })
  my.period=reactive({   #Keep the period we are intrested in
    if(input$period==my.periods[1]) head(my.crypto.data(), n=24)
    else if(input$period==my.periods[2]) head(my.crypto.data(), n=7*24)
    else if(input$period==my.periods[3]) head(my.crypto.data(), n=31*24)
    else if(input$period==my.periods[4]) head(my.crypto.data(), n=365*24)
    else my.crypto.data()
  })
  actual.price=reactive(my.period()$Close[1])
  start.price=reactive(tail(my.period()$Close,n=1))
  diff=reactive(actual.price()-start.price())
  diffpc=reactive(round(diff()/start.price()*100,2))
  col.diff=reactive(
    if(diff()>=0) "chartreuse3"
    else "red"
  )
  sub=reactive(
    paste0(sign(round(diff(),2))," (",signpc(diffpc()),")")
    )
  my.corr=reactive({
    if(input$period==my.periods[1]) head(my.prices, n=24)
    else if(input$period==my.periods[2]) head(my.prices, n=7*24)
    else if(input$period==my.periods[3]) head(my.prices, n=31*24)
    else if(input$period==my.periods[4]) head(my.prices, n=365*24)
    else head(my.prices, n=min.rows)
  })
  corr.matrix=reactive(cor(my.corr()))
  main=reactive(paste(input$crypto," ",actual.price(),"$", sep = ""))
  #second tab
  my.portfolio=reactive(c(input$nbtc*btc.data$Close[1],input$neth*eth.data$Close[1],input$nltc*ltc.data$Close[1],input$nzec*zec.data$Close[1])) #This is the current value of the portfolio
  my.ex.portfolio=reactive({
    if(input$period2==my.periods[1]) c(input$nbtc*btc.data$Close[24],input$neth*eth.data$Close[24],input$nltc*ltc.data$Close[24],input$nzec*zec.data$Close[24])
    else if (input$period2==my.periods[2]) c(input$nbtc*btc.data$Close[24*7],input$neth*eth.data$Close[24*7],input$nltc*ltc.data$Close[24*7],input$nzec*zec.data$Close[24*7])
    else if (input$period2==my.periods[3]) c(input$nbtc*btc.data$Close[24*31],input$neth*eth.data$Close[24*31],input$nltc*ltc.data$Close[24*31],input$nzec*zec.data$Close[24*31])
    else if (input$period2==my.periods[4]) c(input$nbtc*btc.data$Close[24*365],input$neth*eth.data$Close[24*365],input$nltc*ltc.data$Close[24*365],input$nzec*zec.data$Close[24*365])
    else c(input$nbtc*btc.data$Close[lenght(btc.data$Close)],input$neth*eth.data$Close[lenght(eth.data$Close)],input$nltc*ltc.data$Close[lenght(ltc.data$Close)],input$nzec*zec.data$Close[lenght(zec.data$Close)])
    }) #This is the value of the ex portfolio depending on the period we choose
  my.gain=reactive(my.portfolio()-my.ex.portfolio()) #We calculate the gain of the portfolio
  my.gainpc.nr=reactive(my.gain()/my.ex.portfolio()*100) #We calculate the percentage of gain of the portfolio
  my.gainpc=reactive(round(my.gainpc.nr(),2))
  my.table=reactive(cbind(dol(my.portfolio()),sign(my.gain()),signpc(my.gainpc())))
  my.total=reactive(c(dol(sum(my.portfolio())),sign(sum(my.gain())),signpc(round(sum(my.portfolio())/sum(my.gain()),2))))
  my.final.table=reactive(rbind(my.table(),my.total()))
  my.final.table2=reactive(cbind(c(my.cryptos.new,"Total"),my.final.table()))
  my.final.table3=reactive(rbind(c("Name","Portfolio","Gain/Loss","%"),my.final.table2()))
  output$plot <- renderPlot({
    plot(my.period()$Date, my.period()$Close, type="l", xlab = "", ylab = "Price in $", col="blue",lwd = 3, main=main(), cex.main=3, sub = sub() , col.sub=col.diff(), font.sub=2, cex.sub=2)
  })
  output$info=renderText({
    paste0("Click anywhere to show price: ", input$plot_click$y,"$")
  })
  output$desc=renderText({my.desc()})
  output$image=renderImage({
    filename <- normalizePath(file.path(my.image()))
    list(src = filename)
    },deleteFile = FALSE)
  output$portfolio=renderTable(my.final.table3(),rownames = F ,colname=F, caption = "My portfolio", caption.placement = "top", width = 200, spacing ="l")
  output$corr=renderTable(corr.matrix(),rownames = T ,colname=T, caption = "Correlation", caption.placement = "top" , width = 500, spacing ="l")
}

shinyApp(ui = ui, server = server)


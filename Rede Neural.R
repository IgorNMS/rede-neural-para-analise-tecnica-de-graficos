library(shiny)
library(shinythemes)
library(quantmod)
library(forecast)
library(ggplot2)
library(tsbox)
library(zoo)
library(neuralnet)
library(DMwR)
#Para fins didaticos o algoritimo começa gerando uma data aleatoria de 90 dias, entre 2014 e 2018
data = sample(seq(as.Date('2014/01/01'), as.Date('2017/12/31'), by="day"), 1)
data2 = data + 90
#Agora ele obtem as acoes da IBM no periodo gerado aleatoriamente
getSymbols("IBM", src='yahoo',from = data, to = data2)
#Tipo de grafico
TipoGrafico = factor(c("candlesticks","line","bars","matchsticks"))
#Indice Tecnico
IndiceTecnico = factor(c("ROC","CCI","ATR","TRIX","WPR"))

#interface grafica com o shiny
ui  <- fluidPage(theme = shinytheme("cyborg"),
                 fluidRow(
                     column(6,
                            #controles
                            h3("Analise"), 
                            selectInput("TipoGrafico","Tipo de grafico",choices = TipoGrafico),
                            selectInput("IndiceTecnico","Indice Tecnico",choices = IndiceTecnico),
                            numericInput("Dias", "Prever em Dias - ST", 10, min = 1),
                            actionButton("Processar","Processar"),
                            h3(textOutput("Tgrafico")),
                            plotOutput("Graf"),
                            h3(textOutput("Tmm")),
                            plotOutput("Graf2"),
                            h3(textOutput("Tdados")),
                            tableOutput("Dados")
                     ),
                     column(6,
                            h3(textOutput("Tindicetecnico")),
                            tableOutput("Indice")  ,
                            h3(textOutput("Forecast")),
                            plotOutput("st"),
                            h3(textOutput("Espaco")),
                            plotOutput("nn"),
                            h3(textOutput("Tprecisao")),
                            tableOutput("Precisao")
                     )
                 )
)

server <- function(input, output) {
    observeEvent(input$Processar, {
        #Dados da acoes
        output$Dados <- renderTable({head(IBM,10) })
        #Todos os textos
        output$Tgrafico = renderText({"Grafico"})
        output$Forecast = renderText({"Forecast"})
        output$Tmm = renderText({"Medias moveis"})
        output$Tdados = renderText({"Dados"})
        output$Tindicetecnico = renderText({"Indice Tecnico"})
        
        #Dados dos indices tecnicos de acordo com a selecao
        if (input$IndiceTecnico=='ROC')
            output$Indice <- renderTable({head(ROC(IBM)) })
        else if (input$IndiceTecnico=='CCI')
            output$Indice <- renderTable({head(CCI(IBM)) })
        else if (input$IndiceTecnico=='ATR')
            output$Indice <- renderTable({head(ATR(IBM)) })
        else if (input$IndiceTecnico=='TRIX')
            output$Indice <- renderTable({head(TRIX(IBM)) })
        else output$Indice <- renderTable({head(WPR(IBM)) })
        
        #Grafico   principal     
        output$Graf <- renderPlot({ 
            chartSeries(IBM,type=input$TipoGrafico,theme='white', TA=NULL)
            
            #Adiciona o tipo de indice tecnico ao grafico, conforme a selecao
            if (input$IndiceTecnico=='ROC')
                addROC(n = 1, type = c("continuous"), col = "red")
            else if (input$IndiceTecnico=='CCI')
                addCCI(n = 20, maType="SMA", c=0.015)
            else if (input$IndiceTecnico=='ATR')
                addATR(n=12)
            else if (input$IndiceTecnico=='TRIX')
                addTRIX(n=12)
            else addWPR(n = 14)
        })
        
        #Grafico de medias moveis
        output$Graf2 <- renderPlot({ 
            plot(SMA(Cl(IBM), n = 26), type = "l")
            lines(WMA(Cl(IBM) , n=10), col = "yellow")
            lines(DEMA(Cl(IBM) , n=10), col = "pink")
            lines(ZLEMA(Cl(IBM) , n=10), col = "red")
        })
        
        
        #Serie Temporal com ETS
        mdl = ets(Cl(IBM), model = "ZAZ", damped = T, alpha = 0.2)
        prev1 = forecast(mdl, h=as.integer(input$Dias),levels=c(85,90))
        output$st <- renderPlot({
            plot(prev1, type="l" , main="ETS modelo ZAZ")
            lines(as.vector(Cl(IBM)), col="red")
        })
        
        
        #Rede Neural com 5 lags
        ibm = OHLC(IBM)
        names(ibm)<-c("open","high","low","close")
        dat = data.frame(Cl(ibm))
        dat['closem1'] = Lag(Cl(IBM),1)
        dat['closem2'] = Lag(Cl(IBM),2)
        dat['closem3'] = Lag(Cl(IBM),3)
        dat['closem4'] = Lag(Cl(IBM),4)
        dat['closem5'] = Lag(Cl(IBM),5)
        dat = na.fill(dat, "extend")
        dat_scale = scale(dat)
        nn = neuralnet(close  ~ closem1 + closem2 + closem3 + closem4 + closem5, data=dat_scale, hidden=c(2,3),threshold =1,stepmax= 1000)
        prev = predict(nn,dat_scale ) 
        prev = unscale(prev,dat_scale)
        output$Tprecisao = renderText({"Precisão do Modelo"})
        output$Precisao <- renderTable({   accuracy(as.vector(prev),ibm$close) })
        output$nn <- renderPlot({ 
            plot(as.vector( ibm$close)  , type='l', main="Rede Neural com 5 Lags Preditoras")
            lines(prev,col='red')
            
        })
        
        
    })
    
}

shinyApp(ui = ui, server = server)
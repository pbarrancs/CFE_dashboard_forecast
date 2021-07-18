

# paquetes que usaremos
library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape2)
library(forecast)
library(mxmaps)
library(devtools)
library(dplyr)
if (!require("devtools")) {
    install.packages("devtools")
}
devtools::install_github("diegovalle/mxmaps")


#Generamos nuestros dataframes.
#------------------------------------------------------------------------------------------------------------------
flujo <- read.csv(file = 'data/cfe/data_flujo.csv')
gas_mex <- read.csv(file ='data/local/gas_natural_eia_mex.csv')
elec_mex <- read.csv(file ='data/local/electricidad_eia_mex.csv')
indice <- read.csv(file = 'data/local/Indice_Precios_Gas_Natural.csv')
energy <- read.csv(file = 'data/local/produccion.csv')
cons <- read.csv(file = 'data/local/consumption.csv')
df_temp <- read.csv(file = 'data/market/df_temp.csv')

#Les damos formato
#------------------------------------------------------------------------------------------------------------------
flujo <- flujo[-c(2:4)]
colnames(flujo) <- c('date',1:17)
colnames(gas_mex) <- c('year','production','consumption','imports','exports','reserves')
elec_mex <- elec_mex[-c(3:14)]
colnames(elec_mex) <- c('year','generation','consumption','imports','exports')
colnames(indice) <- c('tipe','year','month','indice','tipo_cambio','transacciones_reportadas',
                      'transacciones_atipicas','volumen_comercializado')
colnames(energy) <- c('year','total','coal','natural_gas','petroleum')
colnames(cons) <- c('year','total','coal' ,'natural_gas','petroleum')


# Plot del gas natural de mexico 
#------------------------------------------------------------------------------------------------------------------
GAS_MEX <- ggplot(gas_mex)+
    geom_line(aes(y=production,x=year,colour="Production"),size=1 )+
    geom_line(aes(y=consumption,x=year,colour="Consumption"),size=1) +
    geom_line(aes(y=imports,x=year,colour="Imports"),size=1) +
    scale_color_manual(name = "Curva", values = c("Production" = "darkblue", 
                                                  "Consumption" = "red", 
                                                  "Imports" = "green")) +
    labs( x = "year", y = "bcf",title ="Gas Natural Mexico") +
    theme_classic()

# Plot de la electricidad de mexico
#------------------------------------------------------------------------------------------------------------------
ELEC_MEX <- ggplot(elec_mex)+
    geom_line(aes(y=generation,x=year,colour="Production"),size=1 )+
    geom_line(aes(y=consumption,x=year,colour="Consumption"),size=1) +
    geom_line(aes(y=imports,x=year,colour="Imports"),size=1) +
    scale_color_manual(name = "Curva", values = c("Production" = "darkblue", 
                                                  "Consumption" = "red", 
                                                  "Imports" = "purple")) +
    labs( x = "year", y = "billion kWh",title ="Electricidad en Mexico") +
    theme_classic()


#Plot de flujo total 
#------------------------------------------------------------------------------------------------------------------
flujo$total <- rowSums(flujo[,c(2:18)], na.rm=TRUE)
flujo$date <- as.Date(flujo$date, format = "%d/%m/%y")

FLUJO <- ggplot(flujo) +
    geom_line(aes(x=date,y=total ), colour = 'purple') +
    labs( x = "date", y = "billion kWh",title ="Flujo Total")+
    theme_classic()



#Plot de flujos por punto
#------------------------------------------------------------------------------------------------------------------
d <- melt(flujo[1:18], id.vars="date")
flujos <- ggplot(d, aes(date,value, col=variable)) + 
    geom_line()+
    labs( x = "date", y = "billion kWh",title ="Flujo por Punto")+
    theme_classic()


#Plot de produccion
#------------------------------------------------------------------------------------------------------------------
d <- melt(energy, id.vars="year")
produccion <- ggplot(d, aes(year,value, col=variable)) + 
    geom_line()+
    labs( x = "year", y = "quad Btu",title ="Production")+
    theme_classic()


# Plot de Consumo
#------------------------------------------------------------------------------------------------------------------
d <- melt(cons, id.vars="year")
consumo <- ggplot(d, aes(year,value, col=variable)) + 
    geom_line()+
    labs( x = "year", y = "quad Btu",title ="Consumption")+
    theme_classic()


#Plot de Mapa Mexico
#Observacion de que todos me dan igual asi que por eso usare solo 2021
#------------------------------------------------------------------------------------------------------------------
ind_2020<- indice  %>% 
    group_by(tipe,year = 2020) %>%
    summarise(sum(volumen_comercializado))
ind_2020 <- ind_2020[-c(2)]
colnames(ind_2020) <- c('sector','volumen')
mapa_datos <- df_mxstate[c(1,2)]
mapa_datos$sector <- c('Region IV','Region I','Region I','Region VI','Region II','Region IV','Region VI'
                       ,'Region II','Region V','Region II','Region V','Region V','Region V','Region IV',
                       'Region V','Region V','Region V','Region IV','Region III','Region VI','Region V',
                       'Region V','Region VI','Region V','Region I','Region I','Region VI','Region III',
                       'Region V','Region VI','Region VI','Region IV')
mapa_datos <- merge(mapa_datos,ind_2020,by="sector")
mapa_datos <- mapa_datos[order(mapa_datos$region),]
datamap <- mapa_datos[c(2,4)]
datamap <- datamap[order(datamap$region),]
colnames(datamap) <- c('region','value')
MAPA_VOL <- mxstate_choropleth(datamap)




#Plot de variacion del indice de precios
#-----------------------------------------------------------------------------------------------------------------
serie <- indice %>% filter(tipe == 'IPGN')
serie <- serie$indice
serie<- ts(serie)
serie<- ts(serie,frequency=12,start=c(2017,7))




#Plot de temperatura
#------------------------------------------------------------------------------------------------------------------
df_temp$date<- as.Date(df_temp$date, format = "%Y-%m-%d")
df_temp$AvgTemp <- (df_temp$AvgTemp-32)*(5/9)
TEMP <- ggplot(df_temp) + 
    geom_line(aes(x = date, y = AvgTemp, colour = Country)) +
    labs( x = "date", y = "Celsius degrees",title ="Temperature")+
    theme_classic()



#------------------------------------------------------------------------------------------------------------------
#                                                               SHINY APP
#------------------------------------------------------------------------------------------------------------------


# Definimos el cuerpo de dashboard
#------------------------------------------------------------------------------------------------------------------
body <- dashboardBody(
    fluidRow(
        tabBox(
            title = "Produccion y consumo Gas y Electricidad",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabset1", height = "250px",
            tabPanel("Gas", plotOutput("plot1")),
            tabPanel("Electricidad", plotOutput("plot2"))
        ),
        box(title = "Volumen Comercializado de Gas Natural 2020 (GJ)",
            side = "right",plotOutput("plot3", height = 400))
    ),
    fluidRow(
        # A static valueBox
        valueBoxOutput("proCoalBox"),
        
        valueBoxOutput("proGasBox"),
        
        valueBoxOutput("proPetroleumBox")
    ),
    fluidRow(
        box(title = "Variacion en precio Gas Natural USD/MBtu",
            side = "left",plotOutput("plot4", height = 400)),
        tabBox(
            title = "Produccion y consumo Energetico",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabset2", height = "240px",
            tabPanel("Produccion", plotOutput("plot5")),
            tabPanel("Consumo", plotOutput("plot6")))
    ),
    fluidRow(
        # A static valueBox
        valueBoxOutput("consCoalBox"),
        
        valueBoxOutput("consGasBox"),
        
        valueBoxOutput("consPetroleumBox")
    ),
    fluidRow(
        tabBox(
            title = "Produccion y consumo Energetico",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabset3", height = "240px",
            tabPanel("Flujo Total", plotOutput("plot7")),
            tabPanel("Flujo por Puntos", plotOutput("plot8"))),
        box(title = "Variacion en Temperatura (c)",
            side = "right",plotOutput("plot9", height = 400))
    ),
)


# Juntamos en nuestra aplicacion
#---------------------------------------------------------------------------------------------------------------
ui <- dashboardPage(
    dashboardHeader(title = "CFE dashboard"),
    dashboardSidebar(disable = TRUE),
    body = body,
    skin = "green"
)

#Corremos el servidor
server <- function(input, output) {
    
    # Plots
    output$plot1 <- renderPlot({GAS_MEX})
    output$plot2 <- renderPlot({ELEC_MEX})
    output$plot3 <- renderPlot({MAPA_VOL})
    output$plot4 <- renderPlot({plot.ts(serie, col='red',main="Variacion de los precios",xlab="Time",
                                        ylab="USD/MBtu",sub="Gas Natrual")})
    output$plot5 <- renderPlot({produccion})
    output$plot6 <- renderPlot({consumo})
    output$plot7 <- renderPlot({FLUJO})
    output$plot8 <- renderPlot({flujos})
    output$plot9 <- renderPlot({TEMP})
    
    
    output$proCoalBox <- renderValueBox({
        valueBox(round(energy$coal[40],3), "Coal Production", 
                 icon = icon("asterisk", lib = "glyphicon"),
                 color = "purple"
        )
    })
    # BOXES
    output$proGasBox <- renderValueBox({
        valueBox(round(energy$natural_gas[40],3), "Natrual Gas Production",
                 icon = icon("fire", lib = "glyphicon"),
                 color = "yellow"
        )
    })
    
    output$proPetroleumBox <- renderValueBox({
        valueBox(round(energy$petroleum[40],3), "Petroleum Production",
                 icon = icon("tint", lib = "glyphicon"),
                 color = "teal"
        )
    })
    
    output$consCoalBox <- renderValueBox({
        valueBox(round(cons$coal[40],3), "Coal Consumption", 
                 icon = icon("asterisk", lib = "glyphicon"),
                 color = "purple"
        )
    })
    
    output$consGasBox <- renderValueBox({
        valueBox(round(cons$natural_gas[40],3), "Natrual Gas Consumption",
                 icon = icon("fire", lib = "glyphicon"),
                 color = "yellow"
        )
    })
    
    output$consPetroleumBox <- renderValueBox({
        valueBox(round(cons$petroleum[40],3), "Petroleum Croduction", 
                 icon = icon("tint", lib = "glyphicon"),
                 color = "teal"
        )
    })
}

#-------------------------------------------------------------------------------------------------------------
shinyApp(ui, server)
library(dplyr)
library(shiny)
library(ggplot2)

comprobarNegativos <- function(x) {
    if (x < 0) return(0)
    return(x)
}

Maxraiting <- function(x) {
    if (x > 100) return(100)
    return(x)
}

#categorias
heatingType <- as.character(c("nan",'Electric','Gas','District heating','Heat network','Heat network and steam','Biomass', 'Oil'))
industry <- as.character(c("nan",'Education','Commercial Property', 'Government', 'Multifamily', 'Single Family'))
raiting <- as.character(c("nan",'B','C','D','E','G','H'))
subindustry <- as.character(c("nan",'College/University','Commercial Real Estate','Corporate Office','Other Government Buildings','Primary/Secondary School','Social Services','Bank/Financial Services','Business Services'))
timezone <- as.character(c("nan",'America/Chicago','America/Denver','America/Los_Angeles','America/New_York','America/Phoenix','Asia/Singapore','Europe/London','Europe/Zurich'))
yearbuilt <- as.character(c("nan",'1862-1875','1888-1890','1898-1902','1903-1906','1913-1915','1919-1945','1945-1966','1967-1976','Post 1976','Pre 1919', '11th Century onwards'))
primaryspaceuse <- as.character(c("nan",'Office','PrimClass','UnivClass','UnivDorm','UnivLab','UnivLib','UnivDining'))
categorias = as.character(c("Industry", "Rating","Energystarscore", "Heating_type", "Number_of_floors", "Occupants", "SQM", "Subindustry", "Timezone", "Yearbuilt", "PrimarySpaceUse","Electric_Savings","Gas_Savings"))


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(id = "Seleccion",
                        tabPanel("Edificio",
                                 textInput("buildingName", "write your building name" , value = "nan", placeholder = "buildingType YourName"),
                                 numericInput('numEnergyScore', 'energia score', value = 0),
                                 selectInput('energyRoom', 'type of energy', heatingType),
                                 selectInput('buildingType', 'Type of building', industry),
                                 numericInput('numNumberFloors', 'Number of Floors', value = 0),
                                 numericInput('numNoccupants', 'Occupants', value = 0),
                                 selectInput('raitings', 'raiting', raiting),
                                 numericInput('numSqm', 'size', value = 0),
                                 selectInput('subIndustry','subindustry', subindustry),
                                 selectInput('time', 'timezone', timezone),
                                 selectInput('year', 'yearbuilt', yearbuilt),
                                 selectInput('primaryUse', 'primaryspaceuse', primaryspaceuse),
                                 numericInput('numElectricSaving', 'Electric Saving per year', value = 0),
                                 numericInput('numGASSaving', 'Gas Saving per year', value = 0),
                                 selectInput('varsta1', 'variable de estudio', categorias)
                                 
                        )
                        
            ),
            actionButton("button", "Enviar")
            
            
        ),
        mainPanel(
            
            plotOutput("plot")
            
            
        )
    )
)

server <- function(input, output){
    v <- reactiveValues(doPlot = FALSE)
    
    observeEvent(input$button, {
        v$doPlot <- input$button
    })
    
    observeEvent(input$Seleccion, {
        v$doPlot <- FALSE
    })  
    
    
    ImprimirGrafica <- eventReactive(input$button,{
        command <- paste("python .\\predictor.py", as.character(Maxraiting(comprobarNegativos(input$numEnergyScore))), gsub(" ", "_", input$energyRoom), 
                         gsub(" ", "_", input$buildingType), as.character(comprobarNegativos(input$numNumberFloors)), 
                         as.character(comprobarNegativos(input$numNoccupants)), input$raitings, 
                         as.character(comprobarNegativos(input$numSqm)), gsub(" ", "_", input$subIndustry), input$time, 
                         input$year, input$primaryUse,as.character(input$numElectricSaving),
                         as.character(input$numGASSaving),gsub(" ", "_", input$buildingName),sep=" ")
        salidaPython <- system(command,intern = TRUE,ignore.stdout = FALSE, ignore.stderr = TRUE)
        datos = read.csv(".\\seguro.csv",
                         stringsAsFactors=F, fill = TRUE)
        
        datos <- mutate(datos, energystarscore = case_when(energystarscore < 25 ~ "< 25",
                                                           (energystarscore < 50 & energystarscore >= 25) ~ "25-50",
                                                           (energystarscore < 50 & energystarscore >= 50) ~ "50-75",
                                                           (energystarscore <= 99 & energystarscore >= 75) ~ "75-99",
                                                           energystarscore == 100 ~ "100"))
        
        datos <- mutate(datos, sqm = case_when(sqm < 2500 ~ "< 2500",
                                               (sqm < 5000 & sqm >= 2500) ~ "2500-5000",
                                               (sqm < 10000 & sqm >= 5000) ~ "5000-10000",
                                               (sqm <= 15001 & sqm >= 10000) ~ "10000-15000",
                                               sqm > 20000 ~ "> 20000"))
        
        datos <- mutate(datos, numberoffloors = case_when(numberoffloors < 2 ~ "> 2",
                                                          (numberoffloors < 5 & numberoffloors >= 2) ~ "2-5",
                                                          (numberoffloors <= 10 & numberoffloors >= 5) ~ "5-10",
                                                          numberoffloors > 10 ~ "< 10"))
        
        datos <- mutate(datos, occupants = case_when(occupants < 10 ~ "< 10",
                                                     (occupants < 10 & occupants >= 100) ~ "10-100",
                                                     (occupants < 500 & occupants >= 100) ~ "100-500",
                                                     (occupants <= 1000 & occupants >= 500) ~ "500-1000",
                                                     occupants > 1000 ~ "> 1000"))
        
        datos <- mutate(datos, Electric_Savings = case_when(Electric_Savings < 1000 ~ "< 1000",
                                                     (Electric_Savings < 5000 & Electric_Savings >= 1000) ~ "1000-5000",
                                                     (Electric_Savings <= 10000 & Electric_Savings >= 5000) ~ "5000-10000",
                                                     Electric_Savings > 10000 ~ "> 10000"))
        
        datos <- mutate(datos, Gas_Savings = case_when(Gas_Savings < 100 ~ "< 100",
                                                            (Gas_Savings <= 500 & Gas_Savings >= 100) ~ "100-500",
                                                             Gas_Savings > 500 ~ "> 500"))
        
        resultado = read.csv2(".\\kprototypes-labels-dist-iii15.csv",
                              stringsAsFactors=F)
        
        respuesta = switch(
            input$varsta1,  
            "Energystarscore" = data.frame(VarEstudio = datos$energystarscore),
            "Heating_type" = data.frame(VarEstudio = datos$heatingtype),
            "Industry" =  data.frame(VarEstudio = datos$industry),
            "Rating" = data.frame(VarEstudio = datos$rating),
            "Number_of_floors" = data.frame(VarEstudio = datos$numberoffloors),
            "Occupants" = data.frame(VarEstudio = datos$occupants),
            "SQM" = data.frame(VarEstudio = datos$sqm),
            "Subindustry" = data.frame(VarEstudio = datos$subindustry),
            "Timezone" = data.frame(VarEstudio = datos$timezone),
            "Yearbuilt" = data.frame(VarEstudio = datos$yearbuilt),
            "PrimarySpaceUse" = data.frame(VarEstudio = datos$primaryspaceuse_abbrev),
            "Electric_Savings" = data.frame(VarEstudio = datos$Electric_Savings),
            "Gas_Savings" = data.frame(VarEstudio = datos$Gas_Savings)
            
        )
        respuesta[is.na(respuesta)] <- 0
        respuesta = cbind(respuesta, numCluster = resultado$X0)
        
        for (i in 1:length(respuesta$numCluster)) {
            if(respuesta$VarEstudio[i] == "" || respuesta$VarEstudio[i] == "0"){
                respuesta$VarEstudio[i] = "Null"
            }
        }
        
        
        ggplot(data = respuesta, aes(y = numCluster))+
            geom_bar(aes(fill = VarEstudio))+
            theme_minimal() +
            ggtitle(paste("Su cluster es el", salidaPython, "ahora hacemos el estudio de los clusters con la variable", input$varsta1,sep=" "))+
            theme(plot.title = element_text(hjust = 0.50))
        
    })
    
    output$plot <- renderPlot({
        
        ImprimirGrafica()
        
    })
    
}

shinyApp(ui, server)


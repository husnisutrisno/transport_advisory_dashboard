library(pxweb)
library(tidyverse)
library(shiny)
library(DT)
library(shinythemes)
library(plotly)

table_api_adress_bilinnehav = "https://api.scb.se/OV0104/v1/doris/sv/ssd/TK/TK1001/TK1001A/FordonTrafik"
table_api_addres_befolkning = "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101Y/FolkmDesoAldKonN"

pxweb_query_list <-
  list("Region" = "*", # Use "*" to select all #"Region" motsvara alla kommunkoder i ett län enligt ovan
       "Alder" = "totalt",
       "Kon" = ('1+2'), #1:man, 2:kvinna #1+2:man+kvinna
       "ContentsCode" = c('000005FF'), #table name in SCB webpage
       "Tid" = c("2019")) #TODO: change to latest?
px_query <- pxweb_query(pxweb_query_list) #skapar en pxweb-query/fråga/anrop som använder listan ovan
px <- pxweb_get(table_api_addres_befolkning)
#Läser in listan i en df, ur denna skapas nedanstående listor
regioner <- as.array(px$variables[[1]]$values)
regioner <- read.table(text = regioner, sep = rawToChar(as.raw(2)), col.names = 'values')
regioner$valueTexts <- as.array(px$variables[[1]]$valueTexts)
#Länlista
lan<-(filter(regioner, nchar(regioner$values)==2))
lan = lan[-1,] #tar bort 00 Sweden
lan$valueTexts<-substr(lan$valueTexts, 4,nchar(lan$valueTexts)) #Tar bort länkod framför lännamnet
lan<-lan%>%
  rename(Lan_kod = values, Lan_namn = valueTexts) %>%
  mutate(Lan_namn = str_remove(Lan_namn, ' county'))
#Kommunlista
kommun<-(filter(regioner, nchar(regioner$values)==4)) #skapar en lista med alla kommuner
kommun$valueTexts<-substr(kommun$valueTexts, 6,nchar(kommun$valueTexts))
kommun<-kommun[order(kommun$valueTexts),]
kommun<-kommun%>%
  rename(Kommun_kod = values, Kommun_namn = valueTexts)
kommuner_i_valt_lan_lista <- kommun %>%
  filter(substr(Kommun_kod, 1, 2) == "01") %>%
  select(Kommun_namn, Kommun_kod)
#RegSolista
kommunRegSolista<-as.data.frame(filter(regioner, substr(regioner$values,1,5) == "RegSO")) #skapar en lista med alla RegSO-delar

#Lägg in i funktionsscripts. 
fordon <- tibble(
  Fordon_kod = c("10", "20", "21", "22", "30", "40", "50", 
                 "60", "70", "71", "72", "80"),
  Fordon_namn = c("Personbilar", "Lätt lastbil", "Tung lastbil", 
                  "Dragfordon", "Buss", "Motorcykel", 
                  "Moped klass 1", "Traktor", "Snöskoter", "Terränghjuling", 
                  "Terrängskoter", "Släpvagn")
)

år <- c("2015", "2016", "2017", "2018", "2019", "2020")


ui <- bootstrapPage(
  # Application title
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "statistik SCB", id="nav",
             #####################################################
             tabPanel(
               "Befolkningsstatistik",
               titlePanel(title = div(
                 img(src = "img_wsp.png", hight = 20, width = 50),
                 span("SCB R Shiny", style =
                        "color:red")
               )),
               
               hr(),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("lan_tab1", "Välj län",
                               lan$Lan_namn)
                 ),
                 mainPanel(tabsetPanel(
                   tabPanel("Diagram",
                            plotlyOutput(outputId = "hist"),
                            plotlyOutput(outputId = "hist_alder")),
                   tabPanel("Tabell",
                            DT::dataTableOutput("tabell1"))
                 ))
               )
             ), 
             tabPanel(
               "Bilstatistik",
               titlePanel(title = div(
                 img(src = "img_wsp.jpg", hight = 20, width = 50),
                 span("SCB R Shiny", style =
                        "color:red")
               )),
               
               hr(),
               #outputId och selectId måste vara unika
               sidebarLayout(
                 sidebarPanel(
                   selectInput("lan_tab2", "Välj län", # skapa flera inputs, län och fordonsslag. 
                               lan$Lan_namn)
                 ),
                 mainPanel(tabsetPanel(
                   tabPanel("Diagram",
                            plotlyOutput(outputId = "bilinnehav_hist"), 
                            plotlyOutput(outputId = "bilinnehav_tidsserie")),
                   tabPanel("Tabell",
                   )
                 ))
               )
             )
  ))

server <- function(input, output) {
  
  #Kod som står här kommer att köras när appen laddas (mao inga input-variabler kan användas här)
  
  #En reaktiv variabel skapas för att ständigt hålla sig uppdaterad med ny indata
  
  ################################## Flik 1 #################################
  
  rea_scb_befolkning_kommuner <- reactive({
    
    valt_lan <- lan %>%
      filter(Lan_namn == input$lan_tab1) %>%
      select(Lan_kod) %>%
      pull() #gives vector of length 1
    
    kommuner_i_valt_lan <- kommun %>%
      filter(substr(Kommun_kod, 1, 2) == valt_lan) %>%
      select(Kommun_kod) %>%
      pull() #gives vector with kommun codes in lan
    
    
    pxweb_query_list <-
      list("Region" = kommuner_i_valt_lan, # Use "*" to select all
           "Alder" = c('15-19','20-24','25-29','30-34','35-39',
                       '40-44','45-49','50-54','55-59','60-64',
                       '65-69','70-74','75-79','80-'),
           "Kon" = c('1','2'), #1:man, 2:kvinna
           "ContentsCode" = c('000005FF'), #table name in SCB webpage
           "Tid" = c("2019")) #TODO: change to latest?
    
    px_query <- pxweb_query(pxweb_query_list)
    
    px_data <- pxweb_get(table_api_addres_befolkning,px_query)
    
    scb_befolkning <- as.data.frame(px_data,
                                    column.name.type = "text", #can also be "code"
                                    variable.value.type = "text") %>%
      #kolumnerna ska heta: region (chr), age (chr), sex (chr), year (chr), Population per region (chr) %>%
      mutate(region = str_trim(region, side = "left")) %>% #removing blank character
      rename(Region = region,
             Ålder = age,
             Kön = sex,
             Befolkning = "Population per region") %>%
      select(Region, Ålder, Kön, Befolkning) %>%
      mutate(Män = ifelse(Kön == "men", Befolkning, 0),
             Kvinnor = ifelse(Kön == "women", Befolkning, 0)) %>%
      group_by(Region, Ålder) %>%
      summarise(Befolkning = sum(Befolkning),
                Män = sum(Män),
                Kvinnor = sum(Kvinnor)) %>%
      pivot_wider(names_from = Ålder,
                  values_from = Befolkning) %>%
      replace(is.na(.), 0) %>%
      group_by(Region) %>%
      summarise(across(everything(), list(sum))) %>%
      rename_all(~ str_replace(.,"_1","")) %>%
      rename_all(~ str_replace(., "years", "år")) %>%
      mutate(Befolkning = Kvinnor + Män)
    
    #Formaterar och filtrerar Region-kolumnen, samt lägger in namn på kommun och region
    scb_befolkning_kommuner <- scb_befolkning %>%
      mutate(Region = ifelse(is.na(as.numeric(sapply(strsplit(Region, " "), "[[", 1))),
                             Region,
                             as.numeric(sapply(strsplit(Region, " "), "[[", 1)))) %>% #gets numeric part of name
      mutate(Region = as.character(Region)) %>%
      filter(!grepl("\\D", Region)) %>% #regexp that removes non-digit rows?
      filter(nchar(Region)>2) %>%
      mutate(Region = ifelse(nchar(Region)==3,
                             paste0("0",Region),
                             Region)) %>% #adds 0 in front of 3-digit codes
      mutate(Län = substr(Region, 1, 2)) %>%
      left_join(lan, by = c("Län" = "Lan_kod")) %>%
      left_join(kommun, by = c("Region" = "Kommun_kod"))
    
    scb_befolkning_kommuner
  })
  
  
  rea_alder_hist <- reactive({
    #Plockar ut ålderskolumner
    scb_befolkning_alder_hist <- rea_scb_befolkning_kommuner() %>%
      select(ends_with('år'))
    
    #Skapar totalsumma
    alder_hist <- colSums(scb_befolkning_alder_hist) %>%
      bind_rows() %>% #turns vector into tibble
      pivot_longer(cols = everything(),
                   names_to = "Kategori",
                   values_to = "Befolkning")
    
    alder_hist
  })
  
  
  #LYKKE: renderPlotly instead of renderPlot
  
  output$hist <- renderPlotly({
    
    #Här ska allt som är kopplat till histogrammet göras
    
    p <- ggplot(rea_scb_befolkning_kommuner(), aes(Kommun_namn, Befolkning)) +
      geom_bar(stat="identity", width = 0.5, fill="tomato2") +
      labs(title="Befolkning per kommun") +
      theme(text = element_text(size=20), axis.text.x = element_text(angle=65, vjust=0.6))
    
    ggplotly(p)
    
  })
  
  #Här börjar tabellen
  output$tabell1 = DT::renderDataTable({
    rea_scb_befolkning_kommuner()
  },
  filter = "top",
  extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'excel')
  )
  )
  
  output$hist_alder <- renderPlotly({
    
    p <- ggplot(rea_alder_hist(), aes(Kategori, Befolkning)) +
      geom_bar(stat="identity", width = 0.5, fill="tomato2") +
      labs(title="Åldersfördelning i länet") +
      theme(text = element_text(size=20), axis.text.x = element_text(angle=65, vjust=0.6))
    
    ggplotly(p)
  })
  
  ########################### Flik 2 ##########################################
  #Bilinnehav 
  
  rea_scb_bilinnehav_kommuner <- reactive({
    
    #skapa df likt län och kommun för bilinnehavstabell (personbil, lastbil etc)
    
    valt_lan <- lan %>%
      filter(Lan_namn == input$lan_tab2) %>%
      select(Lan_kod) %>%
      pull() #gives vector of length 1
    
    kommuner_i_valt_lan <- kommun %>%
      filter(substr(Kommun_kod, 1, 2) == valt_lan) %>%
      select(Kommun_kod) %>%
      pull() #gives vector with kommun codes in lan
    
    pxweb_query_list <-
      list("Region" = kommuner_i_valt_lan, # Use "*" to select all
           "Fordonsslag" = c('10'), #'10' personbil
           "ContentsCode" = c('TK1001AC'), #table name in SCB webpage
           "Tid" = c("2015", "2016", "2017", "2018", "2019")) #TODO: change to latest?
    
    px_query <- pxweb_query(pxweb_query_list)
    
    px_data <- pxweb_get(table_api_adress_bilinnehav,px_query)
    
    bilinnehav_df <- as.data.frame(px_data,
                                   column.name.type = "text", #can also be "code"
                                   variable.value.type = "text") 
    
    pxweb_query_list <-
      list("Region" = kommuner_i_valt_lan, # Use "*" to select all
           "Alder" = c('15-19','20-24','25-29','30-34','35-39',
                       '40-44','45-49','50-54','55-59','60-64',
                       '65-69','70-74','75-79','80-'),
           "Kon" = c('1','2'), #1:man, 2:kvinna
           "ContentsCode" = c('000005FF'), #table name in SCB webpage
           "Tid" = c("2015", "2016", "2017", "2018", "2019")) #TODO: change to latest?
    
    px_query <- pxweb_query(pxweb_query_list)
    
    px_data <- pxweb_get(table_api_addres_befolkning,px_query)
    
    scb_befolkning <- as.data.frame(px_data,
                                    column.name.type = "text", #can also be "code"
                                    variable.value.type = "text") %>%
      mutate(region = str_trim(region, side = "left")) %>% #removing blank character
      rename(Region = region,
             År = year,
             Kön = sex,
             Befolkning = "Population per region") %>%
      select(Region, År, Kön, Befolkning) %>%
      mutate(Män = ifelse(Kön == "men", Befolkning, 0),
             Kvinnor = ifelse(Kön == "women", Befolkning, 0)) %>%
      group_by(Region, År) %>%
      summarise(Befolkning = sum(Befolkning)) %>%
      pivot_wider(names_from = År,
                  values_from = Befolkning) %>%
      replace(is.na(.), 0) %>%
      group_by(Region) %>%
      summarise(across(everything(), list(sum))) %>%
      rename_all(~ str_replace(.,"_1","")) %>%
      rename_all(~ str_replace(., "years", "år")) %>%
      pivot_longer(cols = c("2015", "2016", "2017", "2018", "2019"), names_to = "År", values_to = "Befolkning")
    
    #Formaterar och filtrerar Region-kolumnen, samt lägger in namn på kommun och region
    scb_befolkning_kommuner <- scb_befolkning %>%
      mutate(Region = ifelse(is.na(as.numeric(sapply(strsplit(Region, " "), "[[", 1))),
                             Region,
                             as.numeric(sapply(strsplit(Region, " "), "[[", 1)))) %>% #gets numeric part of name
      mutate(Region = as.character(Region)) %>%
      filter(!grepl("\\D", Region)) %>% #regexp that removes non-digit rows?
      filter(nchar(Region)>2) %>%
      mutate(Region = ifelse(nchar(Region)==3,
                             paste0("0",Region),
                             Region)) %>% #adds 0 in front of 3-digit codes
      mutate(Län = substr(Region, 1, 2)) %>%
      left_join(lan, by = c("Län" = "Lan_kod")) %>%
      left_join(kommun, by = c("Region" = "Kommun_kod"))
    
    scb_bilinnehav_kommuner <- bilinnehav_df %>% 
      left_join(scb_befolkning_kommuner, by=c("region" = "Kommun_namn", "år" = "År")) %>%
      mutate(Bilinnehav = `Fordon i trafik`/Befolkning) %>%
      filter(år == "2019") 
    
    #Filtrerar på 2019 - Ha kvar df för tidsserieplot. Måste en ny reactive function göras för detta? Känns onödigt komplext.  
    
    scb_bilinnehav_kommuner
  })
  
  rea_scb_bilinnehav_kommuner_tidsserie <- reactive({
    
    #skapa df likt län och kommun för bilinnehavstabell (personbil, lastbil etc)
    
    valt_lan <- lan %>%
      filter(Lan_namn == input$lan_tab2) %>%
      select(Lan_kod) %>%
      pull() #gives vector of length 1
    
    kommuner_i_valt_lan <- kommun %>%
      filter(substr(Kommun_kod, 1, 2) == valt_lan) %>%
      select(Kommun_kod) %>%
      pull() #gives vector with kommun codes in lan
    
    pxweb_query_list <-
      list("Region" = kommuner_i_valt_lan, # Use "*" to select all
           "Fordonsslag" = c('10'), #'10' personbil
           "ContentsCode" = c('TK1001AC'), #table name in SCB webpage
           "Tid" = c("2015", "2016", "2017", "2018", "2019")) #TODO: change to latest?
    
    px_query <- pxweb_query(pxweb_query_list)
    
    px_data <- pxweb_get(table_api_adress_bilinnehav,px_query)
    
    bilinnehav_df <- as.data.frame(px_data,
                                   column.name.type = "text", #can also be "code"
                                   variable.value.type = "text") 
    
    pxweb_query_list <-
      list("Region" = kommuner_i_valt_lan, # Use "*" to select all
           "Alder" = c('15-19','20-24','25-29','30-34','35-39',
                       '40-44','45-49','50-54','55-59','60-64',
                       '65-69','70-74','75-79','80-'),
           "Kon" = c('1','2'), #1:man, 2:kvinna
           "ContentsCode" = c('000005FF'), #table name in SCB webpage
           "Tid" = c("2015", "2016", "2017", "2018", "2019")) #TODO: change to latest?
    
    px_query <- pxweb_query(pxweb_query_list)
    
    px_data <- pxweb_get(table_api_addres_befolkning,px_query)
    
    scb_befolkning <- as.data.frame(px_data,
                                    column.name.type = "text", #can also be "code"
                                    variable.value.type = "text") %>%
      mutate(region = str_trim(region, side = "left")) %>% #removing blank character
      rename(Region = region,
             År = year,
             Kön = sex,
             Befolkning = "Population per region") %>%
      select(Region, År, Kön, Befolkning) %>%
      mutate(Män = ifelse(Kön == "men", Befolkning, 0),
             Kvinnor = ifelse(Kön == "women", Befolkning, 0)) %>%
      group_by(Region, År) %>%
      summarise(Befolkning = sum(Befolkning)) %>%
      pivot_wider(names_from = År,
                  values_from = Befolkning) %>%
      replace(is.na(.), 0) %>%
      group_by(Region) %>%
      summarise(across(everything(), list(sum))) %>%
      rename_all(~ str_replace(.,"_1","")) %>%
      rename_all(~ str_replace(., "years", "år")) %>%
      pivot_longer(cols = c("2015", "2016", "2017", "2018", "2019"), names_to = "År", values_to = "Befolkning")
    
    #Formaterar och filtrerar Region-kolumnen, samt lägger in namn på kommun och region
    scb_befolkning_kommuner <- scb_befolkning %>%
      mutate(Region = ifelse(is.na(as.numeric(sapply(strsplit(Region, " "), "[[", 1))),
                             Region,
                             as.numeric(sapply(strsplit(Region, " "), "[[", 1)))) %>% #gets numeric part of name
      mutate(Region = as.character(Region)) %>%
      filter(!grepl("\\D", Region)) %>% #regexp that removes non-digit rows?
      filter(nchar(Region)>2) %>%
      mutate(Region = ifelse(nchar(Region)==3,
                             paste0("0",Region),
                             Region)) %>% #adds 0 in front of 3-digit codes
      mutate(Län = substr(Region, 1, 2)) %>%
      left_join(lan, by = c("Län" = "Lan_kod")) %>%
      left_join(kommun, by = c("Region" = "Kommun_kod"))
    
    scb_bilinnehav_kommuner_tidsserie <- bilinnehav_df %>% 
      left_join(scb_befolkning_kommuner, by=c("region" = "Kommun_namn", "år" = "År")) %>%
      mutate(Bilinnehav = `Fordon i trafik`/Befolkning) 
    
    scb_bilinnehav_kommuner_tidsserie$år <- as.numeric(scb_bilinnehav_kommuner_tidsserie$år)
    
    scb_bilinnehav_kommuner_tidsserie
  })
  
  output$bilinnehav_hist <- renderPlotly({
    
    #Här ska allt som är kopplat till histogrammet göras
    
    p <- ggplot(rea_scb_bilinnehav_kommuner(), aes(region, Bilinnehav)) +
      geom_bar(stat="identity", width = 0.5, fill="tomato2") +
      labs(title="Bilinnehav per capita") +
      theme(text = element_text(size=20), axis.text.x = element_text(angle=65, vjust=0.6))
    ggplotly(p)
    
  })
  
  output$bilinnehav_tidsserie <- renderPlotly({
    
    #Här ska allt som är kopplat till histogrammet göras
    
    p <- ggplot(rea_scb_bilinnehav_kommuner_tidsserie(), aes(x = år, y = Bilinnehav, color = region)) +
      geom_line() +
      labs(title="Historisk utveckling av bilinnehav") +
      theme(text = element_text(size=20), axis.text.x = element_text(angle=65, vjust=0.6))
    ggplotly(p)
    
  }) 
  
  
}

shinyApp(ui = ui, server = server)
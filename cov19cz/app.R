## app.R ##
library(shinydashboard)
library(shinydashboardPlus)
library(shiny)
library(tidyverse)
library(readxl)
library(ggthemes)
library(shinyWidgets)
library(shinycssloaders)
library(dashboardthemes)
library(RCzechia)
library(leaflet)
library(sf)
library(RJSONIO)
library(deSolve)


my_colors <- c("#DC143C","#F9A828", "#36648B", "#8B1C62", "#00868B", "#698B69", "#CDC673",
               "#8B5A00", "#EE9572", "#483D8B", "#7A378B", "#CD69C9", "#FFB6C1", "#00C78C",
               "#68838B", "#EE7600")


url <- "https://docs.google.com/spreadsheets/d/1fnv361xjPRxOvIbRf-VXxorTE0jAlYQWVVTVrzM43Js/export?format=csv&id=1fnv361xjPRxOvIbRf-VXxorTE0jAlYQWVVTVrzM43Js&gid=1516272200"
dfWorldCOV19 <- read_csv(url)
dfEuropeCOV19 <- dfWorldCOV19 %>% filter(Region=="Europe")
dfWorldCOV19Death <- as.data.frame(dfWorldCOV19) %>% arrange(desc(Úmrtí)) %>% select(Země,Úmrtí,Region)
dfEuropeCOV19Death <-  dfWorldCOV19Death %>% filter(Region=="Europe")

cz <- dfEuropeCOV19 %>% filter(Země=="Czech Republic (Czechia)")
czEu <- rbind(dfEuropeCOV19[0:10, ], cz)
czW <- rbind(dfWorldCOV19[2:11, ], cz)


df = rjson::fromJSON(file="https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true")

tdf2 <- data.frame(matrix(unlist(df$infectedDaily), nrow=length(df$infectedDaily), byrow=T))
tdf2$X1 <- as.numeric(as.character(tdf2$X1))
tdf2$X2 <- as.Date(as.character(tdf2$X2))
tdf2 <- tdf2[35:nrow(tdf2),]
names(tdf2) <- c("Pocet", "Den")



cz <- RCzechia::kraje("low")
tdf1 <- data.frame(matrix(unlist(df$infectedByRegion), nrow=length(df$infectedByRegion), byrow=T))
names(tdf1) <- c("NAZ_CZNUTS3", "Pocet")
tdf1$NAZ_CZNUTS3 <- as.character(tdf1$NAZ_CZNUTS3)
tdf1$Pocet <- as.numeric(as.character(tdf1$Pocet))
dc <- cz %>% inner_join(tdf1, by = "NAZ_CZNUTS3")

drops <- c("Covid země", "Přírůstek obyvatel", "Rozloha (km2)", "Porodnost",
           "Hustota obyvatelstva (lidí / km2)", "Počet migrantů","Střední věk", "Z toho ve městech" ,
           "Úmrtí p.m. ve městech", "Nakažených p.m. ve městech" )




dfworld <- dfWorldCOV19[ , !(names(dfWorldCOV19) %in% drops)]


# poc <- c(df[[2]],df[[3]])
# year <- c("CZ", "CZ")
# cond <- c("nakazeny","uzdraveny")
# nak <- data_frame(cond,poc,year)


inf <- tdf2[tdf2$Pocet != 0, ]
Infected <- inf$Pocet
Day <- 1:(length(Infected))
N <- 10000000 # population of mainland china

old <- par(mfrow = c(1, 2))

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

init <- c(S = N-Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[ , 3]
  sum((Infected - fit)^2)
}

Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions

Opt_par <- setNames(Opt$par, c("beta", "gamma"))


t <- 1:70 # time in days
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
col <- 1:3 # colour


R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")

fit[fit$I == max(fit$I), "I", drop = FALSE] # height of pandemic





ui <- dashboardPagePlus(collapse_sidebar = FALSE,
                        
                        tags$style(HTML(".main-sidebar { font-size: 30px!important; }")), 
                        
                        tags$head(includeHTML(("google-analytics.html"))),
                        
                        
                        header = dashboardHeaderPlus(title = tagList(
                          span(class = "logo-lg", "COVID19 Czechia"), 
                          img(src = "https://image.flaticon.com/icons/svg/204/204074.svg")), enable_rightsidebar = FALSE, rightSidebarIcon = "gears"),
                        
                        sidebar = dashboardSidebar(
                          sidebarMenu(
                            menuItem("Celkem / Total CZ", tabName = "dashboard", icon = icon("dashboard")),
                            menuItem("Denně / Daily CZ", tabName = "summary" , icon = icon("calendar")),
                            
                            menuItem("Mapa / Map ", tabName = "map" , icon = icon("map-marker-alt")),
                            menuItem("Evropa / Europe", tabName = "europe" , icon = icon("euro-sign")),
                            menuItem("Evropa Úmrtí / Europe Deaths", tabName = "europed" , icon = icon("cross")),
                            
                            menuItem("Svět / World", tabName = "world" , icon = icon("globe")),
                            menuItem("Svět Úmrtí / World Deaths", tabName = "worldd" , icon = icon("cross")),
                            menuItem("Totals", tabName = "totals" , icon = icon("chart-area")),
                            
                            menuItem("Předpověď / Prediction CZ", tabName = "prediction" , icon = icon("bar-chart-o")),
                            menuItem("SIR", tabName = "sir" , icon = icon("calendar-alt"))
                            
                            
                            
                            
                          ),
                          collapsed = FALSE
                        ),
                        
                        
                        body = dashboardBody(
                          
                          
                          
                          
                          
                          ### changing theme
                          shinyDashboardThemes(
                            theme = "grey_light"
                          ),
                          
                          tabItems(
                            
                            
                            # First tab content
                            tabItem(tabName = "dashboard",
                                    
                                    fluidRow(
                                      
                                      #boxPlus(plotlyOutput("chart1", height =300) %>% withSpinner(type = "5"), width = 4),
                                      
                                      boxPlus(background = "blue" , descriptionBlock(
                                        header = h4(df$totalTested, style="color:white"), 
                                        text =  h5("Testovaných", style="color:white"), 
                                        right_border = FALSE,
                                        margin_bottom = FALSE
                                      ), width = 3),
                                      
                                      boxPlus(background = "red",
                                              descriptionBlock(
                                                header = h4(df[[2]], style="color:white"), 
                                                text = h5("Nakažených", style="color:white") , 
                                                right_border = FALSE,
                                                margin_bottom = FALSE
                                              ), width = 3),
                                      
                                      boxPlus(background = "green",
                                              descriptionBlock(
                                                header = h4(df[[3]], style="color:white"), 
                                                text = h5("Uzdraveno", style="color:white"), 
                                                right_border = FALSE,
                                                margin_bottom = FALSE
                                              ), width = 3),
                                      
                                      boxPlus(background = "black",
                                              descriptionBlock(
                                                header = h4(span(df$deceased, style="color:white")), 
                                                text = h5("úmrtí", style="color:white"), 
                                                right_border = FALSE,
                                                margin_bottom = FALSE
                                              ), width = 3)
                                    ),
                                    
                                    fluidRow(boxPlus(plotOutput("chart2", height = 680) %>% withSpinner(type="5"), width = 12)),
                                    
                                    h5("AKTUALIZOVANO  :  ", df$lastUpdatedAtSource),
                                    p("Source", "https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true")
                                    
                            ),
                            
                            
                            tabItem(tabName = "summary",
                                    
                                    fluidRow(boxPlus(plotOutput("chart3", height = 680) %>% withSpinner(type="5"), width = 12),
                                             #boxPlus(plotOutput("chart4", height =600) %>% withSpinner(type = "5"), width = 4)
                                    ),
                                    h5("AKTUALIZOVANO  :  ", df$lastUpdatedAtSource),
                                    p("Source", "https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true")
                                    
                                    
                            ),
                            
                            
                            
                            tabItem(tabName = "prediction",
                                    
                                    fluidRow(boxPlus(plotOutput("chart5", height = 680) %>% withSpinner(type="5"), width = 12)),
                                    h5("AKTUALIZOVANO  :  ", df$lastUpdatedAtSource),
                                    p("Source", "https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true")
                                    
                                    
                            ),
                            
                            tabItem(tabName = "map",
                                    
                                    fluidRow(boxPlus(plotOutput("chart1", height = 680) %>% withSpinner(type="5"), width = 12)),
                                    h5("AKTUALIZOVANO  :  ", df$lastUpdatedAtSource),
                                    p("Source", "https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true")
                                    
                                    
                            ),
                            
                            tabItem(tabName = "europe",
                                    
                                    fluidRow(boxPlus(plotOutput("chart10", height = 680) %>% withSpinner(type="5"), width = 12)),
                                    h5("AKTUALIZOVANO  :  ", df$lastUpdatedAtSource),
                                    p("Source: Tomáš Kapler ", "https://docs.google.com/spreadsheets/d/1fnv361xjPRxOvIbRf-VXxorTE0jAlYQWVVTVrzM43Js/edit#gid=1516272200")
                                    
                                    
                            ),
                            
                            tabItem(tabName = "europed",
                                    
                                    fluidRow(boxPlus(plotOutput("chart13", height = 680) %>% withSpinner(type="5"), width = 12)),
                                    h5("AKTUALIZOVANO  :  ", df$lastUpdatedAtSource),
                                    p("Source: Tomáš Kapler ", "https://docs.google.com/spreadsheets/d/1fnv361xjPRxOvIbRf-VXxorTE0jAlYQWVVTVrzM43Js/edit#gid=1516272200")
                                    
                                    
                            ),
                            
                            tabItem(tabName = "world",
                                    
                                    fluidRow(boxPlus(plotOutput("chart11", height = 680) %>% withSpinner(type="5"), width = 12)),
                                    h5("AKTUALIZOVANO  :  ", df$lastUpdatedAtSource),
                                    p("Source: Tomáš Kapler ", "https://docs.google.com/spreadsheets/d/1fnv361xjPRxOvIbRf-VXxorTE0jAlYQWVVTVrzM43Js/edit#gid=1516272200")
                                    
                                    
                            ),
                            
                            tabItem(tabName = "worldd",
                                    
                                    fluidRow(boxPlus(plotOutput("chart12", height = 680) %>% withSpinner(type="5"), width = 12)),
                                    h5("AKTUALIZOVANO  :  ", df$lastUpdatedAtSource),
                                    p("Source: Tomáš Kapler ", "https://docs.google.com/spreadsheets/d/1fnv361xjPRxOvIbRf-VXxorTE0jAlYQWVVTVrzM43Js/edit#gid=1516272200")
                                    
                                    
                            ),
                            
                            tabItem(tabName = "totals",
                                    
                                    fluidRow(boxPlus(DT::dataTableOutput("tbl1") %>% withSpinner(type="5"), width = 12)),
                                    h5("AKTUALIZOVANO  :  ", df$lastUpdatedAtSource),
                                    p("Source: Tomáš Kapler ", "https://docs.google.com/spreadsheets/d/1fnv361xjPRxOvIbRf-VXxorTE0jAlYQWVVTVrzM43Js/edit#gid=1516272200")
                                    
                                    
                            ),
                            
                            tabItem(tabName = "sir",
                                    
                                    fluidRow(boxPlus(plotOutput("chart6", height = 360) %>% withSpinner(type="5"), width = 6), 
                                             boxPlus(plotOutput("chart7", height = 360) %>% withSpinner(type = "5"), width =6)),
                                    
                                    fluidRow(boxPlus(plotOutput("chart8", height = 360) %>% withSpinner(type="5"), width = 6), 
                                             boxPlus(plotOutput("chart9", height = 360) %>% withSpinner(type = "5"), width = 6)),
                                    
                                    
                                    
                                    
                                    
                            )
                            
                            
                            
                            
                          ),
                          
                          title = "Covid19-CZ"
                          
                        )
)


server <- function(input, output, session) {
  
  
  output$chart1 <- renderPlot(ggplot(data = dc) +
                                geom_sf(aes(fill=Pocet)) +
                                scale_fill_continuous(high = "#DC143C", low = "goldenrod")+
                                geom_sf_text(aes(label=Pocet), size=8)+
                                theme_wsj()+
                                ggtitle("Nakaženo v Kraji / Infected by Region")+
                                
                                theme(axis.text = element_text(colour = "white"),
                                      axis.ticks = element_line(colour = "white"),
                                      plot.background = element_rect(fill = "white", color="black"),
                                      panel.background = element_rect(fill ="white", color = "#17202A"),
                                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                      axis.line = element_blank(),
                                      plot.title = element_text(size = "25"),
                                      legend.position = "none", 
                                      
                                )
                             
                              
                              
                              
                              , bg="white"
                              
                              
                              
                              
                              
  )
  
  
  
  
  
  
  output$chart2 <- renderPlot(ggplot(dc, aes(x=reorder(NAZ_CZNUTS3, Pocet), y=Pocet, fill=NAZ_CZNUTS3))+
                                geom_bar(stat = "identity")+ coord_flip()+ggtitle("Nakažených v Kraji / Infected by Region")+
                                geom_text(aes(label=Pocet), hjust = 0, color="black", size=8)+
                                theme_wsj()+xlab("")+
                                theme(
                                  panel.background = element_rect(fill ="white", color = "#17202A"),
                                  plot.background = element_rect(fill = "white", color="#17202A"),
                                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                  legend.position = "none",axis.title.x=element_blank(), axis.text=element_text(size=14, colour = "black"),
                                  plot.title = element_text(color="black", size=25)
                                )+scale_fill_manual(values = (my_colors))
  )
  
  output$chart3 <- renderPlot(ggplot(tdf2, aes(x=Den, y=Pocet))+geom_line(lwd=3, color="goldenrod")+
                                ggtitle("Nakažených za den / Infected per day")+theme_wsj()+
                                theme(panel.background = element_rect(fill ="white", color = "#17202A"),
                                      plot.background = element_rect(fill = "white", color="#17202A"),
                                      panel.grid.minor = element_blank(),
                                      legend.position = "none",plot.title = element_text(color="#17202A"),
                                      axis.title.x=element_blank())+
                                geom_point(aes(), size=8, color="#4682B4")+
                                geom_text(aes(label=Pocet),hjust=0, vjust=0, color="black", size=7)
                              
                              
                              
  )
  
  output$chart4 <- renderPlot(ggplot(nak, aes(x=year, y=poc, fill=cond))+
                                ggtitle("Nakažených / Uzdravených  Infected / Recovered  ")+
                                geom_col()+scale_fill_manual(values = c("#DC143C","#66CDAA"))+
                                geom_text(aes(label = paste0(poc)), position = position_stack(vjust = 0.7), size=14)+
                                #scale_fill_brewer(palette = "Set2") +
                                theme_minimal(base_size = 16) +
                                ylab("") +
                                xlab(NULL)+
                                theme(#panel.background = element_rect(fill ="#17202A", color = "#17202A"),
                                      #plot.background = element_rect(fill = "#17202A", color="#17202A"),
                                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                      legend.position = "none",plot.title = element_text(color="white", size=25),
                                      axis.title.x=element_blank())
                              
                              
                              
                              
  )
  
  output$chart5 <- renderPlot(ggplot(fit[15:40,], aes(time, I))+
                                geom_line(lwd=3, color="#00868B")+
                                geom_point(aes(), color="#DC143C", size=6)+
                                ggtitle("SIR model prediction COV19CZ")+xlab("Days")+ ylab("Infected")+ theme_minimal()+
                                geom_text(aes(label=ceiling(I)), hjust=1, vjust=0, color="black", size=7)+
                                theme(#panel.background = element_rect(fill ="goldenrod", color = "black"),
                                      #plot.background = element_rect(fill = "#DC143C", color="black"),
                                      legend.position = "none",plot.title = element_text(color="#17202A"),
                                      panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
                                      panel.grid.major = element_line(
                                        colour = "black",
                                        size = 1,
                                        linetype = "dotted",
                                        lineend = NULL,
                                        color = NULL,
                                        arrow = NULL,
                                        inherit.blank = FALSE
                                      ), axis.text = element_text(size=15, colour = "black"),
                                      axis.title = element_text(size = 15),
                                      title = element_text(size=25)
                                      
                                      
                                      
                                )
                              
                              
                              
  )
  
  
  output$chart6 <- renderPlot({plot(Day, Infected, type ="b")
    title("Confirmed Cases 2019-nCoV Czechia", outer = TRUE, line = -2)
    
  }) 
  
  output$chart7 <- renderPlot({plot(Day, Infected, log = "y")
    abline(lm(log10(Infected) ~ Day))
    title("Confirmed Cases 2019-nCoV Czechia log10", outer = TRUE, line = -2)
    
  })
  
  
  output$chart8 <- renderPlot({matplot(fit$time, fit[ , 3], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
    points(Day, Infected)
    legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"), lty = 1, lwd = 2, col = col, inset = 0.05)
    title("SIR simulation model 2019-nCoV Czechia", outer = TRUE, line = -2)
    
    
    
  })
  
  output$chart9 <- renderPlot({matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
    points(Day, Infected)
    legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"), lty = 1, lwd = 2, col = col, inset = 0.05)
    title("SIR model 2019-nCoV Czechia", outer = TRUE, line = -2)
    
    
  })
  
  
  output$chart10 <- renderPlot({ggplot(czEu, aes(x=reorder(Země, `Potvrzených nakažených`), fill=Země,
                                                                 y=`Potvrzených nakažených`))+
      geom_bar(stat = "identity")+
      coord_flip()+
      ggtitle("Evropa Nakažených Celkem / Europe Infected Total")+
      geom_text(aes(label=`Potvrzených nakažených`), hjust = 0, color="black", size=7)+
      theme_wsj()+xlab("")+
      theme(
        panel.background = element_rect(fill ="white", color = "#17202A"),
        plot.background = element_rect(fill = "white", color="#17202A"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none",axis.title.x=element_blank(), axis.text=element_text(size=14, colour = "black"),
        plot.title = element_text(color="black", size=25)
      )+scale_fill_manual(values = (my_colors))
    
    
  })
  
  
  output$chart11 <-  renderPlot({ggplot(czW, aes(x=reorder(Země, `Potvrzených nakažených`), fill=Země,  y=`Potvrzených nakažených`))+
      geom_bar(stat = "identity")+
      coord_flip()+
      ggtitle("Svět Nakažených Celkem / World Infected Total")+
      geom_text(aes(label=`Potvrzených nakažených`), hjust = 0, color="black", size=7)+
      theme_wsj()+xlab("")+
      theme(
        panel.background = element_rect(fill ="white", color = "#17202A"),
        plot.background = element_rect(fill = "white", color="#17202A"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none",axis.title.x=element_blank(), axis.text=element_text(size=14, colour = "black"),
        plot.title = element_text(color="black", size=25)
      )+scale_fill_manual(values = (my_colors))
    
    
      
    
  })
  
  output$chart12 <- renderPlot({ggplot(dfWorldCOV19Death[2:12,], aes(x=reorder(Země, Úmrtí), y=Úmrtí, fill=Země))+
      geom_bar(stat = "identity")+
      coord_flip()+ggtitle("Svět Úmrtí Celkem / World Deceased Total")+
      geom_text(aes(label= Úmrtí), hjust = 0, color="black", size=7)+
      theme_wsj()+xlab("")+
      theme(
        panel.background = element_rect(fill ="white", color = "#17202A"),
        plot.background = element_rect(fill = "white", color="#17202A"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none",axis.title.x=element_blank(), axis.text=element_text(size=14, colour = "black"),
        plot.title = element_text(color="black", size=25)
      )+scale_fill_manual(values = (my_colors))
    
    
    
  })
  
  
  output$chart13 <- renderPlot({ggplot(dfEuropeCOV19Death[0:10,], aes(x=reorder(Země, Úmrtí), y=Úmrtí, fill=Země))+
      geom_bar(stat = "identity")+
      coord_flip()+ggtitle("Evropa Úmrtí Celkem / Europe Deceased Total")+
      geom_text(aes(label= Úmrtí), hjust = 0, color="black", size=7)+
      theme_wsj()+xlab("")+
      theme(
        panel.background = element_rect(fill ="white", color = "#17202A"),
        plot.background = element_rect(fill = "white", color="#17202A"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none",axis.title.x=element_blank(), axis.text=element_text(size=14, colour = "black"),
        plot.title = element_text(color="black", size=25)
      )+scale_fill_manual(values = (my_colors))
    
    
    
    
    
  })
  
  output$tbl1 <- DT::renderDataTable(dfworld)
  
  
}

shinyApp(ui, server)

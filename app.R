library(shiny)
library(tidyverse)
library(lubridate)
library(highcharter)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(shinyWidgets)

#Note:
#This app performs a monte carlo simulation of Covid-19, based on the initial status of the population calculated from the data that the
#user can download from an API. The methodology of this application was derived from the paper found here:
#https://www.medrxiv.org/content/10.1101/2020.12.03.20243220v3.full-text
#The simulation was originally designed to be performed once, and to fit the data regarding the epidemic in the Switzerland, so some
#significant changes were made to the original methodology in order for the simulation to run faster, and to better fit the data about the USA.
#The base values in the simulation were tuned based on the pandemic in Missisipi, so we can't guarantee that the simulation will
#behave realistically for other states, however it will work, and with some tuning of the parameters should perform well. The original design
#of the simulation also didn't take different variants of the virus into account, so when an individual becomes recovered from the disease
#they are immune to it for the rest of the simulation.
#Please have in mind, that the simulation is quite computationally intense, and might take some time (we expect around 1 minute for <10 days
# and around 5-8 minutes for 30 days depending on the computer running it), so please wait some time to see the results. In our opinion
#the best way to see the results would be for 30 days, as the simulation was originally intended to run for a 100 days, but we understand that
#you may not have the time to run it for that long.
#Parameters settable by the user:
# the state the simulation is based on
# the start and end dates of the data extracted from the API
# the number of days to predict in the simulation
# Infection chance parameter - The lower this value the faster the infections spread (we recommend changing by about 0.025 to see significant effects)
# Duration parameter - the higher this parameter the longer the disease last, and therefore the longer the subject infects other subjects
#
#Please mind that the simulation was artificially made less realistic in order for the results to compute faster, and the changes in the 
#infections to be more noticible.

states_dict <- read.csv("states_dict.csv")
choices = setNames(states_dict$code, states_dict$state)

densities <- read.csv("pop_dens.txt", sep = ",") %>% select(State = GEO.display.label, density = Density.per.square.mile.of.land.area)
populations <- read.csv("nst-est2019-01.csv", sep = ";")

states_dict <- left_join(states_dict, densities, by = c("state"="State"))
states_dict <- left_join(states_dict, populations, by = c("state"="Geographic.Area")) %>% select(state, code, density, population = X2019)
states_dict <- na.omit(states_dict)


theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}

theme_transparent <- theme(
  panel.background = element_rect(fill='transparent'), #transparent panel bg
  plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
  panel.grid.major = element_blank(), #remove major gridlines
  panel.grid.minor = element_blank(), #remove minor gridlines
  legend.background = element_rect(fill='transparent'), #transparent legend bg
  legend.box.background = element_rect(fill='transparent'), #transparent legend panel
  axis.title.x=element_text(colour="white"),
  axis.title.y=element_text(colour="white"),
  plot.title = element_text(colour = "white")
)

##########################
##### User interface #####
##########################
ui <- fluidPage(theme = shinytheme("cyborg"),
                chooseSliderSkin("Shiny", color = "red"),
                navbarPage(title=span(img(src="covid.png", height=25),"Advanced Programming in R: Project"),
                           tabPanel("Main",
                                    titlePanel(h5("COVID-19 Monte Carlo Simulation"),
                                      tags$head(tags$link(rel = "shortcut icon", href = "covid.png"))
                                    ),
                                    sidebarLayout(
                                      sidebarPanel(
                                        tags$style("#variable {color: white;}"),
                                        selectInput(
                                          "variable", "State:",
                                          choices,
                                          selected = 'ms'
                                        ),
                                        
                                        dateRangeInput(
                                          'dateRange',
                                          label = paste('Date range:'),
                                          start = as.Date("2020-07-01"), end = as.Date("2020-07-30"),
                                          min = as.Date("2020-03-04"), max = as.Date("2021-03-07"),
                                          separator = " to ", format = "dd/mm/yyyy",
                                          startview = 'month', weekstart = 1
                                        ),
                                        sliderInput("obs", "Number of days to predict:",
                                                    min = 6, max = 30, value = 10
                                        ),
                                        
                                        splitLayout(
                                          cellWidths = "50%",
                                          cellArgs = list(style = "padding: 10px"),
                                          numericInput("dur_param", "Duration parameter:",
                                                       value = 0.6, min = 0, max = 1, step = 0.1, width = "100%"
                                          ),
                                          numericInput("inf_chan_param", "Infection chance parameter:",
                                                       value = 0.125, min = 0, max = 1, step = 0.005, width = "100%"
                                          )
                                        ),
                                        actionButton("submit", icon = icon("bacteria"),label = "Submit", style="margin-bottom:20px;"),
                                        p(strong(em("NOTE: To download the data and start the simulation, click on the Submit button!")))
                                      ),
                                      mainPanel(
                                        plotlyOutput("plot1") %>% withSpinner(color="red"),
                                        img(src="covid.png")
                                      ),
                                      
                                    )
                           ),
                           
                           tabPanel("Simulations",
                                    titlePanel(
                                      h3("Simulations", style = "padding-bottom: 20px")
                                    ),
                                    sidebarLayout(position = "left",
                                                  sidebarPanel(p(strong("Description of the plots:")), br(),
                                                               p("1. This plot shows the number of Susceptible, Infected and Recovered simulation subjects. It is mostly useful when analysing the productive capacity of the analysed population, as it allows to see the number of individuals that are sick compared to non-sick individuals at any given time.", style="text-align: justify;"),
                                                               br(),
                                                               p("2. The second plot shows how the epidemic is developing, by showing the daylight new cases of the sickness. Here one could see wether a peak in the epidemic has been reached, and how many of them there were. This simulation does not consider variants of a disease, so the number of peaks should be 1.", style="text-align: justify;"),
                                                               br(),
                                                               p("3. This graph presents the 5-day reproductive rate. It is a metric closely related to the well known R0 (instant reproductive rate), and it describes the number of susceptible individuals an infected individual is expected to infect during the illness, as can be derived from the preceding 5 day period. In order for the epidemic to stop spreading, the Re value has to fall below 1 (Re < 1 means that individuals are recovering quicker than they are infecting), hence why the line at Re = 1 is visible on the graph.", style="text-align: justify;"),
                                                               br(),
                                                               p(em(strong("Disclamer: ")), "Simulations accuracy strongly depends on the given parameters. Our aim was to create a tool that would simulate the course of a pandemic. We entrust the selection of appropriate parameters to the user of the application."),
                                                               br(),
                                                               p(strong(em("NOTE: Please note that the simulation may take some time! Please be patient :)")))
                                                               ),
                                                  mainPanel(
                                                    tags$div(
                                                      style="margin-bottom:100px;",
                                                      plotlyOutput("plot2") %>% withSpinner(color="red")
                                                    ),
                                                    tags$div(
                                                      style="margin-bottom:100px;",
                                                      plotlyOutput("plot3") %>% withSpinner(color="red")
                                                    ),
                                                    tags$div(
                                                      style="margin-bottom:100px;",
                                                      plotlyOutput("plot4") %>% withSpinner(color="red")
                                                    )
                                                  )
                                    )
                           ),
                           tabPanel("API Raw Data",
                                    titlePanel(
                                      h3("Dataset", style = "padding-bottom: 20px")
                                    ),
                                    sidebarLayout(position = "left",
                                                  sidebarPanel(p("On the right we show the raw dataframe that has been downloaded using API."),
                                                               
                                                               p("If you don't see the table please click", strong("Submit"), "button in the", strong("Main"), "page."), width = 2, style="text-align: justify;"),
                                                  mainPanel(
                                                    dataTableOutput(outputId = "dataTable") %>% withSpinner(color="red")
                                                  )
                                    )
                           )
                )
)

###########################
##### Server function #####
###########################
server <- function(input, output, session) {
  
  
  api_data <- eventReactive(input$submit, {
    api_link <- paste0("https://api.covidtracking.com/v1/states/", input$variable, "/daily.csv")
    api_data = read.csv(api_link)
    api_data$date = as.Date(strptime(api_data$date, "%Y%m%d"), format = "%Y-%m-%d")
    api_data <- api_data %>%
      filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    #api_data <- as.data.frame(api_data)
    api_data
  })
  
  
  
  data_prep <- eventReactive(input$submit, {
    
    
    state_code <- input$variable
    api_data <- api_data()
    df <- states_dict
    
    if(!(state_code %in% states_dict$code)){
      stop('Please input a correct state code')
    }
    
    #windsorization of the population densities to the lowest 25 percentile and highest 90 percentile, in order to avoid 
    #outliers in the simulation
    dens_quantiles <- df %>% summarise(quantile = quantile(density, probs = c(0.25,0.9)))
    
    df <- df %>% mutate(density1 = ifelse(df$density < dens_quantiles$quantile[["25%"]], dens_quantiles$quantile[["25%"]], density))
    df <- df %>% mutate(density_win = ifelse(df$density1 > dens_quantiles$quantile[["90%"]], dens_quantiles$quantile[["90%"]], density1))
    
    #in order for the simulation to look better, we scale the population to 5%, however in a real world application this would not
    #be present
    df <- df %>% mutate(multip = 0.05 * population / 500)
    
    df_state <- filter(df, code == state_code)
    
    #retrieving the data about the infections from api. We assume that the individuals infected during the last 18 days are the ones
    #currently sick. If the data is shorter than 18 days, we scale the infections up, in order to ensure simualation consistency
    if(nrow(api_data >= 18)) {
      api_for_symul <- head(api_data, n=18)
      
      api_now <- api_for_symul[1,]
      api_start <- api_for_symul[18,]
      
      sym_input <- data.frame(Infected = 0,
                              Recovered = 0,
                              Susceptible = 0)
      sym_input$Infected <- max(1,floor((api_now$positive - api_start$positive)/df_state$multip))
    }
    else {
      api_for_symul <- api_data
      
      api_now <- api_for_symul[1,]
      api_start <- api_for_symul[nrow(api_data),]
      
      sym_input <- data.frame(Infected = 0,
                              Recovered = 0,
                              Susceptible = 0)
      sym_input$Infected <- max(1,floor((api_now$positive - api_start$positive)/df_state$multip*(18/nrow(api_data))))
    }
    
    #To estimate number of recovered individuals, we multiply the deaths by 15 (this means we are assuming 7.5% death rate), as this
    #is the only metric which allows us to estimate the number of individuals who have been through the illness.
    sym_input$Recovered <- floor(api_now$death*15/df_state$multip)
    sym_input$Susceptible <- 500 - sym_input$Recovered - sym_input$Infected
    
    sym_input$size_multip <- mean(df$density_win) / df_state$density_win
    
    sym_input$pop_multip <- df_state$multip
    
    
    
    
    
    return(sym_input)
  })
  
  simulate <- eventReactive(input$submit, {
    
    S_init <- data_prep()$Susceptible
    I_init <- data_prep()$Infected
    R_init <- data_prep()$Recovered
    size_multip <- data_prep()$size_multip
    
    if(!is.numeric(S_init)){
      stop('S_init is not a number')
    }
    if(!is.numeric(size_multip)){
      stop('state size multiplier is not a number')
    }
    day_num = input$obs
    
    duration_param <- input$dur_param
    infection_chance_param <- input$inf_chan_param
    
    #scaling the length of the area where the simulation takes place by the multiplier dependant on the state density
    length <- 1000/7*size_multip
    # initial dataframe will be storing current information about all of the subjects
    df <- data.frame(matrix(ncol = 7, nrow=500))
    colnames(df) <- c("id", "status","recovery", "incubation",  "pos_X", "pos_Y", "dist_to_sick")
    
    df$status <- "S"
    df$id <- 1:500
    df$pos_X <- runif(500, min=0, max=length)
    df$pos_Y <- runif(500, min=0, max=length)
    #A dataframe that will track the 3 main parameters of the SIR model is created. It will be later used for the analysis.
    S <- c(S_init)
    I <- c(I_init)
    R <- c(R_init)
    SIR <- data.frame(S,I,R)
    
    #Setting up incubation and recovery periods for all subjects. A day will be subtracted from both during the infection. A subject
    # can infect others only if there are at most 3 days left in the incubation period (and the subject is infected).
    time <- c()
    for(i in 1:500){
      time<- append(time,ceiling(qgamma(runif(1), 6, rate = duration_param)))
    }
    
    # mean is about 9, but contrary to what authors claimed the standard deviation is 3.6 rather than 4.9
    # since the numbers must be integers, the values will be rounded up, to simulate worse state of the medical care in the US than in Switzerland
    df$incubation <- time
    df$recovery <- time*2
    
    #assigning the initial statuses to the subjects.
    for(i in 1:(I_init)){
      df[i,2]="I"
      df[i,4]=df[i,4]-5
    }
    for(i in (I_init+1):(R_init+I_init)){
      df[i,2]="R"
      df[i,4]=df[i,4]-5
    }
    
    
    #the simulation lasts day_num days. The values are therefore calculated iteratively. The order of operations each day is as follows:
    # subjects take their step -> the distance to the closest sick person is calculated -> the dice is rolled to check wether the person becomes infected ->
    # the recovery and incubation times are progressed by 1 day -> SIR values are calculated -> the day ends
    
    for (day in 1:day_num) {
      #step calculations
      distance <-c()
      direction <- c()
      stepXini <- c()
      stepYini <- c()
      stepXprop <- c()
      stepYprop <- c()
      
      for (subject in 1:500){
        dis <- qnorm(runif(1),250,83.333)
        dire <- runif(1)*2*3.14
        X <- sin(dire)*dis
        Y <- cos(dire)*dis
        distance <- append(distance, dis)
        direction <- append(direction, dire)
        stepXini <- append(stepXini, X)
        stepYini <- append(stepYini, Y)
        if (df[subject,5] + X >length){
          X <- -X
        }
        if (df[subject,6] + Y >length){
          Y <- -Y
        }
        stepXprop <- append(stepXprop,X)
        stepYprop <- append(stepYprop,Y)
      }
      df$direction <- direction
      df$step <- distance
      df$stepXini <- stepXini
      df$stepYini <- stepYini
      df$stepX <- stepXprop
      df$stepY <- stepYprop
      #subjects taking the step and changing the location
      for(i in 1:500){
        df[i,5]=df[i,5]+df[i,12]
        df[i,6]=df[i,6]+df[i,13]
      }
      #looking for the closest sick person
      for (i in 1:500){
        if(df[i,2]=="S"){
          closest <- 99999999
          for (j in 1:500){
            if (df[j,2]=="I" & i!=j & df[j,4]<=3){
              space <- sqrt(((df[i,5]-df[j,5])**2)+((df[i,6]-df[j,6])**2))
              if(space<closest){
                closest <- space
              }
            }
          }
          df[i,7]=closest
          #cheching if the subject becomes infected
          if (closest<qexp(runif(1), rate = infection_chance_param)){
            df[i,2]="I"
          }
        }
      }
      #decraesing the incubation and recovery timers
      for (i in 1:500){
        if(df[i,2]=="I"){
          df[i,3]=df[i,3]-1
          df[i,4]=df[i,4]-1
          if(df[i,3]==0){
            df[i,2]="R"
          }
        }
      }
      
      #counting Susceptible, Infected and Recovered subjects
      c<-count(df,df$status)
      v <- c(c[3,2],c[1,2],c[2,2]-1)
      SIR <- rbind(SIR,v)
      #End of a day
      
    }
    
    #population multiplier is saved in the final dataframe
    SIR$pop_multip <- data_prep()$pop_multip
    
    #dates of the simulated days are saved in the final dataframe
    SIR$date <- seq.Date(from = as.Date(input$dateRange[2]),
                         to = as.Date(input$dateRange[2]) + input$obs,
                         by = "day")
    
    return(SIR)
  })
  
  
  output$dataTable <- renderDataTable({
    api_data <- api_data()
    api_data
  })
  
  output$plot1 <- renderPlotly({
    api_data <- api_data()
    plt1 <- ggplotly(ggplot(api_data, aes(x = date, y = positiveIncrease)) +
                       labs(title = states_dict$state[states_dict$code == input$variable], x = "Time", y = "New daily cases") + 
                       theme_transparent +
                       geom_line(color="white", lwd=0.5)
                     #geom_point(color="grey")
    )
    plt1
  })
  
  output$dataTable2 <- renderDataTable({
    api_data <- api_data()
    data_prep <- data_prep(input$variable, api_data)
    data_prep
  })
  
  
  
  output$plot2 <- renderPlotly({
    SIR <- simulate()
    #This plot will show the number of infected, susceptible and recovered individuals over time of the simulation
    plt2 <- ggplotly(ggplot(SIR, aes(x = date)) +
                       labs(title = "Status of subjects over time", x = "Time", y = "S-green, I-red, R-blue") +
                       geom_line(aes(y = S), color = "green", lwd=0.5) +
                       geom_line(aes(y = I), color = "red", lwd=0.5) +
                       geom_line(aes(y = R), color = "blue", lwd=0.5) +
                       theme_transparent)
    plt2
  })
  
  output$plot3 <- renderPlotly({
    
    SIR <- simulate()
    chgI<-c(0)
    for(i in 2:nrow(SIR)){
      chgI <- append(chgI, SIR[i,2]-SIR[i-1,2])
    }
    SIR$change_I <- chgI
    
    chgS<-c(0)
    for(i in 2:nrow(SIR)){
      chgS <- append(chgS, SIR[i-1,1]-SIR[i,1])
    }
    SIR$new_cases <- chgS
    SIR$new_cases <- SIR$new_cases*SIR$pop_multip
    
    api_data <- api_data()
    api_data$new_cases <- api_data$positiveIncrease
    
    #daily new cases plot
    
    plot_df <- rbind(api_data %>%
                       select(new_cases, date),
                     SIR %>%
                       select(new_cases, date) %>%
                       filter(new_cases>0 & !is.na(new_cases)))
    
    plot_df_part1 <- subset(plot_df, date <= input$dateRange[2]) 
    plot_df_part2 <- subset(plot_df, date >= input$dateRange[2])
    
    #this plot will combine the daily new cases from the API with the scaled daily new cases predicted by the simulation
    plt3 <- ggplotly(ggplot(plot_df_part1, aes(x = date, y = new_cases)) +
                       labs(title = "Daily new cases with a forecast", x = "Time", y = "Daily new cases") +
                       geom_line(color = "white", lwd=0.5) + 
                       geom_line(data = plot_df_part2, aes(x = date, y = new_cases), lty=2, color="white", lwd=0.5) + 
                       theme_transparent)
    plt3
    
  })
  
  
  output$plot4 <- renderPlotly({
    
    SIR <- simulate()
    
    if(nrow(SIR)<6){
      stop('in order to calculate 5 days effective reproductive rate choose the number of days to predict higher than 5')
    }
    #lastly, the 5 day effective reproductive rate value over time will be analyzed
    Re<- c(NA,NA,NA,NA,NA)
    for(i in 6:nrow(SIR)){
      Re <- append(Re, 1+log((SIR[i,2]/SIR[i-5,2])^(5*3/2)))
    }
    SIR$Re<-Re
    
    plt4 <- ggplotly(ggplot(SIR, aes(x = 1:nrow(SIR), y = Re)) +
                       labs(title = "5-day reproductive rate", x = "Day", y = "Reproductive rate") +
                       geom_line(color = "white", lwd=0.5) +
                       geom_hline(aes(yintercept = 1), color = "cyan", lty="twodash", lwd=0.5) +
                       xlim(6, nrow(SIR)) +
                       ylim(0, max(Re)) + 
                       theme_transparent)
    plt4
    # The Re value reaches 1 at about 28 days, which coincides with the peak in the dayli new cases. This is correct according to the theory, and reinforces
    # the realism of the simulation. The sudden fluctuations close to the end of the simulation (day 70+) are caused by the low number of cases and can be disregarded.
    
  })
  
}


##################################
##### Call shinyApp function #####
##################################
shinyApp(ui = ui, server = server)
## app.R ## ----
library(shinydashboard)
library(shiny)
library(tidyverse)
library(lubridate)
library(gghighlight)
library(plotly)
library(ggthemes)

# Daten laden ----

raw <- read_csv("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data") %>% 
    mutate(Meldedatum = as_date(Meldedatum)) %>% 
    arrange(Meldedatum)


#raw %>% filter(Landkreis == "StadtRegion Aachen") %>% group_by(Meldedatum) %>% summarise(AnzahlFall = sum(AnzahlFall))


selectable <- function(x)
{
    is.numeric(x) | is.Date(x)
}

our_variables <- names(raw %>% select_if(selectable))

lks <- raw$Landkreis %>% unique() %>% sort()
ages <- raw$Altersgruppe %>% unique() %>% sort()

# UI Begin ----
ui <- 
    dashboardPage(
        # UI Header ----
        dashboardHeader(title = "Basic dashboard"),
        # UI Menu ----
        dashboardSidebar(
            sidebarMenu(id = "unserTabSet",
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("Widgets", tabName = "widgets", icon = icon("address-book"))
            )
        ),
        dashboardBody(
            # UI Tabs ----
            tabItems(
                # UI Tabs dashboard ----
                tabItem(tabName = "dashboard",
                        fluidRow(
                            box( width = 8,
                                 plotOutput("demovariablen"),
                                 selectInput("varselect", "Variable auswählen", our_variables)
                            ),
                            box( width = 8,
                                 title = "Fälle pro Tag",
                                 plotlyOutput("plot1"),
                                 plotOutput("plotDetail"),
                                 plotOutput("plotSumme", 
                                            hover = hoverOpts(id = "plot_hover", delayType = "debounce"),
                                            brush = brushOpts(id = "plot_brush", direction = "x")),
                                 plotOutput("unser_brush")
                            ),
                            box(
                                width = 4, 
                                title = "Controls",
                                selectInput("lk_selector", "Landkreis auswählen", choices = lks),
                                selectInput("age_selector", "Altersgruppe auswählen", choices = ages),
                                checkboxInput("facet_plot_yes_no", "Plot facetieren?", value = FALSE),
                                selectInput("theme_selector", "Theme auswählen", choices = c("bw", "economist", "fivethirtyeight")),
                                actionButton("dashboard_next", "Weiter")
                            ),
                            
                            box(width = 8,
                                title = "Empty",
                                textOutput("error"),
                                verbatimTextOutput("debug")
                            )
                        )
                        
                ),
                # UI Tabs widgets ----
                tabItem(tabName = "widgets", 
                        fluidRow(
                            box(width = 12,
                                title = "Empty2",
                                h1("Überschrift"),
                                h2("Überschrift 2"),
                                h3("Überschrift 3"),
                                h4("Überschrift 4"),
                                p("Hallo das ist ein Test"),
                                br(), br(), 
                                p("Zweiter Paragraph"),
                                a(href = "www.google.com", "Hallo"),
                                strong(p("Test")),
                                hr()
                                
                                
                                
                            )
                        )
                )
                
            )
        )
    )

server <- function(input, output, session) {
    
    # Event Management ----
    # Event Button Weiter ----
    observeEvent(input$dashboard_next, {
        updateTabItems(session, "unserTabSet", "widgets")
    })
    
    
    
    # Reactive Data ----
    lk_daten <- reactive({
        raw %>% 
            filter(Landkreis == input$lk_selector)
    })
    
    cumulative_daten <- reactive({
        lk_daten() %>% 
            group_by(Meldedatum) %>% 
            summarise(AnzahlFall = sum(AnzahlFall)) %>% 
            mutate(AlleFaelle = cumsum(AnzahlFall))
    })
    
    
    # PO - Plot Outputs ----
    # PO Dashboard - main plot ----
    output$plot1 <- renderPlotly({
        
        plt <- lk_daten() %>% 
            ggplot() +
            aes(x = Meldedatum) +
            aes(y = AnzahlFall) +
            aes(fill = Altersgruppe) + 
            geom_col() +
            labs(title = "Anzahl Fälle pro Tag",
                 x = "Datum", y = "Anzahl Fälle")
        
        if(input$facet_plot_yes_no){
            plt <- plt + facet_wrap(~Altersgruppe)
        }
        
        if(input$theme_selector == "bw") {
            plt <- plt + theme_bw()
        }    
        if(input$theme_selector == "economist") {
            plt <- plt + theme_economist()
        }    
        if(input$theme_selector == "fivethirtyeight") {
            plt <- plt + theme_fivethirtyeight()
        }    
        
        
        
        plotly::ggplotly(p = plt)
        
    })
    
    # PO Dashboard - details ----
    output$plotDetail <- renderPlot({
        lk_daten() %>% 
            ggplot() +
            aes(x = Meldedatum) +
            aes(y = AnzahlFall) +
            aes(fill = Altersgruppe) + 
            geom_col() +
            labs(title = "Anzahl Fälle pro Tag",
                 x = "Datum", y = "Anzahl Fälle") +
            gghighlight::gghighlight(Altersgruppe == input$age_selector)
        
    })
    
    
    output$plotSumme <- renderPlot({
        cumulative_daten() %>% 
            ggplot() +
            aes(x = Meldedatum) +
            aes(y = AlleFaelle) +
            geom_col() +
            labs(title = "Anzahl Fälle pro Tag (kumuliert)",
                 x = "Datum", y = "Anzahl Fälle")
        
    })
    
    # UI HIER BIN ICH DRAN ----
    output$demovariablen <- renderPlot({
        req(input$varselect)
        raw %>% ggplot() +
            aes_string(x = input$varselect) +
            geom_histogram()
    })
    
    output$unser_brush <- renderPlot({
        
        ausgewaehlte_daten <- brushedPoints(cumulative_daten(), input$plot_brush, "Meldedatum", "AlleFaelle")
        
        hover_daten <- nearPoints(cumulative_daten(), input$plot_hover)
        
        cumulative_daten() %>% 
            filter(Meldedatum %in% ausgewaehlte_daten$Meldedatum) %>% 
            ggplot() +
            aes(x = Meldedatum) +
            aes(y = AnzahlFall) +
            geom_col() +
            labs(title = "Anzahl Fälle pro Tag",
                 x = "Datum", y = "Anzahl Fälle") +
            gghighlight(Meldedatum %in% hover_daten$Meldedatum)
    })
    
    
    # Debug output ----
    output$debug <- renderPrint({
        str(input$plot_hover)
        cat(str(input$plot_hover))
    })
    
    
}

shinyApp(ui, server)
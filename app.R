library('shiny')
library('tidyverse')
library('patchwork')
# library('lubridate')
# library('readr')
# library('ggrepel')
# library('fitdistrplus')
# library('visNetwork')

source("data.R")

parent_directory <- getwd()
repo_location <- "/data/COVID-19"

# Define UI for application that draws a histogram
ui <- fluidPage({
    tabsetPanel(
        tabPanel(
            "By Country",
            fluidRow(
                column(
                    12,
                    DT::dataTableOutput("growth_rates_table"),
                    fluidRow(
                        column(
                            6,
                            plotOutput("growth_rate_plot")
                        ),
                        column(
                            6,
                            plotOutput("active_cases_plot")
                        )
                    )
                )
            )
        ),
        tabPanel(
            "Growth Rates List",
            fluidRow(
                column(
                    12,
                    plotly::plotlyOutput(
                        "growth_rates_list_plot"
                    )
                )
            )
        ),
        tabPanel(
            "Confirmed Cases Since 100th Case",
            fluidRow(
                column(
                    12,
                    plotly::plotlyOutput("confirmed_cases_since_plot")
                )
            )
        ),
        tabPanel(
            "Active Cases Since 100th Case",
            fluidRow(
                column(
                    12,
                    plotly::plotlyOutput("active_cases_since_plot")
                )
            )
        ),
        tabPanel(
            "Growth Rate vs Active Cases",
            fluidRow(
                column(
                    12,
                    plotly::plotlyOutput("growth_rate_vs_active_cases_plot")
                )
            )
        ),
        tabPanel(
            "Growth Rate vs Confirmed Cases",
            fluidRow(
                column(
                    12,
                    plotly::plotlyOutput("growth_rate_vs_confirmed_cases_plot")
                )
            )
        ),
        tabPanel(
            "Confirmed Cases vs Active Cases",
            fluidRow(
                column(
                    12,
                    plotly::plotlyOutput("confirmed_cases_vs_active_cases_plot")
                )
            )
        ),
        tabPanel(
            "Growth Rate vs Recovery Factor",
            fluidRow(
                column(
                    12,
                    plotly::plotlyOutput("growth_rate_vs_recovery_factor_plot")
                )
            )
        ),
        tabPanel(
            "Growth Rate vs Recovery Factor vs Active Cases",
            fluidRow(
                column(
                    12,
                    plotly::plotlyOutput("growth_rate_vs_recovery_factor_vs_active_cases_plot")
                )
            )
        ),
        tabPanel(
            "Predictions",
            fluidRow(
                column(
                    3,
                    uiOutput("predict_country")
                ),
                column(
                    3,
                    uiOutput("predict_state")
                ),
                column(
                    6,
                    uiOutput("predict_outbreak_date")
                )
            ),
            fluidRow(
                column(
                    12,
                    plotly::plotlyOutput("predict_confirmed_growth_rate_log_linear", height = 600),
                    plotly::plotlyOutput("predict_confirmed_prediction", height = 600),
                    plotly::plotlyOutput("predict_daily_cases", height = 600),
                    plotly::plotlyOutput("predict_confirmed_growth_rate", height = 600)
                )
            )
        ),
        tabPanel(
            "Refresh Data",
            fluidRow(
                column(
                    10, offset = 1,
                    br(),
                    actionButton("refresh_from_git", "Refresh from Git", width = '100%')
                )
            )
        )
    )
    
})

# Define server logic required to draw a histogram
server <- function(input, output) {
    vals <- reactiveValues(
        country = "Australia",
        cutoff_date = as.Date("2020-03-20"),
        refresh = 0
    )
    
    observeEvent(input$refresh_from_git, {
        progress <- shiny::Progress$new()
        progress$set(message = "Refreshing Data from Johns Hopkins", value = 0)
        on.exit(progress$close())
        res <- system(paste0("cd ", parent_directory, repo_location, "; git pull"), intern = T)
        progress$set(message = res, value = 1)
        if(res != "Already up to date."){
            vals$refresh <- vals$refresh + 1
        }
        Sys.sleep(1)
    })
    
    src <- reactive({
        trigger <- vals$refresh
        get_src() %>% return()
    })
    
    country_data <- reactive({
        src() %>% get_country_data() %>% return()
    })
    
    growth_rates <- reactive({
        country_data() %>% get_growth_rates() %>% return()
    })
    
    output$growth_rates_table <- DT::renderDataTable({
        growth_rates() %>% 
            mutate(
                `Recent Growth` = signif(`Recent Growth`, 3),
                `Active Cases` = signif(`Active Cases`, 3),
                `Confirmed Cases` = signif(`Confirmed Cases`, 3),
                `Recovery Factor` = signif(`Recovery Factor`, 3)
            ) %>% 
            DT::datatable(rownames = F, filter = "top", selection = 'single') %>% return()
    })
    
    observeEvent(input$growth_rates_table_rows_selected, {
        vals$country <- growth_rates()$`Country/Region`[input$growth_rates_table_rows_selected]
    })
    
    n_provinces <- reactive({
        src() %>% filter(`Country/Region` == vals$country) %>% dplyr::select(`Province/State`) %>% unique() %>% nrow()
    })
    
    n_countries <- reactive({
        src() %>% dplyr::select(`Country/Region`) %>% unique() %>% nrow()
    })
    
    observe({
        if(n_provinces() > 1){
            output$growth_rate_plot <- renderPlot({
                gg1 <- country_data() %>% 
                    filter(
                        `Country/Region` == vals$country
                    ) %>% 
                    filter(
                        Date >= vals$cutoff_date
                    ) %>% 
                    ggplot(
                        aes(
                            x = Date,
                            y = `Growth Rate`
                        )
                    ) + 
                    geom_point(na.rm=T) + 
                    geom_line(na.rm=T) +
                    theme_bw(20) + 
                    geom_smooth(na.rm=T, method = 'loess', formula=y~x) + 
                    geom_hline(yintercept = 1) + 
                    ggtitle("Overall Country")
                
                gg2 <- src() %>% 
                    filter(
                        `Country/Region` == vals$country
                    ) %>% 
                    filter(
                        Date >= vals$cutoff_date
                    ) %>% 
                    ggplot(
                        aes(
                            x = Date,
                            y = `Growth Rate`
                        )
                    ) + 
                    geom_point(na.rm=T) + 
                    geom_line(na.rm=T) +
                    theme_bw(20) + 
                    geom_smooth(na.rm=T, method = 'loess', formula=y~x) + 
                    geom_hline(yintercept = 1) + 
                    facet_grid(rows = vars(`Province/State`)) + 
                    ggtitle("By State/Province")
                
                return(gg1/gg2+plot_layout(heights = c(1, n_provinces())))
            }, height = 300*n_provinces())
        }else{
            output$growth_rate_plot <- renderPlot({
                country_data() %>% 
                    filter(
                        `Country/Region` == vals$country
                    ) %>% 
                    filter(
                        Date >= vals$cutoff_date
                    ) %>% 
                    ggplot(
                        aes(
                            x = Date,
                            y = `Growth Rate`
                        )
                    ) + 
                    geom_point(na.rm=T) + 
                    geom_line(na.rm=T) +
                    theme_bw(20) + 
                    geom_smooth(na.rm=T, method = 'loess', formula=y~x) + 
                    geom_hline(yintercept = 1) + 
                    ggtitle("Overall Country")
            }, height = 300*n_provinces())
        }
    })
    
    observe({
        if(n_provinces() > 1){
            output$active_cases_plot <- renderPlot({
                gg1 <- country_data() %>% 
                    filter(
                        `Country/Region` == vals$country
                    ) %>% 
                    filter(
                        Date >= vals$cutoff_date
                    ) %>% 
                    mutate(
                        `Active Cases` = ifelse(
                            `Active Cases` == 0,
                            NA,
                            `Active Cases`
                        )
                    ) %>% 
                    ggplot(
                        aes(
                            x = Date,
                            y = `Active Cases`
                        )
                    ) + 
                    geom_point(na.rm=T) + 
                    geom_line(na.rm=T) +
                    theme_bw(20) + 
                    geom_smooth(na.rm=T, method = 'loess', formula=y~x) + 
                    geom_hline(yintercept = 1) + 
                    scale_y_log10() + 
                    ggtitle("Overall Country")
                
                gg2 <- src() %>% 
                    filter(
                        `Country/Region` == vals$country
                    ) %>% 
                    filter(
                        Date >= vals$cutoff_date
                    ) %>% 
                    mutate(
                        `Active Cases` = ifelse(
                            `Active Cases` == 0,
                            NA,
                            `Active Cases`
                        )
                    ) %>% 
                    ggplot(
                        aes(
                            x = Date,
                            y = `Active Cases`
                        )
                    ) + 
                    geom_point(na.rm=T) + 
                    geom_line(na.rm=T) +
                    theme_bw(20) + 
                    geom_smooth(na.rm=T, method = 'loess', formula=y~x) + 
                    geom_hline(yintercept = 1) + 
                    scale_y_log10() +
                    facet_grid(rows = vars(`Province/State`)) +
                    ggtitle("By State/Province")
                
                return(gg1/gg2+plot_layout(heights = c(1, n_provinces())))
            }, height = 300*n_provinces())
        }else{
            output$active_cases_plot <- renderPlot({
                country_data() %>% 
                    filter(
                        `Country/Region` == vals$country
                    ) %>% 
                    filter(
                        Date >= vals$cutoff_date
                    ) %>% 
                    mutate(
                        `Active Cases` = ifelse(
                            `Active Cases` == 0,
                            NA,
                            `Active Cases`
                        )
                    ) %>% 
                    ggplot(
                        aes(
                            x = Date,
                            y = `Active Cases`
                        )
                    ) + 
                    geom_point(na.rm=T) + 
                    geom_line(na.rm=T) +
                    theme_bw(20) + 
                    geom_smooth(na.rm=T, method = 'loess', formula=y~x) + 
                    geom_hline(yintercept = 1) + 
                    scale_y_log10() + 
                    ggtitle("Overall Country")
            }, height = 300*n_provinces())
        }
    })
    
    observe({
        output$growth_rates_list_plot <- plotly::renderPlotly({
            (
                growth_rates() %>% 
                    arrange(`Recent Growth`) %>%
                    mutate(
                        `Country/Region` = factor(`Country/Region`, levels = `Country/Region`, ordered = T)
                    ) %>%
                    ggplot(
                        aes(
                            x = `Country/Region`,
                            y = `Recent Growth`,
                            fill = State
                        )
                    ) + 
                    geom_bar(stat = 'identity', na.rm=T) + 
                    geom_hline(yintercept = 1, linetype = 'dashed') + 
                    coord_flip() + 
                    theme_bw(12)
            ) %>% plotly::ggplotly(height = n_countries()*20)
        })
    })
    
    output$confirmed_cases_since_plot <- plotly::renderPlotly({
        dsc_history <- country_data() %>% 
            group_by(`Country/Region`) %>% 
            filter(
                `Confirmed Cases` >= 100
            ) %>% 
            mutate(
                Date = as.numeric(Date - min(Date, na.rm=T)),
                `Confirmed Cases` = `Confirmed Cases` - min(`Confirmed Cases`, na.rm=T) + 100
            ) %>% 
            ungroup() %>% 
            filter(
                `Country/Region` %in% (
                    country_data() %>% 
                        group_by(`Country/Region`) %>% 
                        summarise(
                            `Most Cases` = max(`Confirmed Cases`, na.rm=T)
                        ) %>% 
                        filter(`Most Cases` >= 100) %>% 
                        dplyr::select(`Country/Region`)
                )[[1]]
            )
        
        (
            dsc_history %>% 
                ggplot(
                    aes(
                        x = Date,
                        y = `Confirmed Cases`,
                        colour = `Country/Region`
                    )
                ) + 
                geom_point(na.rm = T, size = 0.5) + 
                geom_line(na.rm = T, size = 0.5) + 
                theme_bw() + 
                scale_y_log10() + 
                theme(legend.position = 'none')
        ) %>% plotly::ggplotly(height = 700)
    })
    
    output$active_cases_since_plot <- plotly::renderPlotly({
        dsc_history <- country_data() %>% 
            group_by(`Country/Region`) %>% 
            filter(
                `Active Cases` >= 100
            ) %>% 
            mutate(
                Date = as.numeric(Date - min(Date, na.rm=T)),
                `Active Cases` = `Active Cases` - min(`Active Cases`, na.rm=T) + 100
            ) %>% 
            ungroup() %>% 
            filter(
                `Country/Region` %in% (
                    country_data() %>% 
                        group_by(`Country/Region`) %>% 
                        summarise(
                            `Most Cases` = max(`Active Cases`, na.rm=T)
                        ) %>% 
                        filter(`Most Cases` >= 100) %>% 
                        dplyr::select(`Country/Region`)
                )[[1]]
            )
        
        (
            dsc_history %>% 
                ggplot(
                    aes(
                        x = Date,
                        y = `Active Cases`,
                        colour = `Country/Region`
                    )
                ) + 
                geom_point(na.rm = T, size = 0.5) + 
                geom_line(na.rm = T, size = 0.5) + 
                theme_bw() + 
                scale_y_log10() + 
                theme(legend.position = 'none')
        ) %>% plotly::ggplotly(height = 700)
    })
    
    output$growth_rate_vs_active_cases_plot <- plotly::renderPlotly({
        (growth_rates() %>% 
             ggplot(
                 aes(
                     x = `Active Cases`,
                     y = `Recent Growth`,
                     label = `Country/Region`,
                     colour = State
                 )
             ) + 
             geom_point(na.rm=T) + 
             geom_hline(yintercept = 1, linetype = 'dashed') + 
             scale_x_log10() +
             theme_bw(12)
        ) %>% plotly::ggplotly(height = 600)
    })
    
    output$growth_rate_vs_confirmed_cases_plot <- plotly::renderPlotly({
        (growth_rates() %>% 
             ggplot(
                 aes(
                     x = `Confirmed Cases`,
                     y = `Recent Growth`,
                     label = `Country/Region`,
                     colour = State
                 )
             ) + 
             geom_point(na.rm=T) + 
             geom_hline(yintercept = 1, linetype = 'dashed') + 
             scale_x_log10() +
             theme_bw(12)
        ) %>% plotly::ggplotly(height = 600)
    })
    
    output$confirmed_cases_vs_active_cases_plot <- plotly::renderPlotly({
        (growth_rates() %>% 
             ggplot(
                 aes(
                     x = `Confirmed Cases`,
                     y = `Active Cases`,
                     label = `Country/Region`,
                     colour = State
                 )
             ) + 
             geom_point(na.rm=T) + 
             geom_abline(slope = 1, intercept = 0, linetype = 'dashed') + 
             scale_x_log10() +
             scale_y_log10() + 
             theme_bw(12)
        ) %>% plotly::ggplotly(height = 600)
    })
    
    output$growth_rate_vs_recovery_factor_plot <- plotly::renderPlotly({
        (growth_rates() %>% 
             ggplot(
                 aes(
                     x = `Recovery Factor`,
                     y = `Recent Growth`,
                     label = `Country/Region`,
                     colour = State
                 )
             ) + 
             geom_point(na.rm=T) + 
             geom_hline(yintercept = 1, linetype = 'dashed') + 
             theme_bw(12)
        ) %>% plotly::ggplotly(height = 600)
    })
    
    output$growth_rate_vs_recovery_factor_vs_active_cases_plot <- plotly::renderPlotly({
        plotly::plot_ly(data = growth_rates(), x = ~`Recovery Factor`, y = ~log10(`Active Cases`), z = ~`Recent Growth`, name = ~`Country/Region`, color = ~State, height = 700)
    })
    
    predict_outbreak <- reactive({
        if(is.null(input$predict_country)){
            return()
        }
        if(is.null(input$predict_state)){
            return()
        }
        if(is.null(input$predict_outbreak_date)){
            return()
        }
        
        # input <- list(
        #     predict_country = "Australia",
        #     predict_state = "Victoria",
        #     predict_outbreak_date = as.Date("2020-07-18")
        # )
        
        src() %>%
            # filter(
            #     `Country/Region` == "Australia",
            #     `Province/State` == "Victoria"
            # ) %>% 
            # filter(
            #     Date > as.Date("2020-06-12")
            # ) %>% 
            filter(
                `Country/Region` == input$predict_country,
                `Province/State` == input$predict_state
            ) %>% 
            filter(
                Date > input$predict_outbreak_date
            ) %>% 
            arrange(Date) %>% 
            mutate(
                `Confirmed Cases` = `Confirmed Cases` - `Confirmed Cases`[1] + `Active Cases`[1],
                Deaths = Deaths - Deaths[1],
                Recoveries = Recoveries - Recoveries[1],
                Removed = Removed - Removed[1]
            ) %>% 
            mutate(
                `Confirmed Cases Growth Rate` = `Confirmed Cases` / lag(`Confirmed Cases`, 1)
            ) %>% 
            mutate(
                `Confirmed Cases Log Growth Rate` = log(`Confirmed Cases Growth Rate` - 1)
            ) %>% 
            mutate(
                `Confirmed Cases Log Growth Rate` = ifelse(
                    is.infinite(`Confirmed Cases Log Growth Rate`),
                    NA,
                    `Confirmed Cases Log Growth Rate`
                )
            )
    })
    
    output$predict_confirmed_growth_rate <- plotly::renderPlotly({
        if(is.null(predict_outbreak())){
            return()
        }
        (predict_outbreak() %>% 
            ggplot(
                aes(
                    x = Date,
                    y = `Confirmed Cases Growth Rate`
                )
            ) + 
            geom_point(na.rm = T) + 
            geom_line(na.rm = T, colour = "grey", alpha = 0.4) + 
            theme_bw(20)) %>% plotly::ggplotly(height=600)
    })
    
    output$predict_confirmed_growth_rate_log_linear <- plotly::renderPlotly({
        if(is.null(predict_outbreak())){
            return()
        }
        (predict_outbreak() %>% 
            ggplot(
                aes(
                    x = Date,
                    y = `Confirmed Cases Log Growth Rate`
                )
            ) + 
            geom_point(na.rm = T) + 
            geom_line(na.rm = T, colour = "grey", alpha = 0.4) + 
            geom_smooth(method = 'lm', na.rm = T) +
            theme_bw(20)) %>% plotly::ggplotly(height=600)
    })
    
    predict_confirmed <- reactive({
        if(is.null(predict_outbreak())){
            return()
        }
        dt <- predict_outbreak() %>% 
            mutate(
                `Days Since Outbreak` = difftime(Date, Date[1], units = "days") %>% as.numeric()
            )
        
        m <- lm(`Confirmed Cases Log Growth Rate` ~ `Days Since Outbreak`, data = dt)
        
        first_day <- min(dt$`Days Since Outbreak`, na.rm=T)
        today = max(dt$`Days Since Outbreak`, na.rm=T)
        cases_first_day <- head(dt$`Confirmed Cases`, 1)
        cases_today = tail(dt$`Confirmed Cases`, 1)
        confidence <- predict(
            m, 
            newdata = tibble(
                `Days Since Outbreak` = today:(today+99)
            ),
            interval = "confidence",
            level = 0.67
        ) %>% as.data.frame() %>% as_tibble() %>% 
            mutate(
                `Days Since Outbreak` = today:(today+99),
                `Confirmed Cases Growth Rate` = 1+exp(fit),
                `Confirmed Cases Growth Rate Upper` = 1+exp(upr),
                `Confirmed Cases Growth Rate Lower` = 1+exp(lwr)
            ) %>% 
            mutate(
                `Confirmed Cases` = round(cases_today*cumprod(`Confirmed Cases Growth Rate`), 0),
                `Confirmed Cases Upper` = round(cases_today*cumprod(`Confirmed Cases Growth Rate Upper`), 0),
                `Confirmed Cases Lower` = round(cases_today*cumprod(`Confirmed Cases Growth Rate Lower`), 0)
            )
        
        prediction <- predict(
            m, 
            newdata = tibble(
                `Days Since Outbreak` = today:(today+99)
            ),
            interval = "prediction",
            level = 0.95
        ) %>% as.data.frame() %>% as_tibble() %>% 
            mutate(
                `Days Since Outbreak` = today:(today+99),
                `Confirmed Cases Growth Rate` = 1+exp(fit),
                `Confirmed Cases Growth Rate Upper` = 1+exp(upr),
                `Confirmed Cases Growth Rate Lower` = 1+exp(lwr)
            ) %>% 
            mutate(
                `Confirmed Cases` = round(cases_today*cumprod(`Confirmed Cases Growth Rate`), 0),
                `Confirmed Cases Upper` = round(cases_today*cumprod(`Confirmed Cases Growth Rate Upper`), 0),
                `Confirmed Cases Lower` = round(cases_today*cumprod(`Confirmed Cases Growth Rate Lower`), 0)
            )
        
        dt %>% 
            dplyr::select(
                `Days Since Outbreak`,
                `Confirmed Cases`
            ) %>% 
            mutate(
                `Confirmed Cases Upper` = `Confirmed Cases`,
                `Confirmed Cases Lower` = `Confirmed Cases`,
                Case = "Observed"
            ) %>% 
            bind_rows(
                confidence %>% 
                    dplyr::select(
                        `Days Since Outbreak`,
                        `Confirmed Cases`,
                        `Confirmed Cases Upper`,
                        `Confirmed Cases Lower`
                    ) %>% 
                    mutate(
                        Case = "Predicted"
                    )
            ) %>% 
            left_join(
                prediction %>% 
                    dplyr::select(
                        `Days Since Outbreak`,
                        `Upper Prediction Interval` = `Confirmed Cases Upper`,
                        `Lower Prediction Interval` = `Confirmed Cases Lower`
                    ),
                by = "Days Since Outbreak"
            ) %>% 
            arrange(`Days Since Outbreak`) %>% 
            return()
    })
    
    output$predict_confirmed_prediction <- plotly::renderPlotly({
        (predict_confirmed() %>% 
            ggplot(
                aes(
                    x = `Days Since Outbreak`,
                    y = `Confirmed Cases`
                )
            ) + 
             # geom_ribbon(
             #     aes(
             #         ymin = `Lower Prediction Interval`,
             #         ymax = `Upper Prediction Interval`
             #     ),
             #     fill = 'grey',
             #     alpha = 0.25
             # ) + 
            geom_ribbon(
                aes(
                    ymin = `Confirmed Cases Lower`,
                    ymax = `Confirmed Cases Upper`
                ),
                fill = 'grey',
                alpha = 0.5
            ) + 
            geom_line(aes(linetype = Case), na.rm = T) + 
            geom_line(aes(
                y = `Lower Prediction Interval`
            ), linetype = 'dotted') +
            geom_line(aes(
                y = `Upper Prediction Interval`,
                linetype = Case
            ), linetype = 'dotted') +
            scale_y_log10() +
            theme_bw(20)) %>% plotly::ggplotly(height=600)
    })
    
    output$predict_daily_cases <- plotly::renderPlotly({
        (predict_confirmed() %>% 
             mutate(
                 `Daily Cases` = `Confirmed Cases` - lag(`Confirmed Cases`, 1),
                 `Daily Cases Lower` = `Confirmed Cases Lower` - lag(`Confirmed Cases Lower`, 1),
                 `Daily Cases Upper` = `Confirmed Cases Upper` - lag(`Confirmed Cases Upper`, 1),
                 `Lower Prediction Interval` = `Lower Prediction Interval` - lag(`Lower Prediction Interval`, 1),
                 `Upper Prediction Interval` = `Upper Prediction Interval` - lag(`Upper Prediction Interval`, 1)
             ) %>% 
             ggplot(
                 aes(
                     x = `Days Since Outbreak`,
                     y = `Daily Cases`
                 )
             ) + 
             # geom_ribbon(
             #     aes(
             #         ymin = `Lower Prediction Interval`,
             #         ymax = `Upper Prediction Interval`
             #     ),
             #     fill = 'grey',
             #     alpha = 0.25
             # ) + 
             geom_ribbon(
                 aes(
                     ymin = `Daily Cases Lower`,
                     ymax = `Daily Cases Upper`
                 ),
                 fill = 'grey',
                 alpha = 0.5
             ) + 
             geom_line(aes(linetype = Case), na.rm = T) + 
             geom_line(aes(
                 y = `Lower Prediction Interval`
             ), linetype = 'dotted') +
             geom_line(aes(
                 y = `Upper Prediction Interval`,
                 linetype = Case
             ), linetype = 'dotted') +
             scale_y_log10() +
             theme_bw(20)) %>% plotly::ggplotly(height=600)
    })
    
    output$predict_country <- renderUI({
        ch <- (src() %>% 
            dplyr::select(`Country/Region`) %>% 
            unique() %>% 
            arrange(`Country/Region`))[[1]]
        
        s <- isolate(input$predict_country)
        if(is.null(s)){
            s <- "Australia"
        }
        
        selectizeInput(
            "predict_country",
            "Country",
            choices = ch,
            selected = s,
            width = '100%'
        )
    })
    
    output$predict_state <- renderUI({
        if(is.null(input$predict_country)){
            return()
        }
        
        ch <- (src() %>% 
                   filter(`Country/Region` == input$predict_country) %>% 
                   dplyr::select(`Province/State`) %>% 
                   unique() %>% 
                   arrange(`Province/State`))[[1]]
        
        s <- isolate(input$predict_state)
        if(is.null(s)){
            if(input$predict_country == "Australia"){
                s <- "Victoria"
            }else{
                s <- ch[1]
            }
        }
        
        selectizeInput(
            "predict_state",
            "State",
            choices = ch,
            selected = s,
            width = '100%'
        )
    })
    
    observeEvent(input$predict_outbreak_date, {
        # # Initialising:
        # dates <- src %>% dplyr::select(Country = `Country/Region`, State = `Province/State`) %>% unique()
        # dates$Date <- as.Date("2020-06-12")
        # saveRDS(dates, "outbreak_dates.rds")
        
        # Save the date to file any time it changes
        dates <- readRDS("outbreak_dates.rds")
        dates$Date[dates$Country == input$predict_country & dates$State == input$predict_state] <- input$predict_outbreak_date
        saveRDS(dates, "outbreak_dates.rds")
    })
    
    outbreak_date <- reactive({
        # Update whenever Country or State is changed
        if(is.null(input$predict_country)){
            return()
        }
        if(is.null(input$predict_state)){
            return()
        }
        
        dates <- readRDS("outbreak_dates.rds")
        return(
            dates$Date[dates$Country == input$predict_country & dates$State == input$predict_state]
        )
    })
    
    output$predict_outbreak_date <- renderUI({
        if(is.null(outbreak_date())){
            return()
        }
        
        sliderInput(
            "predict_outbreak_date",
            "Outbreak Date",
            min = min(src()$Date, na.rm = T),
            max = max(src()$Date, na.rm = T),
            value = outbreak_date(),
            width = '100%'
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

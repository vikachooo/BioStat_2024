library(shiny)
library(tidyverse)

sd_multiplier <- 3
points_for_ROC <- 1000

ui <- fluidPage(
    
    titlePanel(""),
    
    sidebarLayout(

        sidebarPanel(
            
            fluidRow(
                column(numericInput("M1", label = "M1", value = 0, step = 0.1),
                       numericInput("SD1", label = "SD1", value = 1, step = 0.1),
                       width = 6),
                column(numericInput("M2", label = "M2", value = 1, step = 0.1),
                       numericInput("SD2", label = "SD2", value = 1, step = 0.1),
                       width = 6)
            ),
            
            uiOutput("Cut_off_slider")
            
        ),
        
        
        mainPanel(
            tabsetPanel(
                tabPanel("Density", plotOutput("Density_Plot")),
                tabPanel("Sens | Spec", 
                         plotOutput("Sens_Spec_Plot"),
                         htmlOutput("Descr_Sens")
                ),
                tabPanel("ROC-curve",
                         plotOutput("ROC_plot"),
                         htmlOutput("Descr_ROC")
                )
            )
        ),
        
    )
 
)

server <- function(input, output){

    min_x <- reactive({
        min(input$M1 - sd_multiplier*input$SD1,
            input$M2 - sd_multiplier*input$SD2)
        })
    
    max_x <- reactive({
        max(input$M1 + sd_multiplier*input$SD1,
            input$M2 + sd_multiplier*input$SD2)
        })
    
    output$Cut_off_slider <- renderUI({
        sliderInput(
            "cut_off",
            label = "Cut off value",
            min = min_x(),
            max = max_x(),
            value = 0.5 * (min_x() + max_x()),
            ticks = F,
            step = (max_x() - min_x()) / points_for_ROC
        )
    })
    
    data_ROC <- reactive({
        tibble(
            x = seq(min_x(), 
                    max_x(),
                    (max_x() - min_x()) / points_for_ROC),
            Sens = 1 - pnorm(x, input$M2, input$SD2),
            Spec = 1 - pnorm(x, input$M1, input$SD1),
            J = Sens - Spec,
            Dist = ((1-Sens)^2 + Spec^2)^0.5
        )
    })
    
    ROC_results <- reactive({
        list(
            AUC = sapply(1:(nrow(data_ROC())-1), function(i){
                0.5 * (data_ROC()$Sens[i] + data_ROC()$Sens[i+1]) * (data_ROC()$Spec[i] - data_ROC()$Spec[i+1])
            }) %>% sum(),
            
            Dist_min = data_ROC()$Dist %>% min(),
            
            cut_off_Dist = data_ROC() %>% 
                    dplyr::filter(Dist == data_ROC()$Dist %>% min()) %>% 
                    pull(x) %>% 
                    mean(),
            
            J_max = data_ROC()$J %>% max(),
            
            cut_off_J = data_ROC() %>% 
                    dplyr::filter(J == data_ROC()$J %>% max()) %>% 
                    pull(x) %>% 
                    mean()
            )
    })
    
    output$Density_Plot <- renderPlot({

        Density_Plot <- tibble(x = c(min_x(), max_x())) %>% 
            ggplot(aes(x)) + 
            stat_function(fun = dnorm, 
                          n = 101, 
                          args = list(mean = input$M1, sd = input$SD1),
                          color = "blue") +
            stat_function(fun = dnorm, 
                          n = 101, 
                          args = list(mean = input$M2, sd = input$SD2),
                          color = "red") +
            theme_bw() + 
            labs(x = "Parameter value",
                 y = "Density")
        
        if (!is.null(input$cut_off)){
            Density_Plot <- Density_Plot + 
                geom_vline(xintercept = input$cut_off,
                           linetype = "dashed") + 
                geom_text(y = 0,
                          x = input$cut_off,
                          label = "Positive",
                          hjust = -0.1,
                          vjust = 1.5,
                          color = "red",
                          size = 3) +
                geom_text(y = 0,
                          x = input$cut_off,
                          label = "Negative",
                          hjust = 1.1,
                          vjust = 1.5,
                          color = "blue",
                          size = 3)                
        }
        
        Density_Plot
        
    }, res = 100)
    
    output$Sens_Spec_Plot <- renderPlot({
        
        Sens_Spec_Plot <- tibble(x = c(min_x(), max_x())) %>% 
            ggplot(aes(x)) + 
            stat_function(fun = function(x, m, sd) {1 - pnorm(x, m, sd)}, 
                          n = 101, 
                          args = list(m = input$M2, sd = input$SD2),
                          aes(color = "Sensitivity")) +
            stat_function(fun = pnorm, 
                          n = 101, 
                          args = list(m = input$M1, sd = input$SD1),
                          aes(color = "Specificity")) +

            theme_bw() + 
            scale_y_continuous(breaks = seq(0, 1, 0.2)) + 
            scale_color_manual(name = "", values = c("Sensitivity" = "red", "Specificity" = "blue")) +
            theme(legend.position = "") +
            labs(x = "Parameter value",
                 y = "Probability")

        if (!is.null(input$cut_off)){
            Sens_Spec_Plot <- Sens_Spec_Plot +
                geom_vline(xintercept = input$cut_off,
                       linetype = "dashed") + 
                geom_text(y = Inf,
                      x = min_x(),
                      label = "Sensitivity",
                      hjust = -0.1,
                      vjust = 3,
                      color = "red") +
                geom_text(y = Inf,
                          x = max_x(),
                          label = "Specificity",
                          hjust = 1.1,
                          vjust = 3,
                          color = "blue")
        }
        
        Sens_Spec_Plot
                
    }, res = 100)
    
    output$ROC_plot <- renderPlot({
        
        ROC_plot <- data_ROC() %>% 
        ggplot(aes(x = Spec, y = Sens)) + 
            geom_line() +
            theme_bw() + 
            labs(x = "1 - Specificity",
                 y = "Sensitivity") +
            geom_point(
                aes(y = 1 - pnorm(ROC_results()$cut_off_Dist, input$M2, input$SD2),
                    x = 1 - pnorm(ROC_results()$cut_off_Dist, input$M1, input$SD1)),
                color = "#FF0000",
                size = 5,
                shape = 3
            ) + 
            geom_point(
                aes(y = 1 - pnorm(ROC_results()$cut_off_J, input$M2, input$SD2),
                    x = 1 - pnorm(ROC_results()$cut_off_J, input$M1, input$SD1)),
                color = "#0000FF",
                size = 5,
                shape = 3
            )
        
        
        ROC_plot
        
    }, height = 400, width = 400, res = 100)
    
    output$Descr_Sens <- renderText({
        
        str_c("Cut-off value = ", input$cut_off, "<br>",
              "<font color=\"#FF0000\">Sensitivity = ", 
              1 - pnorm(input$cut_off, input$M2, input$SD2) %>% round(2), 
              "<font><br>",
              "<font color=\"#0000FF\">Specificity = ", 
              pnorm(input$cut_off, input$M1, input$SD1) %>% round(2), 
              "<font>")
        
    })
    
    output$Descr_ROC <- renderText({
        
        str_c("AUC = ", ROC_results()$AUC %>% round(3), "<br>",
              "<font color=\"#FF0000\">Cut-off (Min distance) = ", 
              ROC_results()$cut_off_Dist %>% round(2), 
              ", Distance = ", ROC_results()$Dist_min %>% round(2), "<font><br>",
              "<font color=\"#0000FF\">Cut-off (Youden index) = ", 
              ROC_results()$cut_off_J %>% round(2), 
              ", J = ", ROC_results()$J_max %>% round(2), "<font>")
        
    })
    
}

shinyApp(ui, server)





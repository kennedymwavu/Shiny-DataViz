library(shiny)
library(bslib)

# packages to use during data wrangling & viz:
{
    library(readxl)
    library(tidyverse)
    library(reshape2)
    library(magrittr)
    
    # Read in the sample data:
    all_data <- df <-  read_xlsx(
        path = "data for assignment.xlsx", 
        sheet = 1, col_names = TRUE
    )
    
    # Function to add percentages to results:
    add_percent <- \(x) {x * 100} %>% round(digits = 1) %>% paste0("%")
}



theme <- bs_theme(
    version = 4, bootswatch = "flatly")

# Define UI for application:
ui <- navbarPage(
    # application theme:
    theme = theme,
    
    # Application title
    title = strong("Data Visualization"), 
    
    
    
    tabPanel(
        title = "About",

        mainPanel(
            fluidRow(
                column(
                    width = 12,
                    align = "left",

                    includeMarkdown("documentation.md")
                ))
            ,

            fluidRow(
                column(
                    width = 12,
                    align = "center",

                    # Horizontal line:
                    tags$hr(),
                    strong("Sample Dataset:"),
                    tags$hr(),

                    tableOutput(outputId = "sample_dataset"),
                    downloadButton(outputId = "sample_ds", 
                                   label = "Download Sample Dataset .csv", 
                                   class = "btn-success"),

                    # Horizontal line:
                    tags$hr()
                )
            ),
            width = 12)
        ),

    tabPanel(
        title = "Data",

        sidebarLayout(
            sidebarPanel(
                # Input: Select a file:
                fileInput(inputId = "file1", label = "Choose an Excel File:",
                          multiple = TRUE,
                          accept = c(".xlsx")),

                # Horizontal line:
                tags$hr(),

                # Input: Select type of file:
                radioButtons(inputId = "type", label = "File Type:",
                             choices = c(".xlsx" = ".xlsx"),
                             selected = ".xlsx"),

                # Input: Checkbox if file has header:
                checkboxInput(inputId = "header", label = "Header", 
                              value = TRUE),
                
                hr(), 
                p(strong("Don't have your own dataset?")), 
                checkboxInput(inputId = "use_sample", 
                              label = "Use sample dataset"), 

                # Horizontal line:
                tags$hr(),

                tags$hr(),

                width = 3
            ),

            mainPanel(
                strong("Preview"),
                tableOutput(outputId = "contents"),

                # Horizontal line:
                tags$hr(),
                tags$hr(),

                width = 9
            )
        )
    ),  
    
    tabPanel(
        title = "Summary", 
        
            fluidRow(
                column(
                    selectInput(inputId = "summary_table",
                                label = "Which summary table (or it's equivalent) would you like to view?",
                                choices = c(
                                    "Gross Direct Premium",
                                    "Incurred Claims vs Net Earned Premium", 
                                    "Loss Ratios",
                                    "Combined Ratios", "Commission vs Expense Ratio",
                                    "Underwriting Profit Ratio"),
                                selected = "Gross Direct Premium"
                    ), 
                    width = 12, 
                    offset = 1
                )
            ),
            
            # Horizontal line:
            tags$hr(), 
            
            fluidRow(
                column(
                    width = 4,
                    strong("Summary Table"),
                    tableOutput("summ_table"),
                    # Download buttons for csv and png:
                    downloadButton(outputId = "downloadData", 
                                   label = "Summary Table .csv", 
                                   class = "btn-success"), 
                    
                    offset = 1
                ), 
                
                column(
                    width = 5,
                    strong("Plot"),
                    
                    plotOutput("summ_plot", width = "7in", height = "6in"),
                    
                    downloadButton(outputId = "downloadImage", 
                                   label = "Summary Plot .png", 
                                   class = "btn-success")
                )
            ), 
        hr(), 
        "@MwavuKennedy"
    ), 

    # window title:
    windowTitle = "Data Visualization"
)

# Define server logic required:
server <- function(input, output) {
    # ActServ:
    output$sample_dataset <- renderTable({
        all_data
    })

    # Read in user's selected file:
    dataInput <- reactive({
        if (input$use_sample) {
            return(all_data)
        }else{
            req(input$file1)
            
            whole <- read_excel(input$file1$datapath, sheet = 1, col_names = input$header)
            
            return(whole)
        }

    })

    output$contents <- renderTable(dataInput())


    # Summary tables:
    tidy_d <- reactive({
        tr <- function(whole_d) {
            # The dataset will need to be transformed to wide format:
            # Convert to tidy data ie. each column is a variable and each row is an observation:
            whole_d %<>% t() %>% as.data.frame()

            # The first observation in each column should be the column name:
            colnames(whole_d) <- whole_d[1, ]
            # The rownames should be a column in the data so we add it as the first column:
            whole_d <- whole_d[-1, ]
            whole_d %<>% mutate(Year = rownames(whole_d), .before = 1)

            # # All the data should be numeric but `Year` should be a factor:
            whole_d %<>% sapply(., as.numeric) %>% as_tibble() %>%
                mutate(Year = factor(Year)) %>%
                # Change column names:
                rename_with(function(x) c("Year", all_data |> pull(Year)))

            whole_d
        }
        tr(dataInput())
    })

    # ---------------------------------------------------------------
    # Summary tables:
    # Gross Direct Product:
    gdp <- reactive({
        tidy_d()  %>%  select(Year, `Gross Direct Premium`) %>%
            mutate(`Gross Direct Premium` = `Gross Direct Premium` / 1e6) %>%
            mutate(`Gross Direct Premium` = `Gross Direct Premium` %>% round(digits = 1))

    })

    # Incurred claims vs net earned premium:
    claims_prem <- reactive({
        tidy_d() %>% dplyr::select(Year, `Incurred Claims`, `Net Earned Premium Income`) %>%
            transmute(Year = Year,
                      `Incurred Claims` = `Incurred Claims` / 1e6,
                      `Net Earned Premium Income` = `Net Earned Premium Income` / 1e6) %>%
            mutate(
                `Incurred Claims` = `Incurred Claims` %>% round(digits = 1),
                `Net Earned Premium Income` = `Net Earned Premium Income` %>% round(digits = 1)
            )
    })

    # Loss Ratios:
    loss_ratios <- reactive({
        tidy_d() %>% dplyr::select(Year, `Incurred Claims`, `Net Earned Premium Income`) %>%
            transmute(
                Year = Year,
                `Loss Ratios` = `Incurred Claims` / `Net Earned Premium Income`
            ) %>%
            mutate(`Loss Ratios` = {`Loss Ratios` * 100} %>% round(digits = 1) %>% paste0("%"))
    })

    # Combined Ratios:
    comb_ratios <- reactive({
        tidy_d() %>%
            transmute(
                Year = Year,
                `Combined Ratio` = Combined / `Net Earned Premium Income`,
                `Investment Income Ratio` = `Investment Income` / `Net Earned Premium Income`,
                `Operating Ratio` = `Combined Ratio` - `Investment Income Ratio`
            ) %>%
            mutate(
                `Combined Ratio` = `Combined Ratio` %>% add_percent(),
                `Investment Income Ratio` = `Investment Income Ratio` %>% add_percent(),
                `Operating Ratio` = `Operating Ratio` %>% add_percent()
            )
    })


    # Commission vs Expense Ratio:
    com_exp <- reactive({
        tidy_d() %>%
            transmute(
                Year = Year,
                `Net Commission Ratio` = `Net Commissions` / `Net Earned Premium Income`,
                `Management Expense Ratio` = `Expense of Management` / `Net Earned Premium Income`
            ) %>%
            mutate(
                `Net Commission Ratio` = `Net Commission Ratio` %>% add_percent(),
                `Management Expense Ratio` = `Management Expense Ratio` %>% add_percent()
            )

    })

    # Underwriting Profit Ratio:
    profit_ratio <- reactive({
        tidy_d() %>%
            select(`Underwriting Profit(Loss)`, `Net Earned Premium Income`) %>% {. / 1e6} %>%
            mutate(
                `Profit Ratio` = `Underwriting Profit(Loss)` / `Net Earned Premium Income`,
                Year = colnames(all_data)[-1]
            ) %>% relocate(Year) %>% select(-`Net Earned Premium Income`) %>%
            mutate(
                `Underwriting Profit(Loss)` = `Underwriting Profit(Loss)` %>% round(digits = 1),
                `Profit Ratio` = `Profit Ratio` %>% add_percent()
            )
    })
    # --------------------------------------------------------------------------

    # Reactive for selected table:
    tableInput <- reactive({
        switch(input$summary_table,
               "Gross Direct Premium" = gdp(),
               "Incurred Claims vs Net Earned Premium" = claims_prem(),
               "Loss Ratios" = loss_ratios(),
               "Combined Ratios" = comb_ratios(),
               "Commission vs Expense Ratio" = com_exp(),
               "Underwriting Profit Ratio" = profit_ratio())
    })

    output$summ_table <- renderTable({
        tableInput()
    })


    # -----------------------------------------------------------------------
    # Visualizations:
    viz1 <- reactive({
        tidy_d() %>% select(Year, `Gross Direct Premium`) %>%
            mutate(`Gross Direct Premium` = `Gross Direct Premium` / 1e6) %>%
            ggplot(mapping = aes(x = Year, y = `Gross Direct Premium`))  +
            geom_col(width = 0.5, fill = '#425e72') +
            ylab(expression(italic("Amount in Ksh(Billions)"))) +
            xlab(expression(italic("Year"))) +
            # Underline the ggtitle:
            ggtitle(expression(underline("Gross Direct Premium"))) +
            coord_cartesian(ylim = c(40, 47)) +
            geom_text(mapping = aes(label = `Gross Direct Premium` %>% round(digits = 1)),
                      vjust = -0.2, fontface = "bold") +
            # refine the aesthetics of the graph:
            theme(
                aspect.ratio = 0.65,
                axis.ticks.x = element_blank(),
                plot.title = element_text(hjust = 0.5),
                panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line()
            )
    })

    viz2 <- reactive({
        tidy_d() %>% dplyr::select(Year, `Incurred Claims`, `Net Earned Premium Income`) %>%
            transmute(Year = Year,
                      `Incurred Claims` = `Incurred Claims` / 1e6,
                      `Net Earned Premium Income` = `Net Earned Premium Income` / 1e6) %>%
            pivot_longer(cols = c(`Incurred Claims`, `Net Earned Premium Income`)) %>%
            ggplot(mapping = aes(x = Year, y = value, fill = name)) +
            geom_col(position = "dodge", width = 0.6) +
            xlab(expression(italic("Year"))) + ylab(expression(italic("Amount in Ksh (Billions)"))) +
            ggtitle(expression(underline("Incurred Claims vs Net Earned Premium"))) +
            geom_text(mapping = aes(label = value %>% round(digits = 1), group = name),
                      vjust = 5, hjust = 0.5, position = position_dodge(width = 0.5),
                      fontface = "bold") +
            theme(
                aspect.ratio = 0.65,
                plot.title = element_text(hjust = 0.5),
                plot.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.ticks.x = element_blank(),
                legend.title = element_blank(),
                legend.position = "bottom",
                legend.text = element_text(face = "bold")
            )
    })

    viz3 <- reactive({
        # labels <- {loss_ratios()$`Loss Ratios` * 100} %>% round(digits = 1) %>% paste0("%")
        labels <- tidy_d() %>%
            dplyr::select(Year, `Incurred Claims`, `Net Earned Premium Income`) %>%
            transmute(
                Year = Year,
                `Loss Ratios` = `Incurred Claims` / `Net Earned Premium Income`
            ) %>% pull(`Loss Ratios`) %>% {. * 100} %>%
            round(digits = 1) %>% paste0("%")

        tidy_d() %>%
            dplyr::select(Year, `Incurred Claims`, `Net Earned Premium Income`) %>%
            transmute(
                Year = Year,
                `Loss Ratios` = `Incurred Claims` / `Net Earned Premium Income`
            ) %>%
            ggplot(mapping = aes(x = Year, y = `Loss Ratios`, group = 1)) +
            geom_line(size = 1.5, color = "#0099f9") +
            geom_point(size = 5, color = "#0099f9", shape = 19) +
            geom_text(mapping = aes(label = labels), vjust = -0.8, hjust = 0.9) +
            coord_cartesian(ylim = c(0.5, 0.8)) +
            ggtitle(expression(bold(underline("Loss Ratios")))) +
            ylab("%") + xlab(expression(italic("Year"))) +
            scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
            theme(
                aspect.ratio = 0.65,
                plot.title = element_text(hjust = 0.5),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line = element_line(),
                axis.text.y = element_text()
            )
    })

    viz4 <- reactive({
        tidy_d() %>%
            transmute(
                Year = Year,
                `Combined Ratio` = Combined / `Net Earned Premium Income`,
                `Investment Income Ratio` = `Investment Income` / `Net Earned Premium Income`,
                `Operating Ratio` = `Combined Ratio` - `Investment Income Ratio`
            ) %>% melt(id = "Year") %>%
            ggplot(mapping = aes(x = Year, y = value, color = variable, group = variable)) +
            geom_line(size = 1.5) +
            geom_point(size = 3, shape = 19) +
            geom_text(mapping = aes(label = {value * 100} %>% round(digits = 1) %>% paste0("%")),
                      vjust = -0.6, size = 4, colour = "black") +
            ggtitle(expression(bold(underline("Performance Ratios")))) +
            ylab("%") + xlab(expression(italic("Year"))) +
            scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
            theme(
                aspect.ratio = 0.65,
                legend.position = "bottom",
                legend.title = element_blank(),
                plot.title = element_text(hjust = 0.5),
                axis.ticks.x = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(),
                legend.text = element_text(face = "bold")
            )
    })


    viz5 <- reactive({
        tidy_d() %>%
            transmute(
                Year = Year,
                `Net Commission Ratio` = `Net Commissions` / `Net Earned Premium Income`,
                `Management Expense Ratio` = `Expense of Management` / `Net Earned Premium Income`
            ) %>%
            melt(id = "Year") %>%
            ggplot(mapping = aes(x = Year, y = value, group = variable, color = variable)) +
            geom_line(size = 1.5) +
            geom_point(mapping = aes(shape = variable, fill = variable), size = 3) +
            geom_text(mapping = aes(label = {value * 100} %>% round(digits = 1) %>% paste0("%")),
                      vjust = -0.6, color = "black") +
            scale_shape_manual(values = c(19, 22)) +
            ggtitle(expression(bold(underline("Net Commission Ratio vs Management Expense Ratio")))) +
            xlab(expression(italic("Year"))) + ylab("%") +
            coord_cartesian(ylim = c(0, 0.5)) +
            scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
            theme(
                aspect.ratio = 0.7,
                legend.position = "bottom",
                legend.title = element_blank(),
                plot.title = element_text(hjust = 0.5),
                axis.ticks.x = element_blank(),
                panel.grid.major = element_blank(),
                legend.text = element_text(face = "bold")
            )
    })


    viz6 <- reactive({
        coeff <- 40
        tidy_d() %>%
            select(`Underwriting Profit(Loss)`, `Net Earned Premium Income`) %>% {. / 1e6} %>%
            mutate(
                `Profit Ratio` = `Underwriting Profit(Loss)` / `Net Earned Premium Income`,
                Year = colnames(all_data)[-1]
            ) %>% relocate(Year) %>% select(-`Net Earned Premium Income`) %>%
            ggplot(mapping = aes(x = Year)) +
            geom_col(mapping = aes(y = `Underwriting Profit(Loss)`, fill = "#ff7c7c"),
                     width = 0.7) +
            geom_text(mapping = aes(y = -3,
                                    label = `Underwriting Profit(Loss)` %>% round(digits = 1)),
                      vjust = -10, color = "black", size = 4.5, fontface = "bold") +
            geom_line(mapping = aes(y = `Profit Ratio` * coeff, group = 1, col = "#607c3c"), #color = linecol,
                      size = 1.3) +
            geom_point(mapping = aes(y = `Profit Ratio` * coeff), color = "#607c3c", size = 3) +
            geom_text(mapping = aes(y = `Profit Ratio` * coeff,
                                    label = {`Profit Ratio` * 100} %>%
                                        round(digits = 1) %>% paste0("%")),
                      vjust = -1.3, hjust = 0.2, color = "#607c3c", fontface = "bold") +
            scale_y_continuous(
                name = expression(italic("Amount Ksh (Billions)")),
                sec.axis = sec_axis(~.*coeff, name = "%", labels = function(x) x / 20 * 1.5)
            ) +
            scale_color_identity(
                name = "",
                labels = c("Profit Ratio"),
                guide = "legend") +
            scale_fill_identity(
                name = "",
                labels = c("Underwriting Profit(Loss)"),
                guide = "legend") +
            coord_cartesian(ylim = c(-8, 0))  +
            ggtitle(expression(bold(underline("Underwriting Profit(Loss)")))) +
            labs(color = '', fill = "") +
            theme(
                aspect.ratio = 0.9,
                plot.title = element_text(hjust = 0.5),
                panel.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(),
                legend.position = "bottom",
                legend.text = element_text(face = "bold")
            )
    })

    # Reactive for selected plot:
    plotInput <- reactive({
        switch(input$summary_table,
               "Gross Direct Premium" = viz1(),
               "Incurred Claims vs Net Earned Premium" = viz2(),
               "Loss Ratios" = viz3(),
               "Combined Ratios" = viz4(),
               "Commission vs Expense Ratio" = viz5(),
               "Underwriting Profit Ratio" = viz6())
    })

    output$summ_plot <- renderPlot({
        plotInput()
    })
    # -----------------------------------------------------------------------

    # Enable downloads:
    output$sample_ds <- downloadHandler(
        filename = function() {
            "Sample Dataset.csv"
        },
        content = function(file) {
            write.csv(all_data, file, row.names = FALSE)
        }
    )
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0(input$summary_table, ".csv")
        },
        content = function(file) {
            write.csv(tableInput(), file, row.names = FALSE)
        }
    )

    output$downloadImage <- downloadHandler(
        filename = function() {
            paste0(input$summary_table, ".png")
        },
        content = function(file) {
            ggsave(file, plot = plotInput(), device = "png",
                   width = 7, height = 6, units = "in")
        },
        contentType = "image/png"
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
# run_with_themer(shinyApp(ui, server))

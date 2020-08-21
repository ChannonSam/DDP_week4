#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny); library(shinydashboard); library(shinyWidgets)
library(shinyjs)
library(ggplot2); library(ggpubr)
library(pROC)
wcc <- read.csv("wcc.csv")[, -1]
crp <- read.csv("crp.csv")[, -1]


# Define UI for application
ui <- dashboardPage(
    skin = "green",
    header = dashboardHeader(title = "Diagnostic Test Evaluator", titleWidth = "25%"),
    sidebar = dashboardSidebar(
        width = "25%",
        fileInput(inputId = "file1",
                  label = "Add New File",
                  multiple = FALSE,
                  accept = c(".csv"),
                  width = NULL),
        actionButton(inputId = "update1",
                     label = "Update Options",
                     icon = icon(name = "redo-alt")),
        br(),
        radioButtons(inputId = "inputs",
                     label = "Choose input data",
                     choiceNames = c("White Cell Count data", "CRP data"),
                     choiceValues = c("wcc", "crp"),
                     selected = "wcc"),
        br(),
        radioButtons(inputId = "int_cat",
                     label = "Choose Case Classification",
                     choices = c("Infection", "Not-Infection"),
                     selected = "Infection"),
        sliderInput(inputId = "threshold",
                    label = "Pick Threshold",
                    min = 0.28, max = 31.59, value = 11.32)
    ),
    
    body = dashboardBody(
        tabsetPanel(
            tabPanel(
                title = "Instructions",
                verbatimTextOutput("instructions")
                ),
            tabPanel(
                title = "Plots",
                plotOutput("plot1", width = "1200px", height = "600px")
            ),
            tabPanel(
                title = "White Cell Count data",
                tableOutput("wcc_tab")),
            tabPanel(
                title = "CRP data",
                tableOutput("crp_tab")),
            tabPanel(
                title = "CheatSheet",
                tableOutput("datasets"),
                verbatimTextOutput("inputs")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # update inputs:
    observeEvent(input$update1, {
        if (is.null(input$file1)) {
            updateRadioButtons(session = session,
                               inputId = "inputs",
                               label = "Choose input data",
                               choiceNames = c("White Cell Count data", "CRP data"),
                               choiceValues = c("wcc", "crp"),
                               selected = "wcc")
        }
        if (!is.null(input$file1)) {
            updateRadioButtons(session = session,
                               inputId = "inputs",
                               label = "Choose input data",
                               choiceNames = c("White Cell Count data", "CRP data", "User Defined Data"),
                               choiceValues = c("wcc", "crp", "uid"),
                               selected = "wcc")
        }
    })
    
    observeEvent(input$inputs, {
        if(input$inputs %in% c("wcc", "crp")) {
            updateRadioButtons(session = session,
                               inputId = "int_cat",
                               label = "Choose Case Classification",
                               choices = c("Infection", "Not-Infection"),
                               selected = "Infection")
            if (input$inputs == "wcc") {
                updateSliderInput(session = session,
                                  inputId = "threshold",
                                  label = "Pick Threshold",
                                  min = 0.28, max = 31.59, value = 11.32)
            }
            if (input$inputs == "crp") {
                updateSliderInput(session = session,
                                  inputId = "threshold",
                                  label = "Pick Threshold",
                                  min = 0.46, max = 217.32, value = 30.84)
            }
        } else {
            if (input$inputs == "uid") {
                filepath <- input$file1$datapath[1]
                data <- read.csv(file = filepath)
                case_choices <- unique(data$classification)
                updateRadioButtons(session = session,
                                   inputId = "int_cat",
                                   label = "Choose Case Classification",
                                   choices = case_choices,
                                   selected = case_choices[1])
                min.thresh = round(min(data$value, na.rm = T), 2)
                max.thresh = round(max(data$value, na.rm = T), 2)
                threshold.range = c(min.thresh, max.thresh)
                y.thresh = mean(threshold.range)
                updateSliderInput(session = session,
                                  inputId = "threshold",
                                  label = "Pick Threshold",
                                  min = min.thresh, max = max.thresh, value = y.thresh)
            }
        }
    })
    
    # Data processing:
    
    rval1 <- reactive({
        if (input$inputs == "wcc") {
            data <- wcc
            dataname <- "White Cell Count"
        }
        if (input$inputs == "crp") {
            data <- crp
            dataname <- "CRP"
        }
        if (input$inputs == "uid") {
            filepath <- input$file1$datapath[1]
            data <- read.csv(file = filepath)
            dataname <- input$file1$name[1]
        }
        min.thresh = round(min(data$value, na.rm = T), 2)
        max.thresh = round(max(data$value, na.rm = T), 2)
        list(data = data, dataname = dataname,
             threshold.range = c(min.thresh, max.thresh)) 
    })
    rval2 <- reactive({
        data <- rval1()$data
        cases <- input$int_cat
        data$classification[data$classification != cases] <- "other"
        r <- roc(response = data$classification,
                 predictor = data$value,
                 levels = c("other", cases), ret = "all_coords")
        a <- round(auc(r), 3)
        coords <- data.frame(sens = r$sensitivities,
                             spec = r$specificities,
                             thresh = r$thresholds)
        Y <- coords$sens + coords$spec
        Y <- which(Y == max(Y))
        if (length(Y) != 1) {
            set.seed(660)
            Y <- sample(Y, 1)
        }
        y.thresh = round(coords$thresh[Y], 2)
        list(coords = coords, y.thresh = y.thresh, auc = a)
    })
    
    # INSTRUCTION OUTPUTS:
    
    output$instructions <- renderText({
        para1 <- paste0("Welcome to my shiny app!", "\n",
                        "The aim of this application is to allow you to look at the diagnostic ability of medical (or other) tests.", "\n",
                        "The raw application uses simulated (FAKE!) data on common medical tests - white cell count and CRP - for diagnosing infections.", "\n",
                        "This allows you to experiment with the functionality of this application straight away.", "\n",
                        "There is also the option to upload your own data, and investigate your own results!!")
        para2 <- paste0("Overview:", "\n",
                        "This application takes data for a diagnostic test and plots two important graphs.", "\n",
                        "Plot 1 - This compares the raw values of the test between diagnostic categories, and ", "\n",
                        "also overlays boxplots to show the distribution of the test value for each category", "\n",
                        "Plot2 - This generates a Receiver Operating Characteristic curve (ROC-curve) for the data.", "\n",
                        "If you don't understand what this is I suggest you head to: https://en.wikipedia.org/wiki/Receiver_operating_characteristic", "\n\n",
                        "The application also allows interactivity, allowing the user to look at how the test performs at a specific threshold, and ", "\n",
                        "playing around with which category is the Case category, with the other categories being set to Controls by default.")
        
        para3 <- paste0("Step 1 - Uploading data:", "\n",
                        "Data uploaded will only be useable if it has two specific columns.", "\n",
                        "The first column must be called classification - this contains the categories you are trying to differentiate using your test.", "\n",
                        "For example, in the FAKE white cell count data each sample is either listed as Infection or Not-Infection.", "\n",
                        "The second column must be called value - this contains the numeric value for the test you want to evaluate.", "\n",
                        "Additional tabs show the format of the White Cell Count and CRP data to help you format your own data properly.", "\n\n",
                        "Once you've uploaded this data you must then click the Update Options button, and select User Defined Data.")
        para4 <- paste0("Step 2 - Choose the Case category:", "\n",
                        "You now need to select which category your test is trying to differentiate from other categories.", "\n",
                        "This is called the Case category, and all other categories are then by default set to Controls.", "\n", 
                        "This allows the application to generate the ROC-curve based on a binary classification between Cases and Controls.")
        para5 <- paste0("Step 3 - Choose the threshold value:", "\n",
                        "Every diagnostic test needs to have a threshold, with values one side of this classified as positive, ", "\n",
                        "and values the other side defined as negative.", "\n",
                        "By default the application chooses an arbitrary threshold.", "\n",
                        "The black dotted line on Plot 1 shows the optimal threshold to maximise the sensitivity and specificity of the test", "\n",
                        "using Youden's J statistic. Occasionally there is more than one optimal threshold. The application arbitrarily picks one.", "\n",
                        "The Sensitivity and Specificity at the chosen threshold are plotted as a Red Circle on Plot 2.")
        paste0(para1, "\n\n",
               para2, "\n\n",
               para3, "\n\n",
               para4, "\n\n",
               para5)
    })
    
    # PLOT OUTPUTS:
    
    output$plot1 <- renderPlot({
        L1 <- rval1()
        L2 <- rval2()
        data <- L1$data
        dataname <- L1$dataname
        coords <- L2$coords
        y.thresh <- L2$y.thresh
        auc <- L2$auc
        threshold <- input$threshold
        
        set.seed(144)
        g1 <- ggplot(data, aes(x = classification, y = value, fill = classification)) +
            geom_boxplot(alpha = 0.7, outlier.size = 0) +
            geom_point(shape = 21, position = position_jitterdodge()) +
            geom_hline(yintercept = y.thresh, size = 1.5, linetype = "dashed", col = "gray20") +
            geom_hline(yintercept = input$threshold, size = 1.5, col = "orangered") +
            ggtitle(dataname)
        
        
        d <- abs(coords$thresh - threshold)
        m <- which(d == min(d))[1]
        m.coords <- coords[m, ]
        
        auc.coords <- data.frame(spec = 0.5, sens = 0.1, auc = paste0("AUC: ", auc))
        
        null.line <- data.frame(x = 0, y = 0, xend = 1, yend = 1)
        
        g2 <- ggplot(coords, aes(x = 1 - spec, y = sens)) +
            geom_path(col = "gray20") +
            geom_point(data = m.coords, aes(x = 1 - spec, y = sens),
                       shape = 19, col = "orangered", size = 6) +
            ggtitle(dataname) +
            geom_text(data = auc.coords, aes(x = 1 - spec, y = sens, label = auc),
                        size = 8, col = "gray20") +
            xlab("1 - Specificity") + ylab("Sensitivity") +
            geom_segment(data = null.line, aes(x = x, y = y, xend = xend, yend = yend))
        g <- ggarrange(g1, g2)
        g
    })

    # DATA OUTPUTS:
    output$wcc_tab <- renderTable({
        wcc
    })
    output$crp_tab <- renderTable({
        crp
    })
    
    # CHEATSHEET OUTPUTS (FOR DEBUGGING):
    output$datasets <- renderTable({
        input$file1
    })
    output$inputs <- renderText({
        paste0(" Input: ", input$inputs,
               "\n File1: ", input$file1$name[1],
               "\n Case choice: ", input$int_cat,
               "\n Threshold: ", input$threshold,
               "\n input$file1 null? ", is.null(input$file1))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

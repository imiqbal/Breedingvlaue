#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(randomForest)
library(pls)
library(neuralnet)
library(caret)
library(e1071)
library(xgboost)
library(gbm)
library(rpart)
library(earth)
library(DT)
library(ggplot2)
library(plotly)
library(shinybusy)
library(reshape2)

# Custom CSS (unchanged)
custom_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600&display=swap');

  body {
    background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
    font-family: 'Poppins', sans-serif;
    color: #2d3436;
  }
  .skin-blue .main-header .logo {
    background: linear-gradient(to right, #ff6b6b, #ff8e53);
    font-weight: 600;
    color: white;
    font-size: 20px;
    padding: 10px;
    border-bottom: 2px solid #ff4757;
  }
  .skin-blue .main-header .navbar {
    background: linear-gradient(to right, #ff6b6b, #ff8e53);
    box-shadow: 0 2px 10px rgba(0,0,0,0.1);
  }
  .skin-blue .main-sidebar {
    background: linear-gradient(to bottom, #2c3e50, #1e272e);
  }
  .skin-blue .main-sidebar .sidebar-menu li a {
    color: #dfe6e9;
    font-size: 16px;
    padding: 12px 5px 12px 15px;
    transition: all 0.3s ease;
  }
  .skin-blue .main-sidebar .sidebar-menu li a:hover {
    background-color: #ff6b6b;
    color: white;
  }
  .skin-blue .main-sidebar .sidebar-menu li.active a {
    background-color: #ff6b6b;
    color: white;
    border-left: 4px solid #ff4757;
  }
  .box {
    border-radius: 15px;
    box-shadow: 0 6px 12px rgba(0,0,0,0.15);
    background: white;
    transition: transform 0.3s ease;
    border: none;
    overflow: hidden;
  }
  .box:hover {
    transform: translateY(-5px);
  }
  .box-header {
    background: linear-gradient(to right, #74b9ff, #a29bfe);
    color: white;
    border-radius: 15px 15px 0 0;
    padding: 15px;
  }
  .btn {
    border-radius: 25px;
    font-weight: 600;
    padding: 8px 20px;
    transition: all 0.3s ease;
  }
  #run, #download {
    background: linear-gradient(to right, #ff6b6b, #ff8e53);
    color: white;
    border: none;
    box-shadow: 0 3px 6px rgba(0,0,0,0.1);
  }
  #run:hover, #download:hover {
    background: linear-gradient(to right, #ff4757, #ff6b6b);
    transform: translateY(-2px);
    box-shadow: 0 5px 10px rgba(0,0,0,0.2);
  }
  h4 {
    color: #2d3436;
    font-weight: 600;
    margin-bottom: 15px;
  }
  .dataTables_wrapper {
    background-color: white;
    padding: 15px;
    border-radius: 10px;
    box-shadow: 0 2px 5px rgba(0,0,0,0.05);
  }
  .plotly {
    border-radius: 10px;
    box-shadow: 0 2px 5px rgba(0,0,0,0.05);
  }
  .info-box {
    background: linear-gradient(135deg, #74b9ff 0%, #a29bfe 100%);
    color: white;
    padding: 20px;
    border-radius: 15px;
    margin-bottom: 20px;
    box-shadow: 0 4px 8px rgba(0,0,0,0.1);
  }
  .model-description {
    background-color: #f8f9fa;
    padding: 20px;
    border-radius: 10px;
    margin-bottom: 15px;
    border-left: 5px solid #ff6b6b;
    transition: all 0.3s ease;
  }
  .model-description:hover {
    background-color: #e9ecef;
  }
  .interpretation-box {
    background-color: #f1f3f5;
    padding: 15px;
    border-radius: 8px;
    margin-top: 10px;
    border-left: 4px solid #74b9ff;
    font-size: 14px;
    color: #2d3436;
  }
  .header-banner {
    width: 100%;
    height: 80px;
    background: url('https://www.transparenttextures.com/patterns/white-diamond.png'), linear-gradient(to right, #ff6b6b, #ff8e53);
    background-blend-mode: overlay;
    display: flex;
    align-items: center;
    justify-content: center;
    color: white;
    font-size: 24px;
    font-weight: 600;
    border-bottom: 3px solid #ff4757;
    margin-bottom: 20px;
    box-shadow: 0 4px 10px rgba(0,0,0,0.1);
  }
"

# UI (unchanged except for the interpretation text update in bestModelUI)
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Breeding Value Prediction System"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line", lib = "font-awesome")),
      menuItem("Visualizations", tabName = "visualizations", icon = icon("chart-bar", lib = "font-awesome")),
      menuItem("Model Details", tabName = "models", icon = icon("brain", lib = "font-awesome")),
      menuItem("About", tabName = "about", icon = icon("info-circle", lib = "font-awesome"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    add_busy_spinner(
      spin = "fading-circle",
      color = "#ff6b6b",
      position = "full-page"
    ),
    div(class = "header-banner",
        "Breeding Value Prediction System"
    ),
    tabItems(
      tabItem(tabName = "analysis",
              fluidRow(
                box(width = 4, solidHeader = TRUE, status = "primary",
                    title = "Model Configuration",
                    fileInput("file", "Upload CSV File", accept = ".csv"),
                    uiOutput("target_ui"),
                    selectInput("model", "Choose ML Model", 
                                choices = c("Animal Model" = "animal",
                                            "Random Forest" = "rf",
                                            "Principal Component Regression" = "pcr",
                                            "Neural Network" = "ann",
                                            "XGBoost" = "xgb",
                                            "Support Vector Machine" = "svm",
                                            "Gradient Boosting" = "gbm",
                                            "Classification and Regression Trees" = "cart",
                                            "K-Nearest Neighbors" = "knn",
                                            "Multivariate Adaptive Regression Splines" = "mars")),
                    uiOutput("model_params"),
                    actionButton("run", "Run Model", icon = icon("play")),
                    br(), br(),
                    downloadButton("download", "Download Predictions", icon = icon("download"))
                ),
                box(width = 8, solidHeader = TRUE, status = "info",
                    title = "Results",
                    verbatimTextOutput("summary"),
                    uiOutput("conditionalPlots"),
                    DTOutput("predTable")
                )
              ),
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary",
                    title = "Best Model Overview",
                    uiOutput("bestModelUI")
                )
              )
      ),
      tabItem(tabName = "visualizations",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary",
                    title = "Performance Comparison",
                    h4("RMSE Across Models"),
                    plotlyOutput("rmseBarPlot", height = "300px"),
                    div(class = "interpretation-box",
                        p("This plot shows the Root Mean Square Error (RMSE) for each machine learning model. RMSE measures the average magnitude of prediction errors, with lower values indicating better model performance. A model with the smallest RMSE bar is the most accurate in predicting breeding values.")
                    ),
                    h4("MAE Across Models"),
                    plotlyOutput("maeBarPlot", height = "300px"),
                    div(class = "interpretation-box",
                        p("This plot displays the Mean Absolute Error (MAE) for each model. MAE represents the average absolute difference between predicted and actual breeding values. Lower MAE values suggest better predictive accuracy, with the smallest bar indicating the best-performing model for this metric.")
                    ),
                    h4("R² Across Models"),
                    plotlyOutput("r2BarPlot", height = "300px"),
                    div(class = "interpretation-box",
                        p("This plot illustrates the R-squared (R²) values for each model. R² indicates the proportion of variance in the breeding values explained by the model, ranging from 0 to 1. Higher R² values (closer to 1) signify better model fit, with the tallest bar representing the best model for this metric.")
                    ),
                    h4("Correlation Across Models"),
                    plotlyOutput("corrBarPlot", height = "300px"),
                    div(class = "interpretation-box",
                        p("This plot shows the correlation between actual and predicted breeding values for each model. Correlation measures the linear relationship strength, ranging from -1 to 1. Values closer to 1 indicate a strong positive relationship, meaning better predictive performance. The tallest bar represents the model with the strongest correlation.")
                    ),
                    h4("Actual vs Predicted Values"),
                    plotlyOutput("scatterPlot", height = "400px"),
                    div(class = "interpretation-box",
                        p("This scatter plot compares actual breeding values (x-axis) to predicted values (y-axis) for the selected model. Points closer to the dashed 45-degree line indicate better predictions. The R² value in the subtitle reflects the model’s explanatory power, with a higher R² indicating a better fit.")
                    ),
                    h4("Performance Metrics Table"),
                    DTOutput("metricsTable"),
                    div(class = "interpretation-box",
                        p("This table provides a comprehensive comparison of all models across four metrics: RMSE, MAE, R², and Correlation. Use this table to identify the best-performing model by comparing all metrics simultaneously. You can sort the table by any column to focus on a specific metric.")
                    )
                )
              )
      ),
      tabItem(tabName = "models",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary",
                    title = "Machine Learning Models Explained",
                    div(class = "model-description",
                        h4("Animal Model"),
                        p("A simple linear regression model that serves as a baseline for comparison. It uses ordinary least squares to establish relationships between predictors and breeding values.")
                    ),
                    div(class = "model-description",
                        h4("Random Forest"),
                        p("An ensemble learning method that constructs multiple decision trees and combines their predictions. Excellent for handling non-linear relationships and providing variable importance measures.")
                    ),
                    div(class = "model-description",
                        h4("Principal Component Regression"),
                        p("Combines principal component analysis with linear regression. Reduces dimensionality by transforming predictors into uncorrelated components, useful when predictors are highly correlated.")
                    ),
                    div(class = "model-description",
                        h4("Neural Network"),
                        p("A machine learning model inspired by biological neural networks. Can capture complex non-linear patterns in data through multiple layers of interconnected nodes.")
                    ),
                    div(class = "model-description",
                        h4("XGBoost"),
                        p("An optimized gradient boosting framework that builds models sequentially, with each new model correcting errors from previous ones. Known for high performance in prediction tasks.")
                    ),
                    div(class = "model-description",
                        h4("Support Vector Machine"),
                        p("A versatile algorithm that finds optimal hyperplanes to separate data or predict continuous values. Can handle both linear and non-linear relationships through different kernel functions.")
                    ),
                    div(class = "model-description",
                        h4("Gradient Boosting"),
                        p("An ensemble method that builds models sequentially, each correcting the errors of the previous ones. Effective for capturing complex patterns in data.")
                    ),
                    div(class = "model-description",
                        h4("Classification and Regression Trees"),
                        p("A decision tree-based method that splits data into regions based on feature values. Simple yet effective for both classification and regression tasks.")
                    ),
                    div(class = "model-description",
                        h4("K-Nearest Neighbors"),
                        p("A non-parametric method that predicts values based on the average of the k-nearest data points in the feature space. Simple and effective for small datasets.")
                    ),
                    div(class = "model-description",
                        h4("Multivariate Adaptive Regression Splines"),
                        p("A flexible regression method that models non-linear relationships using piecewise linear functions. Adapts to complex data patterns.")
                    )
                )
              )
      ),
      tabItem(tabName = "about",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary",
                    title = "About This Application",
                    div(class = "info-box",
                        h3("Breeding Value Prediction System"),
                        p("A comprehensive machine learning platform for predicting breeding values in livestock and crop improvement programs.")
                    ),
                    h4("Purpose and Goals"),
                    p("The Breeding Value Prediction System is designed to assist researchers, breeders, and agricultural professionals in predicting breeding values using advanced machine learning techniques. The app aims to streamline the process of evaluating genomic and phenotypic data, enabling users to make informed decisions for selective breeding programs. Our goal is to enhance genetic improvement in livestock and crops by providing accurate, data-driven predictions."),
                    
                    h4("Target Audience"),
                    p("This application is intended for:"),
                    tags$ul(
                      tags$li("Livestock breeders aiming to improve traits like milk yield, growth rate, or disease resistance"),
                      tags$li("Crop scientists working on enhancing yield, drought tolerance, or nutritional quality"),
                      tags$li("Agricultural researchers studying the genetic basis of phenotypic traits"),
                      tags$li("Students and educators in animal science, plant breeding, and genetics")
                    ),
                    
                    h4("Application Features"),
                    tags$ul(
                      tags$li("10 machine learning algorithms for breeding value prediction"),
                      tags$li("Interactive visualizations and performance comparisons"),
                      tags$li("Customizable model parameters for optimal performance"),
                      tags$li("Comprehensive model evaluation metrics"),
                      tags$li("Export functionality for predictions and results"),
                      tags$li("Detailed interpretations of visualizations to aid decision-making"),
                      tags$li("Best model identification with metrics and actual vs predicted plot")
                    ),
                    
                    h4("Technical Stack"),
                    p("The app is built using the following technologies:"),
                    tags$ul(
                      tags$li("R and Shiny for the interactive web application framework"),
                      tags$li("Machine learning libraries: randomForest, neuralnet, xgboost, caret, and more"),
                      tags$li("Visualization libraries: ggplot2, plotly, and DT for interactive plots and tables"),
                      tags$li("Styling: Custom CSS with Google Fonts (Poppins) and Font Awesome icons"),
                      tags$li("Deployment: Designed to run locally or on Shiny Server")
                    ),
                    
                    h4("Instructions"),
                    tags$ol(
                      tags$li("Upload your CSV file containing breeding value data"),
                      tags$li("Select the target breeding value column to predict"),
                      tags$li("Choose from 10 different machine learning models"),
                      tags$li("Adjust model parameters for optimal performance"),
                      tags$li("Run the analysis and view comprehensive results"),
                      tags$li("Compare model performance in the Visualizations tab"),
                      tags$li("View the best model in the Analysis tab"),
                      tags$li("Download predictions for further analysis")
                    ),
                    
                    h4("Model Performance Metrics"),
                    tags$ul(
                      tags$li(tags$strong("RMSE (Root Mean Square Error):"), " Measures prediction accuracy - lower values indicate better performance"),
                      tags$li(tags$strong("MAE (Mean Absolute Error):"), " Average magnitude of prediction errors"),
                      tags$li(tags$strong("R-squared:"), " Proportion of variance explained by the model"),
                      tags$li(tags$strong("Correlation:"), " Linear relationship strength between actual and predicted values")
                    ),
                    
                    h4("Data Requirements"),
                    p("Upload a CSV file with:"),
                    tags$ul(
                      tags$li("Numerical breeding value data"),
                      tags$li("Predictor variables (genomic markers, phenotypic traits, etc.)"),
                      tags$li("At least 20 observations for reliable results"),
                      tags$li("Clean data with minimal missing values")
                    ),
                    
                    h4("Future Improvements"),
                    p("We plan to enhance the app with the following features:"),
                    tags$ul(
                      tags$li("Integration of genomic data preprocessing tools"),
                      tags$li("Support for time-series breeding data"),
                      tags$li("Advanced hyperparameter tuning options"),
                      tags$li("Exportable reports in PDF format"),
                      tags$li("Multi-language support for global accessibility")
                    ),
                    
                    h4("Frequently Asked Questions (FAQ)"),
                    p(tags$strong("Q: What types of data can I use with this app?")),
                    p("A: The app supports CSV files with numerical breeding value data and predictor variables, such as genomic markers or phenotypic traits. Ensure your data has at least 20 observations and minimal missing values."),
                    p(tags$strong("Q: How do I choose the best model?")),
                    p("A: The app automatically identifies the best model in the Analysis tab based on the lowest RMSE. You can also compare all metrics (RMSE, MAE, R², Correlation) in the Visualizations tab to make an informed decision."),
                    p(tags$strong("Q: Can I use this app for non-agricultural data?")),
                    p("A: Yes, as long as your data meets the requirements (numerical target and predictors), the app can be used for any regression task."),
                    p(tags$strong("Q: How can I contribute to this project?")),
                    p("A: Reach out to the developers via email (imstat09@gmail.com) with your suggestions or contributions."),
                    
                    h4("Contact Information"),
                    p(tags$strong("Email:"), " imstat09@gmail.com"),
                    p(tags$strong("YouTube Channel:"), 
                      tags$a("https://www.youtube.com/@Iqbalstat", 
                             href = "https://www.youtube.com/@Iqbalstat", 
                             target = "_blank")),
                    
                    h4("Developers"),
                    p("M. Iqbal Jeelani "),
                    p("Sher-e-Kashmir University of Agricultural Sciences and Technology of Kashmir")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df <- na.omit(df)
    df
  })
  
  # Reset target selection when a new file is uploaded
  observeEvent(input$file, {
    updateSelectInput(session, "target", choices = names(data()), selected = NULL)
  })
  
  output$target_ui <- renderUI({
    req(data())
    selectInput("target", "Select Breeding Value Column", choices = names(data()), selected = NULL)
  })
  
  # Dynamic model parameters UI
  output$model_params <- renderUI({
    req(input$model)
    
    if (input$model == "rf") {
      tagList(
        numericInput("rf_ntree", "Number of Trees", value = 500, min = 10, max = 2000),
        numericInput("rf_mtry", "Variables per Split", value = 3, min = 1, max = 20)
      )
    } else if (input$model == "pcr") {
      numericInput("pcr_ncomp", "Number of Components", value = 5, min = 1, max = 20)
    } else if (input$model == "ann") {
      tagList(
        textInput("ann_hidden", "Hidden Layers (comma-separated)", value = "5,3"),
        numericInput("ann_lr", "Learning Rate", value = 0.01, min = 0.001, max = 0.5, step = 0.005)
      )
    } else if (input$model == "xgb") {
      tagList(
        numericInput("xgb_rounds", "Number of Rounds", value = 100, min = 10, max = 1000),
        numericInput("xgb_eta", "Learning Rate", value = 0.1, min = 0.01, max = 0.5, step = 0.01),
        numericInput("xgb_depth", "Max Depth", value = 6, min = 1, max = 15)
      )
    } else if (input$model == "svm") {
      selectInput("svm_kernel", "Kernel", choices = c("radial", "linear", "polynomial", "sigmoid"))
    } else if (input$model == "gbm") {
      tagList(
        numericInput("gbm_ntrees", "Number of Trees", value = 100, min = 10, max = 1000),
        numericInput("gbm_depth", "Interaction Depth", value = 4, min = 1, max = 10)
      )
    } else if (input$model == "knn") {
      numericInput("knn_k", "Number of Neighbors", value = 5, min = 1, max = 20)
    }
  })
  
  model_result <- reactiveValues(pred = NULL, model = NULL, structure = NULL, test = NULL, model_name = NULL)
  metrics_history <- reactiveValues(data = data.frame(Model = character(), RMSE = numeric(), MAE = numeric(), R2 = numeric(), Correlation = numeric(), TestData = I(list()), stringsAsFactors = FALSE))
  best_model_data <- reactiveValues(name = NULL, metrics = NULL, test = NULL)
  
  observeEvent(input$run, {
    req(data(), input$target)
    
    show_spinner()
    
    df <- data()
    
    # Validate that input$target exists in the data
    if (!(input$target %in% names(df))) {
      hide_spinner()
      showNotification("Error: Target column missing in data. Please select a valid target column.", type = "error")
      return()
    }
    
    predictors <- setdiff(names(df), input$target)
    
    if (length(predictors) == 0) {
      hide_spinner()
      showNotification("Error: No predictor columns available. Please ensure your dataset has at least one predictor column.", type = "error")
      return()
    }
    
    if (length(predictors) > 50) {
      showNotification("Warning: Large number of predictors detected. Consider feature selection for better performance.", type = "warning")
    }
    
    set.seed(123)
    trainIndex <- createDataPartition(df[[input$target]], p = 0.9, list = FALSE)
    trainData <- df[trainIndex, ]
    testData <- df[-trainIndex, ]
    
    x_train <- trainData[, predictors, drop = FALSE]
    y_train <- trainData[[input$target]]
    x_test <- testData[, predictors, drop = FALSE]
    y_test <- testData[[input$target]]
    
    model_result$structure <- NULL
    
    # Model names for display
    model_names <- c("animal" = "Animal Model",
                     "rf" = "Random Forest",
                     "pcr" = "Principal Component Regression",
                     "ann" = "Neural Network",
                     "xgb" = "XGBoost",
                     "svm" = "Support Vector Machine",
                     "gbm" = "Gradient Boosting",
                     "cart" = "Classification and Regression Trees",
                     "knn" = "K-Nearest Neighbors",
                     "mars" = "Multivariate Adaptive Regression Splines")
    
    tryCatch({
      if (input$model == "animal") {
        model <- lm(as.formula(paste(input$target, "~ .")), data = trainData)
        pred <- predict(model, newdata = testData)
      } else if (input$model == "rf") {
        mtry_val <- min(input$rf_mtry, ncol(x_train))
        model <- randomForest(x = x_train, y = y_train, ntree = input$rf_ntree, mtry = mtry_val)
        pred <- predict(model, x_test)
      } else if (input$model == "pcr") {
        model <- pcr(as.formula(paste(input$target, "~ .")), data = trainData, scale = TRUE, validation = "CV")
        ncomp_val <- min(input$pcr_ncomp, min(ncol(trainData)-1, nrow(trainData)-1))
        pred <- predict(model, newdata = testData, ncomp = ncomp_val)[,,1]
      } else if (input$model == "ann") {
        hidden_layers <- as.numeric(unlist(strsplit(input$ann_hidden, ",")))
        train_scaled <- as.data.frame(scale(trainData))
        means <- attr(scale(trainData), "scaled:center")
        sds <- attr(scale(trainData), "scaled:scale")
        test_scaled <- as.data.frame(scale(testData, center = means, scale = sds))
        
        f <- as.formula(paste(input$target, "~", paste(predictors, collapse = " + ")))
        model <- neuralnet(f, data = train_scaled, hidden = hidden_layers,
                           linear.output = TRUE, stepmax = 1e5,
                           threshold = 0.1, learningrate = input$ann_lr)
        
        pred <- compute(model, test_scaled[, predictors])$net.result
        # Store network structure as text
        structure_text <- paste("Input Layer:", length(predictors), "nodes\n",
                                paste("Hidden Layer", 1:length(hidden_layers), ":", hidden_layers, "nodes\n", collapse = ""),
                                "Output Layer: 1 node")
        model_result$structure <- structure_text
      } else if (input$model == "xgb") {
        params <- list(
          objective = "reg:squarederror",
          eta = input$xgb_eta,
          max_depth = input$xgb_depth
        )
        dtrain <- xgb.DMatrix(data = as.matrix(x_train), label = y_train)
        dtest <- xgb.DMatrix(data = as.matrix(x_test))
        model <- xgboost(params = params, data = dtrain, nrounds = input$xgb_rounds, verbose = 0)
        pred <- predict(model, dtest)
      } else if (input$model == "svm") {
        model <- svm(x = x_train, y = y_train, kernel = input$svm_kernel)
        pred <- predict(model, x_test)
      } else if (input$model == "gbm") {
        model <- gbm(as.formula(paste(input$target, "~ .")), data = trainData,
                     distribution = "gaussian", n.trees = input$gbm_ntrees,
                     interaction.depth = input$gbm_depth)
        pred <- predict(model, newdata = testData, n.trees = input$gbm_ntrees)
      } else if (input$model == "cart") {
        model <- rpart(as.formula(paste(input$target, "~ .")), data = trainData)
        pred <- predict(model, newdata = testData)
      } else if (input$model == "knn") {
        model <- caret::knnreg(x = x_train, y = y_train, k = input$knn_k)
        pred <- predict(model, x_test)
      } else if (input$model == "mars") {
        model <- earth(as.formula(paste(input$target, "~ .")), data = trainData)
        pred <- predict(model, newdata = testData)[,1]
      }
      
      model_result$pred <- pred
      model_result$model <- model
      model_result$model_name <- model_names[input$model]
      model_result$test <- data.frame(Actual = y_test, Predicted = as.vector(pred))
      
      # Calculate metrics
      actual <- model_result$test$Actual
      pred_vec <- as.vector(model_result$test$Predicted)
      rmse <- sqrt(mean((actual - pred_vec)^2))
      mae <- mean(abs(actual - pred_vec))
      r <- cor(actual, pred_vec)
      r2 <- r^2
      
      # Store metrics and test data in history
      new_metrics <- data.frame(
        Model = model_names[input$model],
        RMSE = rmse,
        MAE = mae,
        R2 = r2,
        Correlation = r,
        TestData = I(list(model_result$test)),
        stringsAsFactors = FALSE
      )
      
      metrics_history$data <- rbind(metrics_history$data, new_metrics)
      
      # Update best model
      if (nrow(metrics_history$data) >= 2) {
        metrics_df <- metrics_history$data[!duplicated(metrics_history$data$Model, fromLast = TRUE), ]
        best_idx <- which.min(metrics_df$RMSE)
        if (length(best_idx) > 1) {
          best_idx <- best_idx[which.min(metrics_df$MAE[best_idx])]
        }
        best_model_data$name <- metrics_df$Model[best_idx]
        best_model_data$metrics <- metrics_df[best_idx, c("RMSE", "MAE", "R2", "Correlation")]
        best_model_data$test <- metrics_df$TestData[[best_idx]]
      }
      
      hide_spinner()
      showNotification(paste("Model training complete. RMSE:", round(rmse, 4)), type = "message")
      
    }, error = function(e) {
      hide_spinner()
      showNotification(paste("Error in model training:", e$message), type = "error")
    })
  })
  
  output$summary <- renderPrint({
    req(model_result$pred)
    actual <- model_result$test$Actual
    pred <- as.vector(model_result$test$Predicted)
    rmse <- sqrt(mean((actual - pred)^2))
    mae <- mean(abs(actual - pred))
    r <- cor(actual, pred)
    r2 <- r^2
    
    cat("Model Summary:", model_result$model_name, "\n")
    cat("=====================================\n\n")
    cat("Performance Metrics:\n")
    cat("RMSE:", round(rmse, 4), "\n")
    cat("MAE:", round(mae, 4), "\n")
    cat("R-squared:", round(r2, 4), "\n")
    cat("Correlation:", round(r, 4), "\n\n")
    
    if (input$model == "rf") {
      cat("Variable Importance (Top 5):\n")
      imp <- importance(model_result$model)
      imp_df <- data.frame(Variable = rownames(imp), Importance = imp[,1])
      imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE),]
      print(head(imp_df, 5))
    } else if (input$model == "animal") {
      cat("Model Coefficients (Top 5):\n")
      coefs <- summary(model_result$model)$coefficients
      coef_df <- data.frame(Variable = rownames(coefs), Coefficient = coefs[,1], P_value = coefs[,4])
      coef_df <- coef_df[order(abs(coef_df$Coefficient), decreasing = TRUE),]
      print(head(coef_df, 5))
    }
  })
  
  # Neural Network Structure (Text)
  output$nnStructure <- renderPrint({
    req(input$model == "ann", !is.null(model_result$structure))
    cat(model_result$structure)
  })
  
  output$conditionalPlots <- renderUI({
    if (input$model == "ann" && !is.null(model_result$structure)) {
      tagList(
        h4("Neural Network Structure"),
        verbatimTextOutput("nnStructure")
      )
    }
  })
  
  output$predTable <- renderDT({
    req(model_result$test)
    datatable(model_result$test,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv')
              ),
              caption = paste("Predictions from", model_result$model_name)) %>%
      formatRound(columns = c("Actual", "Predicted"), digits = 4)
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("predictions-", gsub(" ", "-", model_result$model_name), "-",
            format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(model_result$test, file, row.names = FALSE)
    }
  )
  
  # Best Model UI
  output$bestModelUI <- renderUI({
    req(!is.null(best_model_data$name))
    tagList(
      h4("Best Performing Model"),
      p(strong("Model: "), best_model_data$name),
      h4("Performance Metrics"),
      DTOutput("bestModelMetricsTable"),
      div(class = "interpretation-box",
          p("The best model is selected based on the lowest RMSE, with MAE as a tiebreaker. Lower RMSE and MAE indicate better predictive accuracy, while higher R² and Correlation suggest better model fit and linear relationship with actual values.")
      ),
      h4("Actual vs Predicted Values for All Models"),
      plotlyOutput("allModelsScatterPlot", height = "400px"),
      div(class = "interpretation-box",
          p("This plot compares actual breeding values (x-axis) to predicted values (y-axis) for all models run, using smoothed lines. Each model is represented by a different color, as shown in the legend. Lines closer to the dashed 45-degree line indicate better predictions. Use this plot to visually compare the predictive trends of all models.")
      )
    )
  })
  
  output$bestModelMetricsTable <- renderDT({
    req(!is.null(best_model_data$metrics))
    datatable(best_model_data$metrics,
              options = list(
                paging = FALSE,
                searching = FALSE,
                ordering = FALSE,
                info = FALSE,
                dom = 't'
              ),
              caption = paste("Metrics for", best_model_data$name)) %>%
      formatRound(columns = c("RMSE", "MAE", "R2", "Correlation"), digits = 4)
  })
  
  output$allModelsScatterPlot <- renderPlotly({
    req(nrow(metrics_history$data) > 0)
    
    # Combine test data from all models
    metrics_df <- metrics_history$data[!duplicated(metrics_history$data$Model, fromLast = TRUE), ]
    
    # Create a single data frame with all actual vs predicted values
    all_data <- do.call(rbind, lapply(1:nrow(metrics_df), function(i) {
      df <- metrics_df$TestData[[i]]
      df$Model <- metrics_df$Model[i]
      return(df)
    }))
    
    # Define a color palette for the models (using a larger set to accommodate up to 10 models)
    model_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f1c40f", "#9b59b6", 
                      "#1abc9c", "#e67e22", "#7f8c8d", "#34495e", "#d35400")
    names(model_colors) <- unique(metrics_df$Model)
    
    # Create the plot with smoothed lines
    p <- ggplot(all_data, aes(x = Actual, y = Predicted, color = Model)) +
      geom_smooth(method = "loess", se = FALSE, size = 1.5) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#2d3436", size = 1) +
      scale_color_manual(values = model_colors) +
      theme_minimal() +
      labs(title = "Actual vs Predicted: All Models",
           x = "Actual Breeding Values",
           y = "Predicted Breeding Values") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", color = "#2d3436"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 10)
      )
    
    ggplotly(p)
  })
  
  # Separate Plots for Each Metric
  output$rmseBarPlot <- renderPlotly({
    req(nrow(metrics_history$data) > 0)
    df <- metrics_history$data
    
    # Remove duplicates and keep latest entry for each model
    df <- df[!duplicated(df$Model, fromLast = TRUE), ]
    
    p <- ggplot(df, aes(x = Model, y = RMSE)) +
      geom_bar(stat = "identity", fill = "#e74c3c") +
      theme_minimal() +
      labs(title = "RMSE Across Models",
           x = "Machine Learning Model",
           y = "RMSE") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", color = "#2d3436"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8)
      )
    
    ggplotly(p)
  })
  
  output$maeBarPlot <- renderPlotly({
    req(nrow(metrics_history$data) > 0)
    df <- metrics_history$data
    
    # Remove duplicates and keep latest entry for each model
    df <- df[!duplicated(df$Model, fromLast = TRUE), ]
    
    p <- ggplot(df, aes(x = Model, y = MAE)) +
      geom_bar(stat = "identity", fill = "#3498db") +
      theme_minimal() +
      labs(title = "MAE Across Models",
           x = "Machine Learning Model",
           y = "MAE") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", color = "#2d3436"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8)
      )
    
    ggplotly(p)
  })
  
  output$r2BarPlot <- renderPlotly({
    req(nrow(metrics_history$data) > 0)
    df <- metrics_history$data
    
    # Remove duplicates and keep latest entry for each model
    df <- df[!duplicated(df$Model, fromLast = TRUE), ]
    
    p <- ggplot(df, aes(x = Model, y = R2)) +
      geom_bar(stat = "identity", fill = "#2ecc71") +
      theme_minimal() +
      labs(title = "R² Across Models",
           x = "Machine Learning Model",
           y = "R²") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", color = "#2d3436"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8)
      )
    
    ggplotly(p)
  })
  
  output$corrBarPlot <- renderPlotly({
    req(nrow(metrics_history$data) > 0)
    df <- metrics_history$data
    
    # Remove duplicates and keep latest entry for each model
    df <- df[!duplicated(df$Model, fromLast = TRUE), ]
    
    p <- ggplot(df, aes(x = Model, y = Correlation)) +
      geom_bar(stat = "identity", fill = "#f1c40f") +
      theme_minimal() +
      labs(title = "Correlation Across Models",
           x = "Machine Learning Model",
           y = "Correlation") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", color = "#2d3436"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8)
      )
    
    ggplotly(p)
  })
  
  output$scatterPlot <- renderPlotly({
    req(model_result$test)
    df <- model_result$test
    
    lm_fit <- lm(Predicted ~ Actual, data = df)
    
    p <- ggplot(df, aes(x = Actual, y = Predicted)) +
      geom_point(color = "#3498db", size = 3, alpha = 0.7) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#e74c3c", size = 1) +
      geom_smooth(method = "lm", se = TRUE, color = "#2ecc71", fill = "#2ecc7155") +
      theme_minimal() +
      labs(title = paste("Actual vs Predicted:", model_result$model_name),
           x = "Actual Breeding Values",
           y = "Predicted Breeding Values",
           subtitle = paste("R² =", round(summary(lm_fit)$r.squared, 4))) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", color = "#2d3436"),
        plot.subtitle = element_text(hjust = 0.5, color = "#7f8c8d")
      )
    
    ggplotly(p)
  })
  
  output$metricsTable <- renderDT({
    req(nrow(metrics_history$data) > 0)
    df <- metrics_history$data[, !names(metrics_history$data) %in% "TestData"]
    # Remove duplicates and keep latest entry for each model
    df <- df[!duplicated(df$Model, fromLast = TRUE), ]
    
    datatable(df,
              options = list(
                pageLength = 12,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv')
              ),
              caption = "Comprehensive Model Performance Comparison") %>%
      formatRound(columns = c("RMSE", "MAE", "R2", "Correlation"), digits = 4)
  })
  
  session$onSessionEnded(function() {
    if (exists("model_result") && !is.null(model_result$model)) {
      rm(model_result$model)
    }
    gc()
  })
}

shinyApp(ui, server)
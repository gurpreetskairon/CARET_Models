shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models
  
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  # load the previously trained models - Note: you can delete files in the SavedModels directory
  for (rdsfile in list.files(path = "SavedModels", pattern = "\\.rds")) {
    name <- gsub(rdsfile, pattern = "\\.rds$", replacement = "")
    rdsfile <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, rdsfile)
    showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
    m <- readRDS(file = rdsfile)  
    models[[name]] <- m
    
    # try to update the preprocessing steps with the ones that were used
    inpId <- paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)), "Preprocess")
    steps <- m$recipe$steps
    seld <- c()
    for (step in steps) {
      s <- gsub(pattern = "step_", replacement = "", x = class(step)[1])
      seld <- c(seld, s)
    }
    if (length(seld) > 0 && seld[1] == "date") { 
      seld <- seld[2:length(seld)] #discard initial date step 
      seld <- seld[seld != "rm"] #discard rm step
    }
    updateSelectizeInput(session = session, inputId = inpId, choices = ppchoices, selected = seld)
    if (length(seld) > 0) {
      showNotification(paste("Setting preprocessing for", name, "to", paste(seld, collapse = ",")), session = session, duration = 3)
    }
  }
  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "Ass3Data.csv", row.names = "ID", stringsAsFactors = TRUE)
    d$TreatmentDate <- as.Date(d$TreatmentDate)
    d
  })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE)
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Y"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode="list", length = n+1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 15, min = 1000, max = 5000)))
    }
    seeds[[n+1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "random",
                 index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, trim = TRUE)
  })
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  

  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    recipe <- recipes::recipe(Y ~ ., data = getTrainData())
  })
  
  # observeEvent NullGo ----
  observeEvent(
    input$NullGo,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 0)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # observeEvent NullGo ----
  output$NullMetrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  # output NullRecipe
  output$NullRecipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------

  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%  # add a numeric date variables
      steps(input$GlmnetPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent GlmnetGo ----
  observeEvent(
    input$GlmnetGo,
    {
      library(glmnet)
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output GlmnetModelSummary (text) ----
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })
  
  # output GlmnetMetrics (table) ----
  output$GlmnetMetrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  # output GlmnetModelPlots (plot) ----
  output$GlmnetModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })
  
  # output GlmnetRecipe (print) ----
  output$GlmnetRecipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  # output GlmnetModelSummary2 (print) ----
  output$GlmnetModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })


  
  
  # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------

  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$PlsPreprocess) %>% 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent PlsGo ----
  observeEvent(
    input$PlsGo,
    {
      library(pls)
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output PlsModelSummary0 (text) ----
  output$PlsModelSummary0 <- renderText({
    description("pls")
  })

  # output PlsMetrics (table) ----
  output$PlsMetrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  # output PlsModelPlots (plot) ----
  output$PlsModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  # output PlsRecipe (print) ----
  output$PlsRecipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  # output PlsModelSummary2 (print) ----
  output$PlsModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  

  
  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------

  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$RpartPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent RpartGo ----
  observeEvent(
    input$RpartGo,
    {
      library(rpart)
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  # output RpartModelSummary0 (print) ----
  output$RpartModelSummary0 <- renderText({
    description("rpart")
  })
  
  # output RpartMetrics (table) ----
  output$RpartMetrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  # output RpartRecipe (print) ----
  output$RpartRecipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  # output RpartModelPlots (plot) ----
  output$RpartModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  # output RpartModelTree (plot) ----
  output$RpartModelTree <- renderPlot({
    library(rpart.plot)
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  })     
  

  
  # maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # add further methods here  
  # METHOD * blasso ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getblassoRecipe ----
  getblassoRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$blassoPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent blassoGo ----
  observeEvent(
    input$blassoGo,
    {
      library(monomvn)
      method <- "blasso"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getblassoRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output blassoModelSummary0 (print) ----
  output$blassoModelSummary0 <- renderText({
    description("blasso")
  })
  
  # output blassoMetrics (table) ----
  output$blassoMetrics <- renderTable({
    req(models$blasso)
    models$blasso$results[ which.min(models$blasso$results[, "RMSE"]), ]
  })
  
  # output blassoRecipe (print) ----
  output$blassoRecipe <- renderPrint({
    req(models$blasso)
    models$blasso$recipe
  })  
  
  # output blassoModelPlots (plot) ----
  output$blassoModelPlots <- renderPlot({
    req(models$blasso)
    plot(models$blasso)
  })
  
  # output blassoModelTree (plot) ----
  output$blassoModelTree <- renderPlot({
    library(monomvn.plot)
    req(models$blasso)
    monomvn.plot::monomvn.plot(models$blasso$finalModel, roundint = FALSE)
  }) 
  
  # output blassoModelSummary2 (print) ----
  output$blassoModelSummary2 <- renderPrint({
    req(models$blasso)
    summary(models$blasso$finalModel)
  })
  
  
  # METHOD * Cubist ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getCubistRecipe ----
  getCubistRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$CubistPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent CubistGo ----
  observeEvent(
    input$CubistGo,
    {
      library(Cubist)
      method <- "cubist"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getCubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output CubistModelSummary0 (print) ----
  output$CubistModelSummary0 <- renderText({
    description("cubist")
  })
  
  # output CubistMetrics (table) ----
  output$CubistMetrics <- renderTable({
    req(models$cubist)
    models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
  })
  
  # output CubistRecipe (print) ----
  output$CubistRecipe <- renderPrint({
    req(models$cubist)
    models$cubist$recipe
  })  
  
  # output CubistModelPlots (plot) ----
  output$CubistModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  })
  
  
  # METHOD * svmLinear3 ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getsvmLinear3Recipe ----
  getsvmLinear3Recipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$svmLinear3Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent svmLinear3Go ----
  observeEvent(
    input$svmLinear3Go,
    {
      library(LiblineaR)
      method <- "svmLinear3"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getsvmLinear3Recipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output svmLinear3ModelSummary0 (print) ----
  output$svmLinear3ModelSummary0 <- renderText({
    description("svmLinear3")
  })
  
  # output svmLinear3Metrics (table) ----
  output$svmLinear3Metrics <- renderTable({
    req(models$svmLinear3)
    models$svmLinear3$results[ which.min(models$svmLinear3$results[, "RMSE"]), ]
  })
  
  # output svmLinear3Recipe (print) ----
  output$svmLinear3Recipe <- renderPrint({
    req(models$svmLinear3)
    models$svmLinear3$recipe
  })  
  
  # output svmLinear3ModelPlots (plot) ----
  output$svmLinear3ModelPlots <- renderPlot({
    req(models$svmLinear3)
    plot(models$svmLinear3)
  })
  
  # output svmLinear3ModelSummary2 (print) ----
  output$svmLinear3ModelSummary2 <- renderPrint({
    req(models$svmLinear3)
    summary(models$svmLinear3$finalModel)
  })

  # METHOD * bridge ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getbridgeRecipe ----
  getbridgeRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$bridgePreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent bridgeGo ----
  observeEvent(
    input$bridgeGo,
    {
      library(monomvn)
      method <- "bridge"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getbridgeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output bridgeModelSummary0 (print) ----
  output$bridgeModelSummary0 <- renderText({
    description("bridge")
  })
  
  # output bridgeMetrics (table) ----
  output$bridgeMetrics <- renderTable({
    req(models$bridge)
    models$bridge$results[ which.min(models$bridge$results[, "RMSE"]), ]
  })
  
  # output bridgeRecipe (print) ----
  output$bridgeRecipe <- renderPrint({
    req(models$bridge)
    models$bridge$recipe
  })  
  
  # output bridgeModelPlots (plot) ----
  output$bridgeModelPlots <- renderPlot({
    req(models$bridge)
    plot(models$bridge)
  })
  
  
  # output bridgeModelSummary2 (print) ----
  output$bridgeModelSummary2 <- renderPrint({
    req(models$bridge)
    summary(models$bridge$finalModel)
  })
  
  
  # METHOD * krlsPoly ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getkrlsPolyRecipe ----
  getkrlsPolyRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$krlsPolyPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent krlsPolyGo ----
  observeEvent(
    input$krlsPolyGo,
    {
      library(KRLS)
      method <- "krlsPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getkrlsPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output krlsPolyModelSummary0 (print) ----
  output$krlsPolyModelSummary0 <- renderText({
    description("krlsPoly")
  })
  
  # output krlsPolyMetrics (table) ----
  output$krlsPolyMetrics <- renderTable({
    req(models$krlsPoly)
    models$krlsPoly$results[ which.min(models$krlsPoly$results[, "RMSE"]), ]
  })
  
  # output krlsPolyRecipe (print) ----
  output$krlsPolyRecipe <- renderPrint({
    req(models$krlsPoly)
    models$krlsPoly$recipe
  })  
  
  # output krlsPolyModelPlots (plot) ----
  output$krlsPolyModelPlots <- renderPlot({
    req(models$krlsPoly)
    plot(models$krlsPoly)
  })
  
  # output krlsPolyModelTree (plot) ----
  output$krlsPolyModelTree <- renderPlot({
    library(KRLS.plot)
    req(models$krlsPoly)
    KRLS.plot::KRLS.plot(models$krlsPoly$finalModel, roundint = FALSE)
  })     
  
  # output krlsPolyModelSummary2 (print) ----
  output$krlsPolyModelSummary2 <- renderPrint({
    req(models$krlsPoly)
    summary(models$krlsPoly$finalModel)
  })
  
  
  # METHOD * qrf ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getqrfRecipe ----
  getqrfRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$qrfPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent qrfGo ----
  observeEvent(
    input$qrfGo,
    {
      library(quantregForest)
      method <- "qrf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getqrfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output qrfModelSummary0 (print) ----
  output$qrfModelSummary0 <- renderText({
    description("qrf")
  })
  
  # output qrfMetrics (table) ----
  output$qrfMetrics <- renderTable({
    req(models$qrf)
    models$qrf$results[ which.min(models$qrf$results[, "RMSE"]), ]
  })
  
  # output qrfRecipe (print) ----
  output$qrfRecipe <- renderPrint({
    req(models$qrf)
    models$qrf$recipe
  })  
  
  # output qrfModelPlots (plot) ----
  output$qrfModelPlots <- renderPlot({
    req(models$qrf)
    plot(models$qrf)
  })

  
  # output qrfModelSummary2 (print) ----
  output$qrfModelSummary2 <- renderPrint({
    req(models$qrf)
    summary(models$qrf$finalModel)
  })
  
  
  # METHOD * bstTree ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getbstTreeRecipe ----
  getbstTreeRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$bstTreePreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent bstTreeGo ----
  observeEvent(
    input$bstTreeGo,
    {
      library(bst)
      library(plyr)
      method <- "bstTree"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getbstTreeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output bstTreeModelSummary0 (print) ----
  output$bstTreeModelSummary0 <- renderText({
    description("bstTree")
  })
  
  # output bstTreeMetrics (table) ----
  output$bstTreeMetrics <- renderTable({
    req(models$bstTree)
    models$bstTree$results[ which.min(models$bstTree$results[, "RMSE"]), ]
  })
  
  # output bstTreeRecipe (print) ----
  output$bstTreeRecipe <- renderPrint({
    req(models$bstTree)
    models$bstTree$recipe
  })  
  
  # output bstTreeModelPlots (plot) ----
  output$bstTreeModelPlots <- renderPlot({
    req(models$bstTree)
    plot(models$bstTree)
  })
  
  # output bstTreeModelTree (plot) ----
  output$bstModelTree <- renderPlot({
    library(bst.plot)
    req(models$bstTree)
    bst.plot::bst.plot(models$bstTree$finalModel, roundint = FALSE)
  }) 
  
  # output bstTreeModelSummary2 (print) ----
  output$bstTreeModelSummary2 <- renderPrint({
    req(models$bstTree)
    summary(models$bstTree$finalModel)
  })
  
  
  # METHOD * M5 ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getM5Recipe ----
  getM5Recipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$M5Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent M5Go ----
  observeEvent(
    input$M5Go,
    {
      library(RWeka)
      method <- "M5"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getM5Recipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output M5ModelSummary0 (print) ----
  output$M5ModelSummary0 <- renderText({
    description("M5")
  })
  
  # output M5Metrics (table) ----
  output$M5Metrics <- renderTable({
    req(models$M5)
    models$M5$results[ which.min(models$M5$results[, "RMSE"]), ]
  })
  
  # output M5Recipe (print) ----
  output$M5Recipe <- renderPrint({
    req(models$M5)
    models$M5$recipe
  })  
  
  # output M5ModelPlots (plot) ----
  output$M5ModelPlots <- renderPlot({
    req(models$M5)
    plot(models$M5)
  })
  
  
  # output M5ModelSummary2 (print) ----
  output$M5ModelSummary2 <- renderPrint({
    req(models$M5)
    summary(models$M5$finalModel)
  })
  
  
  # METHOD * gbm ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getgbmRecipe ----
  getgbmRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$gbmPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent gbmGo ----
  observeEvent(
    input$gbmGo,
    {
      library(gbm)
      library(plyr)
      method <- "gbm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getgbmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output gbmModelSummary0 (print) ----
  output$gbmModelSummary0 <- renderText({
    description("gbm")
  })
  
  # output gbmMetrics (table) ----
  output$gbmMetrics <- renderTable({
    req(models$gbm)
    models$gbm$results[ which.min(models$gbm$results[, "RMSE"]), ]
  })
  
  # output gbmRecipe (print) ----
  output$gbmRecipe <- renderPrint({
    req(models$gbm)
    models$gbm$recipe
  })  
  
  # output gbmModelPlots (plot) ----
  output$gbmModelPlots <- renderPlot({
    req(models$gbm)
    plot(models$gbm)
  })
  
  # output gbmModelSummary2 (print) ----
  output$gbmModelSummary2 <- renderPrint({
    req(models$gbm)
    summary(models$gbm$finalModel)
  })
  
  # output gbmModelTree (plot) ----
  output$gbmModelTree <- renderPlot({
    library(gbm.plot)
    req(models$gbm)
    gbm.plot::gbm.plot(models$gbm$finalModel, roundint = FALSE)
  }) 
  
  # METHOD * treebag ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive gettreebagRecipe ----
  gettreebagRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$treebagPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent treebagGo ----
  observeEvent(
    input$treebagGo,
    {
      library(ipred)
      library(plyr)
      library(e1071)
      method <- "treebag"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(gettreebagRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output treebagModelSummary0 (print) ----
  output$treebagModelSummary0 <- renderText({
    description("treebag")
  })
  
  # output treebagMetrics (table) ----
  output$treebagMetrics <- renderTable({
    req(models$treebag)
    models$treebag$results[ which.min(models$treebag$results[, "RMSE"]), ]
  })
  
  # output treebagRecipe (print) ----
  output$treebagRecipe <- renderPrint({
    req(models$treebag)
    models$treebag$recipe
  })  
  
  # output treebagModelPlots (plot) ----
  output$treebagModelPlots <- renderPlot({
    req(models$treebag)
    plot(models$treebag)
  })
  
  # output treebagModelSummary2 (print) ----
  output$treebagModelSummary2 <- renderPrint({
    req(models$treebag)
    summary(models$treebag$finalModel)
  })
  
  # output treebagModelTree (plot) ----
  output$treebagModelTree <- renderPlot({
    library(ipred.plot)
    req(models$treebag)
    ipred.plot::ipred.plot(models$treebag$finalModel, roundint = FALSE)
  }) 
  
  # METHOD * gaussprRadial ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getgaussprRadialRecipe ----
  getgaussprRadialRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$gaussprRadialPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent gaussprRadialGo ----
  observeEvent(
    input$gaussprRadialGo,
    {
      library(kernlab)
      method <- "gaussprRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getgaussprRadialRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output gaussprRadialModelSummary0 (print) ----
  output$gaussprRadialModelSummary0 <- renderText({
    description("gaussprRadial")
  })
  
  # output gaussprRadialMetrics (table) ----
  output$gaussprRadialMetrics <- renderTable({
    req(models$gaussprRadial)
    models$gaussprRadial$results[ which.min(models$gaussprRadial$results[, "RMSE"]), ]
  })
  
  # output gaussprRadialRecipe (print) ----
  output$gaussprRadialRecipe <- renderPrint({
    req(models$gaussprRadial)
    models$gaussprRadial$recipe
  })  
  
  # output gaussprRadialModelPlots (plot) ----
  output$gaussprRadialModelPlots <- renderPlot({
    req(models$gaussprRadial)
    plot(models$gaussprRadial)
  })
  
  
  # output gaussprRadialModelSummary2 (print) ----
  output$gaussprRadialModelSummary2 <- renderPrint({
    req(models$gaussprRadial)
    summary(models$gaussprRadial$finalModel)
  })
  
  # METHOD * gaussprPoly ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getgaussprPolyRecipe ----
  getgaussprPolyRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$gaussprPolyPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent gaussprPolyGo ----
  observeEvent(
    input$gaussprPolyGo,
    {
      library(kernlab)
      method <- "gaussprPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getgaussprPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output gaussprPolyModelSummary0 (print) ----
  output$gaussprPolyModelSummary0 <- renderText({
    description("gaussprPoly")
  })
  
  # output gaussprPolyMetrics (table) ----
  output$gaussprPolyMetrics <- renderTable({
    req(models$gaussprPoly)
    models$gaussprPoly$results[ which.min(models$gaussprPoly$results[, "RMSE"]), ]
  })
  
  # output gaussprPolyRecipe (print) ----
  output$gaussprPolyRecipe <- renderPrint({
    req(models$gaussprPoly)
    models$gaussprPoly$recipe
  })  
  
  # output gaussprPolyModelPlots (plot) ----
  output$gaussprPolyModelPlots <- renderPlot({
    req(models$gaussprPoly)
    plot(models$gaussprPoly)
  })
  
  # output gaussprPolyModelSummary2 (print) ----
  output$gaussprPolyModelSummary2 <- renderPrint({
    req(models$gaussprPoly)
    summary(models$gaussprPoly$finalModel)
  })
  
  
  # METHOD * gamSpline ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getgamSplineRecipe ----
  getgamSplineRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$gamSplinePreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent gamSplineGo ----
  observeEvent(
    input$gamSplineGo,
    {
      library(gam)
      method <- "gamSpline"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getgamSplineRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output gamSplineModelSummary0 (print) ----
  output$gamSplineModelSummary0 <- renderText({
    description("gamSpline")
  })
  
  # output gamSplineMetrics (table) ----
  output$gamSplineMetrics <- renderTable({
    req(models$gamSpline)
    models$gamSpline$results[ which.min(models$gamSpline$results[, "RMSE"]), ]
  })
  
  # output gamSplineRecipe (print) ----
  output$gamSplineRecipe <- renderPrint({
    req(models$gamSpline)
    models$gamSpline$recipe
  })  
  
  # output gamSplineModelPlots (plot) ----
  output$gamSplineModelPlots <- renderPlot({
    req(models$gamSpline)
    plot(models$gamSpline)
  })
  
  # output gamSplineModelSummary2 (print) ----
  output$gamSplineModelSummary2 <- renderPrint({
    req(models$gamSpline)
    summary(models$gamSpline$finalModel)
  })
  
  
  
  
  # METHOD * kknn ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getkknnRecipe ----
  getkknnRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$kknnPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent kknnGo ----
  observeEvent(
    input$kknnGo,
    {
      library(kknn)
      method <- "kknn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getkknnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output kknnModelSummary0 (print) ----
  output$kknnModelSummary0 <- renderText({
    description("kknn")
  })
  
  # output kknnMetrics (table) ----
  output$kknnMetrics <- renderTable({
    req(models$kknn)
    models$kknn$results[ which.min(models$kknn$results[, "RMSE"]), ]
  })
  
  # output kknnRecipe (print) ----
  output$kknnRecipe <- renderPrint({
    req(models$kknn)
    models$kknn$recipe
  })  
  
  # output kknnModelPlots (plot) ----
  output$kknnModelPlots <- renderPlot({
    req(models$kknn)
    plot(models$kknn)
  })
  
  
  # output kknnModelSummary2 (print) ----
  output$kknnModelSummary2 <- renderPrint({
    req(models$kknn)
    summary(models$kknn$finalModel)
  })
  
  
  
  
  # METHOD * brnn ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getbrnnRecipe ----
  getbrnnRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$brnnPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent brnnGo ----
  observeEvent(
    input$brnnGo,
    {
      library(brnn)
      method <- "brnn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getbrnnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output brnnModelSummary0 (print) ----
  output$brnnModelSummary0 <- renderText({
    description("brnn")
  })
  
  # output brnnMetrics (table) ----
  output$brnnMetrics <- renderTable({
    req(models$brnn)
    models$brnn$results[ which.min(models$brnn$results[, "RMSE"]), ]
  })
  
  # output brnnRecipe (print) ----
  output$brnnRecipe <- renderPrint({
    req(models$brnn)
    models$brnn$recipe
  })  
  
  # output brnnModelPlots (plot) ----
  output$brnnModelPlots <- renderPlot({
    req(models$brnn)
    plot(models$brnn)
  })
  
  # output brnnModelTree (plot) ----
  output$brnnModelTree <- renderPlot({
    library(brnn.plot)
    req(models$brnn)
    brnn.plot::brnn.plot(models$brnn$finalModel, roundint = FALSE)
  })    
  
  # output brnnModelSummary2 (print) ----
  output$brnnModelSummary2 <- renderPrint({
    req(models$brnn)
    summary(models$brnn$finalModel)
  })
  
  
  
  
  # METHOD * rbfDDA ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getrbfDDARecipe ----
  getrbfDDARecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$rbfDDAPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent rbfDDAGo ----
  observeEvent(
    input$rbfDDAGo,
    {
      library(RSNNS)
      method <- "rbfDDA"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getrbfDDARecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output rbfDDAModelSummary0 (print) ----
  output$rbfDDAModelSummary0 <- renderText({
    description("rbfDDA")
  })
  
  # output rbfDDAMetrics (table) ----
  output$rbfDDAMetrics <- renderTable({
    req(models$rbfDDA)
    models$rbfDDA$results[ which.min(models$rbfDDA$results[, "RMSE"]), ]
  })
  
  # output rbfDDARecipe (print) ----
  output$rbfDDARecipe <- renderPrint({
    req(models$rbfDDA)
    models$rbfDDA$recipe
  })  
  
  # output rbfDDAModelPlots (plot) ----
  output$rbfDDAModelPlots <- renderPlot({
    req(models$rbfDDA)
    plot(models$rbfDDA)
  })
  
  
  # output rbfDDAModelSummary2 (print) ----
  output$rbfDDAModelSummary2 <- renderPrint({
    req(models$rbfDDA)
    summary(models$rbfDDA$finalModel)
  })
  
  
  # METHOD * pcaNNet ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getpcaNNetRecipe ----
  getpcaNNetRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$pcaNNetPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent pcaNNetGo ----
  observeEvent(
    input$pcaNNetGo,
    {
      library(nnet)
      method <- "pcaNNet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getpcaNNetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output pcaNNetModelSummary0 (print) ----
  output$pcaNNetModelSummary0 <- renderText({
    description("pcaNNet")
  })
  
  # output pcaNNetMetrics (table) ----
  output$pcaNNetMetrics <- renderTable({
    req(models$pcaNNet)
    models$pcaNNet$results[ which.min(models$pcaNNet$results[, "RMSE"]), ]
  })
  
  # output pcaNNetRecipe (print) ----
  output$pcaNNetRecipe <- renderPrint({
    req(models$pcaNNet)
    models$pcaNNet$recipe
  })  
  
  # output pcaNNetModelPlots (plot) ----
  output$pcaNNetModelPlots <- renderPlot({
    req(models$pcaNNet)
    plot(models$pcaNNet)
  })
  

  # output pcaNNetModelSummary2 (print) ----
  output$pcaNNetModelSummary2 <- renderPrint({
    req(models$pcaNNet)
    summary(models$pcaNNet$finalModel)
  })
  
  
  # METHOD * svmPoly ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getsvmPolyRecipe ----
  getsvmPolyRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$svmPolyPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent svmPolyGo ----
  observeEvent(
    input$svmPolyGo,
    {
      library(kernlab)
      method <- "svmPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getsvmPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output svmPolyModelSummary0 (print) ----
  output$svmPolyModelSummary0 <- renderText({
    description("svmPoly")
  })
  
  # output svmPolyMetrics (table) ----
  output$svmPolyMetrics <- renderTable({
    req(models$svmPoly)
    models$svmPoly$results[ which.min(models$svmPoly$results[, "RMSE"]), ]
  })
  
  # output svmPolyRecipe (print) ----
  output$svmPolyRecipe <- renderPrint({
    req(models$svmPoly)
    models$svmPoly$recipe
  })  
  
  # output svmPolyModelPlots (plot) ----
  output$svmPolyModelPlots <- renderPlot({
    req(models$svmPoly)
    plot(models$svmPoly)
  })
  
  
  # output svmPolyModelSummary2 (print) ----
  output$svmPolyModelSummary2 <- renderPrint({
    req(models$svmPoly)
    summary(models$svmPoly$finalModel)
  })
  
  
  
  
  # METHOD * bagEarth ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getbagEarthRecipe ----
  getbagEarthRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$bagEarthPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent bagEarthGo ----
  observeEvent(
    input$bagEarthGo,
    {
      library(caret)
      method <- "bagEarth"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getbagEarthRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output bagEarthModelSummary0 (print) ----
  output$bagEarthModelSummary0 <- renderText({
    description("bagEarth")
  })
  
  # output bagEarthMetrics (table) ----
  output$bagEarthMetrics <- renderTable({
    req(models$bagEarth)
    models$bagEarth$results[ which.min(models$bagEarth$results[, "RMSE"]), ]
  })
  
  # output bagEarthRecipe (print) ----
  output$bagEarthRecipe <- renderPrint({
    req(models$bagEarth)
    models$bagEarth$recipe
  })  
  
  # output bagEarthModelPlots (plot) ----
  output$bagEarthModelPlots <- renderPlot({
    req(models$bagEarth)
    plot(models$bagEarth)
  })
  
  # output bagEarthModelTree (plot) ----
  output$bagEarthModelTree <- renderPlot({
    library(caret.plot)
    req(models$bagEarth)
    caret.plot::caret.plot(models$bagEarth$finalModel, roundint = FALSE)
  })     
  
  # output bagEarthModelSummary2 (print) ----
  output$bagEarthModelSummary2 <- renderPrint({
    req(models$bagEarth)
    summary(models$bagEarth$finalModel)
  })
  
  
  # METHOD * blackboost ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getblackboostRecipe ----
  getblackboostRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      step_date(has_type("date"), features = "decimal") %>%
      steps(input$blackboostPreprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent blackboostGo ----
  observeEvent(
    input$blackboostGo,
    {
      library(mboost)
      library(party)
      library(plyr)
      library(partykit)
      
      method <- "blackboost"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getblackboostRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output blackboostModelSummary0 (print) ----
  output$blackboostModelSummary0 <- renderText({
    description("blackboost")
  })
  
  # output blackboostMetrics (table) ----
  output$blackboostMetrics <- renderTable({
    req(models$blackboost)
    models$blackboost$results[ which.min(models$blackboost$results[, "RMSE"]), ]
  })
  
  # output blackboostRecipe (print) ----
  output$blackboostRecipe <- renderPrint({
    req(models$blackboost)
    models$blackboost$recipe
  })  
  
  # output blackboostModelPlots (plot) ----
  output$blackboostModelPlots <- renderPlot({
    req(models$blackboost)
    plot(models$blackboost)
  })
  
  # output blackboostModelTree (plot) ----
  output$blackboostModelTree <- renderPlot({
    library(party.plot)
    req(models$blackboost)
    party.plot::party.plot(models$blackboost$finalModel, roundint = FALSE)
  })     
  
  # output blackboostModelSummary2 (print) ----
  output$blackboostModelSummary2 <- renderPrint({
    req(models$blackboost)
    summary(models$blackboost$finalModel)
  })
  
  
  
  
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------

  
  # reactive getResamples ----
  getResamples <- reactive({
    models <- reactiveValuesToList(models)
    results <- caret::resamples(models)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }

    #hide results worse than null model
    subset <- rep(TRUE, length(models))
    if (input$HideWorse & NullModel %in% names(models)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Y, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })

  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Y, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })

  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
    
})

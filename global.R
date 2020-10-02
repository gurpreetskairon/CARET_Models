library(shiny)
library(shinyBS) # Additional Bootstrap Controls
library(DT)
library(corrgram)
library(visdat)
library(shinycssloaders)

# The recipes package is central to preprocessing
library(recipes)

# We employ a form of parallelism that works for MAC/Windows/Ubuntu
library(doParallel)

# This code implements the CARET framework: see http://topepo.github.io/caret/index.html for details
library(caret)

if (!library("mixOmics", quietly = TRUE, logical.return = TRUE)) {
  library(devtools)
  install_github("mixOmicsTeam/mixOmics")  # for some reason this has been withdrawn from CRAN
  library("mixOmics")
}

ppchoices <- c("knnimpute", "bagimpute", "medianimpute", "modeimpute", "YeoJohnson", "naomit", "pca", "pls", "ica", "center", "scale", "date", "nzv", "other", "dummy")

startMode <- function(Parallel = TRUE) {
  if (Parallel) {
    clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
    registerDoParallel(clus)
    clus
  } else {
    NULL
  }
}

stopMode <- function(clus) {
  if (!is.null(clus)) {
    stopCluster(clus)
    registerDoSEQ()
  }
}

steps <- function(recipe, preprocess) {
  for (s in preprocess) {
    if (s == "knnimpute") {
      recipe <- step_knnimpute(recipe, all_numeric(), all_nominal(), -has_role("outcome"), -has_role("id"), k = 5) # 5 is a reasonable guess
    } else if (s == "bagimpute") {
      recipe <- step_bagimpute(recipe, all_numeric(), all_nominal(), -has_role("outcome"), -has_role("id"))
    } else if (s == "medianimpute") {
      recipe <- step_medianimpute(recipe, all_numeric(), -has_role("outcome"), -has_role("id"))
    } else if (s == "modeimpute") {
      recipe <- step_modeimpute(recipe, all_nominal(), -has_role("outcome"), -has_role("id"))
    } else if (s == "YeoJohnson") {
      recipe <- step_YeoJohnson(recipe, all_numeric(), -has_role("outcome"), -has_role("id"))
    } else if (s == "naomit") {
      recipe <- step_naomit(recipe, all_predictors(), skip = TRUE)
    } else if (s == "pca") {
      recipe <- step_pca(recipe, all_numeric(), -has_role("outcome"), -has_role("id"), threshold = 0.95)
    } else if (s == "pls") {
      recipe <- step_pls(recipe, all_numeric(), -has_role("outcome"), -has_role("id"), outcome = "Y", num_comp = 25)
    } else if (s == "ica") {
      recipe <- step_ica(recipe, all_numeric(), -has_role("outcome"), -has_role("id"))
    } else if (s == "center") {
      recipe <- step_center(recipe, all_numeric(), -has_role("outcome"), -has_role("id"))
    } else if (s == "scale") {
      recipe <- step_scale(recipe, all_numeric(), -has_role("outcome"), -has_role("id"))
    } else if (s == "date") {
      recipe <- step_date(recipe, has_type("date"), features = c("dow", "month", "year"), ordinal = FALSE)
    } else if (s == "nzv") {
      recipe <- step_nzv(recipe, all_predictors())
    } else if (s == "other") {
      recipe <- step_other(recipe, all_nominal(), -has_role("outcome"), -has_role("id"))
    } else if (s == "dummy") {
      recipe <- step_dummy(recipe, all_nominal(), -has_role("outcome"), -has_role("id"), one_hot = FALSE)
    } else if (s == "poly") {
      recipe <- step_poly(recipe, all_numeric(), -has_role("outcome"), -has_role("id"), options = list(degree = 2))
    }
  }
  recipe
}

description <- function(name) {
  regexName <- paste0("^", name, "$") # force an regular expression exact match
  mlist <- caret::getModelInfo(model = regexName)[[name]]
  line1 <- paste0("Method \"", name, "\" is able to do ", paste(collapse = " and ", mlist$type), ".")
  line2 <- paste0("It uses parameters: ", paste0(collapse = ", ", mlist$parameters$parameter), ".")
  line3 <- paste0("Its characteristics are: ", paste0(collapse = ", ", mlist$tags))
  paste(sep = "\n", line1, line2, line3)
}

deleteRds <- function(name) {
  ok <- TRUE
  file <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, name, ".rds")
  if (file.exists(file)) {
    ok <- unlink(file, force = TRUE)
  }
  ok
}

saveToRds <- function(object, name) {
  file <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, name, ".rds")
  saveRDS(object, file)
}

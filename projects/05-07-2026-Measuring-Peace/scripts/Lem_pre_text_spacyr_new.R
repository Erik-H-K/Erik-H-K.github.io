### Get factorial_preprocessing from preText to work, with one-time lemmatization

# ---------------------------------------------------------------------------
# Helper: lemmatize the corpus exactly once via the Python (spaCy) helper.
# Reads/writes through intermediate_directory so there are no hardcoded paths,
# and the R<->Python file contract is explicit:
#   R writes  -> to_lem.csv              (column: text)
#   Python    -> reads to_lem.csv, writes string_lemmatized_py.csv (column: lem_text)
#   R reads   <- string_lemmatized_py.csv
# ---------------------------------------------------------------------------
lemmatize_once <- function(text,
                           intermediate_directory,
                           python_script = python_script) {
                        
  
  cat("DEBUG intermediate_directory in lemmatize_once:", intermediate_directory, "\n")

  
  in_path  <- file.path(intermediate_directory, "to_lem.csv")
  out_path <- file.path(intermediate_directory, "string_lemmatized_py.csv")
  
  cat("DEBUG in_path:", in_path, "\n")   # now in_path exists
  cat("DEBUG out_path:", out_path, "\n")
  
  # quanteda::texts() pulls the character vector out of a corpus object;
  # as.character() guards against any non-string entries reaching write_csv.
  raw_strings <- as.character(text)

  text_df <- data.frame(text = raw_strings,
                        index = seq_along(raw_strings),
                        stringsAsFactors = FALSE)
  readr::write_csv(text_df, file = in_path)

  # lem.py reads to_lem.csv and writes string_lemmatized_py.csv.
  # I pass the paths via environment variables so the script isn't hardcoded
  # to one machine. 
  Sys.setenv(LEM_IN_PATH = in_path, LEM_OUT_PATH = out_path)
  cat("DEBUG Sys.getenv LEM_IN_PATH:", Sys.getenv("LEM_IN_PATH"), "\n")
  reticulate::py_run_file(python_script)

  out_df <- readr::read_csv(out_path, show_col_types = FALSE)
  stringr::str_squish(out_df$lem_text)
}


factorial_preprocessing_lem <- function(text,
                                        use_ngrams = TRUE,
                                        infrequent_term_threshold = 0.01,
                                        parallel = FALSE,
                                        cores = 1,
                                        save_dfm = FALSE,
                                        language = "en",
                                        custom_stopwords = NULL,
                                        intermediate_directory = NULL,
                                        parameterization_range = NULL,
                                        return_results = TRUE,
                                        python_script = "scripts/lem.py",
                                        verbose = TRUE){

  # set some intermediate variables
  cur_directory <- getwd()
  # set working directory if given
  if (!is.null(intermediate_directory)) {
    setwd(intermediate_directory)
  } else {
    intermediate_directory <- cur_directory
  }
  cat("DEBUG intermediate_directory at top:", intermediate_directory, "\n")
  # make sure we restore the working directory even if something errors below
  on.exit(setwd(cur_directory), add = TRUE)

  ## check to see if input is a vector of text or corpus object. and add language field
  if(is.character(text) & !quanteda::is.corpus(text)){
    text <- quanteda::corpus(text,
                             meta = list("language" = language))
  }
  if(quanteda::is.corpus(text) & is.null(quanteda::meta(text)$language) ){
    quanteda::meta(text)$language <- language
  }
  # Stop only when the input is NEITHER a character vector NOR a corpus.
  if (!is.character(text) & !quanteda::is.corpus(text)){
    stop("You must provide either a character vector of strings (one per document), or a quanteda corpus object.")
  }

  # create a data.frame with factorial combinations of all choices.
  if (use_ngrams) {
    cat("Preprocessing", length(text), "documents 128 different ways...\n")
    choices <- data.frame(expand.grid(list(removePunctuation = c(TRUE,FALSE),
                                           removeNumbers = c(TRUE,FALSE),
                                           lowercase = c(TRUE,FALSE),
                                           lem = c(TRUE,FALSE), #was stem
                                           removeStopwords = c(TRUE,FALSE),
                                           infrequent_terms = c(TRUE, FALSE),
                                           use_ngrams = c(TRUE, FALSE))))

    labels <- rep("",128)
    # indicators of different preprocessing steps
    labs <- c("P","N","L","LE","W","I","3")
    for (i in 1:128) {
      str <- ""
      for (j in 1:7) {
        if (choices[i,j]) {
          if (str == "") {
            str <- labs[j]
          } else {
            str <- paste(str,labs[j],sep = "-")
          }
        }
      }
      labels[i] <- str
    }
  } else {
    cat("Preprocessing", length(text), "documents 64 different ways...\n")
    choices <- data.frame(expand.grid(list(removePunctuation = c(TRUE,FALSE),
                                           removeNumbers = c(TRUE,FALSE),
                                           lowercase = c(TRUE,FALSE),
                                           lem = c(TRUE,FALSE), #was stem
                                           removeStopwords = c(TRUE,FALSE),
                                           infrequent_terms = c(TRUE, FALSE),
                                           use_ngrams = c(FALSE))))

    labels <- rep("",64)
    # indicators of different preprocessing steps
    labs <- c("P","N","L","LE","W","I","3")
    for (i in 1:64) {
      str <- ""
      for (j in 1:7) {
        if (choices[i,j]) {
          if (str == "") {
            str <- labs[j]
          } else {
            str <- paste(str,labs[j],sep = "-")
          }
        }
      }
      labels[i] <- str
    }
  }

  raw_text <- text

  # Only pay the lemmatization cost if at least one combination needs it.
  if (any(choices$lem)) {
    if (verbose) cat("Lemmatizing corpus once (cached for all combinations)...\n")
    lemmatized_text <- lemmatize_once(text,
                                      intermediate_directory = intermediate_directory,
                                      python_script = python_script)
  } else {
    lemmatized_text <- NULL  # never used, but keep the variable defined
  }

  # create a list object in which to store the different dfm's
  dfm_list <- vector(mode = "list", length = nrow(choices))

  # row range to work on.
  if(!is.null(parameterization_range)){
    # parameterization_range lets you re-run only a subset of combinations
    # (e.g. to resume an interrupted run). It indexes rows of `choices`.
    choices <- choices[parameterization_range, ]
  }

  rows_to_preprocess <- 1:nrow(choices)

  # NOTE: parallel mode is intentionally left as-is and not recommended with the
  # file-based lemmatizer 
  if (parallel) {
    cat("Preprocessing documents", length(rows_to_preprocess),
        "different ways on", cores,"cores. This may take a while...\n")
    warning("parallel = TRUE is not recommended here: the cached corpora are not exported to workers and the file-based lemmatizer is not worker-safe. Consider parallel = FALSE.")

    cl <- parallel::makeCluster(getOption("cl.cores", cores)) #makes cluster

    dfm_list <- parallel::clusterApplyLB(cl,
                                         x = split(choices, seq(nrow(choices))),
                                         fun = preprocessing_pipeline,
                                         raw_text = raw_text,
                                         lemmatized_text = lemmatized_text,
                                         infrequent_term_threshold = infrequent_term_threshold,
                                         verbose = verbose,
                                         intermediate_directory = intermediate_directory)

    parallel::stopCluster(cl) # stop the cluster when we are done
  }
  else { # loop over different preprocessing decisions
    for (i in rows_to_preprocess) {
      if (verbose) {
        cat("Currently working on combination",i,"of",nrow(choices),"\n")
      }
      # FIX (wiring): pass BOTH cached corpora instead of a single `text`.
      current_dfm <- preprocessing_pipeline(choices = choices[i,],
                                            raw_text = raw_text,
                                            lemmatized_text = lemmatized_text,
                                            infrequent_term_threshold = infrequent_term_threshold,
                                            verbose = verbose)
      dfm_list[[i]] <- current_dfm  # store the current dfm
    }
  }

  # if returning results and using parallel, then read in the
  # intermediate dfm's
  if (return_results & save_dfm) {
    cat("Preprocessing complete, loading in intermediate DFMs...\n")
    dfm_list <- vector(mode = "list", length = nrow(choices))
    for (i in 1:length(dfm_list)) {
      load(paste("intermediate_dfm_",i,".Rdata",sep = ""))
      dfm_list[[i]] <- current_dfm
    }
  }

  names(dfm_list) <- labels
  rownames(choices) <- labels

  # combine metadata and dfm list and return
  return_list <- list(choices = choices,
                      dfm_list = dfm_list,
                      labels = labels)

  # (working directory is restored by on.exit above)

  # return results
  return(return_list)
}




### Preprocessing_pipeline
stopwordprocess <- function(removeStopwords, custom_stopwords = NULL, language){
  if(removeStopwords){ #bool
    if(!is.null(custom_stopwords)){
      if(is.character(custom_stopwords)){
        removeStopwords <- custom_stopwords
      }else{
        stop("ERROR: custom_stopwords must be formatted as a character vector of strings. Such as stopwords::stopwords()")
      }
    }else{
      removeStopwords <-  quanteda::stopwords(language = "en")}}
  return(removeStopwords)
}

preprocessing_pipeline <- function(choices,
                                   raw_text,
                                   lemmatized_text = NULL,
                                   infrequent_term_threshold = .01,
                                   verbose = FALSE,
                                   save_dfm = FALSE,
                                   intermediate_directory = NULL,
                                   custom_stopwords = NULL){

  padding <- ifelse(choices$use_ngrams, TRUE, FALSE) # keep whitespace during tokens removal otherwise nonsensical ngrams are created.

  # SPEED-UP core: select the pre-computed corpus rather than re-lemmatizing.
  if (choices$lem) {
    if (is.null(lemmatized_text)) {
      stop("choices$lem is TRUE but no lemmatized_text was provided/cached.")
    }
    active_text <- lemmatized_text
  } else {
    active_text <- raw_text
  }

  text <- quanteda::tokens(active_text,
                           remove_punct = choices$removePunctuation,
                           remove_numbers = choices$removeNumbers,
                           padding = padding)

  if(choices$lowercase){
    text <- quanteda::tokens_tolower(text, keep_acronyms = FALSE)
  }
  if(choices$removeStopwords){
    # Cahnge: language is hardcoded to "en" inside stopwordprocess; meta() is not
    # available here because `text` is now a tokens object, not a corpus.
    stopwords <- stopwordprocess(choices$removeStopwords, custom_stopwords, language = "en")
    text <- quanteda::tokens_remove(text, stopwords, padding = padding)
  }

  if(choices$use_ngrams){ #need to tokenize before ngram before preprocessing
    text <- quanteda::tokens_ngrams(text, n = 1:2)
  }

  text_dfm <- quanteda::dfm(text, verbose = verbose)

  if (choices$infrequent_terms) {
    text_dfm <- remove_infrequent_terms(
      dfm_object = text_dfm,
      proportion_threshold = infrequent_term_threshold,
      verbose = verbose)
  }

  # works in a context that defines it. Left guarded; not used when save_dfm = FALSE.
  if (save_dfm & !is.null(intermediate_directory)){
    save(text_dfm, file = file.path(intermediate_directory,
                                    paste0("intermediate_dfm_", as.integer(Sys.time()), ".Rdata")))
  }

  return(text_dfm)
}


preText_lem <- function(preprocessed_documents,
                        dataset_name = "Documents",
                        distance_method = "cosine",
                        num_comparisons = 50,
                        parallel = FALSE,
                        cores = 1,
                        verbose = TRUE){

  ptm <- proc.time()
  # extract the dfm object list from preprocessed_documents
  dfm_object_list <- preprocessed_documents$dfm_list

  cat("Generating document distances...\n")
  # get document distances
  scaling_results <- scaling_comparison(dfm_object_list,
                                        dimensions = 2,
                                        distance_method = distance_method,
                                        verbose = verbose,
                                        cores = cores)

  # extract distance matrices
  distance_matrices <- scaling_results$distance_matrices
  cat("Generating preText Scores...\n")

  preText_results <- preText_test(
    distance_matrices,
    choices = preprocessed_documents$choices,
    labels = preprocessed_documents$labels,
    baseline_index = length(preprocessed_documents$labels),
    text_size = 1,
    num_comparisons = num_comparisons,
    parallel = parallel,
    cores = cores,
    verbose = verbose)

  preText_scores <- preText_results$dfm_level_results_unordered
  cat("Generating regression results..\n")

  reg_results <- preprocessing_choice_regression_lem(
    Y = preText_scores$preText_score,
    choices = preprocessed_documents$choices,
    dataset = dataset_name,
    base_case_index = length(preprocessed_documents$labels))

  cat("Regression results (negative coefficients imply less risk):\n")
  # create temporary results so we can round coefficients
  reg_results2 <- reg_results
  reg_results2[,1] <- round(reg_results2[,1],3)
  reg_results2[,2] <- round(reg_results2[,2],3)
  print(reg_results2[,c(3,1,2)])

  t2 <- proc.time() - ptm
  cat("Complete in:",t2[[3]],"seconds...\n")
  #extract relevant info
  return(list(preText_scores = preText_scores,
              ranked_preText_scores = preText_results$dfm_level_results,
              choices = preprocessed_documents$choices,
              regression_results = reg_results))

}



preprocessing_choice_regression_lem <- function(Y,
                                                choices,
                                                dataset = "UK",
                                                base_case_index = 128) {

  # get the appropriate response vectors and make datasets
  Y <- Y
  if(!is.null(base_case_index)) {
    choices <- choices[-base_case_index,]
  }
  DATA <- cbind(Y,choices)

  if (nrow(choices) < 127) {
    form <- "Y ~ removePunctuation + removeNumbers + lowercase + lem + removeStopwords + infrequent_terms"

    var_names <- c("Intercept", "Remove Punctuation", "Remove Numbers",
                   "Lowercase","Lemmatization", "Remove Stopwords",
                   "Remove Infrequent Terms")
  } else {
    form <- "Y ~ removePunctuation + removeNumbers + lowercase + lem + removeStopwords + infrequent_terms + use_ngrams"

    var_names <- c("Intercept", "Remove Punctuation", "Remove Numbers",
                   "Lowercase","Lemmatization", "Remove Stopwords",
                   "Remove Infrequent Terms",  "Use NGrams" )
  }

  fit <- lm(formula = form, data = DATA)
  cat("The R^2 for this model is:",summary(fit)$r.squared,"\n")
  sds <- summary(fit)$coefficients[,2]
  results1 <- cbind( stats::coef(fit),  sds)
  results1 <- as.data.frame(results1,
                            stringsAsFactors = FALSE)
  results <- cbind(results1,var_names)
  colnames(results) <- c("estimate", "sd", "variable")
  rownames(results) <- NULL

  if (nrow(choices) < 127) {
    results <- cbind(results, rep(dataset,7))
  } else {
    results <- cbind(results, rep(dataset,8))
  }

  colnames(results) <- c("Coefficient","SE","Variable","Model")

  return(results)
}

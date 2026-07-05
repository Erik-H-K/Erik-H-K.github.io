###Get factorial_preprocessing from preText to work 
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
                                        verbose = TRUE){
  
  # set some intermediate variables
  cur_directory <- getwd()
  # set working directory if given
  if (!is.null(intermediate_directory)) {
    setwd(intermediate_directory)
  } else {
    intermediate_directory <- cur_directory
  }
  
  ## check to see if input is a vector of text or corpus object. and add language field
  if(is.character(text) & !quanteda::is.corpus(text)){
    text <- quanteda::corpus(text,
                             meta = list("language" = language))
  }
  if(quanteda::is.corpus(text) & is.null(meta(text)$language) ){
    meta(text)$language <- language
  }
  if (!is.character(text) | !quanteda::is.corpus(text)){
    stop("You must provide either a character vector of strings (one per document, or a quanteda corpus object.")
  }
  
  # create a data.frame with factorial combinations of all choices.
  if (use_ngrams) {
    cat("Preprocessing", length(text), "documents 128 different ways...\n")
    choices <- data.frame(expand.grid(list(removePunctuation = c(TRUE,FALSE),
                                           removeNumbers = c(TRUE,FALSE),
                                           lowercase = c(TRUE,FALSE),
                                           lem = c(TRUE,FALSE), #stem
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
                                           lem = c(TRUE,FALSE), #stem
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
  
  
  # create a list object in which to store the different dfm's
  dfm_list <- vector(mode = "list", length = nrow(choices))
  
  # row range to work on.
  
  if(!is.null(parameterization_range)){
    choices <- choices[parameterization_range, ] # If I understand correctly parameterization_range is to restart the process, but I'm not sure where the number come from.
  }
  
  rows_to_preprocess <- 1:nrow(choices)
  if (parallel) {
    cat("Preprocessing documents",nrow(rows_to_preprocess),
        "different ways on", cores,"cores. This may take a while...\n")
    
    cl <- parallel::makeCluster(getOption("cl.cores", cores)) #makes cluster
    
    dfm_list <- parallel::clusterApplyLB(cl,
                                         x = split(choices, seq(nrow(choices))), # convert the choices df as list to iterate over
                                         fun = preprocessing_pipeline,
                                         text = text,
                                         infrequent_term_threshold = infrequent_term_threshold ,
                                         verbose = verbose ,
                                         intermediate_directory = intermediate_directory) ## TODO tmp directory
    
    parallel::stopCluster(cl) # stop the cluster when we are done
  }  
  else { # loop over different preprocessing decisions
    for (i in rows_to_preprocess) {
      if (verbose) {
        cat("Currently working on combination",i,"of",nrow(choices),"\n")
      }
      current_dfm <- preprocessing_pipeline(text = text,
                                            choices = choices[i,],
                                            infrequent_term_threshold= infrequent_term_threshold,
                                            verbose = verbose)           
      dfm_list[[i]] <- current_dfm  # store the current dfm
    }
  }
  
  # if we are returning results and using parallel, then read in the
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
  
  # reset the directory
  setwd(cur_directory)
  
  # return results
  return(return_list)
}




### Preproxessing_pipeline
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
                                   text,
                                   infrequent_term_threshold = .01,
                                   verbose = FALSE,
                                   save_dfm = FALSE,
                                   intermediate_directory = NULL,
                                   custom_stopwords = NULL){
  
  padding <- ifelse(choices$use_ngrams, TRUE, FALSE) # keep whitespace during tokens removal otherwise nonsensical ngram are created.
  
  if(choices$lem){
    text_df <- data.frame(text = text, index = 1:NROW(text))
    write_csv(text_df, file = "/Users/karljohankollerup/Desktop/to_lem.csv")
    
    py_run_file("/Users/karljohankollerup/Desktop/spacy_lem.py")
    
    text_df = read_csv(file = "/Users/karljohankollerup/Desktop/string_lemmatized_py.csv")
    text <- str_squish(text_df$Lem_text)
    
    text <- quanteda::tokens(text,
                             remove_punct = choices$removePunctuation,
                             remove_numbers = choices$removeNumbers,
                             padding = padding)
    
  } else {
    text <- quanteda::tokens(text,
                             remove_punct = choices$removePunctuation,
                             remove_numbers = choices$removeNumbers,
                             padding = padding)
  }
  
  
  if(choices$lowercase){
    text <- quanteda::tokens_tolower(text, keep_acronyms = FALSE)
  }
  if(choices$removeStopwords){
    stopwords <- stopwordprocess(choices$removeStopwords, custom_stopwords, meta(text)$language)
    text <- quanteda::tokens_remove(text, stopwords, padding = padding)
  }
  
  if(choices$use_ngrams){ #need to tokenize before ngram before preprocessing
    text <- quanteda::tokens_ngrams(text, n = 1:2)
  }
  
  text_dfm <- quanteda::dfm(text, verbose = verbose)
  
  if (choices$infrequent_terms) {
    text <- remove_infrequent_terms(
      dfm_object = text_dfm,
      proportion_threshold = infrequent_term_threshold,
      verbose = verbose)
  }
  
  if (save_dfm & !is.null(intermediate_directory)){
    save(text_dfm, file = paste("intermediate_dfm_",i,".Rdata",sep = ""))
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
  # extract teh dfm object list from preprocessed_documents
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
  # create temporary results os we can round coefficients
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

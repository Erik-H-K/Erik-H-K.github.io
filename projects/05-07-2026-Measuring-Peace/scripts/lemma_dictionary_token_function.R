make_lemma_dictionary_token <- function(..., engine = 'hunspell', path = NULL,
                                  lang = switch(engine, hunspell = {'en_US'}, treetagger = {'en'},
                                                lexicon = {NULL}, stop('engine not found'))) {
  
  lemma <- token <- NULL
  tokens <- tfse::na_omit(unique(unlist(...)))
  
  switch(engine,
         treetagger = {
           path <- tree_tagger_location(path)
           
           tagged.results <- suppressMessages(koRpus::treetag(
             unlist(tokens),
             treetagger = "manual",
             format = "obj",
             TT.tknz = FALSE ,
             lang = lang,
             TT.options = list(path = path, preset = lang)
           ))
           
           out <- dplyr::rename(dplyr::arrange(dplyr::distinct(dplyr::filter(
             tagged.results@tokens[c("token", "lemma")],
             !lemma %in% c("<unknown>", '@card@') & nchar(token) > 1 & token != tolower(lemma)
           )), token), lemma = lemma)
         },
         hunspell = {
           out <- unlist(Map(function(y, x){
             if (length(y) == 0 | (length(x) == 1 && x %in% keeps)) return(NA)
             y[length(y)]
           },
           hunspell::hunspell_stem(tokens, dict = hunspell::dictionary(lang)),
           tokens
           ))
           
           out <- data.frame(
             token = tokens[!out == tokens & !is.na(out)],
             lemma = out[!out == tokens & !is.na(out)],
             stringsAsFactors = FALSE
           )
         },
         lexicon = {
           out <- as.data.frame(dplyr::filter(dplyr::tbl_df(lexicon::hash_lemmas), token %in%  tokens), stringsAsFactors = FALSE)
         },
         stop('`engine` must be one of: "hunspell", "treetragger", or "lexicon"')
  )
  out
}

keeps <- c('then')

#
# tree_tagger_location <- function(path = NULL) {
#
#     if (is.null(path)){
#         myPaths <- c("TreeTagger",  "~/.cabal/bin/TreeTagger",
#             "~/Library/TreeTagger", "C:\\PROGRA~1\\TreeTagger",
#             "C:/TreeTagger")
#
#         path <- myPaths[file.exists(myPaths)]
#     }
#
#     tt <- file.exists(path)|length(path) == 0
#
#     if (!tt) {
#         message("TreeTagger does not appear to be installed.\nWould you like me to open a download browser?")
#         ans <- utils::menu(c("Yes", "No"))
#
#         if (ans == 1) {
#             utils::browseURL("http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/#Windows")
#             stop("Retry after downloading TreeTagger or setting `path` to the correct location.")
#         } else {
#             stop('Download tagger from: "http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger"\nand place in the root director')
#         }
#     }
#     path
# }





tree_tagger_location <- function(path = NULL) {
  
  if (is.null(path)){
    myPaths <- c("TreeTagger",  "~/.cabal/bin/TreeTagger",
                 "~/Library/TreeTagger", "C:\\PROGRA~1\\TreeTagger",
                 "C:/TreeTagger")
    
    path <- myPaths[file.exists(myPaths)][1]
    
  }
  
  tt <- length(path) == 1 && file.exists(path)
  
  if (!tt) {
    message("TreeTagger does not appear to be installed.\nWould you like me to open a download browser?")
    ans <- utils::menu(c("Yes", "No"))
    
    if (ans == 1) {
      utils::browseURL("http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/#Windows")
      stop("Retry after downloading TreeTagger or setting `path` to the correct location.")
    } else {
      stop('Download tagger from: "http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger"\nand place in the root director')
    }
  }
  path
}


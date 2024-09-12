library(data.table)
library(dplyr)
library(openxlsx)
library(optparse)
library(readr)
library(readxl)
library(stringr)
library(tidyr)
library(tidytext)
library(udpipe)

parser <- OptionParser()
parser <- add_option(parser, c("-i", "--infile"),
                     help = "Input file")
parser <- add_option(parser, c("-o", "--outdir"), 
                     help = "Output directory")
parser <- add_option(parser, c("-d", "--dictionary"), 
                     help = "Stress dictionary")
parser <- add_option(parser, c("-m", "--model"), 
                     help = "Udpipe model")
args <- parse_args(parser)

vowels_position <- function(data) {
  for (i in 2:8) {
    data <- data %>%
      mutate(!!paste0("v",i) := ifelse(!!sym(paste0("v",i-1)) %in% c(-1,0), -1,
                                       ifelse(!!sym(paste0("t",i-1)) == "", -1,
                                              regexpr('[àå¸èîóûışÿaeiou]', !!sym(paste0("t",i-1))) + !!sym(paste0("v",i-1)))),
             !!paste0("t",i) := ifelse(!!sym(paste0("t",i-1)) == "", "",
                                       ifelse(str_detect(substr(!!sym(paste0("t",i-1)),
                                                                start = !!sym(paste0("v",i)) - !!sym(paste0("v",i-1)) + 1,
                                                                stop = str_length(!!sym(paste0("t",i-1)))),
                                                         '[àå¸èîóûışÿaeiou]') == FALSE, "",
                                              substr(!!sym(paste0("t",i-1)),
                                                     start = !!sym(paste0("v",i)) - !!sym(paste0("v",i-1)) + 1,
                                                     stop = str_length(!!sym(paste0("t",i-1)))))))
  }
  data <- select(data, doc_id, sentence, start, token_id, token, v2, v4, v6, v8)
  return(data)
}

stress_dict_join <- function(data, stress_dict){
  data %>%
    left_join(stress_dict, by = "token") %>%
    mutate(position = regexpr("[`'`]", stress) + start - 2) %>%
    group_by(doc_id, token_id) %>%
    mutate(duplicate = duplicated(token)) %>%
    ungroup() %>%
    mutate(duplicate = ifelse(lead(duplicate) == TRUE, TRUE, duplicate)) %>%
    filter(duplicate == FALSE | position == v2 | position == v4 | position == v6 | position == v8) %>%
    group_by(doc_id, token_id) %>%
    slice(n = 1) %>%
    ungroup() %>%
    mutate(position = ifelse(str_detect(stress, "[àå¸èîóûışÿaeiou]") == F, NA, position)) %>%
    select(doc_id, token_id, sentence, token, position) %>%
    na.omit()
}

stress_position <- function(data) {
  data %>%
    group_by(doc_id) %>%
    mutate(stress_id = sequence(n())) %>%
    ungroup() %>%
    mutate(position = position + stress_id - 1) %>% 
    select(-token) %>%  
    spread(token_id, position) %>%
    mutate_all(funs(ifelse(is.na(.), "", .))) %>%
    group_by(doc_id, sentence) %>%
    summarise_all(funs(trimws(paste(., collapse = '')))) %>%
    ungroup() %>%
    arrange(doc_id, stress_id) %>%
    select(-stress_id) %>%
    group_by(doc_id) %>%
    summarise_all(funs(trimws(paste(., collapse = ' ')))) %>%
    ungroup() %>%
    mutate_at(vars(-c("doc_id", "sentence")), as.numeric)
}

stress_indicator <- function(data) {
  for (i in colnames(select(data, -doc_id, -sentence))) {
    data <- data %>%
      mutate(sentence = ifelse(is.na(!!sym(paste0(i))), sentence,
                               paste0(substr(sentence, 1, !!sym(paste0(i))), "*",
                                      substr(sentence, !!sym(paste0(i)) + 1, str_length(sentence)))))
  }
  data <- select(data, sentence)
  return(data)
}

main <- function(args) {
  stress_dict <- fread(args$dictionary, header = T)
  udmodel <- udpipe_load_model(file = args$model)
  
  base <- sub(".xlsx", "", basename(args$infile))
  
  data <- read_excel(args$infile) %>%
    rename(sentence = LineText) %>% 
    mutate(doc_id = sequence(n())) %>%
    select(doc_id, sentence)
  
  stress_data <- udpipe(data$sentence, object = udmodel) %>%
    filter(upos != "PUNCT") %>%
    select(doc_id, sentence, start, token_id, token) %>%
    mutate(doc_id = as.numeric(str_replace(doc_id, "doc", "")),
           token = str_to_lower(token),
           v1 = regexpr('[àå¸èîóûışÿaeiou]', str_to_lower(sentence)),
           t1 = str_to_lower(substr(sentence, start = v1 + 1, stop = str_length(sentence)))) %>%
    vowels_position() %>%
    stress_dict_join(stress_dict) %>%
    stress_position() %>% 
    right_join(data, by = "doc_id") %>%
    arrange(doc_id) %>%
    rename(sentence = sentence.y) %>%
    select(-sentence.x) %>%
    stress_indicator()
  
  fwrite(stress_data, file = paste0(args$outdir, "/", base, ".stress.txt"),
         sep = "\t", row.names = F, col.names = F)
}

main(args)


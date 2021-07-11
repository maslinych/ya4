library(data.table)
library(dplyr)
library(openxlsx)
library(optparse)
library(readxl)
library(stringr)
library(tidyr)
library(udpipe)

parser <- OptionParser()
parser <- add_option(parser, c("-i", "--infile"), 
                     help = "Original file")
parser <- add_option(parser, c("-c", "--conllu"),
                     help = "Conllu file")
parser <- add_option(parser, c("-s", "--stress_file"), 
                     help = "File with indicated stress")
parser <- add_option(parser, c("-o", "--outdir"), 
                     help = "Output directory")
args <- parse_args(parser)

## Обозначаем позиции необходимых символов

position_indicate <- function(data, regex, name) {
  positions <- data.frame(V1 = NA, V2 = NA, V3 = NA, V4 = NA, V5 = NA,
                          V6 = NA, V7 = NA, V8 = NA, V9 = NA, V10 = NA)
  
  for (i in 1:length(str_locate_all(data, regex))) {
    if (length(as.data.frame(unname(t(str_locate_all(data, regex)[[i]][,1])))) > 0) {
      positions <- bind_rows(positions, as.data.frame(unname(t(str_locate_all(data, regex)[[i]][,1]))))
    } else {
      positions <- bind_rows(positions, data.frame(V1 = NA))
    }
  }
  
  colnames(positions) <- str_replace(colnames(positions), "V", name)
  return(positions[-1,])
}

## Объединяем все колонки с позициями

positions_merge <- function(data) {
  cbind(data, position_indicate(str_to_lower(data$text), '[аеёиоуыэюяaeiou]', "v")) %>%
    cbind(position_indicate(str_to_lower(data$V1), '\\*', "s")) %>%
    group_by(token, doc_id) %>%
    mutate(word_id = sequence(n())) %>%
    ungroup() %>%
    group_by(row_number()) %>% 
    mutate(start = str_locate_all(text, token)[[1]][word_id,][[1]],
           end = str_locate_all(text, token)[[1]][word_id,][[2]]) %>%
    ungroup()
}

## Формируем значения позиций ударений в строке без звезд

stress_indicate <- function(data) {
  for (i in 1:10) {
    data <- data %>%
      mutate(!!paste0("s",i) := !!sym(paste0("s",i)) - i)
  }
  return(data)
}

## Обозначаем позицию ударения для каждого токена

stress_position <- function(data) {
  data$position <- NA
  
  for (i in 1:10) {
    data <- data %>%
      group_by(row_number()) %>%
      mutate(position = ifelse(!!sym(paste0("s",i)) %in% start:end, !!sym(paste0("s",i)), position)) %>%
      ungroup()
  }
  return(data)
}

## Создаем икты

ictus_create <- function(data){
  for(i in 1:4){
    data <- data %>%
      mutate(!!paste0("ictus",i) := ifelse(!!sym(paste0("v",i*2)) == position &
                                             !(upos %in% c("CCONJ","SCONJ","PART","ADP")) |
                                             !!sym(paste0("v",i*2)) == position & upos %in% c("CCONJ","SCONJ","PART","ADP") &
                                             str_count(str_to_lower(token), "[аеёиоуыэюяaeiou]") > 1, upos, NA)) 
  }
  return(data)
}

## Создаем колонку HMS

hms_create <- function(data){
  data$hms = NA
  
  for(i in 1:4){
    data <- data %>%
      mutate(hms = ifelse(!!sym(paste0("v",i*2-1)) == position & !(upos %in% c("CCONJ","SCONJ","PART","ADP","PRON","DET")) |
                            !!sym(paste0("v",i*2-1)) == position & upos %in% c("CCONJ","SCONJ","PART","ADP","PRON","DET") &
                            str_count(str_to_lower(token), "[аеёиоуыэюяaeiou]") > 1, i, hms
      ))
  }
  return(data)
}

## Создаем колонки с id слова, id главного слова и связью между ними

word_rel_preparation <- function(data){
  for(i in 1:4){
    data <- data %>%
      mutate(!!paste0("id_",i) := ifelse(!is.na(!!sym(paste0("ictus",i))), token_id, ""),
             !!paste0("h_id_",i) := ifelse(!is.na(!!sym(paste0("ictus",i))), head_token_id, ""),
             !!paste0("r_",i) := ifelse(!is.na(!!sym(paste0("ictus",i))), dep_rel, "")) 
  }
  return(data)
}

## Оставляем только нужные колонки, объединяем результаты предыдущих функций для каждой строки

join_results <- function(data){
  data %>%
    dplyr::select(doc_id, text, ictus1, ictus2, ictus3, ictus4, hms,
                  id_1, h_id_1, r_1, id_2, h_id_2, r_2, id_3, h_id_3, r_3, id_4, h_id_4, r_4) %>%
    group_by(doc_id, text) %>%
    summarise_all(funs(trimws(paste(., collapse = '')))) %>%
    ungroup()
}

## Чистим колонки от образовавшихся na внутри строк и заменяем пустые на NA

na_replace <- function(data){
  data %>%
    mutate_all(funs(str_replace_all(., "NA", ""))) %>%
    mutate_all(funs(ifelse(. == "", "0", .)))
}

## Создаем колонку с ритмическими формами

rhythm_form_create <- function(data, rhythm_forms){
  data %>% mutate(ictus_stress = paste0(ifelse(ictus1 == "0","0","1"),
                                        ifelse(ictus2 == "0","0","1"),
                                        ifelse(ictus3 == "0","0","1"),
                                        ifelse(ictus4 == "0","0","1"))) %>%
    left_join(rhythm_forms, by = "ictus_stress")
}

## Создаем колонки со взаимосвязями между словами на ударных позициях

relations_create <- function(data, relations){
  for(i in 1:nrow(relations)){
    a <- relations[i,]$a
    b <- relations[i,]$b
    
    data <- data %>%
      mutate(!!paste0("rel_",a,"&",b) := ifelse(!!sym(paste0("id_",a)) == !!sym(paste0("h_id_",b)) &
                                                  !!sym(paste0("id_",a)) != "0" &
                                                  !!sym(paste0("h_id_",b)) != "0",
                                                !!sym(paste0("r_",b)),
                                                "0"))
  }
  return(data)
}

## Финальная чистка, добавление исходных колонок NoT, NoS, TextID и строк стихотворений

final_cleaning <- function(data, data_control){
  data <- data %>%
    mutate(doc_id = as.numeric(str_replace(doc_id, "doc", "")),
           notes = NA) %>%
    arrange(doc_id) %>%
    left_join(data_control[,c("LineID","LineText","NoT","NoS","TextID")], by = c("doc_id" = "LineID")) %>%
    rename(line_id = doc_id,
           line_text = LineText,
           not = NoT,
           nos = NoS,
           text_id = TextID)
  
  return(data[,c("line_id","line_text",
                 "ictus1","ictus2","ictus3","ictus4",
                 "hms","rhythm_form","not","nos","text_id",
                 "rel_1&2","rel_2&1","rel_1&3","rel_3&1","rel_1&4","rel_4&1",
                 "rel_2&3","rel_3&2","rel_2&4","rel_4&2","rel_3&4","rel_4&3","notes")])
}

## Функция порядка обработки

file_process <- function(data, rhythm_forms, relations, data_control) {
  data %>%
    positions_merge() %>%
    stress_indicate() %>% 
    stress_position() %>% 
    ictus_create() %>%
    hms_create() %>%
    word_rel_preparation() %>%
    join_results() %>%
    na_replace() %>%
    rhythm_form_create(rhythm_forms) %>%
    relations_create(relations) %>%
    final_cleaning(data_control)
}

## Главная функция

main <- function(args) {
  base <- sub(".xlsx", "", basename(args$infile))
  
  data_control <- read_excel(args$infile) %>%
    mutate(LineID = as.numeric(LineID))
  
  data <- fread(args$stress_file, sep = "\t", header = F) %>%
    mutate(text = str_replace_all(V1, "\\*", ""),
           doc_id = sequence(n()))
  
  conllu <- udpipe_read_conllu(args$conllu) %>%
    mutate(misc = ifelse(is.na(misc), "", misc))
  
  conllu$doc_id <- c(1, cumsum(c(TRUE, str_detect(conllu$misc[-1], "\\\\n")))[1:nrow(conllu)-1])
  
  data <- inner_join(conllu, data, by = "doc_id") %>%
    filter(upos != "PUNCT")
  
  rhythm_forms <- data.frame(ictus_stress = c("1111","0111","1011","1101","1001","0101","0011","0001"),
                             rhythm_form = c("1","2","3","4","5","6","7","8"))
  
  relations <- data.frame(a = c(1:4),
                          b = c(1:4)) %>%
    expand(a, b) %>%
    filter(a != b)
  
  data <- file_process(data, rhythm_forms, relations, data_control)
  
  write.xlsx(data, paste(args$outdir, "/", base, ".markup.xlsx", sep = "", collapse = ""))
}

main(args)

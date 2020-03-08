#!/usr/bin/Rscript
library(readxl)
library(data.table)
library(optparse)

transform_ikts <- function(ikts, extras, stopa="01") {
    if (anyNA(ikts)) {
        return(NA)
    }
    ikts <- as.vector(ikts)
    src <- ikts
    ikts[ikts != "0"] <- 1
    ikts <- as.numeric(ikts)
    out <- sapply(ikts, function(x){ifelse(x==1, stopa, "00")})
    if (tail(src, n=1) == "f") {
        out <- c(out, "0")
    }
    extra_ind <- as.numeric(strsplit(as.character(extras), "")[[1]])
    if (sum(extra_ind>0)) {
        extra_ind <- extra_ind[extra_ind!=0]
    }
    if (length(extra_ind>0)) {
        out[extra_ind] <- sapply(out[extra_ind], function(x) {sub("^.", "1", x)})
    }
    return(paste(out, collapse=""))
}

# 1 = 01010101, 010101010, плюс любой 0, кроме последнего во второй схеме, может заменяться на 1.
# 2 = 00010101, 000101010, плюс любой ноль в нечетной позиции, кроме последнего во второй схеме, может заменяться на 1.
# 3 = 01000101, 010001010, плюс любой ноль в нечетной позиции, кроме последнего во второй схеме, может заменяться на 1.
# 4 = 01010001, 010100010, плюс любой ноль в нечетной позиции, кроме последнего во второй схеме, может заменяться на 1.
# 5 = 01000001, 010000010, плюс любой ноль в нечетной позиции, кроме последнего во второй схеме, может заменяться на 1.
# 6 = 00010001, 000100010, плюс любой ноль в нечетной позиции, кроме последнего во второй схеме, может заменяться на 1.
# 7 = 00000001, 000000010. Плюс любой ноль в нечетной позиции, кроме последнего во второй схеме, может заменяться на 1.
rythmic_form <- function(stress) {
    switch(paste(unlist(strsplit(stress,""))[c(FALSE, TRUE)], collapse=""),
           "1111" = 1,
           "0111" = 2,
           "1011" = 3,
           "1101" = 4,
           "1001" = 5,
           "0101" = 6,
           "0001" = 7)
    }

add_ikts <- function(data, colrange=c(3, 6), extracol=7) {
    outdt<-setDT(data)[
       ,ikt:=lapply(transpose(.SD), as.list),.SDcols=seq(colrange[1],colrange[2])
    ][,
      stress:=as.character(transform_ikts(unlist(ikt), .SD)), .SDcols=extracol, by = 1:nrow(data)
      ][,ikt:=NULL][,form:=rythmic_form(stress),by = 1:nrow(data)]
    return(outdt)
}

## Parse arguments
parser <- OptionParser()
parser <- add_option(parser, c("-i", "--infile"),
                help="Input file")
parser <- add_option(parser, c("-o", "--outfile"), 
                help="Ouptput file")
args <- parse_args(parser)

main <- function(args) {
    infile <- read_xlsx(args$infile)
    out <- add_ikts(infile)
    fwrite(out, args$outfile, quote=TRUE)
}

## and run it
main(args)


library(readr)
library(dplyr)
library(optparse)

parser <- OptionParser()
parser <- add_option(parser, c("-i", "--infile"),
                help="Input file")
parser <- add_option(parser, c("-o", "--outdir"), 
                help="Ouptput directory")
args <- parse_args(parser)

main <- function(args) {
    data <- read_csv(args$infile)
    base <- sub(".ikts.csv", "", basename(args$infile))

    data %>% count(form, `№ в строфе`, stress) %>%
        group_by(form) %>%
        mutate(percent = round(100*(n/sum(n)), 1)) %>%
        write_csv(paste(args$outdir, "/", base, ".form-stance.csv", sep="", collapse=""))
    
    data %>% count(`№ в строфе`, form, stress) %>%
        group_by(`№ в строфе`) %>%
        mutate(percent = round(100*(n/sum(n)), 1)) %>%
        write_csv(paste(args$outdir, "/", base, ".stance-form.csv", sep="", collapse=""))

    data %>% mutate(stance_n = ((`№ в тексте произведения`-1)%/%4)+1) %>%
        count(stance_n, form, stress) %>% group_by(stance_n) %>%
        mutate(percent = round(100*(n/sum(n)), 1)) %>%
        write_csv(paste(args$outdir, "/", base, ".stance_n-form.csv", sep="", collapse=""))

}
    
main(args)


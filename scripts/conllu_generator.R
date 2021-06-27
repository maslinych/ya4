library(optparse)
library(readxl)
library(udpipe)

parser <- OptionParser()
parser <- add_option(parser, c("-i", "--infile"),
                     help = "Input file")
parser <- add_option(parser, c("-o", "--outdir"), 
                     help = "Output directory")
parser <- add_option(parser, c("-m", "--model"), 
                     help = "Udpipe model")
args <- parse_args(parser)

main <- function(data, udmodel, outdir) {
  data <- read_excel(args$infile)
  base <- sub(".xlsx", "", basename(args$infile))
  
  udmodel <- udpipe_load_model(file = args$model)
  data <- as_conllu(udpipe(data$LineText, object = udmodel))
  
  write(data, file = paste0(outdir, "/", base, ".conllu"))
}

main(data = args$infile,
     udmodel = args$model,
     outdir = args$outdir)
if(Sys.info()[4]=="stp-air"){
  pollen_dir <- "~/git/bee_rare/"
}
# Tom's epa laptop
if(Sys.info()[4]=="LZ2626UTPURUCKE"){
  pollen_dir <- "c:/git/bee_rare/"
}

pollen_dir_input <- paste(pollen_dir, "data_in/", sep = "")


# load data files
pesticide_file <- paste(pollen_dir_input, "pesticide201314.csv", sep = "")
pesticide2015_file <- paste(pollen_dir_input, "pesticide2015.csv", sep = "")
deadbees_file <- paste(pollen_dir_input, "deadbees.csv", sep = "")

pesticide <- read.table(pesticide_file,header=T,fill=T)
pesticide2015 <- read.table(pesticide2015_file,header=T,fill=T)
deadbees <- read.table(deadbees_file,header=T,fill=T)
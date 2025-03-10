# getbtw25Results
box::use(
  pbapply[pblapply],
  utils[read.csv]
  #,data.table[normalizePath]
)

box::use(
  
)

# Pattern<-etablepattern

#' @export
getbtw25Results <- function(Directory){
  #Directory<-directoryWithName
  #print(Pattern)
  # rownames<-c("L","Loadstep","S","Substep","R","row","C","Column")    string dann spaltenindicator
  Pattern = "\\.csv$"
  Filenames <- list.files(path = file.path(Directory), recursive = FALSE, pattern = Pattern)
  Filenames_sortet <- Filenames[order(Filenames, decreasing = FALSE)]
  #firstfile<-lapply(paste0(Directory,"/",Filenames_sortet[1]), read.csv, header = TRUE, sep = "",skip=0)#, dec = ",")
  file_path <-normalizePath(paste0(Directory,"/",Filenames_sortet), mustWork = FALSE)
  # Alle Dateien einlesen und zusammenfügen
  data <- rbindlist(pbapply::pblapply(file_path, function(file) fread(file, dec = ",", sep = ";", encoding = "UTF-8")), fill = TRUE)
  # Spaltennamen bereinigen (Leerzeichen durch Unterstriche ersetzen)
  setnames(data, gsub(" ", "_", names(data)))
  
  ret <- list(data = data,
            Filenames_sortet = Filenames_sortet
  )
  return(ret)
}

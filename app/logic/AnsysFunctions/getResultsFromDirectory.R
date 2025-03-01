# getResultsFromDirectory
box::use(
  pbapply[pblapply],
  utils[read.csv]
)

box::use(
  
)

# Pattern<-etablepattern

#' @export
getResultsFromDirectory<-function(Directory){
  #print(Pattern)
  # rownames<-c("L","Loadstep","S","Substep","R","row","C","Column")    string dann spaltenindicator
  Pattern= "\\.csv$"
  Filenames <- list.files(path = file.path(Directory), recursive = FALSE, pattern = Pattern)
  Filenames_sortet <- Filenames[order(Filenames, decreasing = FALSE)]
  #firstfile<-lapply(paste0(Directory,"/",Filenames_sortet[1]), read.csv, header = TRUE, sep = "",skip=0)#, dec = ",")

  # Alle Dateien einlesen und zusammenfügen
  df <- rbindlist(pbapply::pblapply(files, function(file) fread(file,dec=",", sep=";", encoding="UTF-8")), fill = TRUE)
  # Spaltennamen bereinigen (Leerzeichen durch Unterstriche ersetzen)
  setnames(df, gsub(" ", "_", names(df)))
  
  ret <- list(df = df,
            Filenames_sortet = Filenames_sortet
  )
  return(ret)
}
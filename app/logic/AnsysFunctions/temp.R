directory <- "C://WABEO//btw25//downloads//Bayern"
Pattern <- "*.csv"

LoadoneFileType <- function(directory,Pattern){
  Filenames <- list.files(path = file.path(directory), recursive = FALSE, pattern = Pattern)
  Filenames_sortet <- Filenames[order(Filenames, decreasing = FALSE)]
  firstfile <- lapply(paste0(directory,"/",Filenames_sortet[1000]), read.csv2, header = TRUE,skip = 0)#, dec = ",")
  zeilen <- nrow(firstfile[[1]])
  spalten <- ncol(firstfile[[1]])
  
  #str <- paste(eins,zwei,drei,vier,sep = "_")
  Fnames<-Filenames_sortet[1000]
  File_all <- array(unlist(
    pbapply::pblapply(paste0(directory,"/",Fnames), read.csv2, header = TRUE, skip = 0))
    ,dim = c(zeilen, spalten, length(Fnames)
    ))
  
  a<-pbapply::pblapply(paste0(directory,"/",Fnames), read.csv2, header = TRUE, skip = 0)
         
  array(unlist(
    pbapply::pblapply(paste0(directory,"/",Fnames), read.csv2, header = TRUE, skip = 0))
    ,dim = c(zeilen, spalten, length(Fnames)
    ))
  
  
  #header <- 
  #  read.csv2(paste0(directory,"/",Filenames_sortet[1000]), sep = ";", nrows = 1, header = FALSE, stringsAsFactors = FALSE)
  
  header_Cells <- unlist(pbapply::pblapply(paste0(directory,"/",Filenames_sortet)[1], read.csv2, header = FALSE,sep = ";",  nrows = 1))
  dimnames(File_all)[[3]] <- Filenames_sortet
  dimnames(File_all)[[2]] <- as.character(header_Cells)
  #dimnames(File_all)[[1]] <- str
  ret <- list(File_all = File_all,
              Filenames_sortet = Filenames_sortet)
    
  return(ret)
}

df <- unlist(lapply(paste0(directory,"/",Filenames_sortet[50:100]),read.csv2, skip = 0, header = TRUE))

aa <- LoadoneFileType(directory,Pattern)

files <- paste0(directory,"/",Filenames_sortet[50:100])
df_erste <- read.csv2(files, skip = 0, header = TRUE)  # Erste Datei mit Header
df_list <- lapply(files, read.csv2, stringsAsFactors = FALSE, skip = 1)  # Rest ohne Header

df_list <- lapply(files, function(f) read.csv2(f, skip = 0, header = TRUE))

df_gesamt <- do.call(rbind, c(list(df_erste), df_list))
##################
library(purrr)
library(readr)  # Falls du `read_csv2()` statt `read.csv2()` nutzen willst

#files <- list.files("Pfad/zum/Verzeichnis", pattern = "\\.csv$", full.names = TRUE)
files <- paste0(directory,"/",Filenames_sortet[50:100])
df <- map_df(files, read_csv2)  # Automatisch als Dataframe zusammenfügen

print(df)

#################

library(data.table)

files <- paste0(directory,"/",Filenames_sortet)
# Alle CSV-Dateien im Verzeichnis finden
#files <- list.files("Pfad/zum/Verzeichnis", pattern = "\\.csv$", full.names = TRUE)
aaa<-fread(files, dec=",", sep=";", encoding="UTF-8")

# Alle Dateien einlesen und zusammenfügen
df <- rbindlist(pbapply::pblapply(files, function(file) fread(file,dec=",", sep=";", encoding="UTF-8")), fill = TRUE)
# Spaltennamen bereinigen (Leerzeichen durch Unterstriche ersetzen)
setnames(df, gsub(" ", "_", names(df)))

save(df,df)
#################




LoadoneFileType <- function(OldResultDirectory,Pattern,rownames){
  
  #print(Pattern)
  # rownames<-c("L","Loadstep","S","Substep","R","row","C","Column")    string dann spaltenindicator
  
  Filenames <- list.files(path = file.path(OldResultDirectory), recursive = FALSE, pattern = Pattern)
  Filenames_sortet <- Filenames[order(Filenames, decreasing = FALSE)]
  firstfile <- lapply(paste0(OldResultDirectory,"/",Filenames_sortet[1]), read.csv, header = TRUE, sep = "",skip = 0)#, dec = ",")
  zeilen <- nrow(firstfile[[1]])
  spalten <- ncol(firstfile[[1]])
  eins <- paste(rownames[1],firstfile[[1]][,rownames[2]],sep = "")
  zwei <- paste(rownames[3],firstfile[[1]][,rownames[4]],sep = "")
  drei <- paste(rownames[5],firstfile[[1]][,rownames[6]],sep = "")
  vier <- paste(rownames[7],firstfile[[1]][,rownames[8]],sep = "")
  
  #Sub<-paste("S",firstfile[[1]][,"Substep"],sep = "")
  #time<-round(firstfile[[1]][,"Time"])
  
  str <- paste(eins,zwei,drei,vier,sep = "_")
  File_all <- array(as.numeric(unlist(
    pbapply::pblapply(paste0(OldResultDirectory,"/",Filenames_sortet), read.csv, header = TRUE, sep = "", dec = ".",skip = 0)))
    ,dim = c(zeilen, spalten, length(Filenames_sortet)
    ))
  header_Cells <- unlist(pbapply::pblapply(paste0(OldResultDirectory,"/",Filenames_sortet)[1], read.csv, header = FALSE, sep = "", dec = ".",nrows = 1))
  dimnames(File_all)[[3]] <- Filenames_sortet
  dimnames(File_all)[[2]] <- as.character(header_Cells)
  dimnames(File_all)[[1]] <- str
  ret <- list(File_all = File_all,
            Filenames_sortet = Filenames_sortet
  )
  return(ret)
}

#' @export
getResultsFromDirectory<-function( ConfigVariables
                                   #   
                                   # OldResultDirectory
                                   #                       ,cellpattern="cellresult_2LS"
                                   #                       ,etablepattern="Etable_result_2LS"
                                   #                       ,noderesultpattern="node_result"
                                   #                       # ,variablenpattern="Variable_Mold_Dela_result_2LS"
){
  
  #####
  #ConfigVariables<-Co
  ######
  
  
  OldResultDirectory=ConfigVariables$Result_directory
  #ConfigVariables<-Co
  
  
  IFnames <- ConfigVariables$Output_Components$InterfaceName
  
  ret <- list()
  for (IF in 1:length(IFnames)) {
    #for (IF in 1:1) {
    cellpattern=ConfigVariables$Output_file_name$Cellresult_file_name[IF]
    etablepattern=ConfigVariables$Output_file_name$ETableresult_file_name[IF]
    noderesultpattern=ConfigVariables$Output_file_name$Noderesult_file_name[IF]
    EtableCordspattern=ConfigVariables$Output_file_name$ElementNodeCoords_file_name[IF]
    
    # cellpattern=ConfigVariables$cellresult_file_name
    # etablepattern=ConfigVariables$ETableresult_file_name
    # noderesultpattern=ConfigVariables$noderesult_file_name
    # EtableCordspattern=ConfigVariables$ETableNodeCoords_file_name
    
    cellrownames<-c("L","Loadstep","S","Substep","R","row","C","Column")
    etablerownames<-c("L","Loadstep","S","Substep","N","Nset","E","Enum")
    noderownames<-c("L","Loadstep","S","Substep","Nnr","NodeNr","T","Temp")
    EtableCordsownames<-c("Eid","ElementI","Nid","NodeID","X","X_Coord","Y","Y_Coord")
    
    Etablecoords<-LoadoneFileType(OldResultDirectory,EtableCordspattern,EtableCordsownames)$File_all[,,1]
    
    if (IFnames[IF]=="_SMC")
    {
      #print(IFnames[IF])
      cellresult_File_all<-LoadoneFileType(OldResultDirectory,cellpattern,cellrownames)$File_all
    }
    else {
      cellresult_File_all<-NULL
    }
    
    noderesult_File_all <- LoadoneFileType(OldResultDirectory,noderesultpattern,noderownames)$File_all
    etab <- LoadoneFileType(OldResultDirectory,etablepattern,etablerownames)
    Etable_File_all <- etab$File_all
    Etable_Filenames_sortet <- etab$Filenames_sortet
    
    
    if (IFnames[IF]!="_SMC"){
      etable_datase1 <- as.data.frame(Etable_File_all[,,1])
      SimDat_analyser_result <- SimDat_analyser(etable_datase1)
      SimTimes <- SimDat_analyser_result$SimTimes_wo_Substeps
      etable_dataset_substetTime1 <- unique(subset(etable_datase1, Time == SimTimes[length(SimTimes)]))
      zeilen <- nrow(etable_dataset_substetTime1)
      enumLogicOldList <- array(0,c(zeilen,length(Etable_Filenames_sortet)))
      for (i in 1:length(Etable_Filenames_sortet))
      {
        etable_dataset <- as.data.frame(Etable_File_all[,,i])
        etable_dataset_substetTime <- (subset(etable_dataset, Time == SimTimes[length(SimTimes)]))
        nullindex <- which(etable_dataset_substetTime$Sx==0)
        notnullindex <- which(etable_dataset_substetTime$Sx!=0)
        enumLogicOldList[nullindex,i] <- 1
        enumLogicOldList[notnullindex,i] <- 0
      }
      
      rownames(enumLogicOldList)<-etable_dataset_substetTime$Enum
      colnames(enumLogicOldList)<-Etable_Filenames_sortet}
    else enumLogicOldList<-NULL
    
    
    
    
    #cc <- ConfigVariables$cellresult_file_name
    ret[[IFnames[IF]]][[ConfigVariables$cellresult_file_name]] <- cellresult_File_all
    ret[[IFnames[IF]]][[ConfigVariables$noderesult_file_name]] <- noderesult_File_all
    ret[[IFnames[IF]]][[ConfigVariables$ETableresult_file_name]] <- Etable_File_all
    ret[[IFnames[IF]]][["enumLogic"]] <- enumLogicOldList
    ret[[IFnames[IF]]][[ConfigVariables$ETableNodeCoords_file_name]] <- Etablecoords
  }
  
  #aa<-ret$Etable_File_all[,,1]
  return(ret)
  
}
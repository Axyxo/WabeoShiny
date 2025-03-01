
box::use(
  # app/logic/utils[make_directory_text_input
  #                 ,makeCard,make_numeric_input_fluent
  #                 ,make_PrimaryButton_fluent,make_dropdown_fluent,make_numeric_input],
  app/logic/getbtw25Results[getbtw25Results],
)

# Liste nur direkte Unterverzeichnisse auf



#name <- "Berlin Zweitstimme"
directory <- "C://WABEO//btw25//downloads//"
subdirs <- list.dirs(directory, recursive = FALSE, full.names = TRUE)
print(subdirs)
btw25daten<-NULL
btw25daten["namen"]<-list(basename(subdirs))
for (i in 1:length(subdirs)) { #length(subdirs)) {
  name <- basename(subdirs)[i]
  print(name)
  targetDir <- "C://WABEO//btw25//"
  directoryWithName <- paste0(directory,"/",name)
  CSV_Data <- getbtw25Results(directoryWithName)
  btw25daten[[name]] <- CSV_Data
  save(CSV_Data,file = paste0(directory,"/",name,".RData" ))
}
save(btw25daten,file = paste0(directory,"/","btw25daten",".RData" ))


load(file = paste0(directory,"/","btw25daten",".RData" ))

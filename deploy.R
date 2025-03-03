library(rsconnect)
rsconnect::setAccountInfo(name='wabeo',
                          token='8B28F2382C92B991C9E172222D96F716',
                          secret='iiRGlFXmmIsYzO/Yqx0LUAWJw7cvsz35g04TZ4M1')

#deployApp()
#deployApp(appName="MCC2")

fix_encoding <- function(file) {
  text <- readLines(file, encoding = "UTF-8")
  text <- iconv(text, from = "UTF-8", to = "UTF-8")
  writeLines(text, file, useBytes = TRUE)
}

fix_encoding("renv.lock")


rsconnect::deployApp("C:/Rprojekts/WabeoShiny"
#rsconnect::deployApp("/ICORE5-12500/RstudioUserData/WabeoShiny"
                     ,appName = "Wabevaluation")
y
#rsconnect::configureApp("MCC2", size="small")

text <- readLines("renv.lock", encoding = "UTF-8")
writeLines(text, "renv.lock", useBytes = TRUE)


Sys.setlocale("LC_ALL", "German_Germany.65001")
Sys.getlocale()  # Sollte jetzt 65001 enthalten
unlink("renv.lock")
renv::snapshot()
rsconnect::deployApp()


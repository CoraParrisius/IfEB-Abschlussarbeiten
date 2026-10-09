# Deployment auf shinyapps.io (Apps) und Rendern der Seite.
#
# Einmalige Einrichtung:
#   install.packages("rsconnect")
#   rsconnect::setAccountInfo(name = "...", token = "...", secret = "...")
#   (Token: shinyapps.io -> Account -> Tokens)
# Danach in config.R den Account eintragen.

source("config.R")
library(rsconnect)

# Apps deployen -----------------------------------------------------------
for (app in apps) {
  deployApp(
    appDir = app$dir,
    appPrimaryDoc = "app.R",
    appName = app$name,
    account = account,
    server = "shinyapps.io",
    forceUpdate = TRUE
  )
}

# Seite rendern ------------------------------------------------------------
system2("quarto", "render")

# Die Seite liegt danach in _site/ und muss separat gehostet werden, z. B.:
#   system2("quarto", "publish gh-pages --no-prompt")
#   oder _site/ per FTP/rsync auf den Hochschul-Webspace kopieren
# (shinyapps.io hostet keine statischen Seiten.)

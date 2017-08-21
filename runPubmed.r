.libPaths("./R-Portable/App/R-Portable/library")
# the path to portable chrome
###silencing chrome path for test
##browser.path = file.path(getwd(),"GoogleChromePortable/GoogleChromePortable.exe")
##options(browser = browser.path)
shiny::runApp("./getPubmed/",port=8888,launch.browser=TRUE)
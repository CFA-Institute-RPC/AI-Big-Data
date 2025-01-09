library(shinylive)
library(httpuv)

#Render Shiny heatmap app into HTML static file so it can be deployed on GitHub Pages (RUN ONCE)
export(appdir="app", destdir="docs", max_filesize=NA)

#Verify code worked
runStaticServer("docs")

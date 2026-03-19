## 00_colours.R

load_colours <- function(){
  
  colours <- list()
  colours$redblue <- c("#B2182B", "#2166AC") # 10 colours, 2 and 9
  colours$bluered <- c("#2166AC", "#B2182B") # 10 colours, 2 and 9
  
  ## Colorbrewer, green to pink:
  colours$greenpink1 <- c("#4DAC26", "#D01C8B") # 5 colours, 1 and 5
  colours$greenpink2 <- c("#4D9221", "#C51B7D") # 10 colours, 1 and 5
  colours$orangepurple <- c("#E08214", "#8073AC") # 10 colours, 8 and 3
  
  ## Valence (reward/ punishment) colors from Jenn:
  colours$greenred <- c("#009933","#CC0000") # green, red
  colours$redgreen <- c("#CC0000","#009933") # red, green
  
  ## FRR colors:
  colours$poormediumrich <- c("#ED462F", "#FDF733", "#76F013") # red, yellow, green
  
  return(colours)
  
}

## Rainbow color map with base R:
# colVec <- rainbow(nStakes, alpha = 1); colName <- "rainbow"
# colVec <- heat.colors(nStakes, alpha = 1)

## Jet color map with sommer package:
# library(sommer)
# colVec <- jet.colors(nStakes, alpha = 1); colName <- "jet"

## Viridis color maps:
# library(viridis)
# colVec <- viridis(nStakes); colName <- "viridis"
# colVec <- magma(nStakes); colName <- "magma"
# colVec <- plasma(nStakes); colName <- "plasma"
# colVec <- turbo(nStakes); colName <- "turbo"

## Color brewer maps:
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# library(RColorBrewer)
# display.brewer.all()
# colVec <- brewer.pal(nStakes, "RdBu"); colName <- "RdBu"

# ------------------------------------------------------------------------------- #
## Create color palettes with RColorBrewer

# https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
# library(RColorBrewer)
# par(mar=c(3,4,2,2))
# display.brewer.all()

## RdBu:
# nCol <- 5
# palName <- "RdBu"

# nCol <- 5
# palName <- "YlOrRd"

# display.brewer.pal(nCol, palName)
# colVec <- brewer.pal(nCol, palName)

# END
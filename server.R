options(shiny.sanitize.errors = FALSE)

shinyServer(function(input, output, session) {

   ####BOOKMARKS GO HERE

   source("scripts/bookmark.R", local = TRUE)

   #####UPDATESELECTINPUT GO HERE

   source("scripts/reactive.R", local = TRUE)

   #####TREEMAPS GO HERE (RENDER D3 and RENDER UI)
  
   source("scripts/treemap.R", local = TRUE)

   ##### PLOTLY GO HERE (RENDER PLOTLY)

   source("scripts/plotly.R", local = TRUE)

   ##### SUNBURST GO HERE (RENDERSUBURST)
   #TIRADO POR FALTA DE USO

   #source("scripts/sunburst.R", local = TRUE)

   ##### LEAFLET GO HERE (RENDERLEAFLET)

   source("scripts/leaflet.R", local = TRUE)

   ##### FORMATTABLE GO HERE (RENDERFORMATTABLE)

   source("scripts/formattable.R", local = TRUE)

   ##### MARKOV CHAIN CODE GOES HERE (RENDERLEAFLET, FORMATTABLE, ETC)

   source("scripts/markov.R", local = TRUE)     
})

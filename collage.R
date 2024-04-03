html <- minimal_html("
  <h1>This is a heading</h1>
  <p id='first'>This is a paragraph</p>
  <p class='important'>This is an important paragraph</p>
")

library(xml2)
library(rvest)
library(purr)

# html_node is deprecated

npages <- function( what = "bebe,chats" ){
  url <- sprintf( "http://www.photos-animaux.com/photos,%s.html", what )
  read_html(url) %>%
    html_element("li.page") %>%
    html_text() %>%
    gsub( "^.* ([[:digit:]]+)$", "\\1", .) %>%
    as.numeric
}

scrap <- function(page = 1, what = "bebe,chats" ){
  url <- sprintf( "http://www.photos-animaux.com/photos,%s;%d.html", what, page )
  read_html(url) %>%
    html_element( "._image img" ) %>%
    html_attr("src")
}

# flatten_chr is deprecated

scrap_animals <- function(what = "bebe,chats", pages ){
  if(missing(pages)){
    pages <- seq_len(npages(what))
  }
  pages %>%
    map( scrap, what = what) %>%
    unlist()
}

tiles_animals <- function(size = 25, what = "bebe,chats", pages){
  dir.create( tf <- tempfile() )
  message(tf)
  # on.exit( unlink(tf, recursive = TRUE))

  urls <- scrap_animals(what = what, pages = pages)
  dest <- file.path( tf, basename(urls) )

  walk2( urls, dest, download.file, quiet = TRUE  )
  tiles( dest, size = size )
}

library(magick)

tiles <- function(files, size = 25L){
  images <- image_read(files) %>%
    as.list() %>%
    map( image_center_crop ) %>%
    image_join()

  scaled <- image_square_bitmap(images, 1L)
  tiles  <- image_square_bitmap(images, size)
  grab   <- function(i) map_raw(scaled, extract2, i)
  tibble( red = grab(1), green = grab(2), blue = grab(3), alpha = grab(4), tile = tiles)
}





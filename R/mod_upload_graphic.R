# Module UI
#' @title   mod_upload_graphicui and mod_upload_graphic
#' @description  A shiny Module that ...
#'
#' @param id shiny id
#' @param label_text Optional string for [shiny::fileInput()] widget
#'
#' @export 
#' @importFrom shiny NS tagList fileInput
#' @examples 
mod_upload_graphicui <- function(id, label_text = NULL){
  ns <- NS(id)
  tagList(
    fileInput(
      ns("image_upload"),
      label = label_text,
      multiple = FALSE,
      accept = NULL,
      buttonLabel = "Browse...",
      placeholder = "JPEG and PNG formats supported"
    )
  )
}
    
# Module server
#' mod_upload_graphic server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @import fs
#' @import magick
#' @import jpeg
#' @export
#' @rdname mod_upload_graphicui
    
mod_upload_graphic <- function(input, output, session){
  ns <- session$ns
  
  # reactive for processed image file
  img_processed <- reactive({
    req(input$image_upload)
    
    file_obj <- fixUploadedFilesNames(input$image_upload)
    
    # if image is a png, need to convert to jpg via magick package
    if (fs::path_ext(file_obj$datapath) == "png") {
      image_obj <- magick::image_read(file_obj$datapath) %>%
        magick::image_convert("jpg")
      
      image_path <- magick::image_write(image_obj, 
                                        path = fs::path_ext_set(file_obj$datapath, "jpg"))
    } else{
      image_path <- file_obj$datapath
    }
    
    # read in jpg image as new object
    image_obj <- jpeg::readJPEG(image_path)
    
    # return a list with the image object and image path
    return(
      list(
        image_path = image_path,
        image_obj = image_obj
      )
    )
  })
  
  # assemble return object
  img_processed
}
    
## To be copied in the UI
# mod_upload_graphicui("m1")
    
## To be copied in the server
# callModule(mod_upload_graphic, "m1")
 

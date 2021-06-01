

# Image pre-processing section using magick.

library(tensorflow)
library(tesseract)
library(OpenImageR)
library(SuperpixelImageSegmentation)


# Loading test image in the form of pixel matrices representing low level attributes.
x <- "//Users/joellashmore/Downloads/scraped metal logos/Iron Kingdom – On the Hunt Review.jpg"
image_read(x) %>% 
image_convert(colorspace = "gray") %>% 
image_threshold(
  type = c("black", "white"),
  threshold = "50%") %>%   image_enhance() %>%
  image_edge(radius = 2)

preprocesser <- function (imagepath){

  readin <- image_read(imagepath) 
  
  readin <-  image_convert(readin, colorspace = "gray") 
  readin <-  image_morphology(readin, 'Erode', "rectangle")
  readin <-  image_edge(readin,radius = 1)
  readin <- image_canny(readin)
    
  readin <- image_threshold(readin,type = c("black", "white"),threshold = "50%")
  readin
}

preprocesser(x)

test <- image_threshold(
  test,
  type = c("black", "white"),
  threshold = "50%",
  channel = NULL
)
test



image_canny(test,geometry = "0x1+10%+30%")

preprocess <- function (photo){
  image_contrast(photo, sharpen = 1)
  image_convert(photo, colorspace = "greyscale")
  image_canny(photo, geometry = "0x1+10%+30%", color = "#000080")
  image_hou
  
  
}



init = Image_Segmentation$new()


spx = init$spixel_segmentation(input_image = test, 
                               superpixel = 600, 
                               AP_data = TRUE,
                               use_median = TRUE, 
                               sim_wL = 3, 
                               sim_wA = 20, 
                               sim_wB = 10,
                               sim_color_radius = 3, 
                               verbose = TRUE)

OpenImageR::imageShow(spx$AP_image_data)


spx_mbkm = init$spixel_segmentation(input_image = test, 
                                    superpixel = 600, 
                                    AP_data = TRUE,
                                    use_median = TRUE, 
                                    sim_wL = 3, 
                                    sim_wA = 10, 
                                    sim_wB = 10,
                                    sim_color_radius = 10, 
                                    kmeans_method = "mini_batch_kmeans",
                                    kmeans_initializer = "kmeans++",
                                    kmeans_num_init = 3, 
                                    kmeans_max_iters = 100,
                                    minib_kmeans_batch = 10, 
                                    minib_kmeans_init_fraction = 0.75,
                                    verbose = TRUE)
OpenImageR::imageShow(spx_mbkm$AP_image_data)

brutal <- function(photo){
  image_connect(photo,min_pixels = , connectivity = 4)
}
brutal(test)

# Whitelist only certain characters as titles will not have special characters like "&" or "#".

whitelist <-
  "1234567890-qwertzuiopüasdfghjklöäyxcvbnmQWERTZUIOPÜASDFGHJKLYXCVBNM()"



text1 <- ocr(
  "//Users/joellashmore/Downloads/1 image.jpg",
  engine = tesseract(
    language = "eng",
    options = list(tessedit_char_whitelist = whitelist)
  )
)
text1
image_ocr(brutal)
ocr(brutal)
df <- as.data.frame(tesseract_params())



paramsc <- as.data.frame(list(tesseract_params()))
get_param_values(list(tesseract()))
brutal



(tesseract_info())
list(tesseract_info())
image_fuzzycmeans(brutal, smoothing = 10)
image_ocr(brutal)
brutal
unlink(brutal)
cat(brutal)

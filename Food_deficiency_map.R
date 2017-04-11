library("jpeg") # for reading in PNGs
library(magick)
library(magrittr)

imgpath <- "C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/Figures/MAPS_NEWMARCH17/"
a <- list.files(imgpath, pattern="jpg$")
BPL <- a[grep("incgrp1", a)]
INC4 <- a[grep("incgrp4", a)]
nut <- c("Calorie", "Protein", "Iron", "Zinc", "Vitamin A")

# img <- list()
img_BPL <- list()
img_INC4 <- list()
for(i in 1:10) {
  # img[[i]] <- readJPEG(paste0(imgpath, BPL[i])) 
  img_BPL[[i]] <- image_read(paste0(imgpath, BPL[i])) %>% image_trim() %>% image_border("white", "20x10") # %>% image_annotate("This is a test", color="blue") # image_annotate does not work
  # img_BPL[[i]] <- image_trim(img_BPL[[i]])
  # img_BPL[[i]] <- image_trimcrop(img_BPL[[i]], "500x500+190+120")
  img_INC4[[i]] <- image_read(paste0(imgpath, INC4[i])) %>% image_trim() %>% image_border("white", "20x10")
  # img_INC4[[i]] <- image_trim(img_INC4[[i]])
  # img_INC4[[i]] <- image_crop(img_INC4[[i]], "500x500+190+120")
}

# image_browse(img_BPL[[i]])
# image_browse(img_INC4[[i]])

img_BPLr <- do.call("c", unlist(img_BPL)[1:5])
img_BPLu <- do.call("c", unlist(img_BPL)[6:10])
img_INC4r <- do.call("c", unlist(img_INC4)[1:5])
img_INC4u <- do.call("c", unlist(img_INC4)[6:10])

img_BPL <- image_append(c(image_append(img_BPLu), image_append(img_BPLr)), stack=TRUE)
img_INC4 <- image_append(c(image_append(img_INC4u), image_append(img_INC4r)), stack=TRUE)

# for(i in 1:5) {
#   img <- image_annotate(img, "ddd", size = 10, color = "red", gravity = "northwest")
# }

# image_info(img)
# image_border(img, "", "25x0")

image_write(img_BPL, path = paste0(imgpath, "BPL.png"), format = "png")
image_write(img_INC4, path = paste0(imgpath, "INC4.png"), format = "png")

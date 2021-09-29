Get ready to create a fantastic LEGO mosaic! Here is the general procedure to get started:


### Import image

Upload your desired image to be converted into a mosaic. At this time, only images in `png` and `jpeg` format are supported. Note that due to limitations of the hosting platform, you will not be able to process images larger than 2 megabytes. Please use additional software to compress an image file that is larger than this size before uploading to the application.

After the upload completes, you will see a small preview of the raw image in the application.

### Define mosaic settings

You will be presented with a few additional inputs for customizing the features of your mosaic once your image is uploaded. Many of these settings mirror function parameters supplied to the [`image_to_mosaic`](http://brickr.org/reference/image_to_mosaic.html) function contained in the [`{brickr}`](http://brickr.org/index.html) package.

* Each mosaic starts with a base **plate**, much like a typical LEGO set. You can customize the width and height of this plate using the **Plate width** and **Plate height** inputs, with units of LEGO studs.
* The **Brightness** slider controls the overall brightness of the mosaic. Setting the slider less than 1 will decrease the brightness, while values above 1 will increase the brightness. Note that for some mosaics, increasing this value to 3 or more may result in the majority of bricks being colored white.
* The general coloring mechanisms are chosen through the **Color Palette** selector. When selecting the defaults, you are then able to choose one or more specific themes as sources for the colors. The *Universal* theme contains the most colors, while the *Special* theme contains a smaller set. If a black & white mosaic is chosen, you can customize the contrast of the image with a slider. Setting the slider less than 1 will decrease the contrast, while values above 1 will increase the contrast.
* You can choose the algorithm used for matching the colors of the raw image to the available colors from the selected palette. Behind the scenes, the [`{farver}`](https://cran.r-project.org/package=farver) R package drives the analysis. You can view more details about the different algorithms via [Wikipedia](https://en.wikipedia.org/wiki/Color_difference)

Once your selections are complete, press the **Create!** button to begin creating your mosaic. In general, the processing will complete in 5 seconds or less, depending on the raw image.

### Mosaic metrics

After the mosaic is generated, you will see a few summary metrics associated with the bricks necessary to actually create the mosaic in real life (IRL). 

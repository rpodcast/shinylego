Title for post: Shiny Contest Submission: The Shiny LEGO mosaic creator!

Tags for post: shiny, shiny-contest

**ShinyLEGO** is a Shiny application that lets you create a simulated
mosaic composed of LEGO bricks from any image file. Once your picture is
uploaded, you can customize various settings for your mosaic such as
dimensions, brightness, and color types. In addition, the application
will generate specific instructions so that you can build the mosaic
yourself\! These instructions contain the brick colors and types
required for each step, and a diagram showing how the bricks are to be
assembled. You can also see an estimated cost associated with the mosaic 
if you wish to purchase the bricks yourself.

## Links

* Live version: [shinyapps.io/rpodcast/shinylego](https://shinyapps.io/rpodcast/shinylego)
* RStudio Cloud Project: [rstudio.cloud/project/257906](https://rstudio.cloud/project/257906)
* GitHub Repository: [github.com/rpodcast/shinylego](https://github.com/rpodcast/shinylego)

## Usage and Features

When you visit the app, you will be taken to the welcome page that gives a brief description of the app, and links associated with the app developer (yours truly) and influential contributor Ryan Timpe (more on his contributions in the acknowledgements section below).

## Acknowledgements

This application would not be possible without the innovative R scripts
created by [Ryan Timpe](http://www.ryantimpe.com/)\! Ryan has written
excellent blogposts detailing the workflow this application draws upon,
and the code for each post can be found on his [LEGOMosaics GitHub
repository](https://github.com/ryantimpe/LEGOMosaics):

  - [How To: LEGO mosaics from photos using R & the
    Tidyverse](http://www.ryantimpe.com/post/lego-mosaic1/)
  - [LEGO mosaics: Two weeks
    later](http://www.ryantimpe.com/post/lego-mosaic2/)
  - [LEGO mosaics:
    Part 3(D)](http://www.ryantimpe.com/post/lego-mosaic3/)

In addition to Ryan Timpe’s excellent R programs detailed above, this
application utilizes many powerful packages in the `shiny` ecosystem
such as the following:

  - [`shinyjs`](https://deanattali.com/shinyjs): Easily improve the user
    experience of your Shiny apps in seconds by [Dean
    Attali](https://deanattali.com/)
  - [`bs4Dash`](https://rinterface.github.io/bs4Dash/index.html):
    Bootstrap 4 shinydashboard using AdminLTE3 by [David
    Granjob](https://twitter.com/divadnojnarg) (part of the
    [RinteRface](https://rinterface.com/) initiative)
  - [`shinyWidgets`](https://dreamrs.github.io/shinyWidgets/index.html):
    Extend widgets available in Shiny by [Victor
    Perrier](https://twitter.com/_pvictorr) (part of the
    [dreamRs](https://www.dreamrs.fr/)
    organization)
  - [`shinycustomloader`](https://emitanaka.github.io/shinycustomloader/):
    Add a custom loader for R shiny by [Emi
    Tanaka](https://emitanaka.github.io/)
  - [`DT`](https://rstudio.github.io/DT): R Interface to the jQuery
    Plug-in DataTables by [Yihui Xie](https://yihui.name/)
  - [`golem`](https://github.com/ThinkR-open/golem/): A framework for
    building robust Shiny Apps by [Vincent
    Guyader](https://github.com/VincentGuyader) and [Colin
    Fay](http://colinfay.me/) (part of the [R Task
    Force](https://rtask.thinkr.fr/) at [ThinkR](https://thinkr.fr/))



``` r
shinylego::run_app()
```

## Deployments

`shinylego` is also available on the Shinyapps.io hosting service at
[shinyapps.io/rpodcast/shinylego](https://shinyapps.io/rpodcast/shinylego)

## Acknowlegements

In addition to Ryan Timpe’s excellent R programs detailed above, this
application utilizes many powerful packages in the `shiny` ecosystem
such as the following:

  - [`shinyjs`](https://deanattali.com/shinyjs): Easily improve the user
    experience of your Shiny apps in seconds by [Dean
    Attali](https://deanattali.com/)
  - [`bs4Dash`](https://rinterface.github.io/bs4Dash/index.html):
    Bootstrap 4 shinydashboard using AdminLTE3 by [David
    Granjob](https://twitter.com/divadnojnarg) (part of the
    [RinteRface](https://rinterface.com/) initiative)
  - [`shinyWidgets`](https://dreamrs.github.io/shinyWidgets/index.html):
    Extend widgets available in Shiny by [Victor
    Perrier](https://twitter.com/_pvictorr) (part of the
    [dreamRs](https://www.dreamrs.fr/)
    organization)
  - [`shinycustomloader`](https://emitanaka.github.io/shinycustomloader/):
    Add a custom loader for R shiny by [Emi
    Tanaka](https://emitanaka.github.io/)
  - [`DT`](https://rstudio.github.io/DT): R Interface to the jQuery
    Plug-in DataTables by [Yihui Xie](https://yihui.name/)
  - [`golem`](https://github.com/ThinkR-open/golem/): A framework for
    building robust Shiny Apps by [Vincent
    Guyader](https://github.com/VincentGuyader) and [Colin
    Fay](http://colinfay.me/) (part of the [R Task
    Force](https://rtask.thinkr.fr/) at [ThinkR](https://thinkr.fr/))

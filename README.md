# Vista_expresso

This script is useful for analysing the feeding datafiles saved from visual_expresso_gui. You can easily upload the excel files to plot some of the common quantities as indicated in Yapici et al, Cell (2016).

## Getting started

### Prerequisites

For the app to run, the system requires R (version 3.4) to be installed (details can be found at https://www.r-project.org/).

### Deploying

To run the app, download all the files. In R console run the following commands:

```
install.packages("shiny") #just when running for the first time
require(shiny)
runApp("yourfolderpath/feeding_stats.R")
```

The app will open in your browser.

Alternatively, if you are using R studio, you just need to press the "runApp" button on the top right.

## Usage

Browse and select the excel file. In the label field, enter the identifying characteristic of that dataset for which it is being tested (for eg mutant, starvation time) and click upload. Note that the program assumes one datafile contains only one condition/identifying characteristic. You can add more datafiles of the same condition, the program will automatically bundle them all together.

### Download

You can download the figures by clicking on the button on the left. Note, if you are running through RStudio, the saved file will not be of the type .pdf, but you can append the extension and then it should work just fine.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

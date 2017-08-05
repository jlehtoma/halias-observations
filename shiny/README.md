## Running the shiny app

**NOTE**: The actual data (not in this repo) needs to be in `data` directory before running the processing script. Using RStudio project (i.e. running from the project root):

```
# This will take some time
source("R/read_and_process_data.R")

library(shiny)
shiny::runApp("shiny")
```

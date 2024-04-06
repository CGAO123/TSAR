readme file
================

<img src="vignettes/images/TSAR_logo.png" width="400px" />

# TSAR

    TSAR, short for Thermal Shift Analysis in R, presents the simple interface to TSA data analysis and visualization.
    TSAR Package is contains three separate shiny application regarding, data pre-processing, data analysis, and data visualization. All application can be opened in both interactive window or browsing engine by copy pasting server address into web browser. All functions can be run outside shiny applications, refer to vignette, "TSAR Workflow by Command" for instructions.
    Download TSAR package from Bioconductor branch using the below command lines.

``` r
library(BiocManager)
BiocManager::install("TSAR")
library(TSAR)
library(shiny)
```

## Data Pre-Processing

    Load data using RStudio import, read.csv, or read.delim. Select and preview raw curve using `weed_raw()`.

``` r
data("qPCR_data1")
```

```
runApp(weed_raw(qPCR_data1))
```

``` r
qPCR_data1 <- remove_raw(qPCR_data1, removerange = c("B", "H", "1", "12"))
```

    In the plot panel, user can interact with graph and select and un-select data by clicking on curves. Selection by grid is also possible. Click on grid to highlight wells of selection. Click again to un-highlight. User may also `View Selected` wells only and `Remove Selected`. To return to default page and current all change to data set, click `Refresh Screening`. To propagate the same changes to local data set, click `Copy Selected in Full Function Call` to copy a `remove_raw()` call containing all selected wells into clipboard. Paste the call back into console or script and run to propagate changes locally. 
    Use `View Selected` and `Remove Selected` to view and remove selection. To return to viewing rest of the data, click `Refresh Screening`. To propagate the same changes to local data set, click `Copy Selected in Full Function Call` to copy a `remove_raw()` call containing all selected wells into clipboard. Paste the call back into console or script and run to propagate changes locally.

## Data Analysis

    Analyze data by calling function `analyze_norm()` and follow the workflow from top to bottom. Preview data table for changes occurring at each step and refer to graph to view fit of model on each well. Remember to save analysis output locally by clicking `Save File`. Always preview data before saving to ensure data contains all necessary information.

``` r
runApp(analyze_norm(qPCR_data1))
```

    The top left panel output a preview of current data set. The right panel allows user to view the fit of model and Tm estimation by individual wells of selection. Once confirming correct data input and modeling effects,
    Click `Analyze all Wells` to propagate model and analysis onto the rest of data. A preview of analyzed data will also be modeling and analyzing all 96 Wells will take between 30 to 50 seconds.
    Upload well information by excel template and preview to confirm if information is correct. Use `Preview` to preview uploaded information and directly edit inside the window. User may also choose `Manual Input` to put in all condition information by Well. Make sure to hit `Save Changes` after editing and click `Set Conditions` to link all data to the conditions. A success message should be prompted.
    Lastly, to save all analysis locally, click `Save File` after previewing output. 

## Data Visualization

    Use function `graph_tsar()` to start a Shiny application for graphing options. Run `na.omit()` on data if error occurs. Three graphing options are allowed, boxplot of Tm, compare plots, and conditions plot.
    Function takes optional data parameter. If analysis file is already imported in the environment, call function as `graph_tsar(tsar_data)`. Else, user may use the merge data panel to upload and merge data of all test trials. Simply call `graph_tsar()` and click `Upload and Merge Data` button to reveal the panel to merge data.

``` r
runApp(graph_tsar())
```

    Upload is limited to 30MB in data size, but not count size. After upload, user will be prompted corresponding numbers of input boxes to specify date of each experiment. Click `Save Dates`, then `Merge and Save Data`. A short preview of tsar_data will be prompted. For a full list of well_ID and conditions_ID, refer to the helper buttons at the bottom of page.
    Top panel outputs all plots, selected desired graphing features below and click generate to output graphs. p.s. Graphing compare plots and selected curves are takes longer than boxplot, please give it few seconds to load.
    Generating compare plots will output all plausible comparisons by control. To any specific one, a drop list `View Only:` will be prompted below the botton `Generate Compare Plots`. Select by condition_ID to zoom in on graphs.

## Funding

    This research was supported in part by the National Institutes of Health (R01 AI120860 and U54 AI170855 to S.G.S.; W.M.M. was supported in part by T32 GM135060 and F31 AI174951). S.G.S. acknowledges funding from the Nahmias-Schinazi Distinguished Chair in Research. Any opinions, findings, conclusions, or recommendations expressed in this material are those of the authors and do not necessarily reflect the view of the funding agencies.  

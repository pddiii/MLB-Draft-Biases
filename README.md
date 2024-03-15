# MLB-Draft-Biases

The focus of the project within this repository is to analyze and identify Draft Biases within the Major League Baseball (MLB) Amateur Draft. The Draft has been taking place since 1965, and in the past has featured up to 100 rounds (today's version features 20 rounds).

Aside from identifying these draft biases we sought to determine if there was a method for identifying a player's success level in the MLB. Our metric of success used was FanGraphs Wins Above Replacement (fWAR). Our predictors were comprised entirely of variables that included information of player demographics, and physical characteristics. 

## Data Dictionary

To find our dictionary of variables click below:

- [Dictionary](Data/dictionary.md)

## Data Collection and Data Cleaning

Our Data Collection process was performed utilizing `baseballR`, and `pybaseball` respectively. These processes can be found below:

- [R Data Collection](data_collection.R)
- [Python Data Collection](data_collection.py)

Our data cleaning process was performed utilizing R and several packages (primarily those in `tidyverse`)

- [Data Cleaning](data_cleaning.R)

## Data Files

The data files we used to build our models, and the raw data we collected are stored all within the file linked below:

- [Data Folder](Data/)

## Report

To read the report on our findings click the link below:

- [Draft Biases Report](Draft-Biases-Report.pdf)

## Libraries and Resources Utilized

- `bookdown`
  - Used for generating the report utilizing the Bookdown syntax language [Link](https://bookdown.org/)
- `ggplot2`
  - Used to create the visualizations and EDA in the Report [Link](https://ggplot2.tidyverse.org/)
- `gridExtra`
- `corrplot`
- `data.table`
  - Utilized to decrease memory of our data objects to reduce processing time. [Link](https://rdatatable.gitlab.io/data.table/)
- `tidyverse`
  - Utilized for the data cleaning process [Link](https://www.tidyverse.org/)
- `tidymodels`
  - Utilized for the creation of the boosted decision tree prediction model [Link](https://www.tidymodels.org/)
- `xgboost`
  - The xgboost engine was used for the boosted decision tree prediction model [Link](https://xgboost.readthedocs.io/en/stable/)
- `doParallel`
  - Used for parallel processing during the tuning process of the model hyperparameters [Link](https://cran.r-project.org/web/packages/doParallel/index.html)
- `vip`
  - Used to create the variable importance plot for the model. [Link](https://cran.r-project.org/web/packages/vip/index.html)
- `caret`
  - Utilized for the training process of the model [Link](https://topepo.github.io/caret/)
- `Boruta`
  - Used to confirm feature selection importance [Link](https://cran.r-project.org/web/packages/Boruta/)
- `kableExtra`
  - Used to create LaTeX formatted tables within the report. [Link](https://cran.r-project.org/web/packages/kableExtra/index.html)
- `maps`
- `mapsdata`
- `mapproj`
- `reshape2`

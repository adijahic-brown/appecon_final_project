## ReadMe

This repository replicates Adi Jahic's figures for the Applied Economics final project. 

The code can all be run using main.R. Set the working directory to the main repository folder. You can set any of the globals to 0 if you do not wish to create the cleaned datasets or figures associated with that dataset. 

Clean datasets are outputted to "data/clean." Figures are outputted to "output".

This project uses 4 datasets from three sources. 

The first source which is used to create the "Hyderabad" and "Karnataka" data frames is the replication package for the Banerjee et. al. (2024) QJE paper entitled "Changes in Social Network Structure in Response to Exposure to Formal Credit Markets" which can be found at zenodo.org (https://zenodo.org/records/7706650), as of April 16 2025. In order to create the Hyderabad data set from scratch, run the main code in the dataset and ensure that the line of code in the Prepare_Data file which saves the dataset is not turned off (it is commented out by default). The three datasets used to in this project can then be found, by the same name as they appear here. 
- To run this code, you will need to download the since removed "dummies" package. We recommend the following line of code: 

    ```R
    install.packages("https://cran.r-project.org/src/contrib/Archive/dummies/dummies_1.5.6.tar.gz", repos = NULL, type = "source")
    ```

The second source is the Global Entreprenership Monitor. The dataset used in this project is titled "GEM 2021 APS Global Individual Level Data_15 February 2023", as of April 16 2025. 

The third source is the World Bank Enterprise Survey data. We used the panel data set from Kenya which includes individual level data from 2007, 2013, and 2018. We recommend finding these data at https://databank.worldbank.org/source/enterprise-surveys, as of April 16 2025.


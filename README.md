# Description

This repository contains code and data used in the manuscript "Habits and attitudes towards writing affect the publication output of environmental biology trainees", under review in Ecosphere

### Authors

- Yara Alshwairikh*
- Ana Clara Fanton*
- Kyra Prats
- Mary Burak
- Marlyse Duguid
- Freya Rowland

*co-first authors

### Contents

#### (1) Data
Contains all survey data (available on Dryad upon acceptance)

* [Data csv file for analyses](<data/dataclean_Jul22.csv>): Current version of the data for analyses
* [Full survey data with metadata](<data/Surveydata_with_metadata.xlsx): Also archived on Dryad (link to be published upon acceptance of ms)
* The remaining five csv data files include summary data for specific questions which were used to generate figures

#### (2) Code
Contains all R code used to run models in the paper

* [analysis.R](<code/analysis.R>): This script file includes code for all models used in the manuscript
* [SentimentAnalysis.R](<code/SentimentAnalysis.R>): Code for running the sentiment analysis using R package [tidytext](<https://www.tidytextmining.com/sentiment.html>). Ultimately we went with unanimous co-author agreement because too many of our words were missing from established sentiment dictionaries. The results from these analyses closely match the one in the paper.

#### (3) Figures
Copies of all figures generated for the manuscript

# Multi-level strategies for improving trainee scientific writing habits

### Collaborators

- Yara Alshwairikh*
- Ana Clara Fanton*
- Kyra Prats
- Mary Burak
- Marlyse Duguid
- Freya Rowland

*co-first authors

### Links to resources for this manuscript
* [Color palette](<https://coolors.co/405364-585b74-6c5b7b-966480-c6798f-df858e-eda09c> "Color palette") for figures
* [Box figure draft](<https://docs.google.com/presentation/d/1CSUlPH7a5M1es4IyuSy8WH2JvQX9nPUwl5_y_ahd9Xg/edit#slide=id.gcb8342311d_1_0.>)
* [Data csv file](<data/dataclean_Nov2.csv>): Current version of the data for analyses. Last updated for word sentiment (positive/negative/neutral) by all coauthors

### Some other great resources
* [Twitter thread on revisions](<https://twitter.com/ellycknight/status/1456339626310782978>) that may be useful

### Code in this repository
* [SentimentAnalysis.R](<code/SentimentAnalysis.R>): Code for running the sentiment analysis using R package [tidytext](<https://www.tidytextmining.com/sentiment.html>)
* [CareerFigure.R](<code/CareerFigure.R>): Code for importing data, cleaning, and making the career interest figure

### Tables here for a minute for writing (should this be in supplementary material?)

|Parameter     | Median | Mean |  MAP |        95% CI |      pd |   ps |  Rhat |      ESS |     BF |
|-------------|--------|------|------|---------------|---------|------|-------|----------|-------|
plan_regular  |   0.57 | 0.56 | 0.59 | [ 0.18, 0.89] |  99.70% | 0.99 | 1.000 | 26320.00 |   3.85
plan_deadline |   0.20 | 0.19 | 0.20 | [-0.18, 0.55] |  85.33% | 0.70 | 1.000 | 25280.00 |  0.108
plan_no       |   0.85 | 0.85 | 0.83 | [ 0.52, 1.14] | 100.00% | 1.00 | 1.000 | 26039.00 | 308.61


|Parameter     | Median | Mean |  MAP |        95% CI |      pd |   ps |  Rhat |      ESS |     BF |
|-------------|--------|------|------|---------------|---------|------|-------|----------|-------|
tracking_advisor |   0.05 |  0.04 |  0.07 | [-0.34, 0.37] | 60.77% | 0.38 | 1.000 | 40066.00 |    0.063
tracking_group   |  -0.21 | -0.28 | -0.12 | [-1.25, 0.37] | 73.67% | 0.62 | 1.000 |  6976.00 |    0.121
tracking_individ |   0.74 |  0.72 |  0.76 | [ 0.29, 1.08] | 99.84% | 1.00 | 1.000 | 37428.00 |     7.54
tracking_no      |   1.25 |  1.24 |  1.25 | [ 1.03, 1.43] |   100% | 1.00 | 1.000 | 42310.00 | 2.71e+09

# To do
- [ ] Finalize figures (YA, AF) - theme_classic(base_size = 14), color scale, make multiplots in R or photoshop
- [x] Run new analyses of individual behaviors and make figures (FR)
- [ ] Clean up data for archival (YA?)
- [ ] Clean up repo (FR) - Will YA check over areas I've commented out and see if we can delete?
- [ ] Write draft cover letter (KP)
- [ ] Update results and methods (YA, AF, FR)
- [ ] Reformat manuscript for submission (YA, AF)
- [ ] Contact journal about fit (YA)
- [ ] Proofread and streamline (MB)
- [ ] Send manuscript to Marlyse (YA)

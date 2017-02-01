# TED data analysis

## Motivation
A couple of years ago, we did an analysis of the data collected in Market Dojo.
this provide to be quite interesting, and a good opportunity to learn more about oru customers.

Recently I also did some research into OJEU and TED for a Market Dojo 'Lunchtime Lecture' and found the TED dataset for 2015 spend data.
https://data.europa.eu/euodp/en/data/dataset/ted-csv

This seemed like a great opportunity to try some different tools for data analysis, and to see what I could learn about EU spend data.

## Approach
I usually use Excel to analyse data, but I also wanted to experiment with R, an open source tool which is very popular with data analysts and the scientific community.

## Basic facts
The 2015 dataset contains 542, 376 rows and took a while to download. Excel opened it without complaint, and I was able to get a feel for the data.

The total value of the was €763,502,408,895.25 (about €763.5bn).

## Running the script
- make sure you have R and all the necessary packages.
- you may need a version of country_codes from git.
- `RScript TED_analysis.R`

## Next steps

If this has piqued your interest, there is a fantastic website http://yleborgne.net/opented/ where you can investigate for yourself. You can also see how I generated the graphs here (github).

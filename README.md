# Citi Hackathon Project 2018

## Summary
This is a Shiny application written in R, to allow prospective home buyers to view available properties and search for suitable properties based on various criteria. This application is specific to HDB recommendations and does not include private estates/ condominiums.

## Application Architecture

## Requirements and Setup
- R
- Rstudio

Install these packages: `shiny`, `shinydashboard`, `leaflet`,`DT`.

In Rstudio, enter in the console: `shiny::runGitHub(repo = 'citi-hackathon', username = 'citi-hackathon-2018')`


## Assumptions of Solution
The solution assumes the following few implicit assumptions:
1) Effective Annual Rate of house loans are fixed at 3.5%.
2) Daily expenses and savings will be 30% of the buyer's net monthly income.
3) We assume that the buyer will always be awarded the 20,000 CPF grant when buying their choice of houses.

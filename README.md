# Space-Apps-COVID-19-Challenge-Code---Team-Elrond
This is a program in R that uses Mallows' Cp, adjusted r-squared, forward selection, and backwards elimination methods 
to determine the regression lines to the number of COVID-19 cases and deaths as a function of provided input variables that possibly 
could be used to predict the number of COVID-19 cases/deaths in other countries. 

There are two files - the code itself used to determine the regression lines (the comments in the code describe it), and the data
spreadsheet.

I will define the variables in the data spreadsheet here: 

1. covCases - the number of coronavirus cases in a given country
2. covDeaths - the number of deaths due to coronavirus in a given country
3. airQual - the air quality of a given country. Higher values indicate a worse air quality, as it means that more harmful particles are in
    the air. Units: micrograms per cubic meter.
4. percentEducation - the amount a given country's government spends on education as a percentage of its total GDP
5. GDP - gross domestic product of a given country, measured in $USD.
6. avgInc - the average yearly household income in a given country, measured in $USD.
7. landlocked - 1 if a country is landlocked, 0 if it isn't.
8. waterlocked - 1 if a country is entirely surrounded by water, 0 if it isn't.
9. popDens - the average population density in a given country, measured in people per square kilometer.

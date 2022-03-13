# Introduction
As a response to the sudden COVID-19 pandemic, governments world-wide have resorted to a number of social policies designed to curtail the spread of the virus. In theory, these policies significantly reduce the number of infected individuals to a level that can be managed by healthcare systems. On the other hand, introduction of these containment policies has not been without a hefty price on global and local economies. The main goal of this project is to quantify the effect of government action on the spread of Coronavirus infections. Specifically, to measure potential impact of school and workplace closures on the number of daily new cases in order to better understand whether these policies are effective in halting the spread of the ongoing Coronavirus crisis and their potential in future pandemics. The main research question is to find whether these policies have a significant quantifiable effect on the number new cases of Coronavirus. In order to answer this question, three datasets are considered. World Health Organization ("WHO") Coronavirus Dashboard [1] dataset provides an ongoing daily number of new Coronavirus cases by country from early 2020. These are compared to the daily government policy indicator variables in the Oxford Covid-19 Government Response Tracker ("OxCGRT") [2] published by the Blavatnik School of Government at the University of Oxford. Particularly indicators for nation-wide school and workplace closures are used for the analysis. Lastly, the population data published by the World Bank [3] is utilized to better understand by-country trends in the previous datasets. In order to take into account the time series aspect of the WHO and OxCGRT a panel regression is utilized to regress the daily number of new cases on indicator variables of government policies in Portugal, France, Germany, The United Kingdom, and Italy.
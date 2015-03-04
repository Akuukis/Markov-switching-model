# Markov-switching-model
Markov two state autoregressive (lag 1) bivariate multi-asset model. Was made for the course in school under time-pressure, therefore apologies for mess and lack of comments. Ask! Its working now for 2 states and 3 assets, and was programmed for any amount of states and assets (needs repair).

# Short summary

Fits markov switching model to a set of timeseries data.

1. Markov switching model
1.1. x states (tested with x=2)
1.2. x assets (tested with x=3)
1.3. simple autoregression lag x (tested with x=1)
1.4. Calculating 4+1 moments by Timmermann
2. Ordinary least squares (OLS), tried but not used.
3. Relative least square (RLS), used with customized koefficients.
4. Descriptive statistics
4.1. Loads data from csv (In excel use *save as...* and choose csv instead of xlsx. See example for format
5. Monte carlo (a.k.a. search by random) fitting
5.1. Improves x samples for y times every cycle for infinite iterations.
5.2. Custom and dynamic constraints for every variable. Can automaticaly narrow search space by q
5.3. Can load custom starting point from csv, see example for format (its copy/paste from output format).

All output is in csv. 

Built upon:
1. Timmermann, A. (1999), Moments of Markov Switching Models. FMG Discussion Papers, 
	Financial Markets Group. Retrieved January 15, 2015 from EconPapers website: 
	http://EconPapers.repec.org/RePEc:fmg:fmgdps:dp323.

2. Aigner, P., Beyschlag, G., Friederich, T., Kalepky, M., & Zagst, R. (2012). Modeling and 
	managing portfolios including listed private equity. Computers & Operations Research, 39(4), 
	753-764. http://dx.doi.org/10.1016/j.cor.2010.12.015 
	
Lua 5.3

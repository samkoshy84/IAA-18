/*----------------------------------*/
/*    MSA 2018: Survival Analysis   */
/*      Cox Regression Models       */
/*                                  */
/*          Matthew Austin          */
/*----------------------------------*/

/* fit cox models using phreg */
/* same as every other model structure */
proc phreg data=survival.recid;
	/* I specify ties = efron (this isn't the default,
	but I can't think of any reason that you shouldn't use it),
	and risklimits=pl for a CI on the hazard ratios */
	model week*arrest(0)=fin age race wexp mar paro prio / ties = efron risklimits=pl;
run;

/* request baseline survival curve plot with plots=survival */
/* but who is the baseline? */
proc phreg data=survival.recid plots=survival;
	model week*arrest(0)=fin age race wexp mar paro prio / ties = efron risklimits=pl;
run;

/* create a fake dataset with some comparison
you might actually be interested in */
data ref;
input fin age race wexp mar paro prio;
datalines;
1 30 0 1 0 0 0
0 30 0 0 0 0 0
;
run;

/* we can plot curves for the two "groups" of people above */
/* (overlay) will put both on the same plot instead of separate */
proc phreg data=survival.recid plots(overlay)=survival;
	model week*arrest(0)=fin age race wexp mar paro prio / ties = efron risklimits=pl;
	/* baseline statement is how specify what you want to plot */
	/* "covariates" looks for the dataset */
	/* "out" will output a dataset with the estimates in case
	you want to plot them with something other than sas default graphics */
	/* "rowid" will label each curve by the levels of fin */
	baseline covariates=ref out=refs / rowid=fin;
run;

/* checking linearity with "assess" statement */
proc phreg data = survival.recid;
	model week*arrest(0) = fin age prio / ties = efron;
	/* this is basically just doing a simulation */
	assess var=(age prio) / resample;
run;

/* checking ph assumption using zph plots */
proc phreg data = survival.recid zph;
	model week*arrest(0) = fin age prio / ties = efron;
run;

/* checking ph assumption with "assess" for continuous variables */
/* note this doesn't work with time-dependent covariates */
proc phreg data = survival.recid;
	model week*arrest(0) = fin age prio / ties = efron;
	assess ph / resample;
run;
/* for categorical variables, you can just look at stratified
curves from proc lifetest */
proc lifetest data=survival.recid plots=survival(atrisk cb);
	time week*arrest(0);
	strata fin;
run;

/* time-varying coefficients */
/* the wrong way:
create time*age interaction outside of data step */
data recidinter;
set survival.recid;
ageweek = age*week;
run;
proc phreg data=recidinter;
	model week*arrest(0) = fin age race wexp mar paro prio ageweek / risklimits=pl ties=efron;
run;
/* the right way: create time*age WITHIN phreg */
proc phreg data=survival.recid;
	model week*arrest(0) = fin age race wexp mar paro prio ageweek / risklimits=pl ties=efron;
	/* here I just have age*week, but this can really be whatever function of time
	you want it to be -- you could do age*log(week) or whatever you want */
	ageweek = age*week;
run;

/* time-varying predictors */
/* basically what we're going to do
is turn all those emp variables into
ONE variable at each time that represents
employment status for that week */
proc phreg data=survival.recid;
	model week*arrest(0) = fin age race wexp mar paro prio employed / risklimits=pl ties = efron;
	/* create an array of the emp variables */
	array emp(*) emp1-emp52;
	/* and pick out the one corresponding to the current week
	for everyone at risk during this week, not just the week
	a person was re-arrested */
	employed = emp[week];
run;


/* you can do lagged predictions as well */
/* let's instead use employment as of the previous week */
proc phreg data=survival.recid;
	/* since we're doing lags, there's no "previous" employment before week 1,
	so we'll lose the observation for each person for the first week */
	where week>1;
	model week*arrest(0) = fin age race wexp mar paro prio employed / risklimits=pl ties = efron;
	/* create an array of the emp variables */
	array emp(*) emp1-emp52;
	employed = emp[week-1];
run;
/* you can do any data step stuff within phreg */

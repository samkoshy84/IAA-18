/* new time at same probability */

/* fit the model to get stuff output */
proc lifereg data=survival.recid outest=a noprint;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=weibull;
	/* output the linear predictor and the cdf
	(cdf = 1 - S(t)) */
	output out=b xbeta=lp cdf=eventprob;
run;

/* get S(t) */
/* this is each person's estimated survival probability
at the ACTUAL time they had the event (or were censored) */
data b;
set b;
survprob_old = 1 - eventprob;
run;

/* get subset of people who had the event
and did not have financial aid */
/* only keeping the survival probability here */
/* we'll set this aside for now, but will need it later */
data survprob;
set b;
if arrest = 0 then delete;
if fin = 1 then delete;
keep survprob_old;
run;

/* now, back to the actual dataset,
let's pretend the people who didn't get financial
aid did get it */
data pred;
set survival.recid;
if arrest=0 then delete;
if fin=1 then delete;
fin = 1;
/* set week to missing so it's not used in model fitting,
but we will still get a prediction for lp */
week = .;
run;
/* rbind */
proc append base=pred data=survival.recid;
run;

/* now refit the model and output lp */
proc lifereg data=pred outest=a2;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=weibull;
	output out=b2 xbeta=lp;
run;
/* get the subset of my "new" data */
data b2;
set b2;
if week ne . then delete;
run;
/* cbind with estimated S(t) at actual time of rearrest */
data b2;
merge b2 survprob;
run;

/* get new predicted time using weibull quantiles */
data b2;
set b2;
/* the first argument is the name of your distribution: weibull */
/* the second is the probabilities you want: the original S(t) */
/* the third is the SHAPE parameter (as I called it in the notes) --
this is "weibull shape" from your output; I don't know how to output this 
so you'll just have to type it in manually */
/* the last argument is the SCALE (as I called it in the notes) -- this is
just e^{lp} */
newtime = squantile('weibull', survprob_old, 1.4037, exp(lp));
run;
/* print to check this is right */
/* first few obs should have a new time of ~26, 22, 32, 30, ... */
proc print data=b2;
var survprob_old newtime;
run;

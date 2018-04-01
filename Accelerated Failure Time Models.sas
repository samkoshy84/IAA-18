/*-----------------------------------*/
/*    MSA 2018: Survival Analysis    */
/*  Accelerated Failure Time Models  */
/*                                   */
/*          Matthew Austin           */
/*-----------------------------------*/

/* fit AFT models with proc lifereg */
/* in the model statement, the response is
like we saw in proc lifetest: time*status(), where
the # inside parenthesis is the value indicating a
CENSORED observation. otherwise, everything else is the
same as any other model statement you've seen */
proc lifereg data=survival.recid;
	model week*arrest(0) = fin age race wexp mar paro prio;
run;

/* dist statement specifies the distribution of T. default is weibull */
/* probplot asks for a qqplot to see how this fits the data */
proc lifereg data=survival.recid;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=weibull;
	probplot;
run;

/* let's try some others */
/* exponential is a special case of the weibull where scale = 1 */
/* "lagrange multiplier" table tests whether or not it's 1
(null hypothesis is scale = 1; i.e., exponential is ok */
proc lifereg data=survival.recid;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=exponential;
	probplot;
run;

/* gamma includes the weibull: weibull is a special
case where scale = 1/shape
(and obviously that means that exponential is also a special
case of gamma) */
proc lifereg data=survival.recid;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=gamma;
	probplot;
run;

/* log-normal */
proc lifereg data=survival.recid;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=lnormal;
	probplot;
run;
/* if there were no censoring, you'd get the same
estimates as linear regression using log(week) */
/* I'm like 91% sure I didn't just make that up
but let's check anyway */
data nocensor;
set survival.recid;
/* remove censored observations and create log(week) */
if arrest = 0 then delete;
logweek = log(week);
run;
proc lifereg data=nocensor;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=lnormal;
	probplot;
run;
/* so that's not a great fit... */
proc reg data=nocensor;
	model logweek = fin age race wexp mar paro prio;
run;
/* ...and these diagnostic plots are awful,
but yes, they are indeed the same. */

/* one more: log-logistic */
proc lifereg data=survival.recid;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=llogistic;
	probplot;
run;

/* load these two macros */
%include 'C:/Users/IAA-Student/Documents/survival/code mine/lifehaz.sas';
%include 'C:/Users/IAA-Student/Documents/survival/code mine/predict.sas';
/* "predict" gives predicted survival probabilities at a time t that you specify */
/* arguments: - OUTEST= is your dataset from the outest= option in lifereg
			  - OUT= is the name of your dataset from the output statement in lifereg
			  - XBETA= is the name of your xbeta variable in your OUT= dataset
			  - TIME= is the survival time you want */
/* "lifehaz" will plot the predicted hazard. I probably won't use it today,
but you can check out the syntax and play around with it if you want. */ 

/* get predicted median survival times for each individual */
proc lifereg data=survival.recid outest=a noprint;
	/* outest saves coefficient estimates */
	model week*arrest(0) = fin age race wexp mar paro prio / dist=weibull;
	/* quantile = 0.5 is the median. that's the default, but you can
	get 0.25 or 0.75 or whatever you want. or all of them.
	"p" is naming the quantile I asked for -- the median.
	"std" is the SEs of the quantiles you ask for
	"xbeta" is b0 + b1x1 + b2x2 + ... 
	"cdf" is 1 - S(t), so we'll can do that in a data step
	and see "when" (in terms of survival prob) the event happened
	for each observation */
	/* keep in mind that quantiles here are predicted EVENT times,
	so the 75th quantile means "the time by which 75% of the observations
	have had the event," so S(t) = 0.25 */
	output out=b p=med quantile=0.5 std=se xbeta=lp cdf=st;
run;
data b;
set b;
st = 1 - st;
run;
/* do/should you trust these numbers? why or why not? */
proc print data=b;
var st _prob_ med se;
run;
/* let's get the predicted survival probabilities at 10 weeks */
%predict(outest=a, out=b, xbeta=lp, time=10)
/* %lifehaz(outest=a, out=b, xbeta=lp) */

/* now let's predict what would happen if financial aid was
given to all those rearrested who didn't receive it */
/* create a new dataset with those people and set week to missing
so lifereg doesn't use it in the model fitting */
data pred;
set survival.recid;
if arrest=0 then delete;
if fin=1 then delete;
fin = 1;
week = .;
run;
/* rbind */
proc append base=pred data=survival.recid;
run;

/* then we can do all the same stuff we did above */
/* you can compare the differences between the two or whatever you want */
proc lifereg data=pred outest=a2;
	model week*arrest(0) = fin age race wexp mar paro prio / dist=weibull;
	output out=b2 p=med quantile=0.5 std=se xbeta=lp;
run;
data b2;
set b2;
if week ne . then delete;
run;
proc print data=b2;
var _prob_ med se;
run;
/* new predicted probabilities at time 10 */
%predict(outest=a2, out=b2, xbeta=lp, time=10)

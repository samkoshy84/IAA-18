/* complete separation */
data compsep;
	input x y;
datalines;
1 0
2 0
3 0
4 1
5 1
6 1
;
/* first without any correction */
proc logistic data=compsep;
	model y(event='1') = x;
	title 'complete separation';
run;
proc logistic data=compsep;
	/* firth does penalized likelihood */
	model y(event='1') = x / firth;
	title 'complete separation';
run;

/* quasi-complete separation */
data quasisep;
	input x y;
datalines;
1 0
2 0
3 0
4 0
4 1
5 1
6 1
7 1
;
/* first without correction */
proc logistic data=quasisep;
	model y(event='1') = x;
	title 'quasi-complete separation';
run;
proc logistic data=quasisep;
	model y(event='1') = x / firth;
	title 'quasi-complete separation';
run;

proc format;
value racefmt 1 = 'w'
	2 = 'AA'
	3 = 'o';
run;
/* estimate statement */
proc logistic data=logistic.birthweight;
	class race(ref='o') / param=ref;
	model low(event='1') = ptlhist smoke race / clparm=pl clodds=pl;
	estimate 'AA vs o' race 1 0 / e exp cl;
	estimate 'w vs o' race 0 1 / e exp cl;
	estimate 'AA vs w' race 1 -1 / e exp cl;
	/* "exp" option will give odds ratio.
	note that the conf limits for the "exp" option
	isn't the profile likelihood, so use it with caution!
	for the actual coefficients (betas), the default/Wald
	CI is fine */
	title 'estimate statement';
	/* make sure to run the proc format statement above first */
	format race racefmt.;
run;

/* estimate & oddsratio statement */
proc logistic data=logistic.birthweight;
	class race(ref='o') / param=ref;
	model low(event='1') = ptlhist smoke race / clparm=pl clodds=pl;
	/* the three "exp" values from the estimate statement are
	equal to the results from the oddsratio statement.
	"joint" will do an overall test for the entire estimate statement
	that will be equal to the type III analysis of effects for race */
	estimate 'AA vs o' race 1 0,
			'w vs o' race 0 1,
			'AA vs w' race 1 -1 / e exp cl joint;
	oddsratio race / diff=all cl=pl;
	title 'estimate and oddsratio statement';
	format race racefmt.;
run;

/* contrast statement - tests if it equals 0.
not running this, but it will give the same result
as the 'AA vs w' estimate statement above */
proc logistic data=logistic.birthweight;
	class race(ref='o') / param=ref;
	model low(event='1') = ptlhist smoke race / clparm=pl clodds=pl;
	contrast 'AA vs w' race 1 -1 / estimate=all;
	/* estimate= tells SAS what you actually want to estimate:
	- difference in parameters
	- difference in odds ratio
	- difference in predicted probability */
	title 'contrast statement';
	format race racefmt.;
run; 

/* test statement - tests if they're equal to each other.
not running this, but same as contrast & estimate statement above.
you can test whatever numbers you like, e.g., raceAA = 3*racew
but you can only test it, not get conf limits or odds ratio or
anything like that */
proc logistic data=logistic.birthweight;
	class race(ref='o') / param=ref;
	model low(event='1') = ptlhist smoke race / clparm=pl clodds=pl;
	test raceAA = racew;
	title 'test statement';
	format race racefmt.;
run;

/* other comparisons */
proc logistic data=logistic.birthweight;
	class race(ref='o') / param=ref;
	model low(event='1') = ptlhist smoke race / clparm=pl clodds=pl;
	estimate 'ptl+AA vs smoke+w' intercept 1 ptlhist 1 race 1 0,
								intercept 1 smoke 1 race 0 1,
								ptlhist 1 smoke -1 race 1 -1 / e exp ilink cl;
	/* "exp" option will give odds ratio */
	/* "ilink" puts it back on probability scale.
	note that the difference in predicted probability
	is row1 "mean" - row2 "mean"
	row3 "mean" is not the same! */
	title 'estimate statement';
	format race racefmt.;
run;
/* contrast 'ptl+AA vs smoke+w' ptlhist 1 smoke -1 race 1 -1 / estimate=all;
	test ptlhist + raceAA = smoke + racew
would be equal to the third "estimate" above */

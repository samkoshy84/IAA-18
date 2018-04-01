/* Categorical Data Analysis */
/*----------------------------------------------------------------------------------*/
*ods html close;
*ods rtf file="Categorical Data Analysis SAS Report.rtf";
*ods rtf;
*options nodate nonumber ls=95 ps=80;

/*******************************/
/* Describing Categorical Data */
/*******************************/

/* Setting Format for Target Variable */
proc format;
    value bonusfmt 1 = "Bonus Eligible"
                   0 = "Not Bonus Eligible"
                  ;
run;

/* Examining Distributions - Categorical Predictors */
proc freq data=bootcamp.ameshousing3;
    tables Bonus Fireplaces Lot_Shape_2
           Fireplaces*Bonus Lot_Shape_2*Bonus/
           plots(only)=freqplot(scale=percent);
    format Bonus bonusfmt.;
run;

/* Examining Distributions - Continuous Predictors */
proc univariate data=bootcamp.ameshousing3 noprint;
    class Bonus;
    var Basement_Area ;
    histogram Basement_Area;
    inset mean std median min max / format=5.2 position=nw;
    format Bonus bonusfmt.;
run;


/************************/
/* Tests of Association */
/************************/

/* Chi-Square Test */
proc freq data=bootcamp.ameshousing3;
    tables (Lot_Shape_2 Fireplaces)*Bonus
          / chisq expected cellchi2 nocol nopercent 
            relrisk;
    format Bonus bonusfmt.;
    title 'Associations with Bonus';
run;

/* Detecting Ordinal Associations */
proc freq data=bootcamp.ameshousing3;
    tables Fireplaces*Bonus / chisq measures cl;
    format Bonus bonusfmt.;
    title 'Ordinal Association between FIREPLACES and BONUS?';
run;


/***************************************/
/* Introduction to Logistic Regression */
/***************************************/

/* Simple Logistic Regression Model */
proc logistic data=bootcamp.ameshousing3 alpha=0.05
              plots(only)=(effect oddsratio);
    model Bonus(event='1')=Basement_Area / clodds=pl;
    title 'LOGISTIC MODEL (1):Bonus=Basement_Area';
run;

/* Multiple Logistic Regression Model */
proc logistic data=bootcamp.ameshousing3 plots(only)=(effect oddsratio);
    class Fireplaces(ref='0') Lot_Shape_2(ref='Regular') / param=ref;
    model Bonus(event='1')=Basement_Area Fireplaces Lot_Shape_2 / clodds=pl;
    units Basement_Area=100;
    title 'LOGISTIC MODEL (2):Bonus= Basement_Area Fireplaces Lot_Shape_2';
run;

/* Logistic Regression - Backward Elimination */
proc logistic data=bootcamp.ameshousing3 plots(only)=(effect oddsratio);
    class Fireplaces(ref='0') Lot_Shape_2(ref='Regular') / param=ref;
    model Bonus(event='1')=Basement_Area|Fireplaces|Lot_Shape_2 @2 / 
          selection=backward clodds=pl slstay=0.10;
    units Basement_Area=100;
    title 'LOGISTIC MODEL (3): Backward Elimination '
           'Bonus=Basement_Area|Fireplaces|Lot_Shape_2';
run;

/* Logistic Regression - Predictions Using Score Statement */
data newhouses;
	length Lot_Shape_2 $9;
	input Fireplaces Lot_Shape_2 $ Basement_Area;
	datalines;
	0  Regular    1060
	2  Regular     775
	2  Irregular  1100
	1  Irregular   975
	1  Regular     800
	;
run;

proc logistic data=bootcamp.ameshousing3;
    class Fireplaces(ref='0') Lot_Shape_2(ref='Regular') / param=ref;
    model Bonus(event='1')=Basement_Area|Lot_Shape_2 Fireplaces;
    units Basement_Area=100;
	score data=newhouses out=scored_houses;
run;

/* Logistic Regression - Predictions Using PROC PLM */
proc logistic data=bootcamp.ameshousing3;
    class Fireplaces(ref='0') Lot_Shape_2(ref='Regular') / param=ref;
    model Bonus(event='1')=Basement_Area|Lot_Shape_2 Fireplaces;
    units Basement_Area=100;
	store out=isbonus;
run;

data newhouses;
	length Lot_Shape_2 $9;
	input Fireplaces Lot_Shape_2 $ Basement_Area;
	datalines;
	0  Regular    1060
	2  Regular     775
	2  Irregular  1100
	1  Irregular   975
	1  Regular     800
	;
run;

proc plm restore=isbonus;
	score data=newhouses out=scored_houses2 / ILINK;
	title 'Predictions using PROC PLM';
run;

proc print data=scored_houses2;
run;

*ods rtf close;
*ods html;

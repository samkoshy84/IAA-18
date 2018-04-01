/*-----------------------------*/
/*     Autoregressive Models   */
/*                             */
/*        Dr  Simmons          */
/*-----------------------------*/

/* Building an Autoregressive Model */

proc arima data=Time.AR2 plot=all;
	identify var=y nlag=40;
	estimate p=2 method=ML;
run;
quit;

proc arima data=Time.USAirlines plot=all;
	identify var=Passengers nlag=40;
	estimate p=1 method=ML maxiter=100;
	estimate p=6 method=ML maxiter=100;
	estimate p=(6) method=ML maxiter=100;
	estimate p=(1,2,3,6) method=ML maxiter=100;
run;
quit;


/*-----------------------------*/
/*     Moving Average Models   */
/*                             */
/*        Dr Simmons           */
/*-----------------------------*/

/* Building a Moving Average Model */

proc arima data=Time.SimMA1 plot=all;
	identify var=Y nlag=12;
	estimate q=1 method=ML;
run;
quit;


/* OOPS...guessed wrong model....*/
proc arima data=Time.AR2 plot=all;
	identify var=Y nlag=12;
	estimate q=2 method=ML;
run;
quit;


/*-----------------------------*/
/*      ARIMA Forecasting &    */
/*        Identification       */
/*                             */
/*        Dr Aric LaBarr       */
/*-----------------------------*/


/* Building an Autoregressive Moving Average Model */
proc arima data=Time.SimARMA plot=all;
	identify var=Y nlag=12;
	estimate p=1 q=1 method=ML;
run;
quit;

proc arima data=Time.USAirlines plot=all;
	identify var=Passengers(12) nlag=40 stationarity=(adf=5);
	estimate p=(1,2,12) method=ML;
	estimate p=(1,2,12) q=(12) method=ML;
run;
quit;


/* Automatic Model Identification */
proc arima data=Time.USAirlines plot=all;
	identify var=Passengers(12) nlag=40 stationarity=(adf=5);
	identify var=Passengers(12) nlag=40 minic scan esacf P=(0:12) Q=(0:12);
	*estimate p=1 q=12 method=ML;
run;
quit;


/* Forecasting */
proc arima data=Time.USAirlines plot=all;
	identify var=Passengers(12) nlag=40 stationarity=(adf=5);
	estimate p=(1,2,12) q=(12) method=ML;
	forecast lead=24;
run;
quit;


/*Plot synthetic AR(1,1)--Use SAS/IML to generate some ARIMA series and plot them*/

/*-------------------------------ARMA(1,1) model----------------------------------------*/
proc iml;
/*parameter values*/
phi = {1 0.9};
theta = {1 0.5};
/*sample size*/
N = 1000;
/*generate series using the function ARMASIM (phi, theta, mu, sigma, n <, seed> ); */
y1 = armasim(phi, theta, 0, 1, N, 0);

phi = {1 -0.9};
theta = {1 0.5};
y2 = armasim(phi, theta, 0, 1, N, 0);

phi = {1 -0.9};
theta = {1 -0.5};
y3 = armasim(phi, theta, 0, 1, N, 0);

phi = {1 0.9};
theta = {1 -0.5};
y4 = armasim(phi, theta, 0, 1, N, 0);

/*create a time variable*/
time = (1:N)`;


AllId = j(N,1,1) // j(N,1,2) // j(N,1,3) // j(N,1,4); 
AllY = y1 // y2 // y3 // y4;
AllTime = time // time // time // time;

/*stack the time variable and the time series in a Nx2 vector*/
All = AllTime || AllY || AllId;

/*make it a sas data set*/
create work.mydata from All[colname = {'time', 'Y','id'}];
append from All;
close;
quit;

PROC SGPANEL DATA=work.mydata;
PANELBY id;
Series X = time Y = y;
keylegend "";
RUN;
/*-------------------------------------------------------------------------------*

/*Model identification--Use SAS/IML to generate some ARIMA series and check ACFs and PACFs*/

/*-------------------------------AR(3) model----------------------------------------*/
proc iml;
/*parameter values*/
phi = {1 0.4 0.3 -0.3};
theta = {1 };

/*sample size*/
N = 1000;
/*generate series using the function ARMASIM (phi, theta, mu, sigma, n <, seed> ); */
y = armasim(phi, theta, 0, 1, N, 0);

/*create a variable*/
time = 1900+(1:N)`;

/*stack the time variable and the time series in a Nx2 vector*/
All = time || y;

/*make it a sas data set*/
create work.mydata from All[colname = {'time', 'Y'}];
append from All;
close;
quit;


/*ACF Tails Exp, PACF cuts at lag 3*/
proc arima data=work.mydata plot=all;
	identify var=Y nlag=15;
/*	identify var=Y nlag=15 minic p=(0:3) q = (0:3);*/
/*	identify var=Y nlag=15 scan p=(0:3) q = (0:3);*/
/*	estimate p=3 method=ML noint;*/
run;
quit;

/*-----------------------------------------------------------------------------------*


/*-------------------------------MA(3) model----------------------------------------*/
proc iml;
/*parameter values*/
theta = {1 0.4 0.3 -0.3};
phi = {1 };

/*sample size*/
N = 1000;
/*generate series using the function ARMASIM (phi, theta, mu, sigma, n <, seed> ); */
y = armasim(phi, theta, 0, 1, N, 0);

/*create a variable*/
time = 1900+(1:N)`;

/*stack the time variable and the time series in a Nx2 vector*/
All = time || y;

/*make it a sas data set*/
create work.mydata from All[colname = {'time', 'Y'}];
append from All;
close;
quit;


/*ACF cutsoff at lag 3, PACF tails exponentially*/
proc arima data=work.mydata plot=all;
	identify var=Y nlag=15;
/*	estimate q=3 method=ML noint;*/
run;
quit;

/*-------------------------------------------------------------------------------*



/*-------------------------------ARMA(3,3) model----------------------------------------*/
proc iml;
/*parameter values*/
phi = {1 -0.4 0.2 0.2};
theta = {1 0.1 0 -0.4};

/*sample size*/
N = 3000;
/*generate series using the function ARMASIM (phi, theta, mu, sigma, n <, seed> ); */
y = armasim(phi, theta, 0, 1, N, 0);

/*create a variable*/
time = 1900+(1:N)`;

/*stack the time variable and the time series in a Nx2 vector*/
All = time || y;

/*make it a sas data set*/
create work.mydata from All[colname = {'time', 'Y'}];
append from All;
close;
quit;


proc arima data=work.mydata plot=all;
	identify var=Y nlag=15;
/*	estimate p=3 q=3 method=ML noint;*/
run;
quit;

/*-------------------------------------------------------------------------------*


/*Model quarterly retail trade index in the Euro area (17 countries),
1996?2011, covering wholesale and retail trade, and repair of 
motor vehicles and motorcycles. (Index: 2005 = 100)*/
/*Source: Eurostat*/

/*See also https://www.otexts.org/fpp/8/9*/
/*In R the data can be found in the fpp package*/

/*------------------IMPORT DATA--------------------------------*/
proc import datafile =  'C:\Users\Instructor\Desktop\euretail.csv'
out = euretail
dbms = csv;
datarow = 2;
run;
/*-------------------------------------------------------------*/


/*------------------CLEANUP DATA--------------------------------*/
data retail2;
set euretail;
date = intnx( 'qtr', '01JAN1996'd, _n_-1 );
format date date9.;
rename Retail_trade___quarterly_data__ = trade;
run;

 
data retail;
set retail2;
if trade = . then delete;
keep date trade;
run;
/*-------------------------------------------------------------*/

/*------------------Print and plot-----------------------------*/
proc print data = retail(obs = 5);
run;

proc sgplot data = retail;
	series x = date y = trade;
run;
/*-------------------------------------------------------------*/

/*----------------------Modeling-------------------------------*/
/*ARIMA(p,d,q)(P,D,Q)_s*/
proc arima data=retail plot=all;
	/*check seasonality*/
	*identify var=trade nlag=12 stationarity=(adf=2 dlag = 4);

    /*take seasonal difference--model: ARIMA(0,0,0)(0,1,0)_4*/
	*identify var=trade(4) nlag=12 stationarity=(adf=2 );
	
	/*plots and tests suggest normal differencing
	model: ARIMA(0,1,0)(0,1,0)_4*/
	identify var=trade(4,1) nlag=12 stationarity=(adf=2 );

	/*significant lags at spike 1 in ACF suggests nonseasonal MA(1)*/
	/*exponential decay in IACF every 4 lags in ACF seasonal MA(1)*/
	/*Lets try estimating the model: ARIMA(0,1,1)(0,1,1)_4*/
	*estimate q = (1)(4) /*multiplicative*/;

	/*potential significant spikes at lags 2 and 3 in ACF and PACF*/
	/*check AICs and BICs for several models, for example:
	ARIMA(0,1,2)(0,1,1)_4, ARIMA(0,1,3)(0,1,1)_4*/
	estimate q = (1,2,3)(4) /*multiplicative*/;
	forecast lead = 12;

run;
quit;
/*-------------------------------------------------------------*/


/*-----------------------------*/
/* Intervention Models, ARIMAX */
/*   & Transfer Functions      */
/*-----------------------------*/

/* Building an Intervention Model */

Data Time.DEER;
	input date date7. deer nondeer;
	format date monyy.;
	label deer = "NC accidents involving deer";
	label nondeer = "other reported accidents";
cards;
01JAN03     1218   18030
01FEB03      937   16132
01MAR03     1013   16098
01APR03      805   17419
01MAY03      762   19054
01JUN03      833   17308
01JUL03      662   17864
01AUG03      583   18858
01SEP03      744   17595
01OCT03     2154   19531
01NOV03     3619   17530
01DEC03     2098   18590
01JAN04     1322   18507
01FEB04     1118   17501
01MAR04      960   16669
01APR04      818   17063
01MAY04      707   17847
01JUN04      722   17310
01JUL04      683   17314
01AUG04      604   18443
01SEP04      708   18276
01OCT04     2154   18194
01NOV04     3566   18625
01DEC04     2147   19468
01JAN05     1377   17388
01FEB05     1133   15170
01MAR05     1118   17075
01APR05      849   17312
01MAY05      771   17716
01JUN05      864   17534
01JUL05      674   17380
01AUG05      595   17449
01SEP05      870   15433
01OCT05     2285   18213
01NOV05     3311   17443
01DEC05     2093   18063
01JAN06     1575   15499
01FEB06     1094   14120
01MAR06      966   15649
01APR06      789   16597
01MAY06      780   17033
01JUN06      920   17895
01JUL06      776   15777
01AUG06      654   17355
01SEP06      814   17306
01OCT06     2566   19396
01NOV06     4095   18543
01DEC06     2575   17533
01JAN07     1613   16470
01FEB07     1345   15442
01MAR07     1376   17760  
01APR07      839   16777
01MAY07      880   17187
01JUN07     1030   17035
01JUL07      943   16007
01AUG07      867   16884
01SEP07     1112   16838
01OCT07     2444   19596
01NOV07     4207   17230
01DEC07     2621   17804
;


proc arima data=Time.DEER plot(unpack)=series(corr);
	identify var=deer nlag=24;
run;


data Time.DEER;
	set Time.DEER;
	Month = MONTH(Date);
	if month=11 then Nov = 1; else Nov=0;
run;


data Time.DEER2;
	set Time.DEER;
	input Date Deer Nodeer Month;
	informat Date MONYY5.;
	format Date MONYY5.;
cards;
JAN08 . . 1
FEB08 . . 2
MAR08 . . 3
APR08 . . 4
MAY08 . . 5
JUN08 . . 6
JUL08 . . 7
AUG08 . . 8
SEP08 . . 9
OCT08 . . 10
NOV08 . . 11
DEC08 . . 12
JAN09 . . 1
FEB09 . . 2
MAR09 . . 3
APR09 . . 4
MAY09 . . 5
JUN09 . . 6
JUL09 . . 7
AUG09 . . 8
SEP09 . . 9
OCT09 . . 10
NOV09 . . 11
DEC09 . . 12
;


data Time.DEER2;
	set Time.DEER Time.DEER2;
run;


/* Point (Pulse) Intervention - Deterministic */

proc arima data=Time.DEER2 plot(unpack)=(series(corr) forecast(all));
	identify var=deer nlag=24 crosscorr=(Nov);
	estimate input=(Nov);
	forecast lead=24 id=date interval=month;
run;
quit;


/* Point (Pulse) Intervention - Stochastic */

proc arima data=Time.DEER2 plot(unpack)=(series(corr) forecast(all));
	identify var=deer nlag=24 crosscorr=(Nov);
	estimate input=(/(1)Nov);
	forecast lead=24 id=date interval=month;
run;
quit;


/* Point (Pulse) Intervention - Stochastic + ARIMA Model */

proc arima data=Time.DEER2 plot(unpack)=(series(corr) forecast(all));
	identify var=deer nlag=24 crosscorr=(Nov);
	estimate input=( /(1)Nov) p=(12);
	forecast lead=24 id=date interval=month;
run;
quit;


/* Point (Pulse) Intervention - Stochastic + ARIMA Model + Drift */

proc arima data=Time.DEER2 plot(unpack)=(series(corr) forecast(all));
	identify var=deer nlag=24 crosscorr=(Date Nov);
	estimate input=( /(1)Nov Date) p=(12);
	forecast lead=24 id=date interval=month;
run;
quit;


/* Building a Simple Transfer Function Model */

proc arima data=Time.HOUSING plot(unpack)=series(corr);
	identify var=starts stationarity=(ADF=2);
	identify var=sales stationarity=(ADF=2);
	identify var=starts(1) stationarity=(ADF=2) esacf P=(0:10) Q=(0:10);
	identify var=sales(1) stationarity=(ADF=2) esacf P=(0:10) Q=(0:10);
run;
quit;


proc arima data=Time.HOUSING plot(unpack)=(series(corr) forecast(all));
	identify var=starts(1);
	estimate p=(4) q=(3) method=ml noconstant;
	forecast lead=8;
	identify var=sales(1) crosscorr=(starts(1));
	estimate q=1 input=(starts) method=ml;
	forecast lead=8 id=date interval=qtr out=for2;
run;
quit;



/* Building a General Transfer Function Model */

proc arima data=Time.RIVER plot(unpack)=(series(corr) forecast(all));
	identify var=LGold stationarity=(ADF=2);
	identify var=LKins stationarity=(ADF=2);
run;
quit;


proc arima data=Time.RIVER plot(unpack)=(series(corr) forecast(all));
	identify var=LGold(1) stationarity=(ADF=2) scan esacf minic P=(0:10) Q=(0:10);
	estimate p=2 q=2 method=ml noconstant;
	*identify var=LKins(1) nlag=10 crosscorr=(LGold(1));
	*estimate input=(1$(1)LGold) method=ml;
	*estimate p=(1,2,20) q=1 input=(1$(1)LGold) method=ml noconstant;
	*forecast lead=50;
run;
quit;


proc iml;

/*select ARIMA parameters*/
phi = {1 0.5};
theta = {1 -0.3};
N = 1000;

/*y = armasim(phi,theta,mean,variance,sample size,random = 0)*/;
y = armasim(phi,theta,2,1,N,0);

/*add a spike at N/2*/
dummy = j(N,1,0);
dummy[N/2] = 1;

/*step variable*/
*dummy[N/2:N] = 1;


/*add lags of pulse intervention*/
nlags = 10;
do i = 0 to nlags-1;
	y = y + (12-i)*lag(dummy,i);
end;

/*create a sas dataset with the generated series*/
time = (1:N)`;
All = time || y || dummy;
create mydata1 from All[colname= {'time','y', 'dummy'}];
append from All;
close;
quit;


/*fit an arima model to the data*/
ods trace on;
ods output ParameterEstimates = p1;
proc arima data = mydata1;
	*identify var = y ; /*intervention with either 10 many short term effects or a
					 long term effect. This means numerator effect with 10 lags or 
					 denominator term with 1 lag*/
	identify var = y crosscorr = dummy;
	*estimate p=1 q=1 input = ((1,2,3,4,5,6,7,8,9,10) dummy) method = ml maxit = 5000; /*numerator effects*/
	estimate p=1 q=1 input = (/(1) dummy) method = ml; /*denominator effects*/
run;
quit;

/*---------------------------*/

proc iml;

/*select ARIMA parameters*/
phi = {1 0.5};
theta = {1 -0.3};
N = 1000;

/*y = armasim(phi,theta,mean,variance,sample size,random = 0)*/;
y = armasim(phi,theta,2,1,N,0);

/*add a spike at N/2*/
dummy = j(N,1,0);
dummy[N/2] = 1;

/*step variable*/
*dummy[N/2:N] = 1;


/*add lags of pulse intervention*/
nlags = 10;
do i = 0 to nlags-1;
	y = y + (12-i)*lag(dummy,i);
end;

/*create a sas dataset with the generated series*/
time = (1:N)`;
All = time || y || dummy;
create mydata2 from All[colname= {'time','y', 'dummy'}];
append from All;
close;
quit;


/*fit an arima model to the data*/
ods trace on;
ods output ParameterEstimates = p2;
proc arima data = mydata2;
	*identify var = y ; /*intervention with either 10 many short term effects or a
					 long term effect. This means numerator effect with 10 lags or 
					 denominator term with 1 lag*/
	identify var = y crosscorr = dummy;
	*estimate p=1 q=1 input = ((1,2,3,4,5,6,7,8,9,10) dummy) method = ml maxit = 5000; /*numerator effects*/
	estimate p=1 q=1 input = (/(1) dummy) method = ml; /*denominator effects*/
run;
quit;
/*---------------------------*/


proc iml;

/*select ARIMA parameters*/
phi = {1 0.5};
theta = {1 -0.3};
N = 1000;

/*y = armasim(phi,theta,mean,variance,sample size,random = 0)*/;
y = armasim(phi,theta,2,1,N,0);

/*add a spike at N/2*/
dummy = j(N,1,0);
dummy[N/2] = 1;

/*step variable*/
*dummy[N/2:N] = 1;


/*add lags of pulse intervention*/
nlags = 10;
do i = 0 to nlags-1;
	y = y + (12-i)*lag(dummy,i);
end;

/*create a sas dataset with the generated series*/
time = (1:N)`;
All = time || y || dummy;
create mydata3 from All[colname= {'time','y', 'dummy'}];
append from All;
close;
quit;


/*fit an arima model to the data*/
ods trace on;
ods output ParameterEstimates = p3;
proc arima data = mydata3;
	*identify var = y ; /*intervention with either 10 many short term effects or a
					 long term effect. This means numerator effect with 10 lags or 
					 denominator term with 1 lag*/
	identify var = y crosscorr = dummy;
	*estimate p=1 q=1 input = ((1,2,3,4,5,6,7,8,9,10) dummy) method = ml maxit = 5000; /*numerator effects*/
	estimate p=1 q=1 input = (/(1) dummy) method = ml; /*denominator effects*/
run;
/*---------------------------*/

proc iml;
use p1;
read all var{'Estimate'} into Est1;
close;

use p2;
read all var{'Estimate'} into Est2;
close;

use p3;
read all var{'Estimate'} into Est3;
close;

Est = Est1 || Est2 || Est3;

Param = {'p1', 'p2', 'p3','p4', 'p5'};
cn = {'1', '2','3'};
 

create Estimates from Est[colname = cn rowname = Param];
append from Est[rowname = Param];
close;

proc export
date = Estimates;
dbms = xlsx
outfile = "mydir"
replace;
quit;

proc print data = Estimates;
run;



DATA RIVER;
   TITLE 'FLOW RATES OF NEUSE RIVER AT GOLDSBORO AND KINSTON';
   RETAIN DATE '30SEP70'D;
   INPUT GOLD KINS @@;
   LGOLD=LOG(GOLD);
   LKINS=LOG(KINS);
   DATE=INTNX('DAY',DATE,1);
   FORMAT DATE DATE7.;
CARDS;
  525   548   616   615   361   716   298   555   298   433   160   415
  153   342   182   271   212   280   262   295   160   349   184   289
  143   286   190   265   156   280   146   280   138   286   187   271
  361   262   271   373   316   433   382   471   416   583   539   653
  556   643   429   709   475   646   464   594   367   622   271   562
  528   534   493   759  1030   875  1460  1050  1330  1480  1100  1540
  895  1390   753  1190   620  1020   592   864   606   777   620   852
  810  1000  1520  1140  1810  1520  1880  1880  2060  1990  2250  2100
 1770  2250  1810  2090  1350  1950  1110  1720  1010  1410   912  1260
  808  1160   765  1060   736   997   701   952   696   919   689   887
  605   871   591   811   702   751   619   826   563   821   613   738
  612   752   593   768   573   751   562   738   539   723   509   704
  523   676   574   655   512   687   488   682   640   652   962   878
 1150  1240  1710  1360  1770  1670  1600  1880  1390  1830  1400  1690
 1540  1710  1790  1800  2050  1910  2120  2100  1890  2230  1700  2160
 1360  1940  1210  1700  1130  1550  1110  1450  1190  1380  1350  1410
 1490  1520  1730  1640  2770  1830  3500  2620  4000  3080  4400  3610
 4900  4110  5420  4560  5480  5000  5130  5380  4560  5820  3870  5990
 3230  5800  2750  5230  2470  4490  2200  3790  1980  3210  1800  2760
 1690  2460  1900  2310  2540  2580  3130  3040  3560  3420  3670  3780
 3370  4040  2880  4150  2750  4220  2820  4140  3130  4050  3320  3970
 3250  4050  3300  4160  3740  4270  4660  4430  5560  4740  6480  5310
 7200  5950  7870  6480  8630  7000  9510  7830 10300  8670 10700  9350
10600 10100 10300 10700  9770 11000  8970 11100  7570 10800  5990 10200
 4290  9380  4630  8450  5540  7460  6440  6860  7040  6670  7070  6860
 6070  7200  4750  7460  4230  7390  5560  7370  7790  8220  8410  9170
 9400  9300 10400  9510 11400 10000 12200 10700 12300 11500 11800 12400
10700 12900  8570 13000  6260 12500  4080 11300  3500  9070  3410  7010
 3530  5530  3560  4930  3280  5620  2810  4280  2520  3940  2350  3480
 2230  3090  2040  2850  2080  2710  2470  2760  2940  2980  3300  3240
 3900  3560  4510  3920  4930  4320  5190  4770  5360  5200  5080  5560
 3930  5770  4450  5980  5280  5950  6020  6110  6580  6390  6880  6670
 6930  6940  6850  7130  6640  7250  5700  7260  3930  7140  2700  6790
 2350  5530  2060  4160  1840  3160  1690  2660  1560  2260  1600  2060
 1630  1990  1620  2020  1670  2010  1620  1990  1530  1970  1700  1980
 2030  2220  2670  2390  3070  2660  3040  3000  2630  3230  2130  3190
 1850  2830  1600  2350  1360  2000  1240  1730  1180  1540  1100  1440
 1110  1350  0999  1310  1120  1300  1220  1460  2160  1560  2760  2210
 3020  2750  3550  3070  3910  3330  4110  3620  4280  4100  4240  4430
 3620  4450  1900  4370  1370  3520  1180  2110  1030  1520   902  1310
  874  1410  1070  1270  1910  1270  2580  1760  3000  2280  3170  2700
 3350  2940  2880  3110  1490  3180  1020  2320   846  1360   782  1060
  711   946   525   872   630   732   753   764   634   856   634   804
  651   753   623   773   623   763   641   757   683   756   634   779
  578   766   606   844   571   975   588   826   585   758   634   747
  585   762   477   729   407   649   394   580   519   561   842   675
  606   972   560   821   439   722   616   680   749   897   854  1030
  918   999   854  1110  1030  1080  1190  1140   942  1260   627  1160
  532   855   442   708   400   627   314   552   362   506   445   524
  455   588   690   596   752   690   602   825   458   768   388   722
  506   688   602   699  1350   720  1480  1250  1370  1550  1140  1580
  914  1410   814  1170   763   987   588   921   850   784  1090   828
  926  1080   606  1050   529   830   483   670   423   625   350   574
  347   511   445   535   543   718   602   995   609  1090   806   954
  874   968   763  1060   676   997   496   920   648   773   858   779
  958  1030   822  1330   954  1360  1040  1300  1430  1300  1520  1480
 1000  1670   641  1370   557   998   435   807   338   715   296   603
  376   553   474   570   423   654   359   672   376   703   407   939
  439   926  1620   728  2310  1340  2590  2030  2370  2420  1090  2480
  651  1660   451  1060   496   883   464   697   557   648   539   634
  529   658   416   641   376   578   362   504   567   655   790  1840
  906  2360  2400  2680  3470  3080  4040  3580  4420  3990  4490  4350
 4420  4560  4400  4700  4630  4890  4920  5130  5110  5470  5240  5680
 5180  5710  5060  5710  5040  5660  5020  5600  4820  5530  3420  5440
 2070  5240  1650  4250  1530  3410  2330  3740  3470  4440  4600  5130
 5330  5380  6140  5630  6870  5990  7470  6420  7830  6860  7890  7330
 7720  7780  7580  8100  7630  8320  7580  8280 
;
RUN;


/* Building a General Transfer Function Model */

proc arima data=RIVER plot(unpack)=(series(corr) forecast(all));
	identify var=LGold stationarity=(ADF=2);
	identify var=LKins stationarity=(ADF=2);
run;
quit;


proc arima data=RIVER plot(unpack)=(series(corr) forecast(all));
	identify var=LGold(1) stationarity=(ADF=2) scan esacf minic P=(0:10) Q=(0:10);
	estimate p=2 q=2 method=ml noconstant;
	*identify var=LKins(1) nlag=10 crosscorr=(LGold(1));
	*estimate input=(1$(1)LGold) method=ml;
	*estimate p=(1,2,20) q=1 input=(1$(1)LGold) method=ml noconstant;
	*forecast lead=50;
run;
quit;



/*-----------------------------*/
/*    Neural Network Models    */
/*                             */
/*        Dr Simmons           */
/*-----------------------------*/


/* Building a Neural Network Model */
data Time.LagUSAir;
	set Time.USAirlines;
	if Date > '01mar2006'd then delete;
	DiffPass = Passengers - lag12(Passengers);
	Lag1 = lag(DiffPass);
	Lag2 = lag(Lag1);
	Lag3 = lag(Lag2);
	Lag4 = lag(Lag3);
	Lag5 = lag(Lag4);
	Lag6 = lag(Lag5);
	Lag7 = lag(Lag6);
	Lag8 = lag(Lag7);
	Lag9 = lag(Lag8);
	Lag10 = lag(Lag9);
	Lag11 = lag(Lag10);
	Lag12 = lag(Lag11);
run;

proc dmdb data=Time.LagUSAir out=dmUSAir dmdbcat=catUSAir;
	var Lag1-Lag12 DiffPass Date Month;
	target DiffPass;
run;

proc neural data=Time.LagUSAir dmdbcat=catUSAir random=12345;
	hidden 2 / id=hid;
	input Lag1 Lag2 Lag12 / level=interval id=int;
	target DiffPass / level=interval id=tar;
	train outest=Parms;
	score out=Predicted;
run;

data Parms;
	set Parms;
	if _NAME_ ne "_LAST_" then delete;
run;

proc sql noprint;
	select Lag1_hid1 into :L1H1 from WORK.Parms;
	select Lag2_hid1 into :L2H1 from WORK.Parms;
	select Lag12_hid1 into :L12H1 from WORK.Parms;

	select Lag1_hid2 into :L1H2 from WORK.Parms;
	select Lag2_hid2 into :L2H2 from WORK.Parms;
	select Lag12_hid2 into :L12H2 from WORK.Parms;

	select BIAS_hid1 into :BH1 from WORK.Parms;
	select BIAS_hid2 into :BH2 from WORK.Parms;
	select hid1_DiffPass into :H1T from WORK.Parms;
	select hid2_DiffPass into :H2T from WORK.Parms;
	select BIAS_DiffPass into :BT from WORK.Parms;

	select AVG(Lag1) into :Mean_Lag1 from WORK.Predicted;
	select AVG(Lag2) into :Mean_Lag2 from WORK.Predicted;
	select AVG(Lag12) into :Mean_Lag12 from WORK.Predicted;

	select STD(Lag1) into :STD_Lag1 from WORK.Predicted;
	select STD(Lag2) into :STD_Lag2 from WORK.Predicted;
	select STD(Lag12) into :STD_Lag12 from WORK.Predicted;

quit;

data Forecast_Add;
	input Date P_DiffPass Passengers Lead;
	informat Date MONYY7.;
	format Date MONYY7.;
cards;
APR2006 . . 1
MAY2006 . . 2
JUN2006 . . 3
JUL2006 . . 4
AUG2006 . . 5
SEP2006 . . 6
OCT2006 . . 7
NOV2006 . . 8
DEC2006 . . 9
JAN2007 . . 10
FEB2007 . . 11
MAR2007 . . 12
APR2007 . . 13
MAY2007 . . 14
JUN2007 . . 15
JUL2007 . . 16
AUG2007 . . 17
SEP2007 . . 18
OCT2007 . . 19
NOV2007 . . 20
DEC2007 . . 21
JAN2008 . . 22
FEB2008 . . 23
MAR2008 . . 24
;


data Forecast;
	set Predicted (keep=Date P_DiffPass DiffPass Passengers) Forecast_Add;

	Lag1 = lag(DiffPass);
	Lag2 = lag(Lag1);
	Lag3 = lag(Lag2);
	Lag4 = lag(Lag3);
	Lag5 = lag(Lag4);
	Lag6 = lag(Lag5);
	Lag7 = lag(Lag6);
	Lag8 = lag(Lag7);
	Lag9 = lag(Lag8);
	Lag10 = lag(Lag9);
	Lag11 = lag(Lag10);
	Lag12 = lag(Lag11);
run;

proc standard data=Forecast mean=0 std=1 out=S_Forecast;
	var Lag1 Lag2 Lag12;
run;

data Forecast;
	merge Forecast S_Forecast(rename=(Lag1=S_Lag1 Lag2=S_Lag2 Lag12=S_Lag12));

	if Lead = 1 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*S_Lag1 + &L2H1*S_Lag2 + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*S_Lag1 + &L2H2*S_Lag2 + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 2 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*S_Lag2 + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*S_Lag2 + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 3 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 4 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;


data Forecast;
	set Forecast;

	if Lead = 5 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 6 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 7 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 8 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 9 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 10 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 11 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 12 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

%macro Create(n);
	%do i = 13 %to &n;
		data Forecast;
			set Forecast;

			if Lead = &i then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*((LF12 - &Mean_Lag12)/&STD_Lag12))) + 
					  				 		   &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*((LF12 - &Mean_Lag12)/&STD_Lag12)));
			LF1 = lag(Forecast);
			LF2 = lag(LF1);
			LF3 = lag(LF2);
			LF4 = lag(LF3);
			LF5 = lag(LF4);
			LF6 = lag(LF5);
			LF7 = lag(LF6);
			LF8 = lag(LF7);
			LF9 = lag(LF8);
			LF10 = lag(LF9);
			LF11 = lag(LF10);
			LF12 = lag(LF11);
		run;
	%end;
%mend Create;

%Create(24)

data NN_Forecast;
	set Forecast;

	if Forecast = . then Forecast = P_DiffPass;
	keep Forecast Date Passengers NN_Forecast;
	Lag12 = lag12(Passengers);

	NN_Forecast = Lag12 + Forecast;
run;

data NN_Forecast;
	set NN_Forecast;

	keep Date NN_Forecast Passengers;
	Lag12F = lag12(NN_Forecast);
	
	if NN_Forecast = . then NN_Forecast = Lag12F + Forecast;
run;


/*  Combined Models ********************************  */



/* Building an ARIMA Model */
data USAirlines;
	set Time.USAirlines;
	if Year=2001 and Month=9 then Sep11 = 1; else Sep11 = 0;
	if Year=2002 and Month=9 then Anniv = 1; else Anniv = 0;
run; 

data USAirlines;
	set USAirlines;
	if Date > '01mar2006'd then Passengers = .;
run;

proc arima data=USAirlines plot=all;
	identify var=Passengers(12) nlag=40 stationarity=(adf=5) crosscorr=(Sep11 Anniv);
	estimate p=(1,2) q=(12) input=(/(1)Sep11 Anniv) method=ML;
	forecast lead=24 out=F_ARIMA;
run;
quit;

data MAPE_ARIMA;
	merge F_ARIMA Time.USAirlines;
	keep Forecast Passengers Date;
run;

data MAPE_ARIMA;
	set MAPE_ARIMA;
	where Date > '01mar2006'd;
	APE = abs(Passengers - Forecast)/Passengers;
run;

proc means data=MAPE_ARIMA mean;
	var APE;
run;


/* Building an ESM */
proc esm data=USAirlines print=all plot=all lead=0 outfor=F_ESM;
	id date interval=month;
	forecast Passengers / model=winters;
run;

data MAPE_ESM;
	merge F_ESM Time.USAirlines;
	keep Predict Passengers Date;
run;

data MAPE_ESM;
	set MAPE_ESM;
	where Date > '01mar2006'd;
	APE = abs(Passengers - Predict)/Passengers;
run;

proc means data=MAPE_ESM mean;
	var APE;
run;


/* Building a Neural Network Model */
data Time.LagUSAir;
	set Time.USAirlines;
	if Date > '01mar2006'd then delete;
	DiffPass = Passengers - lag(lag(lag(lag(lag(lag(lag(lag(lag(lag(lag(lag(Passengers))))))))))));
	Lag1 = lag(DiffPass);
	Lag2 = lag(Lag1);
	Lag3 = lag(Lag2);
	Lag4 = lag(Lag3);
	Lag5 = lag(Lag4);
	Lag6 = lag(Lag5);
	Lag7 = lag(Lag6);
	Lag8 = lag(Lag7);
	Lag9 = lag(Lag8);
	Lag10 = lag(Lag9);
	Lag11 = lag(Lag10);
	Lag12 = lag(Lag11);
run;

proc dmdb data=Time.LagUSAir out=dmUSAir dmdbcat=catUSAir;
	var Lag1-Lag12 DiffPass Date Month;
	target DiffPass;
run;

proc neural data=Time.LagUSAir dmdbcat=catUSAir random=12345;
	hidden 2 / id=hid;
	input Lag1 Lag2 Lag12 / level=interval id=int;
	target DiffPass / level=interval id=tar;
	train outest=Parms;
	score out=Predicted;
run;

data Parms;
	set Parms;
	if _NAME_ ne "_LAST_" then delete;
run;

proc sql noprint;
	select Lag1_hid1 into :L1H1 from WORK.Parms;
	select Lag2_hid1 into :L2H1 from WORK.Parms;
	select Lag12_hid1 into :L12H1 from WORK.Parms;

	select Lag1_hid2 into :L1H2 from WORK.Parms;
	select Lag2_hid2 into :L2H2 from WORK.Parms;
	select Lag12_hid2 into :L12H2 from WORK.Parms;

	select BIAS_hid1 into :BH1 from WORK.Parms;
	select BIAS_hid2 into :BH2 from WORK.Parms;
	select hid1_DiffPass into :H1T from WORK.Parms;
	select hid2_DiffPass into :H2T from WORK.Parms;
	select BIAS_DiffPass into :BT from WORK.Parms;

	select AVG(Lag1) into :Mean_Lag1 from WORK.Predicted;
	select AVG(Lag2) into :Mean_Lag2 from WORK.Predicted;
	select AVG(Lag12) into :Mean_Lag12 from WORK.Predicted;

	select STD(Lag1) into :STD_Lag1 from WORK.Predicted;
	select STD(Lag2) into :STD_Lag2 from WORK.Predicted;
	select STD(Lag12) into :STD_Lag12 from WORK.Predicted;

quit;

data Forecast_Add;
	input Date P_DiffPass Passengers Lead;
	informat Date MONYY7.;
	format Date MONYY7.;
cards;
APR2006 . . 1
MAY2006 . . 2
JUN2006 . . 3
JUL2006 . . 4
AUG2006 . . 5
SEP2006 . . 6
OCT2006 . . 7
NOV2006 . . 8
DEC2006 . . 9
JAN2007 . . 10
FEB2007 . . 11
MAR2007 . . 12
APR2007 . . 13
MAY2007 . . 14
JUN2007 . . 15
JUL2007 . . 16
AUG2007 . . 17
SEP2007 . . 18
OCT2007 . . 19
NOV2007 . . 20
DEC2007 . . 21
JAN2008 . . 22
FEB2008 . . 23
MAR2008 . . 24
;


data Forecast;
	set Predicted (keep=Date P_DiffPass DiffPass Passengers) Forecast_Add;

	Lag1 = lag(DiffPass);
	Lag2 = lag(Lag1);
	Lag3 = lag(Lag2);
	Lag4 = lag(Lag3);
	Lag5 = lag(Lag4);
	Lag6 = lag(Lag5);
	Lag7 = lag(Lag6);
	Lag8 = lag(Lag7);
	Lag9 = lag(Lag8);
	Lag10 = lag(Lag9);
	Lag11 = lag(Lag10);
	Lag12 = lag(Lag11);
run;

proc standard data=Forecast mean=0 std=1 out=S_Forecast;
	var Lag1 Lag2 Lag12;
run;

data Forecast;
	merge Forecast S_Forecast(rename=(Lag1=S_Lag1 Lag2=S_Lag2 Lag12=S_Lag12));

	if Lead = 1 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*S_Lag1 + &L2H1*S_Lag2 + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*S_Lag1 + &L2H2*S_Lag2 + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 2 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*S_Lag2 + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*S_Lag2 + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 3 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 4 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;


data Forecast;
	set Forecast;

	if Lead = 5 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 6 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 7 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 8 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 9 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 10 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 11 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

data Forecast;
	set Forecast;

	if Lead = 12 then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*S_Lag12)) + 
					  				  &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*S_Lag12));
	LF1 = lag(Forecast);
	LF2 = lag(LF1);
	LF3 = lag(LF2);
	LF4 = lag(LF3);
	LF5 = lag(LF4);
	LF6 = lag(LF5);
	LF7 = lag(LF6);
	LF8 = lag(LF7);
	LF9 = lag(LF8);
	LF10 = lag(LF9);
	LF11 = lag(LF10);
	LF12 = lag(LF11);
run;

%macro Create(n);
	%do i = 13 %to &n;
		data Forecast;
			set Forecast;

			if Lead = &i then Forecast = &BT + &H1T*(TANH(&BH1 + &L1H1*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H1*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H1*((LF12 - &Mean_Lag12)/&STD_Lag12))) + 
					  				 		   &H2T*(TANH(&BH2 + &L1H2*((LF1 - &Mean_Lag1)/&STD_Lag1) + &L2H2*((LF2 - &Mean_Lag2)/&STD_Lag2) + &L12H2*((LF12 - &Mean_Lag12)/&STD_Lag12)));
			LF1 = lag(Forecast);
			LF2 = lag(LF1);
			LF3 = lag(LF2);
			LF4 = lag(LF3);
			LF5 = lag(LF4);
			LF6 = lag(LF5);
			LF7 = lag(LF6);
			LF8 = lag(LF7);
			LF9 = lag(LF8);
			LF10 = lag(LF9);
			LF11 = lag(LF10);
			LF12 = lag(LF11);
		run;
	%end;
%mend Create;

%Create(24)

data NN_Forecast;
	set Forecast;

	if Forecast = . then Forecast = P_DiffPass;
	keep Forecast Date Passengers NN_Forecast;
	Lag12 = lag(lag(lag(lag(lag(lag(lag(lag(lag(lag(lag(lag(Passengers))))))))))));

	NN_Forecast = Lag12 + Forecast;
run;

data NN_Forecast;
	set NN_Forecast;

	keep Date NN_Forecast Passengers;
	Lag12F = lag(lag(lag(lag(lag(lag(lag(lag(lag(lag(lag(lag(NN_Forecast))))))))))));
	
	if NN_Forecast = . then NN_Forecast = Lag12F + Forecast;
run;

data MAPE_NN;
	merge NN_Forecast Time.USAirlines;
	keep NN_Forecast Passengers Date;
run;

data MAPE_NN;
	set MAPE_NN;
	where Date > '01mar2006'd;
	APE = abs(Passengers - NN_Forecast)/Passengers;
run;

proc means data=MAPE_NN mean;
	var APE;
run;


/* Simple Averaging of Models */
data MAPE_AVG;
	merge NN_Forecast F_ARIMA F_ESM Time.USAirlines ;
	keep Passengers Predict Forecast NN_Forecast Date Avg;
	Avg = (Predict + Forecast + NN_Forecast)/3;
run;

data MAPE_AVG;
	set MAPE_AVG;
	where Date > '01mar2006'd;
	APE = abs(Passengers - Avg)/Passengers;
run;

proc means data=MAPE_AVG mean;
	var APE;
run;


/* Weighted Combined Models - Minimum Variance */
data REG_WC;
	merge NN_Forecast F_ARIMA F_ESM;
	keep Passengers Predict Forecast NN_Forecast Date;
run;

proc reg data=REG_WC;
	model Passengers = Predict Forecast NN_Forecast / noint;
	restrict Predict = 1 - Forecast - NN_Forecast;
	output out=F_WC p=WC_Forecasts;
run;
quit;

data MAPE_WC;
	merge F_WC Time.USAirlines;
	keep Passengers WC_Forecasts Date;
run;

data MAPE_WC;
	set MAPE_WC;
	where Date > '01mar2006'd;
	APE = abs(Passengers - WC_Forecasts)/Passengers;
run;

proc means data=MAPE_WC mean;
	var APE;
run;


/* Weighted Combined Models - Adaptive Weighting */
/* Many Different Adaptive Weighting Schemes You Can Select - This is Just Example */
data REG_WCA;
	merge NN_Forecast F_ARIMA F_ESM;
	keep Passengers Predict Forecast NN_Forecast Date Weight;
	Weight = _n_**2;
run;

proc reg data=REG_WCA;
	model Passengers = Predict Forecast NN_Forecast / noint;
	restrict Predict = 1 - Forecast - NN_Forecast;
	weight Weight;
	output out=F_WCA p=WC_Forecasts;
run;
quit;

data MAPE_WCA;
	merge F_WCA Time.USAirlines;
	keep Passengers WC_Forecasts Date;
run;

data MAPE_WCA;
	set MAPE_WCA;
	where Date > '01mar2006'd;
	APE = abs(Passengers - WC_Forecasts)/Passengers;
run;

proc means data=MAPE_WCA mean;
	var APE;
run;

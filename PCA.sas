proc corr data=LA.testscores;
run;

proc princomp data=LA.testscores cov /*Correlation Matrix is the default*/ out=PCscores;
var _all_;
run;

/* SAS cannot do PCA when #variables > #observations */

proc sgplot data=PCscores;
scatter x=Prin1 y=Prin2;
run;

data PCscores2;
set PCscores;
if Prin1>25 then Flag=1;
else Flag=0;
run;

proc sgplot data=PCscores2;
scatter x=test2 y=test5/ group=Flag;
run;

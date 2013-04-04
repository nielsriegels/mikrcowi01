$SETGLOBAL path  "c:\mikr\mikrcowi01\"
*option nlp=IPOPT;
*$SETGLOBAL path  ""
* =============================================================================
*
* BEAM - Basin Economic Allocation Model
* Copyright Mikkel Kromann, Niels David Riegels, COWI A/S and DHI Group A/S 2011-12
* Use and distribution of the model is allowed only by permission of a copyright holder
*
* =============================================================================

* Status text file for web server interaction
file step /%path%output\step.txt/;
PUT step;
PUT "1";
PUTCLOSE;

* Scalar for optimisation of HEPS production in baseline
SCALAR basDisOpt "Optimise HEPS production in baseline" / 0 /;

* The debugging scalar is used for testing/debugging model water balance against water flow data
* Set this scalar to 1, and all water flow constraints should be satisfied by using
* base year flow data (losses are set to zero, all water supply and demand is in January)
* In normal model operation, baseline should be set to zero!
SCALAR debugging / 0 /;

* THe baseline scalar is used for forcing the model into choosing the same land allocation
* for crops as in the base year
SCALAR baseline / 0 /;

* Scaling of equations
* Data is in mm3, but for solver reasons, we might want to scale to Gm3 (e.g. scaleM3 = 1/1000)
SCALAR scaleM3 / 1 /;

* Reservoir buildup parameter for counterfactural scenario
SCALAR ctrfBuild / 0 /;

* Scalar for returning solver status
SCALAR solvStat / 0 /;
* TO DO

* =============================================================================
* Include base sets and scalars (file not written by input Excel sheet)
* =============================================================================
$include "%path%10base.inc";

* =============================================================================
* Include data driven set definitions
* =============================================================================
$offlisting
$include "%path%20sets.inc";
$onlisting
ALIAS(b,bb,bd,bo);
ALIAS(j,jj);
SET bResBuild(b)        / Res_NUR, Res_TOK /;

* =============================================================================
* Include data driven maps (i.e. intake, reservoir and river arcs)
* =============================================================================
$offlisting
$include "%path%30maps.inc";
$onlisting

SETS
    bRes(b)                 "Reservoir body"
    bRiv(b)                 "River body"
    bPlz(b)                 "Planning zone body"
    bSrc(b)                 "Source body"
    bResNOP                 "Reservoirs not yet built"
    bResSto(b)              "Reservoirs in storage operation mode"
    bResEly                 "Reservoirs in operation producing electricity"
    fixDisBL(b,y0,m0)       "Break intra year reservoir volume condition"
;

* =============================================================================
* Include data tables on water flows, agriculture and economics
* =============================================================================
$offlisting
$include "%path%40data.inc";
$onlisting

* =============================================================================
* Include defition of scenario
* =============================================================================
$include "%path%50scen.inc";

* Assign additional sets for reservoirs based on data and maps
bRiv(b)                 = YES$SUM(bd$intk(bd,b), 1);
bRes(b)                 = YES$SUM(bd$resv(bd,b), 1);
bPlz(b)                 = YES$SUM(j, qAWater(b,j));
bSrc(b)                 = YES$SUM((s,m,y0), sup0(s,b,y0,m));

* Reservoirs not in operation
* In baseline, all new reservoirs are not in operation
bResNOP(b)              = YES$resNew(b) ;
bResSto(b)              = YES$(reservoirs(b,"max") gt reservoirs(b,"min"));
bResSto(b)$bResNOP(b)   = NO;
bResEly(b)              = YES$(reservoirs(b,"Ely") > 0 AND not bResNOP(b));

* Calculate CET coefficient
SCALAR sT "Elasticity of transformation" / 0.5 /;
rL = (sT+1)/sT;

* =============================================================================
* Declare and assign model parameters based on data from Excel sheet
* =============================================================================
$include "%path%60parm.inc";

* =============================================================================
* Declare and assign model and equations and solve
* =============================================================================
$include "%path%70eqtn.inc";

* =============================================================================
* Set levels to help solver process faster
* =============================================================================
*$include "%path%80lvls.inc";

* =============================================================================
* Define base scenario and solve
* =============================================================================
$include "%path%90loop.inc";

PUT step;
PUT "4";
PUTCLOSE;

execute_unload "%path%output\BEAM-flows.gdx" rFlow, rsFlow;
execute "gdxxrw.exe I=%path%output\BEAM-flows.gdx O=%path%output\BEAM-flows.xls par=rFlow rng=rFlow!B10 par=rsFlow rdim=4 rng=rsFlow!B10 rdim=4";
execute "pause";

DISPLAY resv, intk, flow, checkoutput;

PARAMETER testTM(*,bd,bo,m);
OPTION testTM:0:3:1;

testTM("upDIS","Res_TMR","Res_TMP",m) = SUM((s,y), DIS.up(s,"Res_TMR","Res_TMP",y,m));
testTM("lvDIS","Res_TMR","Res_TMP",m) = SUM((s,y), DIS.l(s,"Res_TMR","Res_TMP",y,m));
testTM("upFLW","Res_TMR","Res_TMP",m) = SUM((s,y), FLW.up(s,"Res_TMR","Res_TMP",y,m));
testTM("lvFLW","Res_TMR","Res_TMP",m) = SUM((s,y), FLW.l(s,"Res_TMR","Res_TMP",y,m));
testTM("upITK","Res_TMP","AMUMID",m) = SUM((s,y), ITK.up(s,"Res_TMP","AMUMID",y,m));
testTM("lvITK","Res_TMP","AMUMID",m) = SUM((s,y), ITK.l(s,"Res_TMP","AMUMID",y,m));
testTM("upFLW","Res_TMP","AMUMID",m) = SUM((s,y), FLW.up(s,"Res_TMP","AMUMID",y,m));
testTM("lvFLW","Res_TMP","AMUMID",m) = SUM((s,y), FLW.l(s,"Res_TMP","AMUMID",y,m));

DISPLAY testTM, FLW.l, ITK.l, DIS.l;

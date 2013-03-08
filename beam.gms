$SETGLOBAL path  "c:\mikr\beam\"
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
*$offlisting
$include "%path%20sets.inc";
*$onlisting
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
$onlisting
$include "%path%40data.inc";

* =============================================================================
* Include defition of scenario
* =============================================================================
$include "%path%50scen.inc";

y(y0)$modelYear(y0)     = YES;
bRiv(b)                 = YES$SUM(bd$intk(bd,b), 1);
bRes(b)                 = YES$SUM(bd$resv(bd,b), 1);
bPlz(b)                 = YES$SUM(j, qAWater(b,j));
bSrc(b)                 = YES$SUM((s,m,y), sup0(s,b,y,m));

* Reservoirs not in operation
* In baseline, all new reservoirs are not in operation
bResNOP(b)              = YES$resNew(b) ;
bResSto(b)              = YES$(reservoirs(b,"max") gt reservoirs(b,"min"));
bResSto(b)$bResNOP(b)   = NO;
bResEly(b)              = YES$(reservoirs(b,"Ely") > 0 AND not bResNOP(b));

* Calculate CET coefficient
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
$include "%path%80lvls.inc";


* =============================================================================
* Define base scenario and solve
* =============================================================================

* We make wheat the only sluggish crop in baseline, thus it will be fixed
jAF(j)      = 0;
jAF("wht")  = 1;
jAX(j) = 1$(not jAF(j));
IF(not basDisOpt,
    rFixVol(b,m)                = SUM(y0$baseyear(y0), resVolFix0(b,y0,m));
    rFixDis(bd,bo,m)            = SUM(y0$baseyear(y0), resDisFix0(bd,bo,y0,m));
    sup("W","Src_ZAR","m06")    = sup("W","Src_ZAR","m06") + 2000*0;
);

PUT step;
PUT "2";
PUTCLOSE;

*beam.solprint       = yes;
beam.limrow         = 0;
beam.limcol         = 0;
*beam.iterlim        = 0;
SOLVE beam MAXIMIZING twv USING NLP;
*abort "Test baseline only";
modlStatB = beam.modelstat;
solvStatB = beam.solvestat;

DISPLAY "Base scenario", HPP.l;

* =============================================================================
* Generate output parameter for base scenario
* =============================================================================
$include "%path%91out1.inc";

PUT step;
PUT "3";
PUTCLOSE;

* =============================================================================
* Define counterfactual scenario and solve
* =============================================================================

* Allow flexibility in land allocation
jAF(j)                  = jAF0(j);
jAF("fal")              = YES$fallowLand;
jAX(j)                  = 1$(not jAF(j));
* Set water supply (choose between dry and normal)
sup(s,b,m)              = SUM(y0$baseyear(y0), sup0(s,b,y0,m));
* Set minimum inflows according to baseyear
minInflow0(b,m0)        = minInflowN(b,m0)$baseyear("2009") + minInflowD(b,m0)$baseyear("2001");
* Set new reservoirs in operation active in counterfactual scenario
bResNOP(b)              = YES$(resNew(b) AND not resNewOp(b));
bResSto(b)              = YES$(reservoirs(b,"max") gt reservoirs(b,"min"));
bResSto(b)$bResNOP(b)   = NO;
bResEly(b)              = YES$(reservoirs(b,"Ely") > 0 AND not bResNOP(b));
* Require extra nature needs for counterfactual
natExtra(b)             = natExtra0(b);
* Set counterfactual crop prices
pCrop(j)                = pCrop0(j,"ctrf");
* change input prices
pInput(k)               = pInput0(k);
* Reset fixed discharge patterns from baseline
resVolFix0(b,y,m)       = 0;
resDisFix0(bd,bo,y,m)   = 0;
* Set irrigation investment costs
iInv(sInv)              = iInv0(sInv);
* Set reservoir buildup
ctrfBuild = ctrfBuild0;


*DISPLAY minInflow0, sup0, sup, baseyear;
*beam.solprint = yes;
beam.limrow = 5000;
SOLVE beam MAXIMIZING twv USING NLP;
modlStatS = beam.modelstat;
solvStatS = beam.solvestat;

* =============================================================================
* Generate output parameter for action scenario
* =============================================================================
$include "%path%92out2.inc";

*$ontext
IF(beam.solvestat eq 1,
*    DISPLAY rAgriBase, rFlowBase, rEconBase, rAgriCtrf, rFlowCtrf, rEconCtrf;
*    execute_unload "beamOutput.gdx" rAgriBase, rFlowBase, rEconBase, rAgriCtrf, rFlowCtrf, rEconCtrf;
*    execute "gdxxrw.exe I=beamOutput.gdx O=beamOutput.xls index=index!d10";
*    execute "pause";
);
*$offtexts

* =============================================================================
* Include loops for writing csv files to disk
* =============================================================================
$include "%path%93out3.inc";

PUT step;
PUT "4";
PUTCLOSE;

PARAMETER testEly(g,*,m0);

testEly(g,"HPP electricity MWh",m)  = SUM((y,b), HPP.l(b,g,y,m));
testEly(g,"TPP electricity MWh",m)  = SUM((y,u), TPP.l(u,g,y,m));
testEly(g,"All electricity MWh",m)  = SUM((y,u), TPP.l(u,g,y,m))+SUM((y,b), HPP.l(b,g,y,m));
testEly(g,"Demand MWh",m)           = SUM((y,c), elyDemand0(c,g,m))*30*segmentHours(g);
testEly(g,"HPP electricity MW",m)   = SUM((y,b), HPP.l(b,g,y,m))/(30*segmentHours(g));
testEly(g,"TPP electricity MW",m)   = SUM((y,u), TPP.l(u,g,y,m))/(30*segmentHours(g));
testEly(g,"All electricity MW",m)   = SUM((y,u), TPP.l(u,g,y,m))/(30*segmentHours(g))+SUM((y,b), HPP.l(b,g,y,m))/(30*segmentHours(g));
testEly(g,"TPP el capacity MW",m)   = SUM((u), elyThermal("elyCap",u));
testEly(g,"Demand MW",m)            = SUM((y,c), elyDemand0(c,g,m));
testEly(g,"Price USD/MWh",m)        = SUM((y), ELYMKT.M(g,y,m))*1000000;

OPTIONS testEly:0:2:1;

DISPLAY "Action scenario", HPP.l, TPP.l, testEly, pELy;

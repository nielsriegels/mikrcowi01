* Loop over scenarios to be analysed

LOOP(sSce$scenarios("run",sSce),


* Include assigning values to scenario specific parameters
$include "%path%91asgn.inc";

* Load previous solution to increase computational speed
IF(ord(sSce) eq 1, 
    execute_load "%path%solutions\base.gdx", GRW, IRG, HHI, LOS, ITK, FLW, VOL, DIS, STO, HPP, TPP, CRP, LND, TWV;
);
IF(ord(sSce) eq 2, 
    execute_load "%path%solutions\ctrf.gdx", GRW, IRG, HHI, LOS, ITK, FLW, VOL, DIS, STO, HPP, TPP, CRP, LND, TWV;
);

* Fix hydrological variables that can never be non-zero
    FLW.fx(s,bd,bo,y,m)$(not flow(bd,bo))                       = 0;
    ITK.fx(s,bd,bo,y,m)$(not intk(bd,bo))                       = 0;
    IRG.fx(b,j,y,m)$(qAWater(b,j)*agriSeason(b,j,m) eq 0)       = 0;
    DIS.fx(s,bd,bo,y,m)$(not resv(bd,bo))                       = 0;
    DIS.up(s,bd,bo,y,m)$resv(bd,bo)                             = 9999999;
    GRW.fx(b,y,m)$(SUM(ms$mSeason(m,ms), grwtrMax(b,ms)) eq 0)  = 0;
    HPP.fx(b,g,y,m)$(not bResEly(b))                            = 0;
    CRP.fx(b,j,y)$(crp0(b,j) eq 0)                              = 0;

* Lower and upper bounds
    CRP.lo(b,j,y)$jAF(j)                                        = 0.01$crp0(b,j);
    STO.up(s,b,y,m)$bRes(b)                                     = 99999;
    STO.up(s,b,y,m)$sea(b)                                      = INF;
    TPP.up(u,g,y,m)                                             = elyThermal("elyCap",u)*30*segmentHours(g);
 


* Solve model for the give scenario
*    beam.solprint = yes;
*    beam.limrow = 50000;
*    beam.limcol = 50000;
*    beam.iterlim = 0;
    SOLVE beam MAXIMIZING twv USING NLP;

IF(ord(sSce) eq 1, 
    execute_unload "%path%solutions\base.gdx", GRW, IRG, HHI, LOS, ITK, FLW, VOL, DIS, STO, HPP, TPP, CRP, LND, TWV;
);
IF(ord(sSce) eq 2, 
    execute_unload "%path%solutions\ctrf.gdx", GRW, IRG, HHI, LOS, ITK, FLW, VOL, DIS, STO, HPP, TPP, CRP, LND, TWV;
);

* Include assignment of scenario results to parameters to be output to Excel
$include "%path%96rslt.inc";

* Save total income for check print
    checkoutput("totinc",sSce)                  = SUM((b,y,m), rEcon(sSce,b,"TotInc",y,m));
    checkoutput("obj",sSce)                     = TWV.l;

);
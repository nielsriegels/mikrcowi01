* Loop over scenarios to be analysed

LOOP(sSce$scenarios("run",sSce),


* Include assigning values to scenario specific parameters
$include "%path%91asgn.inc";

* Load solution from previous run by copying it into solution.gdx 
* from the scenario specific name solution-sXX.gdx
putclose sRename 'copy /Y %path%solutions\solution-' sSce.tl:0 '.gdx %path%solutions\solution.gdx';
execute '%path%solutions\renameS.cmd';

execute_load "%path%solutions\solution.gdx", GRW, IRG, HHI, LOS, ITK, FLW, VOL, DIS, STO, HPP, TPP, YWR, CRP, LND, TWV;

* Fix hydrological variables that can never be non-zero
    FLW.up(s,bd,bo,y,m)                                         = INF$flow(bd,bo);
    ITK.up(s,bd,bo,y,m)                                         = INF$intk(bd,bo);
    DIS.up(s,bd,bo,y,m)                                         = INF$resv(bd,bo);
    IRG.up(b,j,y,m)                                             = INF$irg0(b,j,y,m);
    HHI.up(s,sSec,b,y,m)                                        = INF$HHIuse(sSec,b,m);
    GRW.up(b,y,m)                                               = INF$SUM(ms$mSeason(m,ms), grwtrMax(b,ms));
    LND.up(b,j,y)                                               = INF$lnd0(b,j);
    VOL.up(s,b,y,m)                                             = INF$(bRes(b) or sea(b));
    STO.up(s,b,y,m)                                             = INF$(bRes(b) or sea(b));
    HPP.up(b,g,y,m)                                             = INF$bResEly(b);
    TPP.up(u,g,y,m)                                             = INF$elyThermal("elyCap",u);
    LOS.up(s,b,l,y0,m0)                                         = INF;
    CRP.lo(b,j,y)$crp0(b,j)                                     = crp0(b,j) * 0.1;
    CRP.up(b,j,y)$crp0(b,j)                                     = crp0(b,j) * 3 ;
    LND.lo(b,j,y)$lnd0(b,j)                                     = lnd0(b,j) * 0.1;
    LND.up(b,j,y)$lnd0(b,j)                                     = lnd0(b,j) * 3 ;

* Solve model for the give scenario
*    beam.solprint = yes;
*    beam.limrow = 50000;
*    beam.limcol = 50000;
*    beam.iterlim = 0;
    SOLVE beam MAXIMIZING twv USING NLP;


* Save solution to re-use later
IF(beam.modelstat le 2, 
    execute_unload "%path%solutions\solution.gdx", GRW, IRG, HHI, LOS, ITK, FLW, VOL, DIS, STO, HPP, TPP, YWR, CRP, LND, TWV;
    putclose sRename 'copy /Y  %path%solutions\solution.gdx %path%solutions\solution-' sSce.tl:0 '.gdx';
    execute '%path%solutions\renameS.cmd';
); 


* Include assignment of scenario results to parameters to be output to Excel
$include "%path%96rslt.inc";

* Save total income for check print
    checkoutput("totinc",sSce)                  = SUM((b,y,m), rEcon(sSce,b,"TotInc",y,m));
    checkoutput("obj",sSce)                     = TWV.l;
    checkoutput("mstat",sSce)                   = beam.modelstat;


checkYRW(sSce,b,j)$lnd0(b,j) = SUM(y, 1 - SUM(p$etcP(b,j,p),  ( 1 - SUM(m, etcM2P(b,j,p,m)*IRG.l(b,j,y,m)) / (etcP(b,j,p)*LND.l(b,j,y)) )
                                                * wPhase(b,j,p) * kYield(p,j) 
                                           ));



);
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
    LND.l(b,j,y)                                                = lnd0(b,j);

* Solve model for the give scenario
*    beam.solprint = yes;
*    beam.limrow = 50000;
*    beam.limcol = 50000;
*    beam.iterlim = 0;
    SOLVE beam MAXIMIZING twv USING NLP;

IF(beam.modelstat < 3,
    IF(ord(sSce) eq 1, 
        execute_unload "%path%solutions\base.gdx", GRW, IRG, HHI, LOS, ITK, FLW, VOL, DIS, STO, HPP, TPP, CRP, LND, TWV;
    );
    IF(ord(sSce) eq 2, 
        execute_unload "%path%solutions\ctrf.gdx", GRW, IRG, HHI, LOS, ITK, FLW, VOL, DIS, STO, HPP, TPP, CRP, LND, TWV;
    );
);

* Include assignment of scenario results to parameters to be output to Excel
$include "%path%96rslt.inc";

* Save total income for check print
    checkoutput("totinc",sSce)                  = SUM((b,y,m), rEcon(sSce,b,"TotInc",y,m));
    checkoutput("obj",sSce)                     = TWV.l;

);
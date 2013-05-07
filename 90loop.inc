* Loop over scenarios to be reported 

LOOP(sSce$(scenarios("run",sSce) or xferResults),

IRG.l(b,q0,j,y0,m0) = 0;

* Include assigning values to scenario specific parameters
$include "%path%91asgn.inc";

* Load solution from previous run by copying it into solution.gdx 
* from the scenario specific name solution-sXX.gdx
putclose sRename 'copy /Y %path%solutions\solution-' sSce.tl:0 '.gdx %path%solutions\solution.gdx';
execute '%path%solutions\renameS.cmd';

execute_load "%path%solutions\solution.gdx", GRW, IRG, HHI, LOS, ITK, LCH, FLW, VOL, DIS, STO, HPP, HHD, TPP, TRX, DEM, USD, YWR, iET, CRP, LND, CMX, TWV ;
USD.l(g,c,d0,y,m)$(not d(d0)) = 0;

    IF(scenarios("run",sSce), 
* Fix hydrological variables that can never be non-zero
        FLW.up(s,bd,bo,y,m)                                         = INF$flow(bd,bo);
        FLW.up(s,bd,bo,y,m)$resv(bd,bo)                             = 99999$resv(bd,bo);
        ITK.lo(s,bd,bo,y,m)                                         = 0;
        ITK.up(s,bd,bo,y,m)                                         = INF$intk(bd,bo);
        DIS.up(s,bd,bo,y,m)                                         = INF$resv(bd,bo);
        IRG.up(b,q,j,y,m)                                           = INF$irg0(b,q,j,y,m);
        HHI.up(s,sSec,b,y,m)                                        = INF$HHIuse(sSec,b,m);
        GRW.up(b,y,m)                                               = INF$SUM(ms$mSeason(m,ms), grwtrMax(b,ms));
        VOL.up(s,b,y,m)                                             = INF$(bRes(b) or sea(b));
        STO.up(s,b,y,m)                                             = INF$(bRes(b) or sea(b));
        HPP.up(b,g,y,m)                                             = INF$bResEly(b);
        TPP.up(c,u,g,y,m)                                           = INF$(elyThermal("elyCap",u) and tppMap(c,u));
        TRX.up(c,cc,g,y,m)                                           = INF$trx0(c,cc);
        LOS.up(s,b,l,y0,m0)                                         = INF;
*    YWR.up(b,q,j,y)                                             = 1;
        CRP.lo(b,q,j,y)                                             = crp0(b,q,j) * 0.1;
        CRP.up(b,q,j,y)                                             = crp0(b,q,j) * 3 ;
        LND.lo(b,q,j,y)                                             = lnd0(b,q,j) * 0.1;
        LND.up(b,q,j,y)                                             = lnd0(b,q,j) * 3 ;
        CRP.fx(b,q0,j,y)$(not q(q0))                                = 0;
        LND.fx(b,q0,j,y)$(not q(q0))                                = 0;
        IRG.fx(b,q0,j,y,m)$(not q(q0))                              = 0;
*    iET.up(b,q0,j,p,y)$(lnd0(b,q0,j) AND etcP(b,j,p) )          = 1;
*    iET.l(b,q0,j,p,y) = 1$(lnd0(b,q0,j) AND etcP(b,j,p) );

* Include module for constrained optimisation
$include "%path%92cstr.inc";

* Solve model for the give scenario
    beam.solprint = yes;
*    beam.limrow = 50000;
*    beam.limcol = 50000;
*    beam.iterlim = 0;
*    if(ord(ssce) eq 3,    beam.solprint = yes; );
    
        SOLVE beam MAXIMIZING twv USING NLP;

* Save solution to re-use later
        IF( ((beam.modelstat le 2) or (beam.modelstat eq 7)), 
            execute_unload "%path%solutions\solution.gdx", GRW, IRG, HHI, LOS, ITK, LCH, FLW, VOL, DIS, STO, HPP, HHD, TPP, USD, DEM, TRX, XPT, YWR, iET, CRP, LND, CMX, TWV;
            putclose sRename 'copy /Y  %path%solutions\solution.gdx %path%solutions\solution-' sSce.tl:0 '.gdx';
            execute '%path%solutions\renameS.cmd';
        ); 

* End If for test to run scenario - write to results file from GDX saved results in any case
    );

* Include assignment of scenario results to parameters to be output to Excel
$include "%path%96rslt.inc";

* Save total income for check print
    checkoutput("totinc",sSce)                  = SUM((b,y,m), rEcon(sSce,b,"TotInc",y,m));
    checkoutput("obj",sSce)                     = TWV.l;
    checkoutput("mstat",sSce)                   = beam.modelstat;

);


* Loop over scenarios to be reported 

LOOP(sSce$scenCalc("out",sSce),

*IRG.l(b,q0,j,y0,m0) = 0;

* Include assigning values to scenario specific parameters
$include "%path%91asgn.inc";

* Load solution from previous run by copying it into solution.gdx 
* from the scenario specific name solution-sXX.gdx
putclose sRename 'copy /Y %path%solutions\solution-' sSce.tl:0 '.gdx %path%solutions\solution.gdx';
execute '%path%solutions\renameS.cmd';

execute_load "%path%solutions\solution.gdx", GRW, IRG, HHI, LOS, ITK, LCH, FLW, VOL, DIS, STO, HPP, HHD, TPP, TRX, XPT, DEM, USD, YWR, iET, CRP, LND, CMX, TWV, ELYMKT ;
*USD.l(g,c,d0,y,m)$(not d(d0)) = 0;
*HPP.l("Res_KA2",g,y,m) = 0;
*HHD.l("Res_KA2",y,m) = 0;
*LND.l(b,q,j,y)$(not bPlz(b)) = 0;

    IF(scenCalc("run",sSce), 
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
        HHD.up(b,y,m)                                               = INF$bResEly(b);
        HPP.up(b,g,y,m)                                             = INF$bResEly(b);
        TPP.up(c,u,g,y,m)                                           = INF$(elyThermal("elyCap",u) and tppMap(c,u));
        TRX.up(c,cc,g,y,m)                                          = INF$trx0(c,cc);
        LOS.up(s,b,l,y0,m0)                                         = INF;
*    YWR.up(b,q,j,y)                                             = 1;
        CMX.lo(b,q,x,y)$bPlz(b)                                     = 0;
        CMX.up(b,q,x,y)$bPlz(b)                                     = INF$bPlz(b);
        CRP.lo(b,q,j,y)$bPlz(b)                                     = 0;
        CRP.up(b,q,j,y)$bPlz(b)                                     = INF$(bPlz(b) and lnd0(b,q,j));
        LND.lo(b,q,j,y)                                             = 0.001$(bPlz(b) and lnd0(b,q,j));
        LND.up(b,q,j,y)$bPlz(b)                                     = INF$(bPlz(b) and lnd0(b,q,j));
        CMX.fx(b,q0,x,y)$(not q(q0))                                = 0;
        CRP.fx(b,q0,j,y)$(not q(q0))                                = 0;
        LND.fx(b,q0,j,y)$(not q(q0))                                = 0;
        IRG.fx(b,q0,j,y,m)$(not q(q0))                              = 0;
*    iET.up(b,q0,j,p,y)$(lnd0(b,q0,j) AND etcP(b,j,p) )          = 1;
*    iET.l(b,q0,j,p,y) = 1$(lnd0(b,q0,j) AND etcP(b,j,p) );

* Include module for constrained optimisation
$include "%path%92cstr.inc";

* Solve model for the give scenario
*    beam.solprint = yes;
*    beam.limrow = 50000;
*    beam.limcol = 50000;
*    beam.iterlim = 0;
*    if(ord(ssce) eq 3,    beam.solprint = yes; );
    
        SOLVE beam MAXIMIZING twv USING NLP;
* Save intake for intake reduction scenarios
*IF(ord(sSce) eq 40, 
*    itk1(s,bd,y,m)$bPlz(bd) = 
*      SUM(bo, ITK.l(s,bd,bo,y,m)+DIS.l(s,bd,bo,y,m)+FLW.l(s,bd,bo,y,m))
*     +GRW.l(bd,y,m) - HHI.l(s,"HH",bd,y,m) - HHI.l(s,"Ind",bd,y,m); 
*);

* Save solution to re-use later
        IF( ((beam.modelstat le 2) or (beam.modelstat eq 7)), 
            execute_unload "%path%solutions\solution.gdx", GRW, IRG, HHI, LOS, ITK, LCH, FLW, VOL, DIS, STO, HPP, HHD, TPP, USD, DEM, TRX, XPT, YWR, iET, CRP, LND, CMX, TWV, ELYMKT;
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

rItk(bd,sSce,"itk1")$bPlz(bd) = SUM((s,y,m), itk1(s,bd,y,m));
rItk(bd,sSce,"itk2")$bPlz(bd) = SUM((s,y,m), itk2(s,bd,y,m));
rItk(bd,sSce,"HHI")$bPlz(bd) = SUM((s,y,m), HHI.l(s,"HH",bd,y,m)+HHI.l(s,"Ind",bd,y,m));
rItk(bd,sSce,"ITKl")$bPlz(bd) = SUM((s,bo,y,m), ITK.l(s,bd,bo,y,m)+DIS.l(s,bd,bo,y,m)+FLW.l(s,bd,bo,y,m)) + SUM((y,m), GRW.l(bd,y,m)) - rItk(bd,sSce,"HHI");
rItk(bd,sSce,"IRGl")$bPlz(bd) = SUM((s,q,j,y,m), IRG.l(bd,q,j,y,m));

rPlz(bd,sSce,"ITK")$bPlz(bd) = SUM((s,bo,y,m),  ITK.l(s,bd,bo,y,m));
rPlz(bd,sSce,"FLW")$bPlz(bd) = SUM((s,bo,y,m),  FLW.l(s,bd,bo,y,m));
rPlz(bd,sSce,"DIS")$bPlz(bd) = SUM((s,bo,y,m),  DIS.l(s,bd,bo,y,m));
rPlz(bd,sSce,"GRW")$bPlz(bd) = SUM((s,y,m),     GRW.l(bd,y,m));
rPlz(bd,sSce,"LOS")$bPlz(bd) =-SUM((s,y,m),     LOS.l(s,bd,"seep",y,m));
rPlz(bd,sSce,"IRG")$bPlz(bd) =-SUM((s,q,j,y,m), IRG.l(bd,q,j,y,m));
rPlz(bd,sSce,"LCH")$bPlz(bd) =-SUM((s,y,m),     LCH.l(s,bd,y,m));
rPlz(bd,sSce,"RTN")$bPlz(bd) = SUM((s,y,m),     rtn0(bd,m));
rPlz(bd,sSce,"HHI")$bPlz(bd) =-SUM((s,y,m),     HHI.l(s,"HH",bd,y,m)+HHI.l(s,"Ind",bd,y,m));
rPlz(bd,sSce,"sup")$bPlz(bd) = rPlz(bd,sSce,"ITK") + rPlz(bd,sSce,"FLW") + rPlz(bd,sSce,"DIS") + rPlz(bd,sSce,"GRW"); 
rPlz(bd,sSce,"dem")$bPlz(bd) = rPlz(bd,sSce,"LOS") + rPlz(bd,sSce,"LCH") + rPlz(bd,sSce,"HHI") + rPlz(bd,sSce,"IRG") + rPlz(bd,sSce,"RTN"); 
rPlz(bd,sSce,"bal")$bPlz(bd) = round(rPlz(bd,sSce,"sup")+ rPlz(bd,sSce,"dem"),3);

*rLos(bd,sSce,"riv")$bRiv(bd) = SUM((s,m,y),           LOS(s,bd,"seep",y,m)+LOS(s,bd,"evap",y,m));
*rLos(bd,sSce,"res")$bRiv(bd) = SUM((s,m,y),           LOS(s,bd,"seep",y,m)+LOS(s,bd,"evap",y,m));
rLos(bd,sSce)                 = SUM((s,m,y),           LOS.l(s,bd,"seep",y,m)+LOS.l(s,bd,"evap",y,m));


 

elyBal(sSce,c,"IMP")        = SUM((g,y,m),          SUM(cc$trx0(cc,c),       TRX.l(cc,c,g,y,m) )*0.9  );
elyBal(sSce,c,"TPP")        = SUM((g,y,m),          SUM(u$tppMap(c,u),       TPP.l(c,u,g,y,m))        );
elyBal(sSce,c,"HPP")        = SUM((g,y,m),          SUM(b$bResEly(b),        HPP.l(b,g,y,m)$bCty(b,c)));
elyBal(sSce,c,"USD")        = SUM((g,y,m),          SUM(d$cUSD(c,g,d),       USD.l(g,c,d,y,m))        );
elyBal(sSce,c,"DEM")        = SUM((g,y,m),          -DEM.l(g,c,y,m)                                   );
elyBal(sSce,c,"EXP")        = SUM((g,y,m),          -SUM(cc$trx0(c,cc),       TRX.l(c,cc,g,y,m) )     );
elyBal(sSce,c,"XPT")        = SUM((g,y,m),          -XPT.l(c,g,y,m)                                   );
elyBal(sSce,c,"BAL")        = elyBal(sSce,c,"IMP") + elyBal(sSce,c,"TPP") + elyBal(sSce,c,"HPP") + elyBal(sSce,c,"USD")
                             +elyBal(sSce,c,"DEM") + elyBal(sSce,c,"EXP") + elyBal(sSce,c,"XPT") ;

natBal(sSce,b)              = SUM((s,y,m), STO.l(s,b,y,m))$sea(b);

);


* Loop over scenarios to be analysed

LOOP(sSce$scenarios("run",sSce),

* Set model year (demography for demand growth etc)
    y(y0)                   = scenRainYear(sSce,y0);

* Allow flexibility in land allocation and account for any fallow land
* unless scenario user chose fixed land allocation (opLND=0)
    IF(scenarios("opLND",sSce) eq 0, 
        jAF(j)                  = NO;
        jAF("wht")              = NO;
    );
    IF(scenarios("opLND",sSce) gt 0, 
        jAF(j)                  = jAF0(j);
    );
    jAF("fal")              = YES$scenarios("fallow",sSce);
    jAX(j)                  = 1$(not jAF(j));
    lnd0(b,"fal")           = SUM(j$jAF(j), lnd0(b,j)) * scenarios("fallow",sSce);
    lShare(b,j)$jAF(j)      = (qALand(b,j) / SUM(jj$jAF(jj), qALand(b,jj)))$SUM(jj$jAF(jj), qALand(b,jj));

* Set water supply (choose between dry and normal)
    sup(s,b,m)              = SUM(y0$scenRainYear(sSce,y0),     sup0(s,b,y0,m));
    hhiUse("HH",b,m)        = SUM(y0$scenModelYear(sSce,y0),    qHwater(b,m)*(1+demography(y0,"HH") ) );
    hhiUse("ind",b,m)       = SUM(y0$scenModelYear(sSce,y0),    qIwater(b,m)*(1+demography(y0,"ind")) );


* Set minimum inflows according to year
    minInflow0(b,m0)        = minInflowN(b,m0)$scenRainYear(sSce,"2009") + minInflowD(b,m0)$scenRainYear(sSce,"2001");

* Set new reservoirs in operation according to scenario definition
    bResNOP(b)              = YES$(resNew(b) AND not resNewOp(b,sSce));
    bResSto(b)              = YES$(reservoirs(b,"max") gt reservoirs(b,"min"));
    bResSto(b)$bResNOP(b)   = NO;
    bResEly(b)              = YES$(reservoirs(b,"Ely") > 0 AND not bResNOP(b));
    HPP.up(b,g,y,m)         = INF$bResEly(b);

* Fix discharge and volumen patterns for selected reservoirs 
    IF(scenarios("opRES",sSce) eq 0,
        resVolFix(b,y,m)              = resVolFix0(b,y,m);
        resDisFix(bd,bo,y,m)          = resDisFix0(bd,bo,y,m);
    );
    IF(scenarios("opRES",sSce) eq 1,
        resVolFix(b,y,m)              = 0;
        resDisFix(bd,bo,y,m)          = 0;
    );

* Require extra nature needs for counterfactual
    natExtra(b)             = natExtra0(b,sSce);

* Set crop prices according to scenario indices for crop price
    pCrop(j)                = pCrop0("baseprice",j)*ipCrop(j,sSce);

* Set crop costs according to scenario indices for crop costs
    cCrop(b,j)$bPlz(b)      = SUM(k, cCrop0(k,j)*icCrop(k,sSce));

* Reset fixed discharge patterns from line
    resVolFix0(b,y,m)       = 0;
    resDisFix0(bd,bo,y,m)   = 0;

* Set irrigation investment costs
    iInv(sInv)              = 0;

* Set reservoir buildup
    ctrfBuild               = scenarios("rBuild",sSce);

* Flow variable levels
    IRG.l(b,j,y,m)              = irg0(b,j,y,m);
    ITK.l(s,bd,bo,y,m)          = itk0(bd,bo)/12;
    FLW.l(s,bd,bo,y,m)          = flw0(bd,bo)/12;
    DIS.l(s,bd,bo,y,m)          = dis0(bd,bo)/12;
    LOS.l(s,b,l,y,m)            = lossRivers(b,l,m)*(rtn0(b,m) + sup(s,b,m) + SUM(bo, FLW.l(s,b,bo,y,m)+DIS.l(s,b,bo,y,m)));
    GRW.l(b,y,m)                = SUM(mS, grwtrMax(b,mS))$(ord(m) eq 1);
    HHI.l(s,sSec,b,y,m)         = HHIuse(sSec,b,m);

* Reservoir variable levels
    STO.l(s,b,y,m)$(not sea(b)) = SUM(bd, DIS.l(s,bd,b,y,m));
    STO.l(s,b,y,m)$sea(b)       = SUM(bo, DIS.l(s,b,bo,y,m)+ITK.l(s,b,bo,y,m)+FLW.l(s,b,bo,y,m)) + rtn0(b,m);
    VOL.l(s,b,y,m)              = ((reservoirs(b,"max") - reservoirs(b,"min"))/ 2 + reservoirs(b,"min") )$bResSto(b);
    LOOP(m,
        VOL.l(s,b,y,m)          = SUM((yy,mm)$mLast(y,m,yy,mm), VOL.l(s,b,yy,mm)) + STO.l(s,b,y,m) - SUM(bd$resv(bd,b), DIS.l(s,bd,b,y,m));
    );
    VOL.l(s,b,y,m)$(not bResSto(b)) = 0;


* ELectricity variable levels
    TPP.l(u,g,y,m)              = elyThermal("elyCap",u) / 2;
    HPP.l(b,g,y,m)$bResEly(b)   = 0.5*SUM((s,bd)$(resv(bd,b) AND sW(s)),  gravity * reservoirs(b,"coef_eta") * DIS.l(s,bd,b,y,m)
                                      * ( reservoirs(b,"coef_a") + reservoirs(b,"coef_b")*VOL.l(s,b,y,m) + reservoirs(b,"coef_c")*POWER(VOL.l(s,b,y,m),2))
                                  ) / 3600;
* Agricultural variable levels
    LND.l(b,j,y)                = lnd0(b,j);
    CRP.l(b,j,y)                = crp0(b,j);

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

*DISPLAY sup, irg0, HHIuse, rtn0, grwtrmax, lossRivers, lossChannels, minInflow0;
*abort "hej";

* Reservoir flows
    rFlow(b,"ResVol",y,m)                       =  SUM(s$sW(s),     VOL.l(s,b,y,m));
    rFlow(b,"ResVolAvg",y,m)                    =  SUM(s$sW(s),     VOL.l(s,b,y,m) + SUM((yy,mm)$mLast(y,m,yy,mm), VOL.l(s,b,yy,mm)) ) / 2 ;
    rFlow(b,"ResChg",y,m)                       =  SUM(s$sW(s),     VOL.l(s,b,y,m) - SUM((yy,mm)$mLast(y,m,yy,mm), VOL.l(s,b,yy,mm)) );
    rFlow(b,"ResChg",y,m)$bResBuild(b)          = -SUM(s$sW(s),    -STO.l(s,b,y,m) + SUM(bd, DIS.l(s,bd,b,y,m) + FLW.l(s,bd,b,y,m)) );
    rFlow(b,"ResChg",y,m)$(sea(b) and mBgn(m))  =  SUM(s$sW(s),     VOL.l(s,b,y,m));

* Inflows
    rFlow(b,"InSrc",y,m)                        =  SUM(s$sW(s),         sup(s,b,m));
    rFlow(b,"InGrw",y,m)                        =                       GRW.l(b,y,m);
    rFlow(b,"InDis",y,m)                        =  SUM((s,bo)$sW(s),    DIS.l(s,b,bo,y,m));
    rFlow(b,"InFlw",y,m)                        =  SUM((s,bo)$sW(s),    FLW.l(s,b,bo,y,m));
    rFlow(b,"InItk",y,m)                        =  SUM((s,bo)$sW(s),    ITK.l(s,b,bo,y,m));
    rFlow(b,"InRtn",y,m)                        = rtn0(b,m);
    rFlow(b,"InTot",y,m)                        = rFlow(b,"InSrc",y,m) + rFlow(b,"InDis",y,m) + rFlow(b,"InFlw",y,m) 
                                                 +rFlow(b,"InItk",y,m) + rFlow(b,"InRtn",y,m) + rFlow(b,"InGrw",y,m);
* Outflows
    rFlow(b,"OutDem",y,m)                       =  SUM(j,               IRG.l(b,j,y,m) - qIwater(b,m) - qHwater(b,m) );
    rFlow(b,"OutSto",y,m)                       = -SUM(s$sW(s),         STO.l(s,b,y,m));
    rFlow(b,"OutDis",y,m)                       = -SUM((s,bd)$sW(s),    DIS.l(s,bd,b,y,m)$(not sea(bd)) );
    rFlow(b,"OutFlw",y,m)                       = -SUM((s,bd)$sW(s),    FLW.l(s,bd,b,y,m)$(not sea(bd)) );
    rFlow(b,"OutInt",y,m)                       = -SUM((s,bd)$sW(s),    ITK.l(s,bd,b,y,m)$(not sea(bd)) );
    rFlow(b,"OutLak",y,m)                       = -SUM((s,bd)$sW(s),    (ITK.l(s,bd,b,y,m)+FLW.l(s,bd,b,y,m)+DIS.l(s,bd,b,y,m))$sea(bd)  );
    rFlow(b,"OutLos",y,m)                       = -SUM((s,l)$sW(s),                LOS.l(s,b,l,y,m));
    rFlow(b,"OutTot",y,m)                       =  rFlow(b,"OutDem",y,m) + rFlow(b,"OutDis",y,m) + rFlow(b,"OutFlw",y,m) 
                                                  +rFlow(b,"OutInt",y,m) + rFlow(b,"OutLos",y,m) + rFlow(b,"OutLak",y,m);
* Balance
    rFlow(b,"Balance",y,m)                      =  round(rFlow(b,"InTot",y,m)+rFlow(b,"OutTot",y,m)-rFlow(b,"ResChg",y,m)$bRes(b), 3) ;
    rFlow(b,e_resFlow,y,"annual")               =  SUM(m,            rFlow(b,e_resFlow,y,m));
    rFlow(b,"ResVol",y,"annual")                =  rFlow(b,"ResVol",y,"annual")  + 0.000001;
    rFlow(b,"Balance",y,"annual")               =  rFlow(b,"Balance",y,"annual") + 0.000001;

* Agricultural line results
    rAgri(b,"WaterUse",j,y)                     = SUM(m, IRG.l(b,j,y,m));
    rAgri(b,"LandEff",j,y)$bPlz(b)              = 0;
    rAgri(b,"LandEff",j,y)$(not jAF(j))         = 0;
    rAgri(b,"LandUse",j,y)                      = LND.l(b,j,y);
    rAgri(b,"AgriSales",j,y)                    =  1000 * CRP.l(b,j,y) * pCrop(j)   / 1000000;
    rAgri(b,"AgriCosts",j,y)                    = -1000 * LND.l(b,j,y) * cCrop(b,j) / 1000000;
    rAgri(b,"IrrInvCost",j,y)                   = -1000 * ( iInv("usdPerHa")*LND.l(b,j,y) + iInv("usdPerMM3")*SUM(m, IRG.l(b,j,y,m)) )/1000000 ;
    rAgri(b,"AgriIncome",j,y)                   = rAgri(b,"AgriSales",j,y) + rAgri(b,"AgriCosts",j,y) + rAgri(b,"IrrInvCost",j,y);
    rAgri(b,"AgriProd",j,y)                     = CRP.l(b,j,y);
    rAgri(b,"LandIncome",j,y)                   = ( 1000000*rAgri(b,"AgriIncome",j,y) / rAgri(b,"LandUse",j,y) )$rAgri(b,"LandUse",j,y);
    rAgri(b,"WatrIncome",j,y)                   = ( 1000000*rAgri(b,"AgriIncome",j,y) / rAgri(b,"WaterUse",j,y) )$rAgri(b,"WaterUse",j,y);
    rAgri(b,"LabrUse",j,y)$lnd0(b,j)            = 1000*LND.l(b,j,y)/lnd0(b,j);

* Economic results, line
    rEcon(b,"AgriSales",y,m)                    = SUM(j, rAgri(b,"AgriSales",j,y) ) / 12;
    rEcon(b,"AgriInc",y,m)                      = SUM(j, rAgri(b,"AgriIncome",j,y)) / 12;
    rEcon(b,"IrrInvCost",y,m)                   = SUM(j, rAgri(b,"IrrInvCost",j,y)) / 12;
    rEcon(b,"AgriCost",y,m)                     = SUM(j, rAgri(b,"AgriCosts",j,y) ) / 12;
    rEcon(b,"HydroCost",y,m)                    = -1000*reservoirs(b,"cost")$(not bResNOP(b));
    rEcon(b,"HydroInc",y,m)                     = 1000 * SUM(g, HPP.l(b,g,y,m) *  pEly(g,y,m) ) / 1000000 + rEcon(b,"HydroCost",y,m);
    rEcon(b,"HydroProd",y,m)                    = SUM(g, HPP.l(b,g,y,m));
    rEcon(b,"HydroHead",y,m)                    = ( reservoirs(b,"coef_a") + reservoirs(b,"coef_b")*rFlow(b,"ResVolAvg",y,m) + reservoirs(b,"coef_c")*POWER(rFlow(b,"ResVolAvg",y,m),2));
    rEcon(b,"HydroGain",y,m)                    = -( rEcon(b,"HydroProd",y,m) / rFlow(b,"OutDis",y,m) )$rFlow(b,"OutDis",y,m);
    rEcon(b,"TotInc",y,m)                       = rEcon(b,"AgriInc",y,m) + rEcon(b,"HydroInc",y,m);
    rEcon(b,"DischVal",y,m)$bResEly(b)          = -1000000*( rEcon(b,"HydroInc",y,m) / rFlow(b,"OutDis",y,m) )$rFlow(b,"OutDis",y,m);
    rEcon(b,"WaterVal",y,m)                     = WBALANCE.M("w",b,y,m)*1000000;
    rEcon(b,"WaterVal",y,"annual")              = ( SUM(m, rEcon(b,"WaterVal",y,m) * rFlow(b,"InTot",y,m) )  / rFlow(b,"InTot",y,"annual")
                                                      )$rFlow(b,"InTot",y,"annual") + 0.000001;

* Save total income for checkout print
    checkoutput("totinc",sSce)                  = SUM((b,y,m), rEcon(b,"TotInc",y,m));
    checkoutput("obj",sSce)                     = TWV.l;

);
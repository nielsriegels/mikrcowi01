* Reservoir flows
    rFlow(sSce,b,"ResVol",y,m)              = SUM(s$sW(s), VOL.l(s,b,y,m));
    rFlow(sSce,b,"ResVolAvg",y,m)           = SUM(s$sW(s), VOL.l(s,b,y,m) + SUM((yy,mm)$mLast(y,m,yy,mm), VOL.l(s,b,yy,mm)) ) / 2 ;
    rFlow(sSce,b,"ResChg",y,m)              = SUM(s$sW(s), VOL.l(s,b,y,m) - SUM((yy,mm)$mLast(y,m,yy,mm), VOL.l(s,b,yy,mm)) );
    rFlow(sSce,b,"ResChg",y,m)$bResBuild(b) = -SUM(s$sW(s), -STO.l(s,b,y,m) + SUM(bd, DIS.l(s,bd,b,y,m) + FLW.l(s,bd,b,y,m)) );
    rFlow(sSce,b,"ResChg",y,m)$(sea(b) and mBgn(m)) = SUM(s$sW(s), VOL.l(s,b,y,m));

* Inflows
    rFlow(sSce,b,"InSrc",y,m)               = SUM(s$sW(s), sup(s,b,m));
    rFlow(sSce,b,"InGrw",y,m)               = GRW.l(b,y,m);
    rFlow(sSce,b,"InDis",y,m)               = SUM((s,bo)$sW(s), DIS.l(s,b,bo,y,m));
    rFlow(sSce,b,"InFlw",y,m)               = SUM((s,bo)$sW(s), FLW.l(s,b,bo,y,m));
    rFlow(sSce,b,"InItk",y,m)               = SUM((s,bo)$sW(s), ITK.l(s,b,bo,y,m));
    rFlow(sSce,b,"InRtn",y,m)               = rtn0(b,m);
    rFlow(sSce,b,"InTot",y,m)               = rFlow(sSce,b,"InSrc",y,m) + rFlow(sSce,b,"InDis",y,m) + rFlow(sSce,b,"InFlw",y,m)
                                             +rFlow(sSce,b,"InItk",y,m) + rFlow(sSce,b,"InRtn",y,m) + rFlow(sSce,b,"InGrw",y,m);
* Outflows
    rFlow(sSce,b,"OutHHI",y,m)              = -SUM((s,sSec)$sW(s), HHI.l(s,sSec,b,y,m));
    rFlow(sSce,b,"OutIrg",y,m)              = -SUM((q,j), IRG.l(b,q,j,y,m));
    rFlow(sSce,b,"OutDem",y,m)              = -SUM((q,j), IRG.l(b,q,j,y,m)) - SUM((s,sSec)$sW(s), HHI.l(s,sSec,b,y,m));
    rFlow(sSce,b,"OutSto",y,m)              = -SUM(s$sW(s), STO.l(s,b,y,m));
    rFlow(sSce,b,"OutDis",y,m)              = -SUM((s,bd)$sW(s), DIS.l(s,bd,b,y,m)$(not sea(bd)) );
    rFlow(sSce,b,"OutFlw",y,m)              = -SUM((s,bd)$sW(s), FLW.l(s,bd,b,y,m)$(not sea(bd)) );
* OBS: Maybe limit to intakes to planning zones?
    rFlow(sSce,b,"OutInt",y,m)              = -SUM((s,bd)$sW(s), ITK.l(s,bd,b,y,m)$(not sea(bd)) );
    rFlow(sSce,b,"OutLak",y,m)              = -SUM((s,bd)$sW(s), (ITK.l(s,bd,b,y,m)+FLW.l(s,bd,b,y,m)+DIS.l(s,bd,b,y,m))$sea(bd) );
    rFlow(sSce,b,"OutLos",y,m)              = -SUM((s,l)$sW(s), LOS.l(s,b,l,y,m));
    rFlow(sSce,b,"OutTot",y,m)              = rFlow(sSce,b,"OutDem",y,m) + rFlow(sSce,b,"OutDis",y,m) + rFlow(sSce,b,"OutFlw",y,m)
                                             +rFlow(sSce,b,"OutInt",y,m) + rFlow(sSce,b,"OutLos",y,m) + rFlow(sSce,b,"OutLak",y,m);
* Balance
    rFlow(sSce,b,"Balance",y,m)             = round(rFlow(sSce,b,"InTot",y,m)+rFlow(sSce,b,"OutTot",y,m)-rFlow(sSce,b,"ResChg",y,m)$bRes(b), 3) ;
    rFlow(sSce,b,e_resFlow,y,"annual")      = SUM(m, rFlow(sSce,b,e_resFlow,y,m));
    rFlow(sSce,b,"ResVol",y,"annual")       = rFlow(sSce,b,"ResVol",y,"annual") + 0.000001;
    rFlow(sSce,b,"Balance",y,"annual")      = rFlow(sSce,b,"Balance",y,"annual") + 0.000001;

* Sum up for basin sections
* - zero out internal flows within the basin section,
* - calculate cross basin section flows
    rsFlow(sSce,bs,y,e_resFlow,m)           = SUM(b$bSec(b,bs), rFlow(sSce,b,e_resFlow,y,m));
    rsFlow(sSce,bs,y,"ResVol",m)            = SUM((b,s)$sW(s), VOL.l(s,b,y,m)$(bSec(b,bs) and not sea(b)) );
    rsFlow(sSce,bs,y,"ResChg",m)            = SUM(b$bSec(b,bs), rFlow(sSce,b,"ResChg",y,m)$(not sea(b)) );
    rsFlow(sSce,bs,y,"InDis",m)             = SUM((s,bd,bo)$sW(s), DIS.l(s,bd,bo,y,m)$sXsecIn(bs,bd,bo));
    rsFlow(sSce,bs,y,"InFlw",m)             = SUM((s,bd,bo)$sW(s), FLW.l(s,bd,bo,y,m)$sXsecIn(bs,bd,bo));
    rsFlow(sSce,bs,y,"InItk",m)             = SUM((s,bd,bo)$sW(s), ITK.l(s,bd,bo,y,m)$sXsecIn(bs,bd,bo));
    rsFlow(sSce,bs,y,"InTot",m)             = rsFlow(sSce,bs,y,"InSrc",m) + rsFlow(sSce,bs,y,"InDis",m) + rsFlow(sSce,bs,y,"InFlw",m)
                                             +rsFlow(sSce,bs,y,"InItk",m) + rsFlow(sSce,bs,y,"InRtn",m) + rsFlow(sSce,bs,y,"InGrw",m);

    rsFlow(sSce,bs,y,"OutSto",m)            = -SUM((s,bo)$sW(s), STO.l(s,bo,y,m)$SUM(bd, sXsecOut(bs,bd,bo)) );
    rsFlow(sSce,bs,y,"OutDis",m)            = -SUM((s,bd,bo)$sW(s), DIS.l(s,bd,bo,y,m)$sXsecOut(bs,bd,bo));
    rsFlow(sSce,bs,y,"OutFlw",m)            = -SUM((s,bd,bo)$sW(s), FLW.l(s,bd,bo,y,m)$sXsecOut(bs,bd,bo));
    rsFlow(sSce,bs,y,"OutLak",m)            = -SUM((s,bo)$sW(s), STO.l(s,bo,y,m)$(bSec(bo,bs) AND sea(bo)) );
    rsFlow(sSce,bs,y,"OutInt",m)            = -SUM((s,bd,bo)$sW(s), ITK.l(s,bd,bo,y,m)$sXsecOut(bs,bd,bo));
    rsFlow(sSce,bs,y,"OutTot",m)            = rsFlow(sSce,bs,y,"OutDem",m) + rsFlow(sSce,bs,y,"OutDis",m) + rsFlow(sSce,bs,y,"OutFlw",m)
                                             +rsFlow(sSce,bs,y,"OutInt",m) + rsFlow(sSce,bs,y,"OutLos",m) + rsFlow(sSce,bs,y,"OutLak",m);

    rsFlow(sSce,bs,y,"TotDis",m)            = -SUM((s,bd,bo)$(sW(s) and bSec(bo,bs)), DIS.l(s,bd,bo,y,m)$(not bResNOP(bo)));


    rsFlow(sSce,bs,y,"Balance",m)           = round(rsFlow(sSce,bs,y,"InTot",m)+rsFlow(sSce,bs,y,"OutTot",m)-rsFlow(sSce,bs,y,"ResChg",m), 3) ;
    rsFlow(sSce,bs,y,e_resFlow,"annual")    = SUM(m, rsFlow(sSce,bs,y,e_resFlow,m));
    rsFlow(sSce,bs,y,"ResVol","annual")     = rsFlow(sSce,bs,y,"ResVol","annual") + 0.000001;
    rsFlow(sSce,bs,y,"Balance","annual")    = rsFlow(sSce,bs,y,"Balance","annual") + 0.000001;

* Sum up for countries
* - zero out internal flows within the basin section,
* - calculate cross basin section flows
    rcFlow(sSce,c,y,e_resFlow,m)            = SUM(b$bCty(b,c), rFlow(sSce,b,e_resFlow,y,m));
    rcFlow(sSce,c,y,"TotDis",m)             = -SUM((s,bd,bo)$(sW(s) and bCty(bo,c)), DIS.l(s,bd,bo,y,m)$(not bResNOP(bo)));
    rcFlow(sSce,c,y,"Balance",m)            = round(rcFlow(sSce,c,y,"InTot",m)+rcFlow(sSce,c,y,"OutTot",m)-rcFlow(sSce,c,y,"ResChg",m), 3) ;
    rcFlow(sSce,c,y,e_resFlow,"annual")     = SUM(m, rcFlow(sSce,c,y,e_resFlow,m));
    rcFlow(sSce,c,y,"ResVol","annual")      = rcFlow(sSce,c,y,"ResVol","annual") + 0.000001;
    rcFlow(sSce,c,y,"Balance","annual")         = rcFlow(sSce,c,y,"Balance","annual") + 0.000001;

* 
    rRbFlow(sSce,rb0,c,y,"InItk")          = SUM((s,bd,bo,m)$(sW(s) AND bCty(bd,c) AND bBas(bd,rb0)), ITK.l(s,bd,bo,y,m));
    rRbFlow(sSce,rb0,c,y,"HydroProd")      = SUM((g,b,m)$(bCty(b,c) AND bBas(b,rb0)), HPP.l(b,g,y,m))/1000;


* Agricultural line results
    rAgri(sSce,b,"WaterUse",y,j,q)            = SUM(m, IRG.l(b,q,j,y,m));
    rAgri(sSce,b,"WaterUseB",y,j,q)           = SUM(m, irg0(b,q,j,y,m));
    rAgri(sSce,b,"WaterChg",y,j,q)            = ( 100 * SUM(m, IRG.l(b,q,j,y,m))  / SUM(m, irg0(b,q,j,y,m)) - 100 )$SUM(m, irg0(b,q,j,y,m)) ;
    rAgri(sSce,b,"WaterInt",y,j,q)            = 1000 * (SUM(m, IRG.l(b,q,j,y,m))/LND.l(b,q,j,y))$LND.l(b,q,j,y);
    rAgri(sSce,b,"WaterIntB",y,j,q)           = 1000 * (SUM(m, irg0(b,q,j,y,m) )/lnd0(b,q,j)   )$lnd0(b,q,j);
    rAgri(sSce,b,"LandUse",y,j,q)             = LND.l(b,q,j,y)/1000;
    rAgri(sSce,b,"LandChg",y,j,q)$lnd0(b,q,j) = 100*LND.l(b,q,j,y)/lnd0(b,q,j)-100;
    rAgri(sSce,b,"AgriProd",y,j,q)            = CRP.l(b,q,j,y)/1000;
    rAgri(sSce,b,"YWR",y,j,q)                 = 100*YWR.l(b,q,j,y);
*    rAgri(sSce,b,"iET",y,j,q)                 = 100*YWR.l(b,q,j,y);
    rAgri(sSce,b,"ActMarg",y,j,q)$LND.l(b,q,j,y) = (pCrop(b,j)*CRP.l(b,q,j,y) - cCrop(b,j)*LND.l(b,q,j,y))/LND.l(b,q,j,y);
    rAgri(sSce,b,"OptMarg",y,j,q)$lnd0(b,q,j) = (pCrop(b,j)* crp0(b,q,j)   - cCrop(b,j) *lnd0(b,q,j)  )/ lnd0(b,q,j);
 
    rIrg(sSce,b,"ActET",y,p,j,q)$LND.l(b,q,j,y)    = 1000000*SUM(m, IRG.l(b,q,j,y,m)*etcM2p(b,j,p,m)) / LND.l(b,q,j,y);
    rIrg(sSce,b,"OptET",y,p,j,q)$lnd0(b,q,j)    = etcP(b,j,p)*1000;
    rIrg(sSce,b,"iET",y,p,j,q)$lnd0(b,q,j)      = iET.l(b,q,j,p,y)*100;
    rIrg(sSce,b,"wPhase",y,p,j,q)$lnd0(b,q,j)   = wPhase(b,j,p)*100;
    
 
* Economic results, line
    rEcon(sSce,b,"AgriSales",y,m)           = 0.000001*SUM((q,j), pCrop(b,j)*CRP.l(b,q,j,y) ) / 12;
    rEcon(sSce,b,"AgriCost",y,m)            = 0.000001*SUM((q,j), cCrop(b,j)*LND.l(b,q,j,y) ) / 12;
    rEcon(sSce,b,"AgriInc",y,m)             = rEcon(sSce,b,"AgriSales",y,m) - rEcon(sSce,b,"AgriCost",y,m);
    rEcon(sSce,b,"HydroCost",y,m)           = -reservoirs(b,"cost")$(not bResNOP(b));
    rEcon(sSce,b,"HydroInc",y,m)            = -SUM(g, HPP.l(b,g,y,m) * SUM(c$bCty(b,c), ELYMKT.m(c,g,y,m)) ) + rEcon(sSce,b,"HydroCost",y,m);
    rEcon(sSce,b,"HydroProd",y,m)           = SUM(g, HPP.l(b,g,y,m));
    rEcon(sSce,b,"HydroHead",y,m)           = HHD.l(b,y,m)$bResEly(b);
    rEcon(sSce,b,"HydroGain",y,m)           = -( 1000*rEcon(sSce,b,"HydroProd",y,m) / rFlow(sSce,b,"OutDis",y,m) )$rFlow(sSce,b,"OutDis",y,m);
    rEcon(sSce,b,"TotInc",y,m)              = rEcon(sSce,b,"AgriInc",y,m) + rEcon(sSce,b,"HydroInc",y,m);
*    rEcon(sSce,b,"DischVal",y,m)$bResEly(b) = -1000000*( rEcon(sSce,b,"HydroInc",y,m) / rFlow(sSce,b,"OutDis",y,m) )$rFlow(sSce,b,"OutDis",y,m);
*    rEcon(sSce,b,"WaterVal",y,m)            = WBALANCE.M("w",b,y,m)*1000000;
*    rEcon(sSce,b,"WaterVal",y,"annual")     = ( SUM(m, rEcon(sSce,b,"WaterVal",y,m) * rFlow(sSce,b,"InTot",y,m) ) / rFlow(sSce,b,"InTot",y,"annual")
*                                              )$rFlow(sSce,b,"InTot",y,"annual") + 0.000001;

* Aggregate results into basin sections and countries
    rsAgri(sSce,bs,y0,e_resAgri,j)          = SUM((b,q)$bSec(b,bs),     rAgri(sSce,b,e_resAgri,y0,j,q));
    rcAgri(sSce,c,y0,e_resAgri,j)           = SUM((b,q)$bCty(b,c),      rAgri(sSce,b,e_resAgri,y0,j,q));
    rcEcon(sSce,y0,e_resEcon,c)             = SUM((m,b)$bCty(b,c),  rEcon(sSce,b,e_resEcon,y0,m));
    rcEcon(sSce,y,"WaterVal",c)             = 0;
    rcEcon(sSce,y,"HydroHead",c)            = 0;
    rcEcon(sSce,y,"HydroGain",c)            = 0;
    rcEcon(sSce,y,"DischVal",c)             = 0;


$ontext
    rIrg(sSce,"irg0",b,q0,j)                = SUM((y,m), irg0(b,q0,j,y,m));
    rIrg(sSce,"IRG.l",b,q0,j)               = SUM((y,m), IRG.l(b,q0,j,y,m));
    rIrg(sSce,"IRG.l (p)",b,q0,j)           = SUM((p,y,m), etcM2P(b,j,p,m)*IRG.l(b,q0,j,y,m));
    rIrg(sSce,"dIRG.l",b,q0,j)$SUM((y,m), irg0(b,q0,j,y,m))              = SUM((y,m), IRG.l(b,q0,j,y,m))/SUM((y,m), irg0(b,q0,j,y,m));
    rIrg(sSce,"dLnd",b,q0,j)$lnd0(b,q0,j)    = SUM(y,     LND.l(b,q0,j,y))/lnd0(b,q0,j);
    rIrg(sSce,"dCrp",b,q0,j)$crp0(b,q0,j)    = SUM(y,     CRP.l(b,q0,j,y))/crp0(b,q0,j);
    rIrg1(sSce,"etcM",q0)                = SUM((b,j,m), etcM(b,j,m)*lnd0(b,q0,j));
    rIrg1(sSce,"etcP",q0)                = SUM((b,j,p), etcP(b,j,p)*lnd0(b,q0,j));
    rIrg1(sSce,"irg0",q0)                = SUM((b,j,y,m), irg0(b,q0,j,y,m));
    rIrg1(sSce,"IRG.l",q0)               = SUM((b,j,y,m), IRG.l(b,q0,j,y,m));
    rIrg1(sSce,"IRG.l (p)",q0)           = SUM((b,j,p,y,m), etcM2P(b,j,p,m)*IRG.l(b,q0,j,y,m));
    rIrg2(sSce,b,q,j,p,"eta0")                = SUM((m,y), etcM2P(b,j,p,m)*irg0(b,q,j,y,m));
    rIrg2(sSce,b,q,j,p,"etc0")                = SUM(y, etcP(b,j,p)*lnd0(b,q,j));
    rIrg2(sSce,b,q,j,p,"iet0")                = ( rIrg2(sSce,b,q,j,p,"eta0") / rIrg2(sSce,b,q,j,p,"etc0") )$rIrg2(sSce,b,q,j,p,"etc0");
    rIrg2(sSce,b,q,j,p,"iETl")                = SUM(y, iET.l(b,q,j,p,y));
$offtext



    rcElec(sSce,"qDemNot",y,c,m)        =  SUM(g,                   -DEM.l(g,c,y,m));
    rcElec(sSce,"qDemSrv",y,c,m)        =  SUM(g,                   -DEM.l(g,c,y,m) + SUM(d, USD.l(g,c,d,y,m)));
    rcElec(sSce,"qDemUsd",y,c,m)        =  SUM((g,d),               USD.l(g,c,d,y,m));
    rcElec(sSce,"qTrxNet",y,c,m)        = -SUM((g,cc),              TRX.l(c,cc,g,y,m)-TRX.l(cc,c,g,y,m)*0.9) ;
    rcElec(sSce,"qTrxSA",y,c,m)         = -SUM(g,                   XPT.l(c,g,y,m));
    rcElec(sSce,"qSupTot",y,c,m)        =  SUM((b,g)$bCty(b,c),     HPP.l(b,g,y,m)) + SUM((u,g)$tppMap(c,u), TPP.l(c,u,g,y,m));;
    rcElec(sSce,"qSupHPP",y,c,m)        =  SUM((b,g)$bCty(b,c),     HPP.l(b,g,y,m));
    rcElec(sSce,"qSupTPP",y,c,m)        =  SUM((u,g)$tppMap(c,u),   TPP.l(c,u,g,y,m));
    rcElec(sSce,"vCstTPP",y,c,m)        = -SUM((u,g)$tppMap(c,u),   TPP.l(c,u,g,y,m)*elyThermal("uFUELEX",u) )/1000 ;
    rcElec(sSce,"vCstUSD",y,c,m)        = -SUM((g,d0),              USD.l(g,c,d0,y,m)*usd0(c,g,d0))/1000 ;
*   rcElec(sSce,"vCstCPX",c,m)          = -SUM(u$uPrd(u),           TNI.l(c,u)*tpp0(c,u,"CAPX"))*TNIweight(m) ;
    rcElec(sSce,"pMCbase",y,c,m)        =                          -1000000 * ELYMKT.m(c,"base",y,m)$(-ELYMKT.m(c,"base",y,m) gt eps);
    rcElec(sSce,"pMCpeak",y,c,m)        =                          -1000000 * ELYMKT.m(c,"peak",y,m)$(-ELYMKT.m(c,"peak",y,m) gt eps);
    rcElec(sSce,"vExpCst",y,c,m)        =  SUM((g,cc),             -1000 * ELYMKT.m(c,g,y,m)    * (TRX.l(c,cc,g,y,m)-TRX.l(cc,c,g,y,m)*0.9) ) ;
    rcElec(sSce,"vExpTrx",y,c,m)        =  SUM((g,cc)$trx0(c,cc),   1000 * ELYTRX.m(c,cc,g,y,m) * (TRX.l(c,cc,g,y,m)-TRX.l(cc,c,g,y,m)*0.9) ) ;
    rcElec(sSce,"vSrpHPP",y,c,m)        =  SUM((b,g)$bCty(b,c),    -1000 * ELYMKT.m(c,g,y,m)*HPP.l(b,g,y,m)  );
*    rcElec(sSce,"vSrpTPP",y,c,m)        =  SUM((u,g)$tppMap(c,u),  -1000 * ELYMKT.m(c,g,y,m)*TPP.l(c,u,g,y,m));
    rcElec(sSce,"vTrxSA",y,c,m)         =  SUM(g,                   1000 * pXpt(c,m)*XPT.l(c,g,y,m)) / 1000000;
    


*vCS_tpp    
*vCS_hpp    
*   rcElec(sSce,"Electricity tariff (mUSD)",c,m)            = tariff(c) * SUM(u$uPrd(u), TPP.l(c,u,m))/1000;
*   rcElec(sSce,"Thermal financial costs (mUSD)",c,m)       = SUM(u$uPrd(u), TPP.l(c,u,m)*tpp0(c,u,"SRMC") + TNI.l(c,u)*tpp0(c,u,"CAPX")*TNIweight(m))/1000 ;
*   rcElec(sSce,"Thermal subsidy req. (mUSD)",c,m)          = rcElec(sSce,"Thermal financial costs (mUSD)",c,m) - rcElec(sSce,"Electricity tariff (mUSD)",c,m);
*   elyValue(c,m)                                           = SUM(d, dem(c,m)*udm(c,d,"cap")*udm(c,d,"wtp"));
*   elyValue(c,m)                                           = SUM(d, DEM.l(g,c,y,m)*udm(c,d,"cap")*udm(c,d,"wtp"));
*   rcElec(sSce,"Electricity cons. surpl. (mUSD)",c,y,m)  = 1000*(elyValue(c,m)  - SUM(d, USD.l(c,d,m) * USD0(c,g,d0)) ) / 1000000 ;
*   rcElec(sSce,"Electricity cons. surpl. (mUSD)",c,y,m)  = 1000*(DEM.l(g,c,y,m) - SUM(d, USD.l(c,d,m) * USD0(c,g,d0)) ) / 1000000 ;
*   rcElec(sSce,"Electricity cons. surpl. (mUSD)",c,y,m)  = 1000*( elyValue(c,m) - SUM(d, USD.l(c,d,m) * USD0(c,d,"wtp")) ) / 1000000 ;
*   rcElec(sSce,"Electricity cons. surpl. (mUSD)",c,y,m)  = 1000*( elyValue(c,m) - SUM(d, USD.l(c,d,m) * USD0(c,d,"wtp")) ) / 1000000 ;
*   rcElec(sSce,"Electricity value added (mUSD)",c,y,m)   = rcElec(sSce,"Electricity cons. surpl. (mUSD)",g,c,y,m) + rcElec(sSce,"Thermal economic costs (mUSD)",g,c,y,m)
*                                                             +rcElec(sSce,"External elec. export (mUSD)",g,c,y,m) ;
*   rcElec(sSce,"Total value added (mUSD)",c,y,m)         = rcElec(sSce,"Electricity value added (mUSD)",g,c,y,m) + rcElec(sSce,"Agricultural value added (mUSD)",g,c,y,m) ;
    rcElec(sSce,e_resElec,y,c,"nvg")    = SUM(m$mWin(m),            rcElec(sSce,e_resElec,y,c,m));
    rcElec(sSce,e_resElec,y,c,"veg")    = SUM(m$mSum(m),            rcElec(sSce,e_resElec,y,c,m));
    rcElec(sSce,e_resElec,y,c,"annual") = SUM(m,                    rcElec(sSce,e_resElec,y,c,m));
*    rcElec(sSce,"pMCpeak",y,c,m0)$(not m(m0))       =(SUM(m, ( SUM(b$bCty(b,c), HPP.l(b,"peak",y,m))+SUM(u, TPP.l(c,u,"peak",y,m)) ) * rcElec(sSce,"pMCpeak",y,c,m0) )
*                                                     /SUM(m, ( SUM(b$bCty(b,c), HPP.l(b,"peak",y,m))+SUM(u, TPP.l(c,u,"peak",y,m)) ) ) )$rcElec(sSce,"pMCpeak",y,c,m0);
*    rcElec(sSce,"pMCbase",y,c,m0)$(not m(m0))       =(SUM(m, ( SUM(b$bCty(b,c), HPP.l(b,"base",y,m))+SUM(u, TPP.l(c,u,"base",y,m)) ) * rcElec(sSce,"pMCbase",y,c,m0) )
*                                                     /SUM(m, ( SUM(b$bCty(b,c), HPP.l(b,"base",y,m))+SUM(u, TPP.l(c,u,"base",y,m)) ) ) )$rcElec(sSce,"pMCbase",y,c,m0);


* Create data sheet for balance charts
    rcElecBal(sSce,y,c,mS,"qSupHPP")   = SUM(m$mSm(m,mS),          rcElec(sSce,"qSupHPP",y,c,m));
    rcElecBal(sSce,y,c,mS,"qSupTPP")   = SUM(m$mSm(m,mS),          rcElec(sSce,"qSupTPP",y,c,m));
*    rcElecBal(sSce,y,c,mS,"qSupTPN")   = SUM(m$mSm(m,mS),          rcElec(sSce,"qSupTPN",y,c,m));
    rcElecBal(sSce,y,c,mS,"qDemNot")   = SUM(m$mSm(m,mS),          rcElec(sSce,"qDemNot",y,c,m));
    rcElecBal(sSce,y,c,mS,"qDemUsd")   = SUM(m$mSm(m,mS),          rcElec(sSce,"qDemUsd",y,c,m));
    rcElecBal(sSce,y,c,mS,"qTrxNet")   = SUM(m$mSm(m,mS),          rcElec(sSce,"qTrxNet",y,c,m));
    rcElecBal(sSce,y,c,mS,"qTrxSA")    = SUM(m$mSm(m,mS),          rcElec(sSce,"qTrxSA" ,y,c,m));
    rcElecBal(sSce,y,c,mS,"pMCbase")    = SUM(m$mSm(m,mS),          rcElec(sSce,"pMCbase" ,y,c,m));
    rcElecBal(sSce,y,c,mS,"pMCpeak")    = SUM(m$mSm(m,mS),          rcElec(sSce,"pMCpeak" ,y,c,m));
    


    rBorder(sSce,y,"Amu-TAD-TUR",m)      = SUM((s)$sW(s),               FLW.l(s,"AMUMID","AMUUPS",y,m));
    rBorder(sSce,y,"Amu-MID-DWN",m)      = SUM((s)$sW(s),               FLW.l(s,"AMUDWN","Res_TMR",y,m) + DIS.l(s,"AMUDWN","Res_TMR",y,m)) ;
    rBorder(sSce,y,"Syr-KYR-UZB",m)      = SUM((s)$sW(s),               FLW.l(s,"NARYN","Res_NA2",y,m)  + DIS.l(s,"NARYN","Res_NA2",y,m));
    rBorder(sSce,y,"Syr-UZB-TAD",m)      = SUM((s)$sW(s),               FLW.l(s,"Res_KAR","SYRFER",y,m) );
    rBorder(sSce,y,"Syr-TAD-UZB",m)      = SUM((s)$sW(s),               FLW.l(s,"Res_FAR","Res_KAR",y,m) + DIS.l(s,"Res_FAR","Res_KAR",y,m));
    rBorder(sSce,y,"Syr-UZB-KAZ",m)      = SUM((s)$sW(s),               ITK.l(s,"Res_SHA","SYRMID",y,m) + FLW.l(s,"Res_SHA","SYRMID",y,m) - ITK.l(s,"SYR_KAZ","SYRMID",y,m));
    rBorder(sSce,y,"DwnToktogul",m)      = SUM((s)$sW(s),               DIS.l(s,"Res_NA2","Res_TOK",y,m)+FLW.l(s,"Res_NA2","Res_TOK",y,m));
    rBorder(sSce,y,"DwnNurek",m)         = SUM((s)$sW(s),               DIS.l(s,"Res_VA1","Res_NUR",y,m)+FLW.l(s,"Res_VA1","Res_NUR",y,m));

    rcView(sSce,y,"AgriSales",c)            =  SUM((b,q,j)$bCty(b,c),       CRP.l(b,q,j,y) * pCrop(b,j)  ) / 1000000;
    rcView(sSce,y,"AgriCosts",c)            = -SUM((b,q,j)$bCty(b,c),       LND.l(b,q,j,y) * cCrop(b,j)  ) / 1000000;
    rcView(sSce,y,"AgriVA",c)               = rcView(sSce,y,"AgriSales",c) - rcView(sSce,y,"AgriCosts",c);
    rcView(sSce,y,"AgriCot",c)              = SUM((b,q,j)$bCty(b,c),        pCrop(b,j)*CRP.l(b,q,j,y)$cot(j))/1000000;
    rcView(sSce,y,"AgriWht",c)              = SUM((b,q,j)$bCty(b,c),        pCrop(b,j)*CRP.l(b,q,j,y)$wht(j))/1000000;
    rcView(sSce,y,"AgriAlf",c)              = SUM((b,q,j)$bCty(b,c),        pCrop(b,j)*CRP.l(b,q,j,y)$alf(j))/1000000;
    rcView(sSce,y,"AgriVeg",c)              = SUM((b,q,j)$bCty(b,c),        pCrop(b,j)*CRP.l(b,q,j,y)$veg(j))/1000000;
    rcView(sSce,y,"AgriRic",c)              = SUM((b,q,j)$bCty(b,c),        pCrop(b,j)*CRP.l(b,q,j,y)$ric(j))/1000000;
    rcView(sSce,y,"AgriFru",c)              = SUM((b,q,j)$bCty(b,c),        pCrop(b,j)*CRP.l(b,q,j,y)$fru(j))/1000000;
    rcView(sSce,y,"AgriOth",c)              = SUM((b,q,j)$bCty(b,c),        pCrop(b,j)*CRP.l(b,q,j,y)$oth(j))/1000000;
    rcView(sSce,y,"AgriWater",c)            = SUM((b,q,j,m)$bCty(b,c),      IRG.l(b,q,j,y,m));
    rcView(sSce,y,"AgriWaterSum",c)         = SUM((b,q,j,m)$bCty(b,c),      IRG.l(b,q,j,y,m)$mSum(m));
    rcView(sSce,y,"AgriWaterWin",c)         = SUM((b,q,j,m)$bCty(b,c),      IRG.l(b,q,j,y,m)$mWin(m));
    rcView(sSce,y,"AgriWaterInt",c)         = ( SUM((b,q,j,m)$bCty(b,c),    IRG.l(b,q,j,y,m)*1000000$mWin(m))  / SUM((b,q,j)$bCty(b,c), LND.l(b,q,j,y)) )$SUM((b,q,j)$bCty(b,c), LND.l(b,q,j,y));
    rcView(sSce,y,"AgriLandChg",c)          = ( SUM((b,q,j)$bCty(b,c),      LND.l(b,q,j,y)*100      )  / SUM((b,q,j)$bCty(b,c), LND.l(b,q,j,y)) - 100   )$SUM((b,q,j)$bCty(b,c), LND.l(b,q,j,y));
    rcView(sSce,y,"HHIuse",c)               = SUM((s,b,sSec,m)$bCty(b,c),   HHI.l(s, sSec,b,y,m)$sW(s));
    rcView(sSce,y,"Intake",c)               = SUM((rb,s), SUM((bd,bo,m)$(bNukus(bo,rb) and bCty(bd,c)),             ITK.l(s,bd,bo,y,m)+FLW.l(s,bd,bo,y,m)+DIS.l(s,bd,bo,y,m)  )); 
    rcView(sSce,y,"Nukus",c)                = SUM((rb,s), SUM((bd,bo,m)$(bNukus(bd,rb) and (not bNukus(bo,rb)) ),   ITK.l(s,bd,bo,y,m)+FLW.l(s,bd,bo,y,m)+DIS.l(s,bd,bo,y,m)  )*sNukus(c,rb)) 
                                             +SUM((rb,s), SUM((bd,m)$bNukus(bd,rb), rtn0(bd,m))*sNukus(c,rb));
    




    rcView(sSce,y,"Losses",c)               = SUM((s,b,l,m)$bCty(b,c),      LOS.l(s,b,l,y,m)$sW(s));
    rcView(sSce,y,"Nature",c)               = SUM((s,b,m)$bCty(b,c),        STO.l(s,b,y,m)$(bNat(b) AND sW(s)) );
    rcView(sSce,y,"Amu-TAD-TUR","TAD")      =-SUM(m, rBorder(sSce,y,"AMU-TAD-TUR",m));
    rcView(sSce,y,"Amu-TAD-TUR","TUR")      = SUM(m, rBorder(sSce,y,"AMU-TAD-TUR",m));
    rcView(sSce,y,"Amu-MID-DWN","TUR")      =-SUM(m, rBorder(sSce,y,"Amu-MID-DWN",m));
    rcView(sSce,y,"Amu-MID-DWN","UZB")      = SUM(m, rBorder(sSce,y,"Amu-MID-DWN",m));
    rcView(sSce,y,"Syr-KYR-UZB","KYR")      =-SUM(m, rBorder(sSce,y,"Syr-KYR-UZB",m));
    rcView(sSce,y,"Syr-KYR-UZB","UZB")      = SUM(m, rBorder(sSce,y,"Syr-KYR-UZB",m));
    rcView(sSce,y,"Syr-UZB-TAD","UZB")      =-SUM(m, rBorder(sSce,y,"Syr-UZB-TAD",m));
    rcView(sSce,y,"Syr-UZB-TAD","TAD")      = SUM(m, rBorder(sSce,y,"Syr-UZB-TAD",m));
    rcView(sSce,y,"Syr-TAD-UZB","TAD")      =-SUM(m, rBorder(sSce,y,"Syr-TAD-UZB",m));
    rcView(sSce,y,"Syr-TAD-UZB","UZB")      = SUM(m, rBorder(sSce,y,"Syr-TAD-UZB",m));
    rcView(sSce,y,"Syr-UZB-KAZ","UZB")      =-SUM(m, rBorder(sSce,y,"Syr-UZB-KAZ",m));
    rcView(sSce,y,"Syr-UZB-KAZ","KAZ")      = SUM(m, rBorder(sSce,y,"Syr-UZB-KAZ",m));
    rcView(sSce,y,"cTherm",c)               = -SUM((u,g,m)$tppMap(c,u),     TPP.l(c,u,g,y,m)*elyThermal("uFUELEX",u)*1000 )/1000000;
    rcView(sSce,y,"iTherm",c)               = 0;
    rcView(sSce,y,"sTherm",c)               = 0;
    rcView(sSce,y,"cField",c)               = 0;
    rcView(sSce,y,"iField",c)               = 0;
    rcView(sSce,y,"cConvey",c)              = 0;
    rcView(sSce,y,"iConvey",c)              = 0;
    rcView(sSce,y,"cHydro",c)               = 0;
    rcView(sSce,y,"iHydro",c)               = SUM(b$bCty(b,c),              resNewOp(b,sSce)*reservoirs(b,"cost") );
    rcView(sSce,y,"qTherm",c)               = SUM((u,g,m)$tppMap(c,u),      TPP.l(c,u,g,y,m));
    rcView(sSce,y,"qThermWin",c)            = SUM((u,g,m)$tppMap(c,u),      TPP.l(c,u,g,y,m)$mWin(m));
    rcView(sSce,y,"qThermSum",c)            = SUM((u,g,m)$tppMap(c,u),      TPP.l(c,u,g,y,m)$mSum(m));
    rcView(sSce,y,"qHydro",c)               = SUM((b,g,m)$bCty(b,c),        HPP.l(b,g,y,m));
    rcView(sSce,y,"qHydroWin",c)            = SUM((b,g,m)$bCty(b,c),        HPP.l(b,g,y,m)$mWin(m));
    rcView(sSce,y,"qHydroSum",c)            = SUM((b,g,m)$bCty(b,c),        HPP.l(b,g,y,m)$mSum(m));
    rcView(sSce,y,"qElyDem",c)              = SUM((g,m)$elyDemand0(c,g,m),  DEM.l(g,c,y,m));
    rcView(sSce,y,"qElySrvd",c)             = SUM((g,m)$elyDemand0(c,g,m),  DEM.l(g,c,y,m)-SUM(d, USD.l(g,c,d,y,m)));
    rcView(sSce,y,"qElyUsrv",c)             = SUM((g,m)$elyDemand0(c,g,m),  SUM(d, USD.l(g,c,d,y,m)));
    rcView(sSce,y,"qElyUSDw",c)             = SUM((g,m)$elyDemand0(c,g,m),  SUM(d, USD.l(g,c,d,y,m))$mWin(m));
    rcView(sSce,y,"qElyUSDs",c)             = SUM((g,m)$elyDemand0(c,g,m),  SUM(d, USD.l(g,c,d,y,m))$mSum(m));

    rcView(sSce,y,"usdVal",c)               =-SUM((d,g,m)$usd0(c,g,d),      usd0(c,g,d) * 1000 * USD.l(g,c,d,y,m) ) / 1000000;
    rcView(sSce,y,"aggWTP",c)               = SUM((d,g,m)$usd0(c,g,d),      usd0(c,g,d) * 30 * elyHours(c,g,m) * elyDemand0(c,g,m) * usdSh(d) ) / 1000000 + rcView(sSce,y,"usdVal",c) ;
    rcView(sSce,y,"demVal",c)               = SUM((g,m)$elyDemand0(c,g,m),  -1000*ELYMKT.m(c,g,y,m) * DEM.l(g,c,y,m)$(-ELYMKT.m(c,g,y,m) gt 0.000000001) );
    rcView(sSce,y,"tppVal",c)               = SUM((g,m),                    -1000*ELYMKT.m(c,g,y,m) * SUM(u$tppMap(c,u), TPP.l(c,u,g,y,m))$(-ELYMKT.m(c,g,y,m) gt 0.000000001) );
    rcView(sSce,y,"expVal",c)               = SUM((g,m),                    1000 * pXpt(c,m)*XPT.l(c,g,y,m)) / 1000000;

    rcView(sSce,y,"elyCS",c)                = rcView(sSce,y,"aggWTP",c) + rcView(sSce,y,"usdVal",c) - rcView(sSce,y,"demVAL",c);
    rcView(sSce,y,"tppPS",c)                = rcView(sSce,y,"tppVal",c) + rcView(sSce,y,"cTherm",c);
    rcView(sSce,y,"elyPS",c)                = rcView(sSce,y,"demVAL",c) + rcView(sSce,y,"cTherm",c) + rcView(sSce,y,"cHydro",c) + rcView(sSce,y,"expVal",c);

    rcView(sSce,y,"Invest",c)               = rcView(sSce,y,"iField",c) + rcView(sSce,y,"iConvey",c) + rcView(sSce,y,"iHydro",c) + rcView(sSce,y,"iTherm",c) ;
    rcView(sSce,y,"OandM",c)                = rcView(sSce,y,"cField",c) + rcView(sSce,y,"cConvey",c) + rcView(sSce,y,"cHydro",c);
    rcView(sSce,y,"Surplus",c)              = rcView(sSce,y,"AgriVA",c) + rcView(sSce,y,"elyCS",c) + rcView(sSce,y,"elyPS",c) - rcView(sSce,y,"Invest",c) - rcView(sSce,y,"OandM",c);




    rcRbView(sSce,y,"AgriSales",rb0,c)           =  SUM((b,q,j)$(bCty(b,c)AND bBas(b,rb0)),       CRP.l(b,q,j,y) * pCrop(b,j)  ) / 1000000;
    rcRbView(sSce,y,"AgriCosts",rb0,c)           = -SUM((b,q,j)$(bCty(b,c)AND bBas(b,rb0)),       LND.l(b,q,j,y) * cCrop(b,j)  ) / 1000000;
    rcRbView(sSce,y,"AgriVA",rb0,c)              = rcRbView(sSce,y,"AgriSales",rb0,c) - rcRbView(sSce,y,"AgriCosts",rb0,c);
    rcRbView(sSce,y,"AgriCot",rb0,c)             = SUM((b,q,j)$(bCty(b,c)AND bBas(b,rb0)),        pCrop(b,j)*CRP.l(b,q,j,y)$cot(j))/1000000;
    rcRbView(sSce,y,"AgriWht",rb0,c)              = SUM((b,q,j)$(bCty(b,c)AND bBas(b,rb0)),        pCrop(b,j)*CRP.l(b,q,j,y)$wht(j))/1000000;
    rcRbView(sSce,y,"AgriAlf",rb0,c)              = SUM((b,q,j)$(bCty(b,c)AND bBas(b,rb0)),        pCrop(b,j)*CRP.l(b,q,j,y)$alf(j))/1000000;
    rcRbView(sSce,y,"AgriVeg",rb0,c)              = SUM((b,q,j)$(bCty(b,c)AND bBas(b,rb0)),        pCrop(b,j)*CRP.l(b,q,j,y)$veg(j))/1000000;
    rcRbView(sSce,y,"AgriRic",rb0,c)              = SUM((b,q,j)$(bCty(b,c) AND bBas(b,rb0)),        pCrop(b,j)*CRP.l(b,q,j,y)$ric(j))/1000000;
    rcRbView(sSce,y,"AgriFru",rb0,c)              = SUM((b,q,j)$(bCty(b,c) AND bBas(b,rb0)),        pCrop(b,j)*CRP.l(b,q,j,y)$fru(j))/1000000;
    rcRbView(sSce,y,"AgriOth",rb0,c)              = SUM((b,q,j)$(bCty(b,c) AND bBas(b,rb0)),        pCrop(b,j)*CRP.l(b,q,j,y)$oth(j))/1000000;
    rcRbView(sSce,y,"AgriWater",rb0,c)            = SUM((b,q,j,m)$(bCty(b,c) AND bBas(b,rb0)),      IRG.l(b,q,j,y,m));
    rcRbView(sSce,y,"AgriWaterSum",rb0,c)         = SUM((b,q,j,m)$(bCty(b,c) AND bBas(b,rb0)),      IRG.l(b,q,j,y,m)$mSum(m));
    rcRbView(sSce,y,"AgriWaterWin",rb0,c)         = SUM((b,q,j,m)$(bCty(b,c) AND bBas(b,rb0)),      IRG.l(b,q,j,y,m)$mWin(m));
    rcRbView(sSce,y,"AgriWaterInt",rb0,c)         = ( SUM((b,q,j,m)$(bCty(b,c) AND bBas(b,rb0)),    IRG.l(b,q,j,y,m)*1000000$mWin(m))  / SUM((b,q,j)$(bCty(b,c) AND bBas(b,rb0)), LND.l(b,q,j,y)) )$SUM((b,q,j)$(bCty(b,c) AND bBas(b,rb0)), LND.l(b,q,j,y));
    rcRbView(sSce,y,"AgriLandChg",rb0,c)          = ( SUM((b,q,j)$(bCty(b,c) AND bBas(b,rb0)),      LND.l(b,q,j,y)*100      )  / SUM((b,q,j)$(bCty(b,c) AND bBas(b,rb0)), LND.l(b,q,j,y)) - 100   )$SUM((b,q,j)$(bCty(b,c) AND bBas(b,rb0)), LND.l(b,q,j,y));
    rcRbView(sSce,y,"HHIuse",rb0,c)               = SUM((s,b,sSec,m)$(bCty(b,c) AND bBas(b,rb0)),   HHI.l(s, sSec,b,y,m)$sW(s));
*    rcRbView(sSce,y,"Intake",rb,c)               = SUM((s,bd,bo,m)$(bNukus(bo,rb) and (not bNukus(bd,rb)) and bCty(bd,c) and bPlz(bd)),  
*                                                    (ITK.l(s,bd,bo,y,m)+FLW.l(s,bd,bo,y,m)+DIS.l(s,bd,bo,y,m))$(bBas(bd,rb) and bCty(bd,c))  );
    rcRbView(sSce,y,"Intake",rb,c)               = SUM((s,bd,bo,m)$(bCty(bd,c) and bPlz(bd)),  
                                                    (ITK.l(s,bd,bo,y,m)+FLW.l(s,bd,bo,y,m)+DIS.l(s,bd,bo,y,m))$(bBas(bd,rb) and bCty(bd,c))  );
    rcRbView(sSce,y,"Nukus",rb,c)                = SUM((s,bd,bo,m)$(bNukus(bd,rb) and (not bNukus(bo,rb)) ),  
                                                    (ITK.l(s,bd,bo,y,m)+FLW.l(s,bd,bo,y,m)+DIS.l(s,bd,bo,y,m))$(bBas(bd,rb)) * sNukus(c,rb) )
                                                  +SUM((b,m)$bNukus(b,rb), rtn0(b,m)*sNukus(c,rb)); 



    rcRbView(sSce,y,"Losses",rb0,c)               = SUM((s,b,l,m)$(bCty(b,c) AND bBas(b,rb0) and sW(s)),      LOS.l(s,b,l,y,m)$(not bPlz(b)));
    rcRbView(sSce,y,"Nature",rb0,c)               = SUM((s,b,m)$(bCty(b,c) AND bBas(b,rb0)),        STO.l(s,b,y,m)$(bNat(b) AND sW(s)) );
    rcRbView(sSce,y,"Amu-TAD-TUR","AMU","TAD")      =-SUM(m, rBorder(sSce,y,"AMU-TAD-TUR",m));
    rcRbView(sSce,y,"Amu-TAD-TUR","AMU","TUR")      = SUM(m, rBorder(sSce,y,"AMU-TAD-TUR",m));
    rcRbView(sSce,y,"Amu-MID-DWN","AMU","TUR")      =-SUM(m, rBorder(sSce,y,"Amu-MID-DWN",m));
    rcRbView(sSce,y,"Amu-MID-DWN","AMU","UZB")      = SUM(m, rBorder(sSce,y,"Amu-MID-DWN",m));
    rcRbView(sSce,y,"Syr-KYR-UZB","SYR","KYR")      =-SUM(m, rBorder(sSce,y,"Syr-KYR-UZB",m));
    rcRbView(sSce,y,"Syr-KYR-UZB","SYR","UZB")      = SUM(m, rBorder(sSce,y,"Syr-KYR-UZB",m));
    rcRbView(sSce,y,"Syr-UZB-TAD","SYR","UZB")      =-SUM(m, rBorder(sSce,y,"Syr-UZB-TAD",m));
    rcRbView(sSce,y,"Syr-UZB-TAD","SYR","TAD")      = SUM(m, rBorder(sSce,y,"Syr-UZB-TAD",m));
    rcRbView(sSce,y,"Syr-TAD-UZB","SYR","TAD")      =-SUM(m, rBorder(sSce,y,"Syr-TAD-UZB",m));
    rcRbView(sSce,y,"Syr-TAD-UZB","SYR","UZB")      = SUM(m, rBorder(sSce,y,"Syr-TAD-UZB",m));
    rcRbView(sSce,y,"Syr-UZB-KAZ","SYR","UZB")      =-SUM(m, rBorder(sSce,y,"Syr-UZB-KAZ",m));
    rcRbView(sSce,y,"Syr-UZB-KAZ","SYR","KAZ")      = SUM(m, rBorder(sSce,y,"Syr-UZB-KAZ",m));
    rcRbView(sSce,y,"cTherm",rb0,c)                 = -SUM((u,g,m)$tppMap(c,u),     TPP.l(c,u,g,y,m)*elyThermal("uFUELEX",u)*1000 )/1000000;
    rcRbView(sSce,y,"iTherm",rb0,c)                 = 0;
    rcRbView(sSce,y,"sTherm",rb0,c)                 = 0;
    rcRbView(sSce,y,"cField",rb0,c)                 = 0;
    rcRbView(sSce,y,"iField",rb0,c)                 = 0;
    rcRbView(sSce,y,"cConvey",rb0,c)                = 0;
    rcRbView(sSce,y,"iConvey",rb0,c)                = 0;
    rcRbView(sSce,y,"cHydro",rb0,c)                 = 0;
    rcRbView(sSce,y,"iHydro",rb0,c)                 = SUM(b$(bCty(b,c) AND bBas(b,rb0)),              resNewOp(b,sSce)*reservoirs(b,"cost") );
    rcRbView(sSce,y,"qTherm",rb0,c)                 = SUM((u,g,m)$tppMap(c,u),      TPP.l(c,u,g,y,m));
    rcRbView(sSce,y,"qThermWin",rb0,c)              = SUM((u,g,m)$tppMap(c,u),      TPP.l(c,u,g,y,m)$mWin(m));
    rcRbView(sSce,y,"qThermSum",rb0,c)              = SUM((u,g,m)$tppMap(c,u),      TPP.l(c,u,g,y,m)$mSum(m));
    rcRbView(sSce,y,"qHydro",rb0,c)                 = SUM((b,g,m)$(bCty(b,c) AND bBas(b,rb0)),        HPP.l(b,g,y,m));
    rcRbView(sSce,y,"qHydroWin",rb0,c)              = SUM((b,g,m)$(bCty(b,c) AND bBas(b,rb0)),        HPP.l(b,g,y,m)$mWin(m));
    rcRbView(sSce,y,"qHydroSum",rb0,c)              = SUM((b,g,m)$(bCty(b,c) AND bBas(b,rb0)),        HPP.l(b,g,y,m)$mSum(m));
    rcRbView(sSce,y,"qElyDem",rb0,c)                = SUM((g,m)$elyDemand0(c,g,m),  DEM.l(g,c,y,m));
    rcRbView(sSce,y,"qElySrvd",rb0,c)               = SUM((g,m)$elyDemand0(c,g,m),  DEM.l(g,c,y,m)-SUM(d, USD.l(g,c,d,y,m)));
    rcRbView(sSce,y,"qElyUsrv",rb0,c)               = SUM((g,m)$elyDemand0(c,g,m),  SUM(d, USD.l(g,c,d,y,m)));
    rcRbView(sSce,y,"qElyUSDw",rb0,c)               = SUM((g,m)$elyDemand0(c,g,m),  SUM(d, USD.l(g,c,d,y,m))$mWin(m));
    rcRbView(sSce,y,"qElyUSDs",rb0,c)               = SUM((g,m)$elyDemand0(c,g,m),  SUM(d, USD.l(g,c,d,y,m))$mSum(m));

    rcRbView(sSce,y,"usdVal",rb0,c)                 =-SUM((d,g,m)$usd0(c,g,d),      usd0(c,g,d) * 1000 * USD.l(g,c,d,y,m) ) / 1000000;
    rcRbView(sSce,y,"aggWTP",rb0,c)                 = SUM((d,g,m)$usd0(c,g,d),      usd0(c,g,d) * 30 * elyHours(c,g,m) * elyDemand0(c,g,m) * usdSh(d) ) / 1000000 + rcRbView(sSce,y,"usdVal",rb0,c) ;
    rcRbView(sSce,y,"demVal",rb0,c)                 = SUM((g,m)$elyDemand0(c,g,m),  -1000*ELYMKT.m(c,g,y,m) * DEM.l(g,c,y,m)$(-ELYMKT.m(c,g,y,m) gt 0.000000001) );
    rcRbView(sSce,y,"tppVal",rb0,c)                 = SUM((g,m),                    -1000*ELYMKT.m(c,g,y,m) * SUM(u$tppMap(c,u), TPP.l(c,u,g,y,m))$(-ELYMKT.m(c,g,y,m) gt 0.000000001) );
    rcRbView(sSce,y,"expVal",rb0,c)                 = SUM((g,m),                    1000 * pXpt(c,m)*XPT.l(c,g,y,m)) / 1000000;

    rcRbView(sSce,y,"elyCS",rb0,c)                  = rcRbView(sSce,y,"aggWTP",rb0,c) + rcRbView(sSce,y,"usdVal",rb0,c) - rcRbView(sSce,y,"demVAL",rb0,c);
    rcRbView(sSce,y,"tppPS",rb0,c)                  = rcRbView(sSce,y,"tppVal",rb0,c) + rcRbView(sSce,y,"cTherm",rb0,c);
    rcRbView(sSce,y,"elyPS",rb0,c)                  = rcRbView(sSce,y,"demVAL",rb0,c) + rcRbView(sSce,y,"cTherm",rb0,c) + rcRbView(sSce,y,"cHydro",rb0,c) + rcRbView(sSce,y,"expVal",rb0,c);

    rcRbView(sSce,y,"Invest",rb0,c)                 = rcRbView(sSce,y,"iField",rb0,c) + rcRbView(sSce,y,"iConvey",rb0,c) + rcRbView(sSce,y,"iHydro",rb0,c) + rcRbView(sSce,y,"iTherm",rb0,c) ;
    rcRbView(sSce,y,"OandM",rb0,c)                  = rcRbView(sSce,y,"cField",rb0,c) + rcRbView(sSce,y,"cConvey",rb0,c) + rcRbView(sSce,y,"cHydro",rb0,c);
    rcRbView(sSce,y,"Surplus",rb0,c)                = rcRbView(sSce,y,"AgriVA",rb0,c) + rcRbView(sSce,y,"elyCS",rb0,c) + rcRbView(sSce,y,"elyPS",rb0,c) - rcRbView(sSce,y,"Invest",rb0,c) - rcRbView(sSce,y,"OandM",rb0,c);


***********************************************
***********************************************

    rSView(y,"AgriSales",sSce)            =  SUM((b,q,j),       CRP.l(b,q,j,y) * pCrop(b,j)  ) / 1000000;
    rSView(y,"AgriCosts",sSce)            = -SUM((b,q,j),       LND.l(b,q,j,y) * cCrop(b,j)  ) / 1000000;
    rSView(y,"AgriVA",sSce)               = rSView(y,"AgriSales",sSce) - rSView(y,"AgriCosts",sSce);
    rSView(y,"AgriCot",sSce)              = SUM((b,q,j),        pCrop(b,j)*CRP.l(b,q,j,y)$cot(j))/1000000;
    rSView(y,"AgriWht",sSce)              = SUM((b,q,j),        pCrop(b,j)*CRP.l(b,q,j,y)$wht(j))/1000000;
    rSView(y,"AgriAlf",sSce)              = SUM((b,q,j),        pCrop(b,j)*CRP.l(b,q,j,y)$alf(j))/1000000;
    rSView(y,"AgriVeg",sSce)              = SUM((b,q,j),        pCrop(b,j)*CRP.l(b,q,j,y)$veg(j))/1000000;
    rSView(y,"AgriRic",sSce)              = SUM((b,q,j),        pCrop(b,j)*CRP.l(b,q,j,y)$ric(j))/1000000;
    rSView(y,"AgriFru",sSce)              = SUM((b,q,j),        pCrop(b,j)*CRP.l(b,q,j,y)$fru(j))/1000000;
    rSView(y,"AgriOth",sSce)              = SUM((b,q,j),        pCrop(b,j)*CRP.l(b,q,j,y)$oth(j))/1000000;
    rSView(y,"AgriWater",sSce)            = SUM((b,q,j,m),      IRG.l(b,q,j,y,m));
    rSView(y,"AgriWaterSum",sSce)         = SUM((b,q,j,m),      IRG.l(b,q,j,y,m)$mSum(m));
    rSView(y,"AgriWaterWin",sSce)         = SUM((b,q,j,m),      IRG.l(b,q,j,y,m)$mWin(m));
    rSView(y,"AgriWaterInt",sSce)         = ( SUM((b,q,j,m),    IRG.l(b,q,j,y,m)*1000000$mWin(m))  / SUM((b,q,j), LND.l(b,q,j,y)) )$SUM((b,q,j), LND.l(b,q,j,y));
    rSView(y,"AgriLandChg",sSce)          = ( SUM((b,q,j),      LND.l(b,q,j,y)*100      )  / SUM((b,q,j), LND.l(b,q,j,y)) - 100   )$SUM((b,q,j), LND.l(b,q,j,y));
    rSView(y,"HHIuse",sSce)               = SUM((s,b,sSec,m),   HHI.l(s, sSec,b,y,m)$sW(s));
    rSView(y,"Intake",sSce)               = SUM((s,rb,bd,bo,m)$bPlz(bd),    (ITK.l(s,bd,bo,y,m)+FLW.l(s,bd,bo,y,m)+DIS.l(s,bd,bo,y,m))$(bBas(bd,rb))  );
    rSView(y,"Nukus",sSce)                = SUM((s,rb,bd,bo,m,c)$bPlz(bd),  (ITK.l(s,bd,bo,y,m)+FLW.l(s,bd,bo,y,m)+DIS.l(s,bd,bo,y,m))$(bBas(bd,rb)) * sNukus(c,rb)  );
    rSView(y,"Losses",sSce)               = SUM((s,b,l,m),      LOS.l(s,b,l,y,m)$sW(s));
    rSView(y,"Nature",sSce)               = SUM((s,b,m),        STO.l(s,b,y,m)$(bNat(b) AND sW(s)) );
    rSView(y,"cTherm",sSce)               = -SUM((u,g,m,c)$tppMap(c,u),     TPP.l(c,u,g,y,m)*elyThermal("uFUELEX",u)*1000 )/1000000;
    rSView(y,"iTherm",sSce)               = 0;
    rSView(y,"sTherm",sSce)               = 0;
    rSView(y,"cField",sSce)               = 0;
    rSView(y,"iField",sSce)               = 0;
    rSView(y,"cConvey",sSce)              = 0;
    rSView(y,"iConvey",sSce)              = 0;
    rSView(y,"cHydro",sSce)               = 0;
    rSView(y,"iHydro",sSce)               = SUM(b,                resNewOp(b,sSce)*reservoirs(b,"cost") );
    rSView(y,"qTherm",sSce)               = SUM((u,g,m,c)$tppMap(c,u),      TPP.l(c,u,g,y,m));
    rSView(y,"qThermWin",sSce)            = SUM((u,g,m,c)$tppMap(c,u),      TPP.l(c,u,g,y,m)$mWin(m));
    rSView(y,"qThermSum",sSce)            = SUM((u,g,m,c)$tppMap(c,u),      TPP.l(c,u,g,y,m)$mSum(m));
    rSView(y,"qHydro",sSce)               = SUM((b,g,m),        HPP.l(b,g,y,m));
    rSView(y,"qHydroWin",sSce)            = SUM((b,g,m),        HPP.l(b,g,y,m)$mWin(m));
    rSView(y,"qHydroSum",sSce)            = SUM((b,g,m),        HPP.l(b,g,y,m)$mSum(m));
    rSView(y,"qElyDem",sSce)              = SUM((g,m,c)$elyDemand0(c,g,m),  DEM.l(g,c,y,m));
    rSView(y,"qElySrvd",sSce)             = SUM((g,m,c)$elyDemand0(c,g,m),  DEM.l(g,c,y,m)-SUM(d, USD.l(g,c,d,y,m)));
    rSView(y,"qElyUsrv",sSce)             = SUM((g,m,c)$elyDemand0(c,g,m),  SUM(d, USD.l(g,c,d,y,m)));
    rSView(y,"qElyUSDw",sSce)             = SUM((g,m,c)$elyDemand0(c,g,m),  SUM(d, USD.l(g,c,d,y,m))$mWin(m));
    rSView(y,"qElyUSDs",sSce)             = SUM((g,m,c)$elyDemand0(c,g,m),  SUM(d, USD.l(g,c,d,y,m))$mSum(m));

    rSView(y,"usdVal",sSce)               =-SUM((d,g,m,c)$usd0(c,g,d),      usd0(c,g,d) * 1000 * USD.l(g,c,d,y,m) ) / 1000000;
    rSView(y,"aggWTP",sSce)               = SUM((d,g,m,c)$usd0(c,g,d),      usd0(c,g,d) * 30 * elyHours(c,g,m) * elyDemand0(c,g,m) * usdSh(d) ) / 1000000 + rSView(y,"usdVal",sSce) ;
    rSView(y,"demVal",sSce)               = SUM((g,m,c)$elyDemand0(c,g,m),  -1000*ELYMKT.m(c,g,y,m) * DEM.l(g,c,y,m)$(-ELYMKT.m(c,g,y,m) gt 0.000000001) );
    rSView(y,"tppVal",sSce)               = SUM((g,m,c),                    -1000*ELYMKT.m(c,g,y,m) * SUM(u$tppMap(c,u), TPP.l(c,u,g,y,m))$(-ELYMKT.m(c,g,y,m) gt 0.000000001) );
    rSView(y,"expVal",sSce)               = SUM((g,m,c),                    1000 * pXpt(c,m)*XPT.l(c,g,y,m)) / 1000000;

    rSView(y,"elyCS",sSce)                = rSView(y,"aggWTP",sSce) + rSView(y,"usdVal",sSce) - rSView(y,"demVAL",sSce);
    rSView(y,"tppPS",sSce)                = rSView(y,"tppVal",sSce) + rSView(y,"cTherm",sSce);
    rSView(y,"elyPS",sSce)                = rSView(y,"demVAL",sSce) + rSView(y,"cTherm",sSce) + rSView(y,"cHydro",sSce) + rSView(y,"expVal",sSce);

    rSView(y,"Invest",sSce)               = rSView(y,"iField",sSce) + rSView(y,"iConvey",sSce) + rSView(y,"iHydro",sSce) + rSView(y,"iTherm",sSce) ;
    rSView(y,"OandM",sSce)                = rSView(y,"cField",sSce) + rSView(y,"cConvey",sSce) + rSView(y,"cHydro",sSce);
    rSView(y,"Surplus",sSce)              = rSView(y,"AgriVA",sSce) + rSView(y,"elyCS",sSce) + rSView(y,"elyPS",sSce) - rSView(y,"Invest",sSce) - rSView(y,"OandM",sSce);
 


    checkElybal(sSce,g,c,y,"TPP",m)               = SUM(u$tppMap(c,u),  TPP.l(c,u,g,y,m));
    checkElybal(sSce,g,c,y,"HPP",m)               = SUM(b$bCty(b,c),    HPP.l(b,g,y,m));
    checkElybal(sSce,g,c,y,"DEM",m)               = -DEM.l(g,c,y,m);
    checkElybal(sSce,g,c,y,"TRX",m)               = -SUM(cc, TRX.l(c,cc,g,y,m));
    checkElybal(sSce,g,c,y,"USD",m)               = SUM(d, USD.l(g,c,d,y,m));
    checkElybal(sSce,g,c,y,"bal",m)               =   checkElybal(sSce,g,c,y,"TPP",m) + checkElybal(sSce,g,c,y,"HPP",m) + checkElybal(sSce,g,c,y,"USD",m)
                                                    + checkElybal(sSce,g,c,y,"DEM",m) + checkElybal(sSce,g,c,y,"TRX",m);

*   "Agricultural results"     rAgri(sSce,b,e_resAgri,j,q0,y0


*        AgriSales       "Agricultural sales (mUSD/month)",
*        AgriInc         "Agricultural income (mUSD/month)",
*        IrrInvCost      "Investments in irrigation efficiency (mUSD/month)",
*        AgriCost        "Agricultural costs (mUSD/month)",
*        HydroCost       "Hydro power operational and capital costs (USD/month)",
*        HydroProd       "Hydro power generation MWh/month"
*        HydroInc        "Hydro power energy income (1000 USD/month)",
*        TotInc          "Total income from HEPS and agriculture",


*  1) "Agricultural results"
    rRbAgri(sSce,y,"AgriProd",rb0,c)          =    SUM((b,j,q)$(bCty(b,c) AND bBas(b,rb0)), rAgri(sSce,b,"AgriProd",y,j,q));
    rRbAgri(sSce,y,"WaterUse",rb0,c)          =    SUM((b,j,q)$(bCty(b,c) AND bBas(b,rb0)), rAgri(sSce,b,"WaterUse",y,j,q));
    rRbAgri(sSce,y,"LandUse",rb0,c)          =    SUM((b,j,q)$(bCty(b,c) AND bBas(b,rb0)), rAgri(sSce,b,"LandUse",y,j,q));

    rRbAgri(sSce,y,"LandChg","AMU","KAZ")    =     0.000001;
    rRbAgri(sSce,y,"LandChg","AMU","KYR")    =     0.000001;
    rRbAgri(sSce,y,"LandChg","SYR","TUR")    =     0.000001;
    rRbAgri(sSce,y,"LandChg","SYR","TUR")    =     0.000001;


* 2)"Economic results"

    rrbEcon(sSce,y,"AgriSales",rb0,c)        =     SUM((b,m)$(bCty(b,c) AND bBas(b,rb0)), rEcon(sSce,b,"AgriSales",y,m)  );
    rrbEcon(sSce,y,"AgriSales",rb0,c)        =     SUM((b,m)$(bCty(b,c) AND bBas(b,rb0)), rEcon(sSce,b,"AgriSales",y,m)  );
    rrbEcon(sSce,y,"AgriInc",rb0,c)          =     SUM((b,m)$(bCty(b,c) AND bBas(b,rb0)), rEcon(sSce,b,"AgriInc",y,m)    );
    rrbEcon(sSce,y,"IrrInvCost",rb0,c)       =     SUM((b,m)$(bCty(b,c) AND bBas(b,rb0)), rEcon(sSce,b,"IrrInvCost",y,m) );
    rrbEcon(sSce,y,"AgriCost",rb0,c)         =     SUM((b,m)$(bCty(b,c) AND bBas(b,rb0)), rEcon(sSce,b,"AgriCost",y,m)   );
    rrbEcon(sSce,y,"HydroCost",rb0,c)        =     SUM((b,m)$(bCty(b,c) AND bBas(b,rb0)), rEcon(sSce,b,"HydroCost",y,m)  );
    rrbEcon(sSce,y,"HydroProd",rb0,c)        =     SUM((b,m)$(bCty(b,c) AND bBas(b,rb0)), rEcon(sSce,b,"HydroProd",y,m)  );
    rrbEcon(sSce,y,"HydroInc",rb0,c)         =     SUM((b,m)$(bCty(b,c) AND bBas(b,rb0)), rEcon(sSce,b,"HydroInc",y,m)   );
    rrbEcon(sSce,y,"TotInc",rb0,c)           =     SUM((b,m)$(bCty(b,c) AND bBas(b,rb0)), rEcon(sSce,b,"TotInc",y,m)     );
    rrbEcon(sSce,y,"TotInc","AMU","KAZ")    =     0.000001;
    rrbEcon(sSce,y,"TotInc","AMU","KYR")    =     0.000001;
    rrbEcon(sSce,y,"TotInc","SYR","TUR")    =     0.000001;


* 3)  "Reservoir information")


    rResv(sSce,y,"HydroProd",b,m)                =       SUM(g, HPP.l(b,g,y,m));
    rResv(sSce,y,"HydroHead",b,m)                =       HHD.l(b,y,m);
    rResv(sSce,y,"ResLevel",b,m)                 =       SUM(s, VOL.l(s,b,y,m));
    rResv(sSce,y,"Discharge",b,m)$bResEly(b)     =       -rFlow(sSce,b,"OutDis",y,m);
    rResv(sSce,y,"Overflow",b,m)$bResEly(b)      =       -rFlow(sSce,b,"OutFlw",y,m);
    rResv(sSce,y,"HydroGain",b,m)$bResEly(b)     =       rEcon(sSce,b,"HydroGain",y,m);
*    rResv(sSce,y,"DischVal",b,m)$bResEly(b)      =       rEcon(sSce,b,"DischVal",y,m) ;
*    rResv(sSce,y,"WaterVal",b,m)                 =       rEcon(sSce,b,"WaterVal",y,m)$(rEcon(sSce,b,"WaterVal",y,m) gt eps);


*  4) "Description of reservoirs on basin level"


    rrbResv(sSce,y,"HydroCost",rb0,c)     =     SUM((b,m)$(bCty(b,c) AND bBas(b,rb0)),   rEcon(sSce,b,"HydroCost",y,m)    );
    rrbResv(sSce,y,"HydroInc",rb0,c)      =     SUM((b,m)$(bCty(b,c) AND bBas(b,rb0)),   rEcon(sSce,b,"HydroInc",y,m)     );
    rrbResv(sSce,y,"HydroProd",rb0,c)     =     SUM((b,m)$(bCty(b,c) AND bBas(b,rb0)),   rResv(sSce,y,"HydroProd",b,m)    );
    rrbResv(sSce,y,"Discharge",rb0,c)     =     SUM((b,m)$(bCty(b,c) AND bBas(b,rb0)),   rResv(sSce,y,"Discharge",b,m)    );
    rrbResv(sSce,y,"Overflow",rb0,c)      =     SUM((b,m)$(bCty(b,c) AND bBas(b,rb0)),   rResv(sSce,y,"Overflow",b,m)     );
    rrbResv(sSce,y,"ResLevel",rb0,c)      =     SUM((b,m)$(bCty(b,c) AND bBas(b,rb0)),   rResv(sSce,y,"Reslevel",b,m)     );




    rBorder(sSce,y,e_resBorder,"nvg")    = sum(m$mWin(m),  rBorder(sSce,y,e_resBorder,m));
    rBorder(sSce,y,e_resBorder,"veg")    = sum(m$mSum(m),  rBorder(sSce,y,e_resBorder,m));
    rBorder(sSce,y,e_resBorder,"annual") = sum(m,  rBorder(sSce,y,e_resBorder,m));


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

* Agricultural line results
    rAgri(sSce,b,"WaterUse",j,q,y)            = SUM(m, IRG.l(b,q,j,y,m));
    rAgri(sSce,b,"LandUse",j,q,y)             = LND.l(b,q,j,y)/1000;
    rAgri(sSce,b,"LandChg",j,q,y)$lnd0(b,q,j) = 100*LND.l(b,q,j,y)/lnd0(b,q,j)-100;
    rAgri(sSce,b,"AgriProd",j,q,y)            = CRP.l(b,q,j,y)/1000;
    rAgri(sSce,b,"YWR",j,q,y)                 = 100*YWR.l(b,q,j,y);
    rAgri(sSce,b,"ActMarg",j,q,y)$lnd0(b,q,j) = (pCrop(j)*CRP.l(b,q,j,y) - cCrop(b,j)*LND.l(b,q,j,y))/LND.l(b,q,j,y);
    rAgri(sSce,b,"OptMarg",j,q,y)$lnd0(b,q,j) = (pCrop(j)* crp0(b,q,j)   - cCrop(b,j) *lnd0(b,q,j)  )/ lnd0(b,q,j);

* Economic results, line
    rEcon(sSce,b,"AgriSales",y,m)           = SUM((q,j), pCrop(j)*CRP.l(b,q,j,y) ) / 12;
    rEcon(sSce,b,"AgriCost",y,m)            = SUM((q,j), cCrop(b,j)*LND.l(b,q,j,y) ) / 12;
    rEcon(sSce,b,"AgriInc",y,m)             = rEcon(sSce,b,"AgriSales",y,m) - rEcon(sSce,b,"AgriCost",y,m);
    rEcon(sSce,b,"HydroCost",y,m)           = -1000*reservoirs(b,"cost")$(not bResNOP(b));
    rEcon(sSce,b,"HydroInc",y,m)            = 1000 * SUM(g, HPP.l(b,g,y,m) * SUM(c$bCty(b,c), ELYMKT.m(c,g,y,m)) ) / 1000000 + rEcon(sSce,b,"HydroCost",y,m);
    rEcon(sSce,b,"HydroProd",y,m)           = SUM(g, HPP.l(b,g,y,m));
    rEcon(sSce,b,"HydroHead",y,m)           = ( reservoirs(b,"coef_a") + reservoirs(b,"coef_b")*rFlow(sSce,b,"ResVolAvg",y,m) + reservoirs(b,"coef_c")*POWER(rFlow(sSce,b,"ResVolAvg",y,m),2));
    rEcon(sSce,b,"HydroGain",y,m)           = -( rEcon(sSce,b,"HydroProd",y,m) / rFlow(sSce,b,"OutDis",y,m) )$rFlow(sSce,b,"OutDis",y,m);
    rEcon(sSce,b,"TotInc",y,m)              = rEcon(sSce,b,"AgriInc",y,m) + rEcon(sSce,b,"HydroInc",y,m);
    rEcon(sSce,b,"DischVal",y,m)$bResEly(b) = -1000000*( rEcon(sSce,b,"HydroInc",y,m) / rFlow(sSce,b,"OutDis",y,m) )$rFlow(sSce,b,"OutDis",y,m);
    rEcon(sSce,b,"WaterVal",y,m)            = WBALANCE.M("w",b,y,m)*1000000;
    rEcon(sSce,b,"WaterVal",y,"annual")     = ( SUM(m, rEcon(sSce,b,"WaterVal",y,m) * rFlow(sSce,b,"InTot",y,m) ) / rFlow(sSce,b,"InTot",y,"annual")
                                              )$rFlow(sSce,b,"InTot",y,"annual") + 0.000001;

* Aggregate results into basin sections and countries
    rsAgri(sSce,bs,y0,e_resAgri,j)          = SUM((b,q)$bSec(b,bs),     rAgri(sSce,b,e_resAgri,j,q,y0));
    rcAgri(sSce,c,y0,e_resAgri,j)           = SUM((b,q)$bCty(b,c),      rAgri(sSce,b,e_resAgri,j,q,y0));
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
    rcElec(sSce,"qTrxNet",y,c,m)        = -SUM((g,cc),              TRX.l(c,cc,g,y,m)-TRX.l(cc,c,g,y,m)) ;
    rcElec(sSce,"qTrxSA",y,c,m)         =  elySAqExp(c,m);
    rcElec(sSce,"qSupTot",y,c,m)        =  SUM((b,g)$bCty(b,c),     HPP.l(b,g,y,m)) + SUM((u,g)$tppMap(c,u), TPP.l(c,u,g,y,m));;
    rcElec(sSce,"qSupHPP",y,c,m)        =  SUM((b,g)$bCty(b,c),     HPP.l(b,g,y,m));
    rcElec(sSce,"qSupTPP",y,c,m)        =  SUM((u,g)$tppMap(c,u),   TPP.l(c,u,g,y,m));
    rcElec(sSce,"vCstTPP",y,c,m)        = -SUM((u,g)$tppMap(c,u),   TPP.l(c,u,g,y,m)*elyThermal("uFUELEX",u) )/1000 ;
    rcElec(sSce,"vCstUSD",y,c,m)        = -SUM((g,d0),              USD.l(g,c,d0,y,m)*usd0(c,g,d0))/1000 ;
*   rcElec(sSce,"vCstCPX",c,m)          = -SUM(u$uPrd(u),           TNI.l(c,u)*tpp0(c,u,"CAPX"))*TNIweight(m) ;
    rcElec(sSce,"pMCbase",y,c,m)        = -ELYMKT.m(c,"base",y,m)*1000;
    rcElec(sSce,"pMCpeak",y,c,m)        = -ELYMKT.m(c,"peak",y,m)*1000;
    rcElec(sSce,"vExpCst",y,c,m)        =  SUM((g,cc),             -ELYMKT.m(c,g,y,m)    * (TRX.l(c,cc,g,y,m)-TRX.l(cc,c,g,y,m)) ) ;
    rcElec(sSce,"vExpTrx",y,c,m)        =  SUM((g,cc)$trx0(c,cc),   ELYTRX.m(c,cc,g,y,m) * (TRX.l(c,cc,g,y,m)-TRX.l(cc,c,g,y,m)) ) ;
    rcElec(sSce,"vSrpHPP",y,c,m)        =  SUM((b,g)$bCty(b,c),    -ELYMKT.m(c,g,y,m)*HPP.l(b,g,y,m));
    rcElec(sSce,"vTrxSA",y,c,m)         =  1000 * elySAqExp(c,m)*elySApExp(c,m) / 1000000;
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
    rcElec(sSce,"pMCpeak",y,c,m0)$(not m(m0)) = 0;
    rcElec(sSce,"pMCbase",y,c,m0)$(not m(m0)) = 0;



    rcView(sSce,y,"AgriSales",c)            =  SUM((b,q,j)$bCty(b,c),       CRP.l(b,q,j,y) * pCrop(j)  ) / 1000000;
    rcView(sSce,y,"AgriCosts",c)            = -SUM((b,q,j)$bCty(b,c),       LND.l(b,q,j,y) * cCrop(b,j)) / 1000000;
    rcView(sSce,y,"AgriVA",c)               = rcView(sSce,y,"AgriSales",c) - rcView(sSce,y,"AgriCosts",c);
    rcView(sSce,y,"AgriCot",c)              = SUM((b,q,j)$bCty(b,c),        pCrop(j)*CRP.l(b,q,j,y)$cot(j))/1000000;
    rcView(sSce,y,"AgriWht",c)              = SUM((b,q,j)$bCty(b,c),        pCrop(j)*CRP.l(b,q,j,y)$wht(j))/1000000;
    rcView(sSce,y,"AgriAlf",c)              = SUM((b,q,j)$bCty(b,c),        pCrop(j)*CRP.l(b,q,j,y)$alf(j))/1000000;
    rcView(sSce,y,"AgriVeg",c)              = SUM((b,q,j)$bCty(b,c),        pCrop(j)*CRP.l(b,q,j,y)$veg(j))/1000000;
    rcView(sSce,y,"AgriRic",c)              = SUM((b,q,j)$bCty(b,c),        pCrop(j)*CRP.l(b,q,j,y)$ric(j))/1000000;
    rcView(sSce,y,"AgriFru",c)              = SUM((b,q,j)$bCty(b,c),        pCrop(j)*CRP.l(b,q,j,y)$fru(j))/1000000;
    rcView(sSce,y,"AgriOth",c)              = SUM((b,q,j)$bCty(b,c),        pCrop(j)*CRP.l(b,q,j,y)$oth(j))/1000000;
    rcView(sSce,y,"AgriWater",c)            = SUM((b,q,j,m)$bCty(b,c),      IRG.l(b,q,j,y,m));
    rcView(sSce,y,"AgriWaterSum",c)         = SUM((b,q,j,m)$bCty(b,c),      IRG.l(b,q,j,y,m)$mSum(m));
    rcView(sSce,y,"AgriWaterWin",c)         = SUM((b,q,j,m)$bCty(b,c),      IRG.l(b,q,j,y,m)$mWin(m));
    rcView(sSce,y,"HHIuse",c)               = SUM((s,b,sSec,m)$bCty(b,c),   HHI.l(s, sSec,b,y,m)$sW(s));
    rcView(sSce,y,"Losses",c)               = SUM((s,b,l,m)$bCty(b,c),      LOS.l(s,b,l,y,m)$sW(s));
    rcView(sSce,y,"Nature",c)               = SUM((s,b,m)$bCty(b,c),        STO.l(s,b,y,m)$(bNat(b) AND sW(s)) );
    rcView(sSce,y,"Amu-TAD-TUR","TUR")      = SUM((s,m)$sW(s),              FLW.l(s,"AMUMID","AMUUPS",y,m));
    rcView(sSce,y,"Amu-TUR-UZB","UZB")      = SUM((s,m)$sW(s),              FLW.l(s,"AMUMID","AMUUPS",y,m) - ITK.l(s,"Res_ZD","AMUMID",y,m));
    rcView(sSce,y,"Syr-KYR-UZB","UZB")      = SUM((s,m)$sW(s),              FLW.l(s,"Res_FAR","SYRMID",y,m) + DIS.l(s,"Res_FAR","SYRMID",y,m));
    rcView(sSce,y,"Syr-UZB-KAZ","KAZ")      = SUM((s,m)$sW(s),              FLW.l(s,"Res_SHA","SYRMID",y,m) - ITK.l(s,"SYR_KAZ","SYRMID",y,m));
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
    rcView(sSce,y,"demVal",c)               = SUM((g,m)$elyDemand0(c,g,m),  -ELYMKT.m(c,g,y,m) * DEM.l(g,c,y,m)$(-ELYMKT.m(c,g,y,m) gt 0.000001) );
    rcView(sSce,y,"tppVal",c)               = SUM((g,m),                    -ELYMKT.m(c,g,y,m) * SUM(u$tppMap(c,u), TPP.l(c,u,g,y,m))$(-ELYMKT.m(c,g,y,m) gt 0.000001) );
    rcView(sSce,y,"expVal",c)               = SUM(m,                        1000*elySAqExp(c,m) * elySAqExp(c,m) )/1000000;

    rcView(sSce,y,"elyCS",c)                = rcView(sSce,y,"aggWTP",c) + rcView(sSce,y,"usdVal",c) - rcView(sSce,y,"demVAL",c);
    rcView(sSce,y,"tppPS",c)                = rcView(sSce,y,"tppVal",c) + rcView(sSce,y,"cTherm",c);
    rcView(sSce,y,"elyPS",c)                = rcView(sSce,y,"demVAL",c) + rcView(sSce,y,"cTherm",c) + rcView(sSce,y,"cHydro",c) + rcView(sSce,y,"expVal",c);

    rcView(sSce,y,"Invest",c)               = rcView(sSce,y,"iField",c) + rcView(sSce,y,"iConvey",c) + rcView(sSce,y,"iHydro",c) + rcView(sSce,y,"iTherm",c) ;
    rcView(sSce,y,"OandM",c)                = rcView(sSce,y,"cField",c) + rcView(sSce,y,"cConvey",c) + rcView(sSce,y,"cHydro",c);
    rcView(sSce,y,"Surplus",c)              = rcView(sSce,y,"AgriVA",c) + rcView(sSce,y,"elyCS",c) + rcView(sSce,y,"elyPS",c) - rcView(sSce,y,"Invest",c) - rcView(sSce,y,"OandM",c);
    
    checkElybal(sSce,g,c,y,"TPP",m)               = SUM(u$tppMap(c,u),  TPP.l(c,u,g,y,m));
    checkElybal(sSce,g,c,y,"HPP",m)               = SUM(b$bCty(b,c),    HPP.l(b,g,y,m));
    checkElybal(sSce,g,c,y,"DEM",m)               = -DEM.l(g,c,y,m);
    checkElybal(sSce,g,c,y,"TRX",m)               = -SUM(cc, TRX.l(c,cc,g,y,m));
    checkElybal(sSce,g,c,y,"USD",m)               = SUM(d, USD.l(g,c,d,y,m));
    checkElybal(sSce,g,c,y,"bal",m)               =   checkElybal(sSce,g,c,y,"TPP",m) + checkElybal(sSce,g,c,y,"HPP",m) + checkElybal(sSce,g,c,y,"USD",m)
                                                    + checkElybal(sSce,g,c,y,"DEM",m) + checkElybal(sSce,g,c,y,"TRX",m);

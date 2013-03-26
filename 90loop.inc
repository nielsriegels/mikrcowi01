* Loop over scenarios to be analysed

LOOP(sSce,

* Allow flexibility in land allocation
    jAF(j)                  = jAF0(j);
    jAF("fal")              = YES$fallowLand;
    jAF("fal")              = YES;
    jAX(j)                  = 1$(not jAF(j));

* Set water supply (choose between dry and normal)
    sup(s,b,m)              = SUM(y0$year(y0), sup0(s,b,y0,m));

* Set minimum inflows according to year
    minInflow0(b,m0)        = minInflowN(b,m0)$year("2009") + minInflowD(b,m0)$year("2001");

* Set new reservoirs in operation active in counterfactual scenario
    bResNOP(b)              = YES$(resNew(b) AND not resNewOp(b));
    bResSto(b)              = YES$(reservoirs(b,"max") gt reservoirs(b,"min"));
    bResSto(b)$bResNOP(b)   = NO;
    bResEly(b)              = YES$(reservoirs(b,"Ely") > 0 AND not bResNOP(b));
    HPP.up(b,g,y,m)$bResEly(b) = INF;

* Require extra nature needs for counterfactual
    natExtra(b)             = natExtra0(b);

* Set counterfactual crop prices
    pCrop(j)                = pCrop0(j,"ctrf");

* change input prices
    pInput(k)               = pInput0(k);

* Reset fixed discharge patterns from line
    resVolFix0(b,y,m)       = 0;
    resDisFix0(bd,bo,y,m)   = 0;

* Set irrigation investment costs
    iInv(sInv)              = iInv0(sInv);

* Set reservoir buildup
    ctrfBuild = ctrfBuild0;

* Force run-off river for all reservoirs
*    bResSto(b)$scenario("RunOfRiver","ctrf") = NO;
*    bResBuild(b)$scenario("RunOfRiver","ctrf") = NO;
*    irg0(b,"wht",y,m) = irg0(b,"wht",y,m)/20;

* Solve model for the give scenario
    beam.solprint = no;
    beam.limrow = 0;
    SOLVE beam MAXIMIZING twv USING NLP;



* Reservoir flows
rFlow(b,"ResVol",y,m)                       =  SUM(s$sW(s),     VOL.l(s,b,y,m));
rFlow(b,"ResVolAvg",y,m)                    =  SUM(s$sW(s),     VOL.l(s,b,y,m) + SUM((yy,mm)$mLast(y,m,yy,mm), VOL.l(s,b,yy,mm)) ) / 2 ;
rFlow(b,"ResChg",y,m)                       =  SUM(s$sW(s),     VOL.l(s,b,y,m) - SUM((yy,mm)$mLast(y,m,yy,mm), VOL.l(s,b,yy,mm)) );
rFlow(b,"ResChg",y,m)$bResBuild(b)          = -SUM(s$sW(s),    -STO.l(s,b,y,m) + SUM(bd, DIS.l(s,bd,b,y,m) + FLW.l(s,bd,b,y,m)) );
rFlow(b,"ResChg",y,m)$(sea(b) and mBgn(m))  =  SUM(s$sW(s),     VOL.l(s,b,y,m));

* Inflows
rFlow(b,"InSrc",y,m)                        =  SUM(s$sW(s),     sup(s,b,m));
rFlow(b,"InGrw",y,m)                        =                   GRW.l(b,y,m);
rFlow(b,"InDis",y,m)                        =  SUM((s,bo),      DIS.l(s,b,bo,y,m));
rFlow(b,"InFlw",y,m)                        =  SUM((s,bo),      FLW.l(s,b,bo,y,m));
rFlow(b,"InItk",y,m)                        =  SUM((s,bo),      ITK.l(s,b,bo,y,m));
rFlow(b,"InRtn",y,m)                        = rtn0(b,m);
rFlow(b,"InTot",y,m)                        = rFlow(b,"InSrc",y,m) + rFlow(b,"InDis",y,m) + rFlow(b,"InFlw",y,m) 
                                             +rFlow(b,"InItk",y,m) + rFlow(b,"InRtn",y,m) + rFlow(b,"InGrw",y,m);
* Outflows
rFlow(b,"OutDem",y,m)                       =  SUM(j,           IRG.l(b,j,y,m) - qIwater(b,m) - qHwater(b,m) );
rFlow(b,"OutSto",y,m)                       = -SUM(s$sW(s),     STO.l(s,b,y,m));
rFlow(b,"OutDis",y,m)                       = -SUM((s,bd)),     DIS.l(s,bd,b,y,m)$(not sea(bd));
rFlow(b,"OutFlw",y,m)                       = -SUM((s,bd)),     FLW.l(s,bd,b,y,m)$(not sea(bd));
rFlow(b,"OutInt",y,m)                       = -SUM((s,bd)),     ITK.l(s,bd,b,y,m)$(not sea(bd));
rFlow(b,"OutLak",y,m)                       = -SUM((s,bd),      ITK.l(s,bd,b,y,m)+FLW.l(s,bd,b,y,m)+DIS.l(s,bd,b,y,m)  )$sea(bd);
rFlow(b,"OutLos",y,m)                       = -SUM((s,l),                LOS.l(s,b,l,y,m));
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
rEcon(b,"HydroInc",y,m)                     = 1000 * SUM(g, HPP.l(b,g,y,m) *  pEly(g,y,m)) / 1000000 + rEcon(b,"HydroCost",y,m);
rEcon(b,"HydroProd",y,m)                    = SUM(g, HPP.l(b,g,y,m));
rEcon(b,"HydroHead",y,m)                    = ( reservoirs(b,"coef_a") + reservoirs(b,"coef_b")*rFlow(b,"ResVolAvg",y,m) + reservoirs(b,"coef_c")*POWER(rFlow(b,"ResVolAvg",y,m),2));
rEcon(b,"HydroGain",y,m)                    = -( rEcon(b,"HydroProd",y,m) / rFlow(b,"OutDis",y,m) )$rFlow(b,"OutDis",y,m);
rEcon(b,"TotInc",y,m)                       = rEcon(b,"AgriInc",y,m) + rEcon(b,"HydroInc",y,m);
rEcon(b,"DischVal",y,m)$bResEly(b)          = -1000000*( rEcon(b,"HydroInc",y,m) / rFlow(b,"OutDis",y,m) )$rFlow(b,"OutDis",y,m);
rEcon(b,"WaterVal",y,m)                     = WBALANCE.M("w",b,y,m)*1000000;
rEcon(b,"WaterVal",y,"annual")              = ( SUM(m, rEcon(b,"WaterVal",y,m) * rFlow(b,"InTot",y,m) )  / rFlow(b,"InTot",y,"annual")
                                                  )$rFlow(b,"InTot",y,"annual") + 0.000001;

* Save total income for checkout print
checkoutput("totinc","")              = SUM((b,y,m), rEcon(b,"TotInc",y,m));
checkoutput("obj","")                 = TWV.l;



);
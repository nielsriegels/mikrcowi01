* Set model year (demography for demand growth etc)
    y(y0)                   = scenRainYear(sSce,y0);
    yR(y0)                  = scenRainYear(sSce,y0);
    yM(y0)                  = scenModelYear(sSce,y0);

* Set elasticity of transformation 
    rL = 1;

* Allow yield response
    noYWR                   = 0;

* Disable CET function by scenario choice
    useCET                  = 0;

* Enable crop mix method
    useCMX                  = 1;

* Enable Nukus agreement
    useNukus                = 1$scenarios(sSce,"nukus");

* Impose head limits on selected reservoirs
    maxHead(b)              = maxHead0(sSce,b);

* Allow transmission according to transmission limits in trx0
    transx(c,cc)                            = 0;
    transx(c,cc)$(scenarios(sSce,"trCAPS") eq 3) = 1$(caps3(c) AND caps3(cc));
    transx(c,cc)$(scenarios(sSce,"trCAPS") eq 5) = 1$(caps5(c) AND caps5(cc));
    transx(c,cc)$(scenarios(sSce,"trCAPS") eq 6) = 1$(caps6(c) AND caps6(cc));

* Enable electricity exports to South Asia
    pXpt(c,m)               = elyExp0("pExp",c,m)$scenarios(sSce,"SAexpt");
    qXpt(c,m)               = elyExp0("qExp",c,m)$scenarios(sSce,"SAexpt");

* Set thermal OPEX and CAPEX - (u denote unit cost in USD/MWh, c denotes capacitcy costs in mUSD/MW)
* CAPEX is for whole construction period, ie not annualised
    cTPP(u0,"uSRMC")          = ( elyThermal("uVOPEX",u0)+elyThermal("uFUELEX",u0) )$SUM(y0$yM(y0), tppMapYear(y0,u0));
    cTPP(u0,"cFOPEX")         = 0.000001 * elyThermal("cFOPEX",u0)$SUM(y0$yM(y0), tppMapYear(y0,u0));
    cTPP(u0,"cCAPEX")         = elyThermal("cCAPEX",u0)$SUM(y0$yM(y0), tppMapYear(y0,u0));
* Calculate annualised finacial and economic capex
    cTPP(u0,"fCAPEX")         = rFin * cTPP(u0,"cCAPEX") / (1 - (1+rFin)**(-40) );
    cTPP(u0,"eCAPEX")         = rFin * cTPP(u0,"cCAPEX") / (1 - (1+rEco)**(-40) );
    cTPP(u,"elyCap")          = elyThermal("elyCap",u)$SUM(y0$yM(y0), tppMapYear(y0,u));
    tppMap(c,u)               = tppMap0(c,u)$SUM(y0$yM(y0), tppMapYear(y0,u));
 
* Cost of unserved demand
    cUSD(c,g,d)               = SUM(y0$yM(y0), usd0(c,y0,g,d));
 
* Electricity demand
    elyDemand(c,g,m)          = SUM(y0$yM(y0), elyDemand0(c,g,y0,m));
 
* Reset country and node objective function weights to their normal 1's and 0's
    objAGRweight(b)         = 1;
    objELYweight            = 1;
    objCTYweight(c)         = 1;
    objNODEweight(b)        = 1;
    objHPPweight(b,m)       = 0;
    objDISweight(b,m)       = 0;


* Allow flexibility in land allocation and account for any fallow land
* unless scenario user chose fixed land allocation (opLND=0)
*    IF(scenarios("fixLND",sSce) gt 0,
*        jAF(j)                  = NO;
*        jAF("wht")              = YES;
*    );
*    IF(scenarios("fixLND",sSce) eq 0,
*        jAF(j)                  = jAF0(j);
*    );
*    jAF("fal")              = YES$scenarios("fallow",sSce);
*    jAX(j)                  = 1$(not jAF(j));
*    lnd0(b,q,"fal")         = SUM(j$jAF(j), lnd0(b,q,j)) * scenarios("fallow",sSce);

* Disable average yields and enable partitioning by soil suitability class
*    q(qAvg)                 = YES$scenarios("avgSoil",sSce);
*    q(qPar)                 = YES$(not scenarios("avgSoil",sSce));

    jAF(j)                  = YES;
    q(qPar)                 = YES;

    CRP.l(b,q0,j,y)$(not q(q0))         = 0;
    LND.l(b,q0,j,y)$(not q(q0))         = 0;


*  Minimum land use for crops in 1000 ha
    lndMin(b,j)         = SUM(y0$y(y0), lndMin0(b,y0,j))$(scenarios(sSce,"lndRfm") eq 0);

* Maximum crop production limited by country demand 1000 tonnes
    crpMax(c,j)             = SUM(y0$y(y0), crpMax0(c,y0,j));

* Calculate land share for CET function
    lShare(b,q,j)$jAF(j)    = (lnd0(b,q,j) / SUM(jj$jAF(jj), lnd0(b,q,jj)))$SUM(jj$jAF(jj), lnd0(b,q,jj));

* Set water supply (choose between dry and normal)
    sup(s,b,m)              = SUM(y0$scenRainYear(sSce,y0),     sup0(s,b,y0,m));
    hhiUse("HH",b,m)        = SUM(y0$scenModelYear(sSce,y0),    qHwater(b,m)*(1+demography(y0,"HH") ) );
    hhiUse("ind",b,m)       = SUM(y0$scenModelYear(sSce,y0),    qIwater(b,m)*(1+demography(y0,"ind")) );

* Set new reservoirs in operation according to scenario definition
    bResNOP(b)              = YES$(resNew(b) AND not resNewOp(sSce,b));
    bResSto(b)              = YES$(reservoirs(b,"max") gt reservoirs(b,"min"));
    bResSto(b)$bResNOP(b)   = NO;
    bResEly(b)              = YES$(reservoirs(b,"Ely") > 0 AND not bResNOP(b));
    HPP.up(b,g,y,m)         = INF$bResEly(b);
    HHD.up(b,y,m)           = INF$bResEly(b);
    DIS.up(s,bd,bo,y,m)     = INF$bResEly(bo);
    FLW.up(s,bd,bo,y,m)$bRes(bo) = INF$resv(bd,bo);
    FLW.up(s,bd,bo,y,m)$resv(bd,bo)                             = INF$resv(bd,bo);

* Set maximum discharge capacity
    resDisMax0(b,m)$bRes(b) = min(  reservoirs(b,"capacity")*3600*24*30/1000000,
                                   (reservoirs(b,"effect")*24*30/reservoirs(b,"ely"))$reservoirs(b,"ely")
                                 )$bResEly(b);

* Fix discharge and volumen patterns for selected reservoirs
$ontext
    IF(scenarios("fixRES",sSce) ne 0,
        resVolFix(b,y,m)              = resVolFix0(b,y,m);
        resDisFix(bd,bo,y,m)          = resDisFix0(bd,bo,y,m);
    );
    IF(scenarios("fixRES",sSce) eq 0,
        resVolFix(b,y,m)              = 0;
        resDisFix(bd,bo,y,m)          = 0;
    );
$offtext
    resVolFix(b,y,m)              = 0;
    resDisFix(bd,bo,y,m)          = 0;


* Require extra nature needs for counterfactual
    natExtra(b)             = natExtra0(b,sSce);

* Set crop prices according to scenario indices for crop price
*    pCrop(j)                = pCrop0("economic","baseprice",j)*ipCrop(j,sSce)$(scenarios("rMkt",sSce) eq 0)
*                            + pCrop0("financial","baseprice",j)*ipCrop(j,sSce)$(scenarios("rMkt",sSce) ne 0);
    pCrop(b,j)$bPlz(b)       = (1+ipCrop(j,sSce)) * SUM(c$bCty(b,c), pAgri0(c,j,"economic","pCrp")  )$(scenarios(sSce,"aPrice") eq 0)
                              +(1+ipCrop(j,sSce)) * SUM(c$bCty(b,c), pAgri0(c,j,"financial","pCrp") )$(scenarios(sSce,"aPrice") ne 0);

* Set crop costs according to scenario indices for crop costs
*    cCrop(b,j)$bPlz(b)      = SUM(k, cCrop0("noReform",k,j)*icCrop(k,sSce))$(scenarios("rMkt",sSce) eq 0)
*                            + SUM(k, cCrop0("reform",k,j)*icCrop(k,sSce))$(scenarios("rMkt",sSce) ne 0);
    cCrop(b,j)$bPlz(b)      = SUM(c$bCty(b,c), 
                                    (1+icCrop("labr",sSce)) * pAgri0(c,j,"financial","qLbr") * pAgri0(c,j,"financial","pLbr")
                                   +(1+icCrop("inpt",sSce)) * pAgri0(c,j,"financial","pIpt")
                                   +(1+icCrop("cptl",sSce)) * pAgri0(c,j,"financial","pCap")
                              )$(scenarios(sSce,"aPrice") ne 0)
                             +SUM(c$bCty(b,c), 
                                    (1+icCrop("labr",sSce)) * pAgri0(c,j,"economic","qLbr") * pAgri0(c,j,"economic","pLbr")
                                   +(1+icCrop("inpt",sSce)) * pAgri0(c,j,"economic","pIpt")
                                   +(1+icCrop("cptl",sSce)) * pAgri0(c,j,"economic","pCap")
                              )$(scenarios(sSce,"aPrice") eq 0);

    
* Set leaching requirements
    qLch(b)      = SUM(y0$y(y0), qLch0(b,"dry")$y("2001") +  qLch0(b,"nor")$y("2009") ); 

* Set irrigation investment costs
    iInv(sInv)              = 0;

* Set reservoir buildup
    ctrfBuild               = 0;

* Set levels for intake reduction scenarios
    itk2(s,bd,y,m)$bPlz(bd)       = itk1(s,bd,y,m)*minIntake0(sSce);

* Set losses
    lossChannels(b,l,m)             = lossChannels0("bsline",b,l,m)$yM("2009")
                                     +lossChannels0("BaU-30",b,l,m)*(1-scenarios(sSce,"iCanal")/100)$yM("2020")
                                     +lossChannels0("action",b,l,m)*(  scenarios(sSce,"iCanal")/100)$yM("2020");
    lossFields(b,l,m)               = lossFields0("bsline",b,l,m)$yM("2009")
                                     +lossFields0("BaU-30",b,l,m) * (1-scenarios(sSce,"iField")/100)$yM("2020")
                                     +lossFields0("action",b,l,m) * (  scenarios(sSce,"iField")/100)$yM("2020");  
    lossReservoirs(b,l,m)           = lossReservoirs0("bsline",b,l,m)$(scenarios(sSce,"clmChg") eq 0)
                                     +lossReservoirs0("clmchg",b,l,m)$(scenarios(sSce,"clmChg") ne 0);
    lossLakes(b,l,m)                = 0;
DISPLAY lossChannels, LossFields;
* Set conveyance cost according to investment choices
    conveyanceCost(c)       = conveyCost0(c)  * scenarios(sSce,"cCanal"); 
*   cConv(c)                = cConv0(c)$yM("2009") 
*   cField(c)               = 
*


* Change pumping requirements due to investment in chanellosses
    elyPump(bd,bo)          = elyPump0(bd,bo) * scenarios(sSce,"iCanal");




* Set tentative levels if first run
IF(SUM((s,bd,bo,y,m), FLW.l(s,bd,bo,y,m)) eq 0,
* Flow variable levels
        YWR.l(b,q,j,y)              = 1$lnd0(b,q,j);
        iET.l(b,q,j,p,y)            = 1$lnd0(b,q,j);
        LCH.l(s,b,y,m)                = qLch0(b,"nor")$mLch0(b,m);
        IRG.l(b,q,j,y,m)            = irg0(b,q,j,y,m);
        ITK.l(s,bd,bo,y,m)          = SUM((q,j), irg0(bd,q,j,y,m))$intk(bd,bo);
        DIS.l(s,bd,bo,y,m)          = 0.5*reservoirs(bo,"capacity")*3600*30/1000000;
        FLW.l(s,bd,bo,y,m)          = SUM(b, DIS.l(s,bo,b,y,m)$resv(bo,b) + sup0(s,bo,y,m)$flow(bd,bo));
        FLW.l(s,bd,bo,y,m)$(FLW.l(s,bd,bo,y,m) eq 0)          = SUM(b, FLW.l(s,bo,b,y,m)$flow(bo,b));
        FLW.l(s,bd,bo,y,m)$(FLW.l(s,bd,bo,y,m) eq 0)          = SUM(b, FLW.l(s,bo,b,y,m)$flow(bo,b));
        FLW.l(s,bd,bo,y,m)$(FLW.l(s,bd,bo,y,m) eq 0)          = SUM(b, FLW.l(s,bo,b,y,m)$flow(bo,b));
        LOS.l(s,b,l,y,m)            = lossRivers(b,l,m)*(rtn0(b,m) + sup(s,b,m) + SUM(bo, FLW.l(s,b,bo,y,m)+DIS.l(s,b,bo,y,m)));
        GRW.l(b,y,m)                = SUM(mS, grwtrMax(b,mS))$mBgn(m);
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
        TPP.l(c,u,g,y,m)$tppMap(c,u)= elyThermal("elyCap",u) / 2;
        HPP.l(b,g,y,m)$bResEly(b)   = 0.5*SUM((s,bd)$(resv(bd,b) AND sW(s)),  gravity * reservoirs(b,"coef_eta") * DIS.l(s,bd,b,y,m)
                                          * ( reservoirs(b,"coef_a") + reservoirs(b,"coef_b")*VOL.l(s,b,y,m) + reservoirs(b,"coef_c")*POWER(VOL.l(s,b,y,m),2))
                                      ) / 3600;
* Agricultural variable levels

        LND.l(b,q,j,y)              = lnd0(b,q,j);
        CRP.l(b,q,j,y)              = crp0(b,q,j);
    );    
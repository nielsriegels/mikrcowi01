* Set model year (demography for demand growth etc)
    y(y0)                   = scenRainYear(sSce,y0);

* Set elasticity of transformation 
    rL = (scenarios("sT",sSce)+1)/scenarios("sT",sSce);

* Allow yield response
    noYWR                   = 1$scenarios("noYWR",sSce);

* Disable CET function by scenario choice
    useCET                  = 1$scenarios("useCET",sSce);

* Allow transmission according to transmission limits in trx0
    transx                  = 1$scenarios("transx",sSce);

* Reset country and node weights to their normal 1's
    objCTYweight(c)         = 1;
    objNODEweight(b)        = 1;
    objELYweight            = 1;
    objDISweight(b,m)       = 1;
    objDISweight(b,m)       = 1;
    



* Allow flexibility in land allocation and account for any fallow land
* unless scenario user chose fixed land allocation (opLND=0)
    IF(scenarios("fixLND",sSce) gt 0,
        jAF(j)                  = NO;
*        jAF("wht")              = YES;
    );
    IF(scenarios("fixLND",sSce) eq 0,
        jAF(j)                  = jAF0(j);
    );
    jAF("fal")              = YES$scenarios("fallow",sSce);
    jAX(j)                  = 1$(not jAF(j));
    lnd0(b,q,"fal")         = SUM(j$jAF(j), lnd0(b,q,j)) * scenarios("fallow",sSce);

* Disable average yields and enable partitioning by soil suitability class
    q(qAvg)                 = YES$scenarios("avgSoil",sSce);
    q(qPar)                 = YES$(not scenarios("avgSoil",sSce));

    CRP.l(b,q0,j,y)$(not q(q0))         = 0;
    LND.l(b,q0,j,y)$(not q(q0))         = 0;


*  Index for minimum land use for crops in procent of base
    iMinLnd(b,j)         = SUM(c$bCty(b,c), iMinLnd0(c,j,sSce));

* Calculate land share for CET function
    lShare(b,q,j)$jAF(j)    = (lnd0(b,q,j) / SUM(jj$jAF(jj), lnd0(b,q,jj)))$SUM(jj$jAF(jj), lnd0(b,q,jj));

* Set water supply (choose between dry and normal)
    sup(s,b,m)              = SUM(y0$scenRainYear(sSce,y0),     sup0(s,b,y0,m));
    hhiUse("HH",b,m)        = SUM(y0$scenModelYear(sSce,y0),    qHwater(b,m)*(1+demography(y0,"HH") ) );
    hhiUse("ind",b,m)       = SUM(y0$scenModelYear(sSce,y0),    qIwater(b,m)*(1+demography(y0,"ind")) );

* Set new reservoirs in operation according to scenario definition
    bResNOP(b)              = YES$(resNew(b) AND not resNewOp(b,sSce));
    bResSto(b)              = YES$(reservoirs(b,"max") gt reservoirs(b,"min"));
    bResSto(b)$bResNOP(b)   = NO;
    bResEly(b)              = YES$(reservoirs(b,"Ely") > 0 AND not bResNOP(b));
    HPP.up(b,g,y,m)         = INF$bResEly(b);

* Enable spillways via FLW in all reservoirs
    flow(bd,bo)$(SUM(m, resDisMax0(bo,m)) gt 0 and SUM(m, resDisMax0(bo,m)) lt INF)  = YES$resv(bd,bo);
flow(bd,bo)$resv(bd,bo)                 = NO;

* Set loss in channels according to investment choices
    lossChannels(b,l,m) = lossChannels0(b,l,m)$(scenarios("iChnl",sSce))* (1-scenarios("iChnl",sSce))
                        + lossChannels1(b,l,m)$(scenarios("iChnl",sSce))* scenarios("iChnl",sSce);

* Set conveyance cost according to investment choices
    conveyanceCost(c) = conveyCost0(c)$(scenarios("iChnl",sSce)= 0) + conveyCost1(c)$scenarios("iChnl",sSce);

* Change pumping requirements due to investment in chanellosses
    elyPump(bd,bo) = elyPump0(bd,bo)$(scenarios("iChnl",sSce))* (1-scenarios("iChnl",sSce))
                   + elyPump1(bd,bo)$(scenarios("iChnl",sSce))* scenarios("iChnl",sSce);

* Fix discharge and volumen patterns for selected reservoirs
    IF(scenarios("fixRES",sSce) ne 0,
        resVolFix(b,y,m)              = resVolFix0(b,y,m);
        resDisFix(bd,bo,y,m)          = resDisFix0(bd,bo,y,m);
    );
    IF(scenarios("fixRES",sSce) eq 0,
        resVolFix(b,y,m)              = 0;
        resDisFix(bd,bo,y,m)          = 0;
    );

* Require extra nature needs for counterfactual
    natExtra(b)             = natExtra0(b,sSce);

* Set crop prices according to scenario indices for crop price
    pCrop(j)                = pCrop0("economic","baseprice",j)*ipCrop(j,sSce)$(scenarios("rMkt",sSce) eq 0)
                            + pCrop0("financial","baseprice",j)*ipCrop(j,sSce)$(scenarios("rMkt",sSce) ne 0);

* Set crop costs according to scenario indices for crop costs
    cCrop(b,j)$bPlz(b)      = SUM(k, cCrop0("noReform",k,j)*icCrop(k,sSce))$(scenarios("rMkt",sSce) eq 0)
                            + SUM(k, cCrop0("reform",k,j)*icCrop(k,sSce))$(scenarios("rMkt",sSce) ne 0);

* Set irrigation investment costs
    iInv(sInv)              = 0;

* Set reservoir buildup
    ctrfBuild               = scenarios("rBuild",sSce);

* Set levels if first run
IF(ord(sSce) eq 1,
* Flow variable levels
        YWR.l(b,q0,j,y)             = 1;
        IRG.l(b,q,j,y,m)            = irg0(b,q,j,y,m);
        ITK.l(s,bd,bo,y,m)          = itk0(bd,bo)/12;
        FLW.l(s,bd,bo,y,m)          = flw0(bd,bo)/12;
        DIS.l(s,bd,bo,y,m)          = dis0(bd,bo)/12;
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
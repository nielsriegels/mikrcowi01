


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

* Enable spillways via FLW in all reservoirs
    flow(bd,bo)$(SUM(m, resDisMax0(bo,m)) gt 0 and SUM(m, resDisMax0(bo,m)) lt INF)  = YES$resv(bd,bo);

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
    TPP.l(u,g,y,m)              = elyThermal("elyCap",u) / 2;
    HPP.l(b,g,y,m)$bResEly(b)   = 0.5*SUM((s,bd)$(resv(bd,b) AND sW(s)),  gravity * reservoirs(b,"coef_eta") * DIS.l(s,bd,b,y,m)
                                      * ( reservoirs(b,"coef_a") + reservoirs(b,"coef_b")*VOL.l(s,b,y,m) + reservoirs(b,"coef_c")*POWER(VOL.l(s,b,y,m),2))
                                  ) / 3600;
* Agricultural variable levels
    LND.l(b,j,y)                = lnd0(b,j);
    CRP.l(b,j,y)                = crp0(b,j);

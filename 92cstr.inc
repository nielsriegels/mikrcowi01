

* Constrained optimisation - maximise winter discharges
    IF(scenarios(sSce,"objvar") eq 3,
        objAGRweight(b)             = 0.001;
        objELYweight                = 0.001;
        objHPPweight(b,m)           = 0.001;
        objDISweight("Res_ROG",m)   = -10$mSum(m) + 10$mWin(m);
        objDISweight("Res_TOK",m)   = -10$mSum(m) + 10$mWin(m);
        objDISweight("Res_NUR",m)   = -10$mSum(m) + 10$mWin(m);
    );

* Constrained optimisation - maximise summer discharges
    IF(scenarios(sSce,"objvar") eq 4,
        objAGRweight(b)             = 0.001;
        objELYweight                = 0.001;
        objHPPweight(b,m)           = 0.001;
        objDISweight("Res_ROG",m)   = -10$mWin(m) + 10$mSum(m);
        objDISweight("Res_TOK",m)   = -10$mWin(m) + 10$mSum(m);
        objDISweight("Res_NUR",m)   = -10$mWin(m) + 10$mSum(m);
    );

* Constrained optimisation - maximise total HPP electricity
    IF(scenarios(sSce,"objvar") eq 2,
        objAGRweight(b)             = 0.001;
        objHPPweight(b,m)           = 10$( bCty(b,"KYR") or bCty(b,"TAD"));
        objELYweight                = 0.001;
        objDISweight(b,m)           = 0;
    );

* Constrained optimisation - maximise winter HPP electricity
    IF(scenarios(sSce,"objvar") eq 1,
        objAGRweight(b)             = 0.001;
        objHPPweight(b,m)           = 10$( (bCty(b,"KYR") or bCty(b,"TAD")) AND mWin(m) );
        objELYweight                = 0.001;
        objDISweight(b,m)           = 0;
    );


    IF(scenarios(sSce,"objvar") gt 0,
* Run model with altered weighting of objective parts
        SOLVE beam MAXIMIZING twv USING NLP;

* Reset objective weights
        objAGRweight(b)             = 1;
        objELYweight                = 1;
        objHPPweight(b,m)           = 0;
        objDISweight(b,m)           = 0;

* Set fixes for 2nd optimisation run, but only for reservoirs in operation
        resDisFix(bd,"Res_ROG",y,m)$bResEly("Res_ROG")  = DIS.l("w",bd,"Res_ROG",y,m)+FLW.l("w",bd,"Res_ROG",y,m)+eps$resv(bd,"Res_ROG");
        resDisFix(bd,"Res_TOK",y,m)$bResEly("Res_TOK")  = DIS.l("w",bd,"Res_TOK",y,m)+FLW.l("w",bd,"Res_TOK",y,m)+eps$resv(bd,"Res_TOK");
        resDisFix(bd,"Res_NUR",y,m)$bResEly("Res_NUR")  = DIS.l("w",bd,"Res_NUR",y,m)+FLW.l("w",bd,"Res_NUR",y,m)+eps$resv(bd,"Res_NUR");
        resVolFix("Res_ROG",y,m)$bResEly("Res_ROG")     = VOL.l("w","Res_ROG",y,m);
        resVolFix("Res_TOK",y,m)$bResEly("Res_TOK")     = VOL.l("w","Res_TOK",y,m);
        resVolFix("Res_NUR",y,m)$bResEly("Res_NUR")     = VOL.l("w","Res_NUR",y,m);
    );

* Constrained optimisation - single country sequential optimisation
* (KYR-TAD)-TUR-UZB-KAZ priority
    IF(scenarios(sSce,"seqnce") gt 0,
* Disable reservoir spillways (they cause troubles) and clear all countries from obj.
        flow(bd,bo)$resv(bd,bo)                 = NO;
        objCTYweight(c)                         = 0;
        objNODEweight(b)                        = 0;


* Individual country optimisation
* Sequence A (s30+s33) - 1st: TAD+KYR+FER_UZB;                         2nd: TUR; 3rd: UZB; 4th: KAZ 
* Sequence B (s31+s34) - 1st: TAD+KYR;                                 2nd: TUR; 3rd: UZB; 4th: KAZ 
* Sequence C (s32+s35) - 1st: KYR;             2nd: FER_UZB; 3rd: TAD; 4th: TUR; 5th: UZB, 6th: KAZ


* Optimise for KYR first in all sequences
        objCTYweight("KYR")                     = 1;
        objNODEweight(b)$bCty(b,"KYR")          = 1;
* In sequence A, TAD is also in first pick
        objCTYweight("TAD")                     = 1$(scenarios(sSce,"seqnce") eq 1);
        objNODEweight(b)$bCty(b,"TAD")          = 1$(scenarios(sSce,"seqnce") eq 1);
* In sequence A we also include FER_UZB in 1st pick (but not CTYweight, as we get all UZB electricity in
        objNODEweight("FER_UZB")                = 1$(scenarios(sSce,"seqnce") eq 1);
        SOLVE beam MAXIMIZING twv USING NLP;
        LOOP(b$objNODEweight(b),
            resDisFix(bd,b,y,m)$bResEly(b)      = DIS.l("w",bd,b,y,m)+FLW.l("w",bd,b,y,m)+eps$resv(bd,b);
            resVolFix(b,y,m)$bResEly(b)         = VOL.l("w",b,y,m);
            ITK.fx(s,b,bo,y,m)                  = ITK.l(s,b,bo,y,m);
        );

* In sequence C, FER_UZB is 2nd pick
* We disregard Uzbek electricity market (CTYweight)
        IF(scenarios(sSce,"seqnce") eq 3,
*            objCTYweight("UZB")                     = 1;
            objNODEweight("FER_UZB")                = 1;
DISPLAY resDisFix;
            SOLVE beam MAXIMIZING twv USING NLP;
            LOOP(b$objNODEweight(b),
                resDisFix(bd,b,y,m)$bResEly(b)      = DIS.l("w",bd,b,y,m)+FLW.l("w",bd,b,y,m) + eps$resv(bd,b);
                resVolFix(b,y,m)$bResEly(b)         = VOL.l("w",b,y,m);
                ITK.fx(s,b,bo,y,m)                  = ITK.l(s,b,bo,y,m);
            );
        );

* In sequence B and C, TAD is next (2nd and 3rd pick)
        IF( ((ord(sSce) eq 32) or (ord(sSce) eq 35)),
*            objCTYweight("UZB")                     = 0;
            objCTYweight("TAD")                     = 1;
            objNODEweight(b)$bCty(b,"TAD")          = 1;
DISPLAY resDisFix;
            SOLVE beam MAXIMIZING twv USING NLP;
            LOOP(b$objNODEweight(b),
                resDisFix(bd,b,y,m)$bResEly(b)      = DIS.l("w",bd,b,y,m)+FLW.l("w",bd,b,y,m)+eps$resv(bd,b);
                resVolFix(b,y,m)$bResEly(b)         = VOL.l("w",b,y,m);
                ITK.fx(s,b,bo,y,m)                  = ITK.l(s,b,bo,y,m);
            );
        );

* Assuming TUR (and not UZB) have control over intake to Res_ZD and Kairakum canal
* given fixed intakes upstream
        objCTYweight("TUR")                     = 1;
        objNODEweight(b)$bCty(b,"TUR")          = 1;
DISPLAY resDisFix;
        SOLVE beam MAXIMIZING twv USING NLP;
        LOOP(b$objNODEweight(b),
            resDisFix(bd,b,y,m)$bResEly(b)      = DIS.l("w",bd,b,y,m)+FLW.l("w",bd,b,y,m)+eps$resv(bd,b);
            resVolFix(b,y,m)$bResEly(b)         = VOL.l("w",b,y,m);
            ITK.fx(s,b,bo,y,m)                  = ITK.l(s,b,bo,y,m);
        );

* Optimise for UZB given fixed intakes upstream
        objCTYweight("UZB")                     = 1;
        objNODEweight(b)$bCty(b,"UZB")          = 1;
        SOLVE beam MAXIMIZING twv USING NLP;
        LOOP(b$objNODEweight(b),
            resDisFix(bd,b,y,m)$bResEly(b)      = DIS.l("w",bd,b,y,m)+FLW.l("w",bd,b,y,m)+eps$resv(bd,b);
            resVolFix(b,y,m)$bResEly(b)         = VOL.l("w",b,y,m);
            ITK.fx(s,b,bo,y,m)                  = ITK.l(s,b,bo,y,m);
        );
    
* Optimise for all region (now KAZ is last) given fixed releases
        objCTYweight(c)                         = 1;
        objNODEweight(b)                        = 1;


    );
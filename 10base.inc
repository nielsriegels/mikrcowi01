* =============================================================================
* Base sets and scalars
* =============================================================================


* Other scalars
SCALARS
    rL                      "Transformation parameter"
    gravity                 "Gravity acceleration"                  / 9.82 /
    solvStatB               "Solver status, base"                   / 0 /
    solvStatS               "Solver status, scenario"               / 0 /
    modlStatB               "Model status, base"                    / 0 /
    modlStatS               "Model status, base"                    / 0 /
;

* Sets not depending on data
SETS
    c                       "Countries"
    s                       "Substances"            / W /
    sW(s)                   "Water substance"       / W /
    k                       "Prod. inputs"          / watr, labr, cptl, inpt, land /
    kLimit(k)               "Fixed inputs"          / watr, land /
    kWater(k)               "Water input"           / watr /
    kLand(k)                "Land input"            / land /
    m0                      "Season"                / m01, m02, m03, m04, m05, m06, m07, m08, m09, m10, m11, m12, annual, veg, nvg /
    m(m0)                   "Season"                / m01, m02, m03, m04, m05, m06, m07, m08, m09, m10, m11, m12 /
    mN                      "Month names"           / oct, nov, dec, jan, feb, mar, apr, may, jun, jul, aug, sep /
    mX(mN,m0)               "Months xref"           / oct.m10, nov.m11, dec.m12, jan.m01, feb.m02, mar.m03, apr.m04, may.m05, jun.m06, jul.m07, aug.m08, sep.m09 /
    mBgn(m0)                "First season month"    / m10 /
    mEnd(m0)                "Last season month"     / m09 /
    mA(m0)                  "Annual sum"            / annual /
    mS(m0)                  "Seasons"               / veg, nvg  /
    y0                      "Years"                 / 2001, 2009, 2020, 2050 /
    y(y0)                   "Modelled years"
    yB(y0)                  "Base year"
    g0                      "Energy demand segment" / base, peak /
    g(g0)                   "Energy demand segment" / base, peak /
    u0                      "Energy prod. units"    / u01*u99 /
    u(u0)                   "Energy prod. units"    / u01*u20 /
    sBal                    "Balance table cols"    / Demand, FromSrc, DisTo, FromUp, FromIn, Return, ToIn, DisFrom, ToDwn, ToLoss /
    sInv                    "Irrigation investment" / usdPerMM3, usdPerHa, improve /
    sSec                    "Non-agri sectors"      / HH, ind /
    sRtn                    "Return flow types"     / total, irrig, river, lakes/
    sEly                    "Elec. data types"      / elyCap, aCAPEX, aFOPEX, uVOPEX, uFUELEX /
    sSce                    "Scenarios"             / base, ctrf /
    l                       "Loss types"            / seep, evap /
;

* Aliases 
ALIAS(m0,mm0);
ALIAS(m,mm);
ALIAS(y0,yy0);
ALIAS(y,yy);
ALIAS(s,ss);

* Link between season and months
SET mSeason(m,mS) / m01.nvg, m02.nvg, m03.nvg, m04.veg, m05.veg, m06.veg, m07.veg, m08.veg, m09.veg, m10.nvg, m11.nvg, m12.nvg /;

* Month y.m follows month yy.mm
SET mLast(y0,m,yy0,mm) "Connection between months" /
    2001.m01.2001.m12 YES
    2001.m02.2001.m01 YES
    2001.m03.2001.m02 YES
    2001.m04.2001.m03 YES
    2001.m05.2001.m04 YES
    2001.m06.2001.m05 YES
    2001.m07.2001.m06 YES
    2001.m08.2001.m07 YES
    2001.m09.2001.m08 YES
    2001.m10.2001.m09 YES
    2001.m11.2001.m10 YES
    2001.m12.2001.m11 YES

    2009.m01.2009.m12 YES
    2009.m02.2009.m01 YES
    2009.m03.2009.m02 YES
    2009.m04.2009.m03 YES
    2009.m05.2009.m04 YES
    2009.m06.2009.m05 YES
    2009.m07.2009.m06 YES
    2009.m08.2009.m07 YES
    2009.m09.2009.m08 YES
    2009.m10.2009.m09 YES
    2009.m11.2009.m10 YES
    2009.m12.2009.m11 YES

    2020.m01.2020.m12 YES
    2020.m02.2020.m01 YES
    2020.m03.2020.m02 YES
    2020.m04.2020.m03 YES
    2020.m05.2020.m04 YES
    2020.m06.2020.m05 YES
    2020.m07.2020.m06 YES
    2020.m08.2020.m07 YES
    2020.m09.2020.m08 YES
    2020.m10.2020.m09 YES
    2020.m11.2020.m10 YES
    2020.m12.2020.m11 YES

    2050.m01.2050.m12 YES
    2050.m02.2050.m01 YES
    2050.m03.2050.m02 YES
    2050.m04.2050.m03 YES
    2050.m05.2050.m04 YES
    2050.m06.2050.m05 YES
    2050.m07.2050.m06 YES
    2050.m08.2050.m07 YES
    2050.m09.2050.m08 YES
    2050.m10.2050.m09 YES
    2050.m11.2050.m10 YES
    2050.m12.2050.m11 YES

/;



* =============================================================================
* Calculate reporting results
* =============================================================================

SETS
    e_resFlow /
        ResVol          "Reservoir volume",
        ResVolAvg       "Reservoir volume average over month (mm3)",
        ResChg          "Reservoir change",
        InSrc           "Contribution from surface sources",
        InGrw           "Contribution from ground water sources",
        InDis           "Water discharged into node",
        InFlw           "Water from upstream rivers",
        InItk           "Water from intakes",
        InRtn           "Water from return flows",
        InTot           "Water from upstream rivers, reservoirs, returns and intakes",
        OutDem          "Water used for human purposes",
        OutDis          "Water discharged from node",
        OutFlw          "Water to downstream by river",
        OutInt          "Water to intake",
        OutSto          "Water stored",
        OutLos          "Water losses",
        OutLak          "Water to lakes"
        OutTot          "Water stored, used, lost or to intake or downstream river",
        Balance         "Water balance" /

    e_resAgri /
        WaterUse        "Agricultural water use (mm3/year)",
        LandUse         "Agricultural land use (ha)",
        LandEff         "Yield change index (1000=baseline)",
        AgriSales       "Agricultural sales (mUSD/year)",
        AgriIncome      "Agricultural income (mUSD/year)",
        AgriCosts       "Agricultural costs (mUSD/year)",
        AgriProd        "Agricultural production (ton/year)",
        LandIncome      "Agricultural income by land (USD/ha)",
        WatrIncome      "Agricultural income by land (USD/1000m3)",
        LabrUse         "Agricultural labour use (mUSD/year)",
        IrrInvCost      "Investments in irrigation efficiency (mUSD/year)",
        WaterSave       "Water saved by irrigation efficiency (mm3/year)" /

    e_resEcon /
        AgriSales       "Agricultural sales (mUSD/month)",
        AgriInc         "Agricultural income (mUSD/month)",
        IrrInvCost      "Investments in irrigation efficiency (mUSD/month)",
        AgriCost        "Agricultural costs (mUSD/month)",
        HydroCost       "Hydro power operational and capital costs (USD/month)",
        HydroProd       "Hydro power generation MWh/month"
        HydroInc        "Hydro power energy income (1000 USD/month)",
        HydroHead       "Head (m)"
        HydroGain       "Electricity per volume discharged (MWh/mm3)"
        TotInc          "Total income from HEPS and agriculture",
        DischVal        "Unit wlectricity earnings (USD/mm3)"
        WaterVal        "Marginal value of water USD/mm3" /

;


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
    rb0                     "River basins"          / AMU, SYR, ALL /
    rb(rb0)                 "River basins"          / AMU, SYR /
    s                       "Substances"            / W /
    sW(s)                   "Water substance"       / W /
    k                       "Prod. inputs"          / watr, labr, cptl, inpt, land /
    kLimit(k)               "Fixed inputs"          / watr, land /
    kWater(k)               "Water input"           / watr /
    kLand(k)                "Land input"            / land /
    p0                      "Growth stages"         / init, deve, midi, late, annl /
    p(p0)                   "Growth stages used"    / init, deve, midi, late  /
    q0                      "Land suit. class"      / 1vs, 2su, 3ms, avg /
    q(q0)                   "Land suit. class used" / 1vs, 2su, 3ms /
    qAvg(q0)                "Land suit. avg"        / avg /
    qPar(q0)                "Land suit. partition"  / 1vs, 2su, 3ms /
    m0                      "Season"                / m10, m11, m12, m01, m02, m03, m04, m05, m06, m07, m08, m09, annual, veg, nvg /
    m(m0)                   "Season"                / m01, m02, m03, m04, m05, m06, m07, m08, m09, m10, m11, m12 /
    mWin(m0)                "Winter months"         / m10, m11, m12, m01, m02, m03 /
    mSum(m0)                "Summer months"         / m04, m05, m06, m07, m08, m09 /
    mN                      "Month names"           / oct, nov, dec, jan, feb, mar, apr, may, jun, jul, aug, sep /
    mX(mN,m0)               "Months xref"           / oct.m10, nov.m11, dec.m12, jan.m01, feb.m02, mar.m03, apr.m04, may.m05, jun.m06, jul.m07, aug.m08, sep.m09 /
    mBgn(m0)                "First season month"    / m10 /
    mEnd(m0)                "Last season month"     / m09 /
    mA(m0)                  "Annual sum"            / annual /
    mS(m0)                  "Seasons"               / veg, nvg  /
    mSm(m0,m0)              "Seasons map"           / m10.nvg, m11.nvg, m12.nvg, m01.nvg, m02.nvg, m03.nvg, m04.veg, m05.veg, m06.veg, m07.veg, m08.veg, m09.veg / 
    y0                      "Years"                 / 2001, 2009, 2020, 2050 /
    y(y0)                   "Modelled years"
    yB(y0)                  "Base year"
    ry                      "Rainfall years"        / nor, dry /
    g0                      "Energy demand segment" / base, peak /
    g(g0)                   "Energy demand segment" / base, peak /
    u0                      "Energy prod. units"    / u01*u90 /
    u(u0)                   "Energy prod. units"    / u01*u21 /
    d0                      "Unserved demand steps" / d95, d90, d85, d80, d75, d70, d65, d60, d55, d50, d45, d40, d35, d30 /
    d(d0)                   "Unserved demand steps" / d95, d90, d85, d80, d75, d70, d65, d60, d55, d50, d45 /
    sBal                    "Balance table cols"    / Demand, FromSrc, DisTo, FromUp, FromIn, Return, ToIn, DisFrom, ToDwn, ToLoss /
    sInv                    "Irrigation investment" / usdPerMM3, usdPerHa, improve /
    sSec                    "Non-agri sectors"      / HH, ind /
    sRtn                    "Return flow types"     / total, irrig, river, lakes/
    sEly                    "Elec. data types"      / elyCap, aCAPEX, aFOPEX, uVOPEX, uFUELEX, eELEC, eHEAT, rHEAT /
    sSce                    "Scenarios"             / s01*s111 /
    l                       "Loss types"            / seep, evap /
    x                       "Crop mixes"            / cotn, whet, alfa, rice, ric2, stfr, tgrp, meln, tomt, shav, pota, mung, subt, mazf, mazg /

;

* Aliases
ALIAS(c,cc);
ALIAS(m0,mm0);
ALIAS(m,mm);
ALIAS(y0,yy0);
ALIAS(y,yy);
ALIAS(s,ss);
ALIAS(p,pp);
ALIAS(q,qq);
ALIAS(q0,q00);


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
        OutHHI          "Water used for households and industries",
        OutIrg          "Water used on field for irrigation",
        OutDem          "Water used for human purposes",
        OutDis          "Water discharged from node",
        OutFlw          "Water to downstream by river",
        OutInt          "Water to intake",
        OutSto          "Water stored",
        OutLos          "Water losses",
        OutLak          "Water to lakes"
        OutTot          "Water stored, used, lost or to intake or downstream river",
        TotDis          "Total discharge inside section excl non-built reservoirs"
        Balance         "Water balance" /

    e_resAgri /
        AgriProd        "Agricultural production (ton/year)",
        WaterUse        "Agricultural water use (mm3/year)",
        WaterUseB       "Agricultural water use base year (mm3/year)",
        WaterChg        "Water change compared to base year (%)"
        WaterInt        "Water intensity (m3/ha)"
        WaterIntB       "Water intensity (m3/ha)"
        YieldAct        "Yield, actual (ton/ha)"
        LandUse         "Agricultural land use (ha)",
        LandChg         "Agricultural land use change rel. to baseyear (%)"
        LabrUse         "Agricultural labour use (mUSD/year)",
        YWR             "Yield water response (1=optimal yield)"
        ActMarg         "Actual margin (USD/ha)"
        OptMarg         "Optmial margin (USD/ha)"
        /

    e_resIrg /
        actET           "Actual evapotranspiration (m3/ha)"
        optET           "Optimal evapotranspiration (m3/ha)"
        iET             "Index for actual ET (100=optimal)"
        wPhase          "Phase weight (%)"
    /

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



    e_resElec /
        qDemNot         "Notional demand (GWh)"
        qSupTPP         "Thermal electricity production (GWh)"
        qSupHPP         "Hydro electricity production (GWh)"
        qSupHPPr        "Hydro electricity production, reservoirs (GWh)"
        qSupHPPh        "Hydro electricity production, run-of-river (GWh)"
        qSupTot         "Electricity production (GWh)"
        qDemSrv         "Served demand (GWh)"
        qDemUsd         "Unserved demand (GWh)"
        qTrxNet         "Net CAPS electricity export (GWh)"
        qTrxSA          "Export to SA (GWh)"         
        vCstTPP         "Thermal costs (mUSD)"
        vCstUSD         "Unserved demand costs (mUSD)"
        vCstCPX         "Capital costs (mUSD)"
        pMCbase         "Base marginal cost (USD/MWh)"
        pMCpeak         "Peak marginal cost (USD/MWh)"
        vExpCst         "CAPS export cost coverage (mUSD)" 
        vExpTrx         "CAPS transmission profits (mUSD)" 
        vSrpHPP         "Hydro power PS at marg cost (mUSD)"
        vSrpTPP         "Thermal power PS at marg cost (mUSD)"
        vTrxSA          "SA export income (mUSD)" 
        vCS_tpp         "Consumer surplus attributed to thermal (mUSD)"
        vCS_hpp         "Consumer surplus attributed to hydro (mUSD)"
    /





* by scenario and country
    e_resView /
        AgriSales       "Sales value of crops"
        AgriCosts       "Cost of crop production"
        AgriVA          "Value added of agriculture"
        AgriCot         "Cotton production value"
        AgriWht         "Wheat production value"
        AgriVeg         "Vegetables production value"
        AgriFru         "Fruit production value "
        AgriAlf         "Alfalfa production value"
        AgriRic         "Rice production value"
        AgriOth         "Other production value"
        AgriWater       "Water for agriculture"
        AgriWaterSum    "Water for agriculture, summer"
        AgriWaterWin    "Water for agriculture, winter"
        AgriWaterInt    "Water intensity (m3/ha)"
        AgriLandChg     "Land use change (%)"
        HHIuse          "Water use by households and industries"
        Intake          "Total intake (mm3)"
        Nukus           "Maximum intake allowed by Nukus (mm3)"
        Losses          "Losses to ground water and evaporation"
        Nature          "Water allocated to nature"
        Amu-TAD-TUR     "Amu Water flows from TAD to TUR"
        Amu-MID-DWN
        Syr-KYR-UZB     "Syr Water flows from FER to UZB"
        Syr-UZB-TAD     "Syr Water flows from UZb to TAD"   
        Syr-TAD-UZB     "Syr Water flows from TAD to UZB"
        Syr-UZB-KAZ     "Syr Water flows from UZB to KAZ"
        cTherm          "Thermal fuel and OM costs"
        iTherm          "Thermal capital costs"
        sTherm          "Thermal subsidies"
        cField          "Field level OM costs"
        iField          "Field level capital costs"
        cConvey         "Conveyence OM costs"
        iConvey         "Conveyence capital costs"
        cHydro          "HPP OM costs"
        iHydro          "HPP capital costs"
        aggWTP          "Aggregate Willingness to Pay"
        elyCS           "Consumer surplus electricity market"
        elyPS           "Producer surplus electricity market"
        tppPS           "Thermal producer surplus"
        tppVal          "Thermal power value at marg. cost"
        elyVal          "Value of electricity at marg. cost"
        expVal          "Value of electricity exported at export price"
        usdVal          "Value of unserved demand"
        demVal          "Value of served demand at marg. cost"
        qTherm          "Thermal production total"
        qThermWin       "Thermal production winter"
        qThermSum       "Thermal production summer"
        qHydro          "Hydro production total"
        qHydroWin       "Hydro production winter"  
        qHydroSum       "Hydro production summer" 

        qElyDem         "Notional electricity demand (GWh)"
        qElySrvd        "Served electricity demand (GWh)"
        qElyUsrv        "Unserved demand (GWh)"
        qElyUSDw        "Unserved demand, winter (GWh)"
        qElyUSDs        "Unserved demand (GWh)"

        Invest          "Total investments"
        OandM           "Total O and M except thermal fuel"
        Surplus         "Total surplus" /

    e_resBorder /
        Amu-TAD-TUR     
        Amu-MID-DWN
        Syr-KYR-UZB
        Syr-UZB-TAD
        Syr-TAD-UZB
        Syr-UZB-KAZ
        DwnToktogul
        DwnNurek
    /

    e_resPublic /
        canals          "Canal lining"
        pumps           "Pump refurbishment"
        thermal         "Thermal power plants (MW)"
        rogunHPP        "Rogun HPP (mUSD/year)"
        kam1HPP         "Kamberata-1 HPP (mUSD/year)"
        RoRHPP          "Run-of-River HPP (mUSD/year)"
        elyTariff       "Electricity tariff income (mUSD/year)"
        SAxlines        "SA transmission lines"
        CAxlines        "CA transmission lines"
        cotton          "Cotton public income"
        water           "Water public income"
        elec            "Electricity tariff"
    /

    e_resPubItems     / capex, opex, physical, revenue /

;

PARAMETER usdSh(d0) "Share of demand in step unserved"
/ d95 0.05, d90 0.05, d85 0.05, d80 0.05, d75 0.05, d70 0.05, d65 0.05, d60 0.05, d55 0.05, d50 0.05, d45 0.50 /;

PARAMETER usdLvl(d0) "Share of demand in step unserved"
/ d95 0.975, d90 0.925, d85 0.875, d80 0.825, d75 0.775, d70 0.725, d65 0.675, d60 0.625, d55 0.575, d50 0.525, d45 0.475 /;
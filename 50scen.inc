* Automatically created file, please enter changes into beam.xls
TABLE pElyCtrf(y0,m) "Electricity prices USD/MWh"
                m01             m02             m03             m04             m05             m06             m07             m08             m09             m10             m11             m12             
2009            7.000000E+01    7.000000E+01    6.000000E+01    5.000000E+01    4.000000E+01    3.000000E+01    3.000000E+01    4.000000E+01    5.000000E+01    5.000000E+01    6.000000E+01    6.500000E+01    
2020            7.000000E+01    7.000000E+01    6.000000E+01    5.000000E+01    4.000000E+01    3.000000E+01    3.000000E+01    4.000000E+01    5.000000E+01    5.000000E+01    6.000000E+01    6.500000E+01    
2050            7.000000E+01    7.000000E+01    6.000000E+01    5.000000E+01    4.000000E+01    3.000000E+01    3.000000E+01    4.000000E+01    5.000000E+01    5.000000E+01    6.000000E+01    6.500000E+01    

;

TABLE pCrop0(j,sSce) "Crop prices (USD/ton)" 
                base            ctrf            
cot             1.000000E+03    1.000000E+03    
wht             3.000000E+02    3.000000E+02    
ric             5.000000E+02    5.000000E+02    
alf             1.000000E+02    1.000000E+02    
veg             4.000000E+01    4.000000E+01    
fru             4.500000E+02    4.500000E+02    
oth             7.500000E+01    7.500000E+01    

;

PARAMETER pInput0(k) "Input price change (%)" /
labr                            1.000000E+00    
cptl            1.000000E+00    
inpt            1.000000E+00    

/;

PARAMETER baseyear(y0) "Base year choice" /
2001                            0.000000E+00    
2009            1.000000E+00    

/;

TABLE demography(y0,sSec) "Demographic contribution to water use growth" 
                HH              Ind             
2009            0.000000E+00    0.000000E+00    
2020            2.000000E-01    1.000000E-01    
2050            5.000000E-01    2.000000E-01    
;

SCALAR fixedOp "Fixed mode operation for reservoirs" /
0                               

/;

PARAMETER iInv0(sInv) "Irrigation efficiency investments" /
usdPerMM3                       0.000000E+00    
usdPerHa        0.000000E+00    
improve         0.000000E+00    

/;

PARAMETER resNewOp(b) "New reservoirs put in operation" /
res_DAS                         0.000000E+00    
res_ROG         0.000000E+00    
res_KAM         0.000000E+00    
res_ZAR         0.000000E+00    
res_NAR         0.000000E+00    
res_VAH         0.000000E+00    

/;

SCALAR sT "Elasticity of transformation" /
                5.000000E-01    

/;

PARAMETER jAF0(j) "Crops with sluggish land allocation" /
cot                             1.000000E+00    
wht             1.000000E+00    
ric             1.000000E+00    
alf             1.000000E+00    
fal             0.000000E+00    

/;

PARAMETER modelYear "Model year choice" /
2009                            1.000000E+00    
2020            0.000000E+00    
2050            0.000000E+00    
/;

PARAMETER natExtra0(b) "Extra nature demands relative to baseline" /
LAK_ARN                         0.000000E+00    
LAK_ARS         0.000000E+00    
LAK_GOL         0.000000E+00    

/;

SCALAR ctrfBuild0 "Buildup or rundown of reservoirs, share of total capacity" /
                0.000000E+00    

/;

SCALAR fallowLand "Percentage of planning zone fallow land relative to crop land" /
                0.000000E+00    

/;


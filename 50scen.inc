* Automatically created file, please enter changes into beam.xls
PARAMETER pInput0(k) "Input price change (%)" /
labr                            1.000000E+00    
cptl            1.000000E+00    
inpt            1.000000E+00    

/;

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


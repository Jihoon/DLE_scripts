 use "P:\ene.general\DecentLivingEnergy\Surveys\India\NSS 68 2011-2012\Data\NSS68_Sch1-T1_bk_5_6.dta", clear
 
 keep if B5_v02 == 101 | B5_v02 == 107
 sort ID
 merge using "P:\ene.general\DecentLivingEnergy\Surveys\India\NSS 68 2011-2012\Data\NSS68_Sch1-T1_bk_3.dta"
 keep if _merge==3
 
 g price_PDS = B5_v06 / B5_v05 
 
 graph box price_PDS [pweight=hhwt], over(B5_v02) over(B3_v23)
 graph box price_PDS if B3_v23!=3 [pweight=hhwt], over(B5_v02) over(B3_v23) 
 graph box price_PDS [pweight=hhwt], over(B5_v02) over(B3_v23) 
 
 graph box price_PDS if B3_v23!=3 [pweight=hhwt], over(B5_v02) over(B3_v22) 
 graph box B3_v25 if B3_v23!=3 [pweight=hhwt], over(B3_v23) 
 
 sort B3_v23
 by B3_v23: su B3_v25 [aweight=hhwt] 
 
 use "P:\ene.general\DecentLivingEnergy\Surveys\India\NSS 68 2011-2012\Data\NSS68_Sch1-T1_bk_3.dta"
 

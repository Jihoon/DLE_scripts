cd "P:\ene.general\DecentLivingEnergy\Surveys\China\CHFS 2011"
use "P:\ene.general\DecentLivingEnergy\Surveys\China\CHFS 2011\edta\hh_release_eng_20130109.dta", clear
log using CHFS_hh_summary.txt, replace t
de
log close

use "P:\ene.general\DecentLivingEnergy\Surveys\China\CHFS 2011\edta\ind_release_eng_20130109.dta", clear
log using CHFS_ind_summary.txt, replace t
de
log close

use "P:\ene.general\DecentLivingEnergy\Surveys\China\CHFS 2011\edta\master_release_20130109.dta", clear
log using CHFS_master_summary.txt, replace t
de
log close


ta rural c8001_1_mc [iw=swgt], r	// Camera
ta rural c8001_2_mc [iw=swgt], r	// TV
ta rural c8001_3_mc [iw=swgt], r	// Washing machine
ta rural c8001_4_mc [iw=swgt], r	// Refrigerator
ta rural c8001_5_mc [iw=swgt], r	// AC
ta rural c8001_6_mc [iw=swgt], r	// Computer
ta rural c8001_7_mc [iw=swgt], r	// Stereo
ta rural c8001_8_mc [iw=swgt], r	// Solar/Elec water heater
ta rural c8001_9_mc [iw=swgt], r	// Furniture
ta rural c8001_10_mc [iw=swgt], r	// Satellite Receiver
ta rural c8001_11_mc [iw=swgt], r	// Musical Instrument
ta rural c8001_12_mc [iw=swgt], r	// None of the above

g energy_acc = 0
replace energy_acc = 1 if g1005>0
ta energy_acc [iw=swgt]

ta rural c7001 [iw=swgt], r nof 	// Car
ta rural c7053 [iw=swgt], r nof 	// Other vehicle


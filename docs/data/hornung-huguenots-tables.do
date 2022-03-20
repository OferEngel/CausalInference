
use hornung-data-textiles, clear

global firmlist 	ln_workers1802_all ln_looms1802_all ln_input1802_all no_looms imputed_dummy
global townlist 	ln_workers1802_all ln_looms1802_all ln_input1802_all no_looms ln_popcity1802 vieh1816_schaf_ganz_veredelt_pc imputed_dummy
global xlist    	ln_workers1802_all ln_looms1802_all ln_input1802_all no_looms ln_popcity1802 vieh1816_schaf_ganz_veredelt_pc pop1816_prot_pc no_hugue_possible imputed_dummy
global mixlist 		ln_workers1802_all ln_looms1802_all mi_ln_input1802_all no_looms ln_popcity1802 vieh1816_schaf_ganz_veredelt_pc pop1816_prot_pc no_hugue_possible imputed_dummy
global mixlistp 	ln_workers1802_all ln_looms1802_all mi_ln_input1802_all no_looms ln_popcity1802 vieh1816_schaf_ganz_veredelt_pc no_hugue_possible imputed_dummy
global vamixlist 	ln_workers1802_all ln_looms1802_all no_looms ln_popcity1802 vieh1816_schaf_ganz_veredelt_pc pop1816_prot_pc no_hugue_possible imputed_dummy
global lomixlist 	ln_workers1802_all ln_output1802_all no_looms ln_popcity1802 vieh1816_schaf_ganz_veredelt_pc pop1816_prot_pc no_hugue_possible


***	Table 1: Descriptive Statistics

sum ln_output1802_all ln_workers1802_all ln_looms1802_all ln_input1802_all mi_ln_input1802_all hugue_1700_pc hugue_1720_pc ///
	hugue_1795_pc ln_occ1700_hugue_textile ln_popcity1802 vieh1816_schaf_ganz_veredelt_pc pop1816_prot_pc no_hugue_possible ///
	poploss_aggregated_pr if output1802_all!=. & ln_workers1802_all!=. & groupid>100 


*** Table 2: Huguenot Population Share and Productivity in Textile Manufactories in Prussia, 1802

reg ln_output1802_all hugue_1700_pc, cluster(groupid)
reg ln_output1802_all hugue_1700_pc $firmlist, cluster(groupid)
reg ln_output1802_all hugue_1700_pc $townlist, cluster(groupid)
reg ln_output1802_all hugue_1700_pc $xlist, cluster(groupid)
reg ln_output1802_all hugue_1700_pc $xlist textil_1680_dummy, cluster(groupid)
reg ln_output1802_all hugue_1700_pc $mixlist textil_1680_dummy, cluster(groupid)


*** Table 3: Alternative Measures of the Huguenot Influence

reg ln_output1802_all hugue_1720_pc $mixlist textil_1680_dummy, cluster(groupid)
reg ln_output1802_all hugue_1795_pc $mixlist textil_1680_dummy, cluster(groupid)
reg ln_output1802_all ln_occ1700_hugue_textile $mixlist textil_1680_dummy, cluster(groupid)
reg ln_output1802_all hugue_dummy $mixlist textil_1680_dummy, cluster(groupid)
reg ln_output1802_all hugue_1700_pc hugue_1720_pc hugue_1795_pc ln_occ1700_hugue_textile hugue_dummy $mixlist textil_1680_dummy, cluster(groupid)
reg ln_output1802_all hugue_1700_pc $mixlist textil_1680_dummy if hugue_1700!=0, cluster(groupid)
reg ln_output1802_all ln_dist_host $mixlist textil_1680_dummy, cluster(groupid)


*** Table 4: Instrumenting the Huguenot Population Share with Population Losses during the Thirty Years' War, Part 1

reg ln_output1802_all  hugue_1700_pc $mixlist textil_1680_dummy if poploss_keyser!=., cluster(groupid)
reg hugue_1700_pc poploss_keyser $mixlist textil_1680_dummy , cluster(groupid)
ivreg2 ln_output1802_all (hugue_1700_pc=poploss_keyser) $mixlist textil_1680_dummy , first cluster(groupid)
reg hugue_1700_pc poploss_keyser_projected $mixlist textil_1680_dummy , cluster(groupid)
ivreg2 ln_output1802_all (hugue_1700_pc=poploss_keyser_projected) $mixlist textil_1680_dummy , first cluster(groupid)


*** Table 5: Instrumenting the Huguenot Population Share with Population Losses during the Thirty Years' War, Part 2

reg ln_output1802_all  hugue_1700_pc $mixlist textil_1680_dummy if poploss_aggregated_pr!=., cluster(groupid)
reg hugue_1700_pc poploss_aggregated_pr $mixlist textil_1680_dummy , cluster(groupid)
ivreg2 ln_output1802_all (hugue_1700_pc=poploss_aggregated_pr) $mixlist textil_1680_dummy , first cluster(groupid)
reg ln_output1802_all ln_occ1700_hugue_textile $mixlist textil_1680_dummy if poploss_aggregated_pr!=., cluster(groupid)
reg ln_occ1700_hugue_textile poploss_aggregated_pr $mixlist textil_1680_dummy , cluster(groupid)
ivreg2 ln_output1802_all (ln_occ1700_hugue_textile=poploss_aggregated_pr) $mixlist textil_1680_dummy , first cluster(groupid)


*** Table 6: Robustness Tests
*** Panel A

reg ln_output1802_all hugue_1700_pc $mixlistp textil_1680_dummy rel1816_protluth_pc rel1816_protref_pc, cluster(groupid)
reg ln_output1802_all hugue_1700_pc $mixlist textil_1680_dummy ln_piece_value, cluster(groupid)
reg ln_output1802_all hugue_1700_pc $mixlist textil_1680_dummy hats-posament, cluster(groupid)
reg ln_output1802_all hugue_1700_pc $mixlist textil_1680_dummy if looms1802_all!=0, cluster(groupid)
reg ln_output1802_all hugue_1700_pc $mixlist textil_1680_dummy if workers1802_all>=16, cluster(groupid)
preserve
collapse (mean) ln_output1802_all hugue_1700_pc $mixlist poploss_aggregated_pr textil_1680_dummy, by (groupid)
reg ln_output1802_all hugue_1700_pc $mixlist textil_1680_dummy, r
restore
reg mi_ln_value_added hugue_1700_pc $vamixlist textil_1680_dummy, cluster(groupid)

*** Panel B

reg ln_output1802_all hugue_1700_pc $mixlistp textil_1680_dummy rel1816_protluth_pc rel1816_protref_pc if poploss_aggregated_pr!=., cluster(groupid)
reg ln_output1802_all hugue_1700_pc $mixlist textil_1680_dummy ln_piece_value if poploss_aggregated_pr!=., cluster(groupid)
reg ln_output1802_all hugue_1700_pc $mixlist textil_1680_dummy hats-posament if poploss_aggregated_pr!=., cluster(groupid)
reg ln_output1802_all hugue_1700_pc $mixlist textil_1680_dummy if looms1802_all!=0 & poploss_aggregated_pr!=., cluster(groupid)
reg ln_output1802_all hugue_1700_pc $mixlist textil_1680_dummy if workers1802_all>=16 & poploss_aggregated_pr!=., cluster(groupid)
preserve
collapse (mean) ln_output1802_all hugue_1700_pc $mixlist poploss_aggregated_pr textil_1680_dummy, by (groupid)
reg ln_output1802_all hugue_1700_pc  $mixlist textil_1680_dummy if poploss_aggregated_pr!=., r
restore
reg mi_ln_value_added hugue_1700_pc $vamixlist textil_1680_dummy if poploss_aggregated_pr!=., cluster(groupid)

*** Panel C

ivreg2 ln_output1802_all (hugue_1700_pc=poploss_aggregated_pr) $mixlistp textil_1680_dummy rel1816_protluth_pc rel1816_protref_pc if poploss_aggregated_pr!=., cluster(groupid)
ivreg2 ln_output1802_all (hugue_1700_pc=poploss_aggregated_pr) $mixlist textil_1680_dummy ln_piece_value if poploss_aggregated_pr!=., cluster(groupid)
ivreg2 ln_output1802_all (hugue_1700_pc=poploss_aggregated_pr) $mixlist textil_1680_dummy hats-posament if poploss_aggregated_pr!=., cluster(groupid)
ivreg2 ln_output1802_all (hugue_1700_pc=poploss_aggregated_pr) $mixlist textil_1680_dummy if looms1802_all!=0 & poploss_aggregated_pr!=., cluster(groupid)
ivreg2 ln_output1802_all (hugue_1700_pc=poploss_aggregated_pr) $mixlist textil_1680_dummy if workers1802_all>=16 & poploss_aggregated_pr!=., cluster(groupid)
preserve
collapse (mean) ln_output1802_all hugue_1700_pc $mixlist poploss_aggregated_pr textil_1680_dummy, by (groupid)
ivreg2 ln_output1802_all (hugue_1700_pc=poploss_aggregated_pr) $mixlist textil_1680_dummy if poploss_aggregated_pr!=., r
restore
ivreg2 mi_ln_value_added (hugue_1700_pc=poploss_aggregated_pr) $vamixlist textil_1680_dummy if poploss_aggregated_pr!=., cluster(groupid)


*** Table 7: Huguenot Population Share and Productivity in Different Textile Manufactories

reg ln_output1802_all hugue_1700_pc $mixlist textil_1680_dummy wool-silk, cluster(groupid)
reg ln_output1802_all hugue_1700_pc $mixlist textil_1680_dummy if wool==1, cluster(groupid)
reg ln_output1802_all hugue_1700_pc $mixlist textil_1680_dummy if linen==1, cluster(groupid)
reg ln_output1802_all hugue_1700_pc $mixlist textil_1680_dummy if cotton==1 | silk==1, cluster(groupid)


*** Table 8: Huguenot Population Share and Productivity in Different Non-Textile Manufactories

use hornung-data-nontextiles, clear

reg ln_output1802_all hugue_1700_pc ln_workers1802_all mi_ln_input1802_all ln_popcity1802 vieh1816_schaf_ganz_veredelt_pc pop1816_prot_pc no_hugue_possible imputed_dummy textil_1680_dummy leather-wax, cluster(groupid)
reg ln_output1802_all hugue_1700_pc ln_workers1802_all mi_ln_input1802_all ln_popcity1802 vieh1816_schaf_ganz_veredelt_pc pop1816_prot_pc no_hugue_possible imputed_dummy textil_1680_dummy if leather==1, cluster(groupid)
reg ln_output1802_all hugue_1700_pc ln_workers1802_all mi_ln_input1802_all ln_popcity1802 vieh1816_schaf_ganz_veredelt_pc pop1816_prot_pc no_hugue_possible imputed_dummy textil_1680_dummy if metal==1, cluster(groupid)
reg ln_output1802_all hugue_1700_pc ln_workers1802_all mi_ln_input1802_all ln_popcity1802 vieh1816_schaf_ganz_veredelt_pc pop1816_prot_pc no_hugue_possible imputed_dummy textil_1680_dummy if tobacco==1, cluster(groupid)
reg ln_output1802_all hugue_1700_pc ln_workers1802_all mi_ln_input1802_all ln_popcity1802 vieh1816_schaf_ganz_veredelt_pc pop1816_prot_pc no_hugue_possible imputed_dummy textil_1680_dummy if mill==1, cluster(groupid)
reg ln_output1802_all hugue_1700_pc ln_workers1802_all mi_ln_input1802_all ln_popcity1802 vieh1816_schaf_ganz_veredelt_pc pop1816_prot_pc no_hugue_possible imputed_dummy textil_1680_dummy leather-wax if other==1 | wax==1 | vinegar==1 | earthenware==1 | powder==1 | glas==1 | sugar==1 | papermill==1, cluster(groupid)
reg ln_output1802_all hugue_1700_pc ln_workers1802_all mi_ln_input1802_all ln_popcity1802 vieh1816_schaf_ganz_veredelt_pc pop1816_prot_pc no_hugue_possible imputed_dummy textil_1680_dummy if soap==1 , cluster(groupid)


*** Table 9: Exogeneity of the Instrument

use hornung-data-textiles, clear

reg poploss_aggregated_pr ln_pop_before_aggregated if uniquetown==1
reg poploss_aggregated_pr hanseatictraderoutes if uniquetown==1
reg poploss_aggregated_pr river if uniquetown==1
reg poploss_aggregated_pr dist_next_city if uniquetown==1
reg poploss_aggregated_pr km40 if uniquetown==1
reg poploss_aggregated_pr elevation if uniquetown==1
reg poploss_aggregated_pr cityrights if uniquetown==1
reg poploss_aggregated_pr no_markets if uniquetown==1
reg poploss_aggregated_pr bishop if uniquetown==1


*** Table 10: Huguenot Influence and Technology Adoption

use hornung-data-1769, clear

reg ln_looms hugue_1700_pc ln_workers ln_pop1730 no_hugue_possible if textile==1, cluster (groupid)
reg ln_looms hugue_1700_pc ln_workers ln_age ln_pop1730 no_hugue_possible if textile==1, cluster (groupid)
reg ln_looms hugue_1700_pc ln_workers ln_age ln_pop1730 no_hugue_possible if textile==1 & worker>=16, cluster (groupid)

use hornung-data-textiles, clear

reg ln_looms1802_all hugue_1700_pc $lomixlist, cluster(groupid)
reg ln_looms1802_all hugue_1700_pc $lomixlist if workers1802_all>=16, cluster(groupid)

use hornung-data-1816, clear

reg ln_primary ln_hugue1700 ln_pop ln_area ln_agri ln_ships prot_pc ln_sheep urban_pc no_hugue_possible, r
reg ln_secondary ln_hugue1700 ln_pop ln_area ln_agri ln_ships prot_pc ln_sheep urban_pc no_hugue_possible, r

set pf="Program Files"
set config=F:\ProjDir\WorldBank_tabulation\cfg
set geodmsversion=GeoDMS15.0.1

set results=SSP1_2010 SSP1_2020 SSP1_2030 SSP1_2040 SSP1_2050 SSP1_2060 SSP1_2070 SSP1_2080 SSP1_2090 SSP1_2100 SSP2_2010 SSP2_2020 SSP2_2030 SSP2_2040 SSP2_2050 SSP2_2060 SSP2_2070 SSP2_2080 SSP2_2090 SSP2_2100 SSP3_2010 SSP3_2020 SSP3_2030 SSP3_2040 SSP3_2050 SSP3_2060 SSP3_2070 SSP3_2080 SSP3_2090 SSP3_2100 SSP4_2010 SSP4_2020 SSP4_2030 SSP4_2040 SSP4_2050 SSP4_2060 SSP4_2070 SSP4_2080 SSP4_2090 SSP4_2100 SSP5_2010 SSP5_2020 SSP5_2030 SSP5_2040 SSP5_2050 SSP5_2060 SSP5_2070 SSP5_2080 SSP5_2090 SSP5_2100

for %%r in (%results%) do (
	rem C:\%pf%\ObjectVision\%geodmsversion%\GeoDmsRun.exe %config%\main.dms /tabulation/gen_results_per_popgrid_tot/%%r/country_agg /tabulation/gen_results_per_popgrid_tot/%%r/region_agg
	rem C:\%pf%\ObjectVision\%geodmsversion%\GeoDmsRun.exe %config%\main.dms /tabulation/gen_results_per_popgrid_urb/%%r/country_agg /tabulation/gen_results_per_popgrid_urb/%%r/region_agg
	C:\%pf%\ObjectVision\%geodmsversion%\GeoDmsRun.exe %config%\main.dms /tabulation/gen_results_per_popgrid_tot_inform/%%r/country_agg /tabulation/gen_results_per_popgrid_tot_inform/%%r/region_agg
	C:\%pf%\ObjectVision\%geodmsversion%\GeoDmsRun.exe %config%\main.dms /tabulation/gen_results_per_popgrid_tot_rf10/%%r/country_agg /tabulation/gen_results_per_popgrid_tot_rf10/%%r/region_agg
)

pause



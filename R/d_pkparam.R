#' Generate PK reference dataset
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @return A `data.frame` of PK parameters.
#'
#' @examples
#' pk_reference_dataset <- d_pkparam()
#'
#' @export
d_pkparam <- function() {
  pk_dataset <- as.data.frame(matrix(
    c(
      "TMAX", "Time of CMAX", "Tmax", "Plasma/Blood/Serum", "1",
      "CMAX", "Max Conc", "Cmax", "Plasma/Blood/Serum", "2",
      "CMAXD", "Max Conc Norm by Dose", "Cmax/D", "Plasma/Blood/Serum", "3",
      "AUCIFO", "AUC Infinity Obs", "AUCinf obs", "Plasma/Blood/Serum", "4",
      "AUCIFP", "AUC Infinity Pred", "AUCinf pred", "Plasma/Blood/Serum", "5",
      "AUCIFOD", "AUC Infinity Obs Norm by Dose", "AUCinf/D obs", "Plasma/Blood/Serum", "6",
      "AUCIFD", "AUC Infinity Pred Norm by Dose", "AUCinf/D pred", "Plasma/Blood/Serum", "7",
      "AUCPEO", "AUC %Extrapolation Obs", "AUCinf extrap obs", "Plasma/Blood/Serum", "8",
      "AUCPEP", "AUC %Extrapolation Pred", "AUCinf extrap pred", "Plasma/Blood/Serum", "9",
      "AUCINT", "AUC from T1 to T2", "AUCupper-lower ", "Plasma/Blood/Serum", "10",
      "AUCTAU", "AUC Over Dosing Interval", "AUCtau", "Plasma/Blood/Serum", "11",
      "AUCLST", "AUC to Last Nonzero Conc", "AUClast", "Plasma/Blood/Serum", "12",
      "AUCALL", "AUC All", "AUCall", "Plasma/Blood/Serum", "13",
      "AUMCIFO", "AUMC Infinity Obs", "AUMCinf obs", "Plasma/Blood/Serum", "14",
      "AUMCIFP", "AUMC Infinity Pred", "AUMCinf pred", "Plasma/Blood/Serum", "15",
      "AUMCPEO", "AUMC % Extrapolation Obs", "AUMC extrap obs", "Plasma/Blood/Serum", "16",
      "AUMCPEP", "AUMC % Extrapolation Pred", "AUMC extrap pred", "Plasma/Blood/Serum", "17",
      "AUMCTAU", "AUMC Over Dosing Interval", "AUMCtau", "Plasma/Blood/Serum", "18",
      "AUMCLST", "AUMC to Last Nonzero Conc", "AUMClast", "Plasma/Blood/Serum", "19",
      "AURCIFO", "AURC Infinity Obs", "AURCinf obs", "Plasma/Blood/Serum", "20",
      "AURCIFP", "AURC Infinity Pred", "AURCinf pred", "Plasma/Blood/Serum", "21",
      "AURCPEO", "AURC % Extrapolation Obs", "AURC extrap obs", "Plasma/Blood/Serum", "22",
      "AURCPEP", "AURC % Extrapolation Pred", "AURC extrap pred", "Plasma/Blood/Serum", "23",
      "AURCLST", "AURC Dosing to Last Conc", "AURClast", "Plasma/Blood/Serum", "24",
      "AURCALL", "AURC All", "AURCall", "Plasma/Blood/Serum", "25",
      "TLST", "Time of Last Nonzero Conc", "Tlast", "Plasma/Blood/Serum", "26",
      "CO", "Initial Conc", "CO", "Plasma/Blood/Serum", "27",
      "C0", "Initial Conc", "C0", "Plasma/Blood/Serum", "28",
      "CAVG", "Average Conc", "Cavg", "Plasma/Blood/Serum", "29",
      "CLST", "Last Nonzero Conc", "Clast", "Plasma/Blood/Serum", "30",
      "CMIN", "Min Conc", "Cmin", "Plasma/Blood/Serum", "31",
      "LAMZHL", "Half-Life Lambda z", "t1/2", "Plasma/Blood/Serum", "32",
      "CLFO", "Total CL Obs by F", "CL/F obs", "Plasma/Blood/Serum", "33",
      "CLFP", "Total CL Pred by F", "CL/F pred", "Plasma/Blood/Serum", "34",
      "CLO", "Total CL Obs", "CL obs", "Plasma/Blood/Serum", "35",
      "CLP", "Total CL Pred", "CL pred", "Plasma/Blood/Serum", "36",
      "CLSS", "Total CL Steady State Pred", "CLss", "Plasma/Blood/Serum", "37",
      "CLSSF", "Total CL Steady State Pred by F", "CLss/F", "Plasma/Blood/Serum", "38",
      "VZFO", "Vz Obs by F", "Vz/F obs", "Plasma/Blood/Serum", "39",
      "VZFP", "Vz Pred by F", "Vz/F pred", "Plasma/Blood/Serum", "40",
      "VZO", "Vz Obs", "Vz obs", "Plasma/Blood/Serum", "41",
      "VZP", "Vz Pred", "Vz pred", "Plasma/Blood/Serum", "42",
      "VSSO", "Vol Dist Steady State Obs", "Vss obs", "Plasma/Blood/Serum", "43",
      "VSSP", "Vol Dist Steady State Pred", "Vss pred", "Plasma/Blood/Serum", "44",
      "LAMZ", "Lambda z", "Lambda z", "Plasma/Blood/Serum", "45",
      "LAMZLL", "Lambda z Lower Limit", "Lambda z lower", "Plasma/Blood/Serum", "46",
      "LAMZUL", "Lambda z Upper Limit", "Lambda z upper", "Plasma/Blood/Serum", "47",
      "LAMZNPT", "Number of Points for Lambda z", "No points Lambda z", "Plasma/Blood/Serum", "48",
      "MRTIFO", "MRT Infinity Obs", "MRTinf obs", "Plasma/Blood/Serum", "49",
      "MRTIFP", "MRT Infinity Pred", "MRTinf pred", "Plasma/Blood/Serum", "50",
      "MRTLST", "MRT to Last Nonzero Conc", "MRTlast", "Plasma/Blood/Serum", "51",
      "R2", "R Squared", "Rsq", "Plasma/Blood/Serum", "52",
      "R2ADJ", "R Squared Adjusted", "Rsq adjusted", "Plasma/Blood/Serum", "53",
      "TLAG", "Time Until First Nonzero Conc", "TIag", "Plasma/Blood/Serum", "54",
      "TMIN", "Time of CMIN Observation", "Tmin", "Plasma/Blood/Serum", "55",
      "ACCI", "Accumulation Index", "Accumulation Index", "Plasma/Blood/Serum/Urine", "56",
      "FLUCP", "Fluctuation%", "Fluctuation", "Plasma/Blood/Serum", "57",
      "CORRXY", "Correlation Between TimeX and Log ConcY", "Corr xy", "Plasma/Blood/Serum", "58",
      "RCAMINT", "Amt Rec from T1 to T2", "Ae", "Urine", "59",
      "RCPCINT", "Pct Rec from T1 to T2", "Fe", "Urine", "60",
      "VOLPK", "Sum of Urine Vol", "Urine volume", "Urine", "61",
      "RENALCL", "Renal CL", "CLR", "Plasma/Blood/Serum/Urine", "62",
      "ERTMAX", "Time of Max Excretion Rate", "Tmax Rate", "Urine", "63",
      "RMAX", "Time of Maximum Response", "Rmax", "Matrix of PD", "64",
      "RMIN", "Time of Minimum Response", "Rmin", "Matrix of PD", "65",
      "ERMAX", "Max Excretion Rate", "Max excretion rate", "Urine", "66",
      "MIDPTLST", "Midpoint of Collection Interval", "Midpoint last", "Urine", "67",
      "ERLST", "Last Meas Excretion Rate", "Rate last", "Urine", "68",
      "TON", "Time to Onset", "Tonset", "Matrix of PD", "69",
      "TOFF", "Time to Offset", "Toffset", "Matrix of PD", "70",
      "TBBLP", "Time Below Baseline %", "Time %Below Baseline", "Matrix of PD", "71",
      "TBTP", "Time Below Threshold %", "Time %Below Threshold", "Matrix of PD", "72",
      "TABL", "Time Above Baseline", "Time Above Baseline", "Matrix of PD", "73",
      "TAT", "Time Above Threshold", "Time Above Threshold", "Matrix of PD", "74",
      "TBT", "Time Below Threshold", "Time Below Threshold", "Matrix of PD", "75",
      "TBLT", "Time Between Baseline and Threshold", "Time Between Baseline Threshold", "Matrix of PD", "76",
      "BLRSP", "Baseline Response", "Baseline", "Matrix of PD", "77",
      "TSHDRSP", "Response Threshold", "Threshold", "Matrix of PD", "78",
      "AUCABL", "AUC Above Baseline", "AUC above baseline", "Matrix of PD", "79",
      "AUCAT", "AUC Above Threshold", "AUC above threshold", "Matrix of PD", "80",
      "AUCBBL", "AUC Below Baseline", "AUC below baseline", "Matrix of PD", "81",
      "AUCBT", "AUC Below Threshold", "AUC below threshold", "Matrix of PD", "82",
      "AUCBLDIF", "Diff AUC Above Base and AUC Below Base", "AUC diff baseline", "Matrix of PD", "83",
      "AUCTDIF", "Diff AUC Above Thr and AUC Below Thr", "AUCnet threshold", "Matrix of PD", "84",
      "TDIFF", "Diff Time to Offset and Time to Onset", "Diff toffset-tonset", "Matrix of PD", "85",
      "AUCPBEO", "AUC %Back Extrapolation Obs", "AUC%Back extrap obs", "Plasma/Blood/Serum", "86",
      "AUCPBEP", "AUC %Back Extrapolation Pred", "AUC%Back extrap pred", "Plasma/Blood/Serum", "87",
      "TSLP1L", "Lower Time Limit Slope 1st", "Slope1 lower", "Matrix of PD", "88",
      "TSLP1U", "Upper Time Limit Slope 1st Segment", "Slope1 upper", "Matrix of PD", "89",
      "TSLP2L", "Lower Time Limit Slope 2nd Segment", "Slope2 lower", "Matrix of PD", "90",
      "TSLP2U", "Upper Time Limit Slope 2nd Segment", "Slope2 upper", "Matrix of PD", "91",
      "SLP1", "Slope, 1st Segment", "Slope1", "Matrix of PD", "92",
      "SLP2", "Slope, 2nd Segment", "Slope2", "Matrix of PD", "93",
      "SLP1PT", "Number of Points for Slope 1st Segment", "No points slope1", "Matrix of PD", "94",
      "SLP2PT", "Number of Points for Slope 2nd Segment", "No points slope2", "Matrix of PD", "95",
      "R2ADJS1", "R-Squared Adjusted Slope, 1st Segment", "Rsq adjusted slope1", "Matrix of PD", "96",
      "R2ADJS2", "R-Squared Adjusted Slope, 2nd Segment", "Rsq adjusted slope2", "Matrix of PD", "97",
      "R2SLP1", "R Squared, Slope, 1st Segment", "Rsq slope1", "Matrix of PD", "98",
      "R2SLP2", "R Squared, Slope, 2nd Segment", "Rsq slope2", "Matrix of PD", "99",
      "CORRXYS1", "Corr Btw TimeX and Log ConcY, Slope 1st", "Corr xy slope1", "Plasma/Blood/Serum", "100",
      "CORRXYS2", "Corr Btw TimeX and Log ConcY, Slope 1st Slope 2nd", "Corr xy slope2", "Plasma/Blood/Serum", "101",
      "AILAMZ", "Accumulation Index using Lambda z", "AILAMZ", "Plasma/Blood/Serum", "102",
      "ARAUC", "Accumulation Ratio AUCTAU", "ARAUC", "Plasma/Blood/Serum", "103",
      "ARAUCD", "Accum Ratio AUCTAU norm by dose", "ARAUCD", "Plasma/Blood/Serum", "104",
      "ARAUCIFO", "Accum Ratio AUC Infinity Obs", "ARAUCIFO", "Plasma/Blood/Serum", "105",
      "ARAUCIFP", "Accum Ratio AUC Infinity Pred", "ARAUCIFP", "Plasma/Blood/Serum", "106",
      "ARAUCIND", "Accum Ratio AUC T1 to T2 norm by dose", "ARAUCIND_T1_T2_UNIT", "Plasma/Blood/Serum", "107",
      "ARAUCINT", "Accumulation Ratio AUC from T1 to T2", "ARAUCINT_T1_T2_UNIT", "Plasma/Blood/Serum", "108",
      "ARAUCIOD", "Accum Ratio AUCIFO Norm by Dose", "ARAUCIOD", "Plasma/Blood/Serum", "109",
      "ARAUCIPD", "Accum Ratio AUCIFP Norm by Dose", "ARAUCIPD", "Plasma/Blood/Serum", "110",
      "ARAUCLST", "Accum Ratio AUC to Last Nonzero Conc", "ARAUCLST", "Plasma/Blood/Serum", "111",
      "ARCMAX", "Accumulation Ratio Cmax", "ARCMAX", "Plasma/Blood/Serum", "112",
      "ARCMAXD", "Accum Ratio Cmax norm by dose", "ARCMAXD", "Plasma/Blood/Serum", "113",
      "ARCMIN", "Accumulation Ratio Cmin", "ARCMIN", "Plasma/Blood/Serum", "114",
      "ARCMIND", "Accum Ratio Cmin norm by dose", "ARCMIND", "Plasma/Blood/Serum", "115",
      "ARCTROUD", "Accum Ratio Ctrough norm by dose", "ARCTROUD", "Plasma/Blood/Serum", "116",
      "ARCTROUG", "Accumulation Ratio Ctrough", "ARCTROUG", "Plasma/Blood/Serum", "117",
      "AUCALLB", "AUC All Norm by BMI", "AUCall_B", "Plasma/Blood/Serum", "118",
      "AUCALLD", "AUC All Norm by Dose", "AUCall_D", "Plasma/Blood/Serum", "119",
      "AUCALLS", "AUC All Norm by SA", "AUCall_S", "Plasma/Blood/Serum", "120",
      "AUCALLW", "AUC All Norm by WT", "AUCall_W", "Plasma/Blood/Serum", "121",
      "AUCIFOB", "AUC Infinity Obs Norm by BMI", "AUCINF_obs_B", "Plasma/Blood/Serum", "122",
      "AUCIFOLN", "AUC Infinity Obs LN Transformed", "AUCIFOLN", "Plasma/Blood/Serum", "123",
      "AUCIFOS", "AUC Infinity Obs Norm by SA", "AUCINF_obs_S", "Plasma/Blood/Serum", "124",
      "AUCIFOUB", "AUC Infinity Obs, Unbound Drug", "AUCIFOUB", "Plasma/Blood/Serum", "125",
      "AUCIFOW", "AUC Infinity Obs Norm by WT", "AUCINF_obs_W", "Plasma/Blood/Serum", "126",
      "AUCIFPB", "AUC Infinity Pred Norm by BMI", "AUCINF_pred_B", "Plasma/Blood/Serum", "127",
      "AUCIFPD", "AUC Infinity Pred Norm by Dose", "AUCINF_pred_D", "Plasma/Blood/Serum", "128",
      "AUCIFPS", "AUC Infinity Pred Norm by SA", "AUCINF_pred_S", "Plasma/Blood/Serum", "129",
      "AUCIFPUB", "AUC Infinity Pred, Unbound Drug", "AUCIFPUB", "Plasma/Blood/Serum", "130",
      "AUCIFPW", "AUC Infinity Pred Norm by WT", "AUCINF_pred_W", "Plasma/Blood/Serum", "131",
      "AUCINTB", "AUC from T1 to T2 Norm by BMI", "AUC_B_T1_T2_UNIT", "Plasma/Blood/Serum", "132",
      "AUCINTD", "AUC from T1 to T2 Norm by Dose", "AUC_D_T1_T2_UNIT", "Plasma/Blood/Serum", "133",
      "AUCINTS", "AUC from T1 to T2 Norm by SA", "AUC_S_T1_T2_UNIT", "Plasma/Blood/Serum", "134",
      "AUCINTW", "AUC from T1 to T2 Norm by WT", "AUC_W_T1_T2_UNIT", "Plasma/Blood/Serum", "135",
      "AUCLSTB", "AUC to Last Nonzero Conc Norm by BMI", "AUClast_B", "Plasma/Blood/Serum", "136",
      "AUCLSTD", "AUC to Last Nonzero Conc Norm by Dose", "AUClast_D", "Plasma/Blood/Serum", "137",
      "AUCLSTLN", "AUC to Last Nonzero Conc LN Transformed", "AUCLSTLN", "Plasma/Blood/Serum", "138",
      "AUCLSTS", "AUC to Last Nonzero Conc Norm by SA", "AUClast_S", "Plasma/Blood/Serum", "139",
      "AUCLSTUB", "AUC to Last Nonzero Conc, Unbound Drug", "AUCLSTUB", "Plasma/Blood/Serum", "140",
      "AUCLSTW", "AUC to Last Nonzero Conc Norm by WT", "AUClast_W", "Plasma/Blood/Serum", "141",
      "AUCTAUB", "AUC Over Dosing Interval Norm by BMI", "AUC_TAU_B", "Plasma/Blood/Serum", "142",
      "AUCTAUD", "AUC Over Dosing Interval Norm by Dose", "AUC_TAU_D", "Plasma/Blood/Serum", "143",
      "AUCTAUS", "AUC Over Dosing Interval Norm by SA", "AUC_TAU_S", "Plasma/Blood/Serum", "144",
      "AUCTAUW", "AUC Over Dosing Interval Norm by WT", "AUC_TAU_W", "Plasma/Blood/Serum", "145",
      "AUMCIFOB", "AUMC Infinity Obs Norm by BMI", "AUMCINF_obs_B", "Plasma/Blood/Serum", "146",
      "AUMCIFOD", "AUMC Infinity Obs Norm by Dose", "AUMCINF_obs_D", "Plasma/Blood/Serum", "147",
      "AUMCIFOS", "AUMC Infinity Obs Norm by SA", "AUMCINF_obs_S", "Plasma/Blood/Serum", "148",
      "AUMCIFOW", "AUMC Infinity Obs Norm by WT", "AUMCINF_obs_W", "Plasma/Blood/Serum", "149",
      "AUMCIFPB", "AUMC Infinity Pred Norm by BMI", "AUMCINF_pred_B", "Plasma/Blood/Serum", "150",
      "AUMCIFPD", "AUMC Infinity Pred Norm by Dose", "AUMCINF_pred_D", "Plasma/Blood/Serum", "151",
      "AUMCIFPS", "AUMC Infinity Pred Norm by SA", "AUMCINF_pred_S", "Plasma/Blood/Serum", "152",
      "AUMCIFPW", "AUMC Infinity Pred Norm by WT", "AUMCINF_pred_W", "Plasma/Blood/Serum", "153",
      "AUMCLSTB", "AUMC to Last Nonzero Conc Norm by BMI", "AUMClast_B", "Plasma/Blood/Serum", "154",
      "AUMCLSTD", "AUMC to Last Nonzero Conc Norm by Dose", "AUMClast_D", "Plasma/Blood/Serum", "155",
      "AUMCLSTS", "AUMC to Last Nonzero Conc Norm by SA", "AUMClast_S", "Plasma/Blood/Serum", "156",
      "AUMCLSTW", "AUMC to Last Nonzero Conc Norm by WT", "AUMClast_W", "Plasma/Blood/Serum", "157",
      "AUMCTAUB", "AUMC Over Dosing Interval Norm by BMI", "AUMCTAUB", "Plasma/Blood/Serum", "158",
      "AUMCTAUD", "AUMC Over Dosing Interval Norm by Dose", "AUMCTAUD", "Plasma/Blood/Serum", "159",
      "AUMCTAUS", "AUMC Over Dosing Interval Norm by SA", "AUMCTAUS", "Plasma/Blood/Serum", "160",
      "AUMCTAUW", "AUMC Over Dosing Interval Norm by WT", "AUMCTAUW", "Plasma/Blood/Serum", "161",
      "AURCALLB", "AURC All Norm by BMI", "AURCALLB", "Plasma/Blood/Serum", "162",
      "AURCALLD", "AURC All Norm by Dose", "AURCALLD", "Plasma/Blood/Serum", "163",
      "AURCALLS", "AURC All Norm by SA", "AURCALLS", "Plasma/Blood/Serum", "164",
      "AURCALLW", "AURC All Norm by WT", "AURCALLW", "Plasma/Blood/Serum", "165",
      "AURCIFOB", "AURC Infinity Obs Norm by BMI", "AURCIFOB", "Plasma/Blood/Serum", "166",
      "AURCIFOD", "AURC Infinity Obs Norm by Dose", "AURCIFOD", "Plasma/Blood/Serum", "167",
      "AURCIFOS", "AURC Infinity Obs Norm by SA", "AURCIFOS", "Plasma/Blood/Serum", "168",
      "AURCIFOW", "AURC Infinity Obs Norm by WT", "AURCIFOW", "Plasma/Blood/Serum", "169",
      "AURCIFPB", "AURC Infinity Pred Norm by BMI", "AURCIFPB", "Plasma/Blood/Serum", "170",
      "AURCIFPD", "AURC Infinity Pred Norm by Dose", "AURCIFPD", "Plasma/Blood/Serum", "171",
      "AURCIFPS", "AURC Infinity Pred Norm by SA", "AURCIFPS", "Plasma/Blood/Serum", "172",
      "AURCIFPW", "AURC Infinity Pred Norm by WT", "AURCIFPW", "Plasma/Blood/Serum", "173",
      "AURCINT", "AURC from T1 to T2", "AURCINT_T1_T2_UNIT", "Plasma/Blood/Serum", "174",
      "AURCINTB", "AURC from T1 to T2 Norm by BMI", "AURCINTB_T1_T2_UNIT", "Plasma/Blood/Serum", "175",
      "AURCINTD", "AURC from T1 to T2 Norm by Dose", "AURCINTD_T1_T2_UNIT", "Plasma/Blood/Serum", "176",
      "AURCINTS", "AURC from T1 to T2 Norm by SA", "AURCINTS_T1_T2_UNIT", "Plasma/Blood/Serum", "177",
      "AURCINTW", "AURC from T1 to T2 Norm by WT", "AURCINTW_T1_T2_UNIT", "Plasma/Blood/Serum", "178",
      "AURCLSTB", "AURC to Last Nonzero Rate Norm by BMI", "AURCLSTB", "Plasma/Blood/Serum", "179",
      "AURCLSTD", "AURC to Last Nonzero Rate Norm by Dose", "AURCLSTD", "Plasma/Blood/Serum", "180",
      "AURCLSTS", "AURC to Last Nonzero Rate Norm by SA", "AURCLSTS", "Plasma/Blood/Serum", "181",
      "AURCLSTW", "AURC to Last Nonzero Rate Norm by WT", "AURCLSTW", "Plasma/Blood/Serum", "182",
      "C0B", "Initial Conc Norm by BMI", "C0B", "Plasma/Blood/Serum", "183",
      "C0D", "Initial Conc Norm by Dose", "C0D", "Plasma/Blood/Serum", "184",
      "C0S", "Initial Conc Norm by SA", "C0S", "Plasma/Blood/Serum", "185",
      "C0W", "Initial Conc Norm by WT", "C0W", "Plasma/Blood/Serum", "186",
      "CAVGB", "Average Conc Norm by BMI", "CAVGB", "Plasma/Blood/Serum", "187",
      "CAVGD", "Average Conc Norm by Dose", "CAVGD", "Plasma/Blood/Serum", "188",
      "CAVGINT", "Average Conc from T1 to T2", "CAVGINT_T1_T2_UNIT", "Plasma/Blood/Serum", "189",
      "CAVGINTB", "Average Conc from T1 to T2 Norm by BMI", "CAVGINTB_T1_T2_UNIT", "Plasma/Blood/Serum", "190",
      "CAVGINTD", "Average Conc from T1 to T2 Norm by Dose", "CAVGINTD_T1_T2_UNIT", "Plasma/Blood/Serum", "191",
      "CAVGINTS", "Average Conc from T1 to T2 Norm by SA", "CAVGINTS_T1_T2_UNIT", "Plasma/Blood/Serum", "192",
      "CAVGINTW", "Average Conc from T1 to T2 Norm by WT", "CAVGINTW_T1_T2_UNIT", "Plasma/Blood/Serum", "193",
      "CAVGS", "Average Conc Norm by SA", "CAVGS", "Plasma/Blood/Serum", "194",
      "CAVGW", "Average Conc Norm by WT", "CAVGW", "Plasma/Blood/Serum", "195",
      "CHTMAX", "Concentration at Half Tmax", "CHTMAX", "Plasma/Blood/Serum", "196",
      "CLFOB", "Total CL Obs by F Norm by BMI", "CLFOB", "Plasma/Blood/Serum", "197",
      "CLFOD", "Total CL Obs by F Norm by Dose", "CLFOD", "Plasma/Blood/Serum", "198",
      "CLFOS", "Total CL Obs by F Norm by SA", "CLFOS", "Plasma/Blood/Serum", "199",
      "CLFOW", "Total CL Obs by F Norm by WT", "CLFOW", "Plasma/Blood/Serum", "200",
      "CLFPB", "Total CL Pred by F Norm by BMI", "CLFPB", "Plasma/Blood/Serum", "201",
      "CLFPD", "Total CL Pred by F Norm by Dose", "CLFPD", "Plasma/Blood/Serum", "202",
      "CLFPS", "Total CL Pred by F Norm by SA", "CLFPS", "Plasma/Blood/Serum", "203",
      "CLFPW", "Total CL Pred by F Norm by WT", "CLFPW", "Plasma/Blood/Serum", "204",
      "CLFTAU", "Total CL by F for Dose Int", "CLFTAU", "Plasma/Blood/Serum", "205",
      "CLFTAUB", "Total CL by F for Dose Int Norm by BMI", "CLFTAUB", "Plasma/Blood/Serum", "206",
      "CLFTAUD", "Total CL by F for Dose Int Norm by Dose", "CLFTAUD", "Plasma/Blood/Serum", "207",
      "CLFTAUS", "Total CL by F for Dose Int Norm by SA", "CLFTAUS", "Plasma/Blood/Serum", "208",
      "CLFTAUW", "Total CL by F for Dose Int Norm by WT", "CLFTAUW", "Plasma/Blood/Serum", "209",
      "CLFUB", "Apparent CL for Unbound Drug", "CLFUB", "Plasma/Blood/Serum", "210",
      "CLOB", "Total CL Obs Norm by BMI", "CLOB", "Plasma/Blood/Serum", "211",
      "CLOD", "Total CL Obs Norm by Dose", "CLOD", "Plasma/Blood/Serum", "212",
      "CLOS", "Total CL Obs Norm by SA", "CLOS", "Plasma/Blood/Serum", "213",
      "CLOUB", "Total CL Obs for Unbound Drug", "CLOUB", "Plasma/Blood/Serum", "214",
      "CLOW", "Total CL Obs Norm by WT", "CLOW", "Plasma/Blood/Serum", "215",
      "CLPB", "Total CL Pred Norm by BMI", "CLPB", "Plasma/Blood/Serum", "216",
      "CLPD", "Total CL Pred Norm by Dose", "CLPD", "Plasma/Blood/Serum", "217",
      "CLPS", "Total CL Pred Norm by SA", "CLPS", "Plasma/Blood/Serum", "218",
      "CLPUB", "Total CL Pred for Unbound Drug", "CLPUB", "Plasma/Blood/Serum", "219",
      "CLPW", "Total CL Pred Norm by WT", "CLPW", "Plasma/Blood/Serum", "220",
      "CLRPCLEV", "Renal CL as Pct CL EV", "CLRPCLEV", "Urine", "221",
      "CLRPCLIV", "Renal CL as Pct CL IV", "CLRPCLIV", "Urine", "222",
      "CLSTB", "Last Nonzero Conc Norm by BMI", "CLSTB", "Plasma/Blood/Serum", "223",
      "CLSTD", "Last Nonzero Conc Norm by Dose", "CLSTD", "Plasma/Blood/Serum", "224",
      "CLSTS", "Last Nonzero Conc Norm by SA", "CLSTS", "Plasma/Blood/Serum", "225",
      "CLSTW", "Last Nonzero Conc Norm by WT", "CLSTW", "Plasma/Blood/Serum", "226",
      "CLTAU", "Total CL for Dose Int", "CLTAU", "Plasma/Blood/Serum", "227",
      "CLTAUB", "Total CL for Dose Int Norm by BMI", "CLTAUB", "Plasma/Blood/Serum", "228",
      "CLTAUD", "Total CL for Dose Int Norm by Dose", "CLTAUD", "Plasma/Blood/Serum", "229",
      "CLTAUS", "Total CL for Dose Int Norm by SA", "CLTAUS", "Plasma/Blood/Serum", "230",
      "CLTAUW", "Total CL for Dose Int Norm by WT", "CLTAUW", "Plasma/Blood/Serum", "231",
      "CMAXB", "Max Conc Norm by BMI", "CMAX_B", "Plasma/Blood/Serum", "232",
      "CMAXLN", "Max Conc LN Transformed", "CMAXLN", "Plasma/Blood/Serum", "233",
      "CMAXS", "Max Conc Norm by SA", "CMAXS", "Plasma/Blood/Serum", "234",
      "CMAXUB", "Max Conc, Unbound Drug", "CMAXUB", "Plasma/Blood/Serum", "235",
      "CMAXW", "Max Conc Norm by WT", "CMAXW", "Plasma/Blood/Serum", "236",
      "CMINB", "Min Conc Norm by BMI", "CMINB", "Plasma/Blood/Serum", "237",
      "CMIND", "Min Conc Norm by Dose", "CMIND", "Plasma/Blood/Serum", "238",
      "CMINS", "Min Conc Norm by SA", "CMINS", "Plasma/Blood/Serum", "239",
      "CMINW", "Min Conc Norm by WT", "CMINW", "Plasma/Blood/Serum", "240",
      "CONC", "Concentration", "CONC", "Plasma/Blood/Serum", "241",
      "CONCB", "Conc by BMI", "CONCB", "Plasma/Blood/Serum", "242",
      "CONCD", "Conc by Dose", "CONCD", "Plasma/Blood/Serum", "243",
      "CONCS", "Conc by SA", "CONCS", "Plasma/Blood/Serum", "244",
      "CONCW", "Conc by WT", "CONCW", "Plasma/Blood/Serum", "245",
      "CTROUGH", "Conc Trough", "CTROUGH", "Plasma/Blood/Serum", "246",
      "CTROUGHB", "Conc Trough by BMI", "CTROUGHB", "Plasma/Blood/Serum", "247",
      "CTROUGHD", "Conc Trough by Dose", "CTROUGHD", "Plasma/Blood/Serum", "248",
      "CTROUGHS", "Conc Trough by SA", "CTROUGHS", "Plasma/Blood/Serum", "249",
      "CTROUGHW", "Conc Trough by WT", "CTROUGHW", "Plasma/Blood/Serum", "250",
      "EFFHL", "Effective Half-Life", "EFFHL", "Plasma/Blood/Serum", "251",
      "ERINT", "Excret Rate from T1 to T2", "ERINT_T1_T2_UNIT", "Plasma/Blood/Serum", "252",
      "ERINTB", "Excret Rate from T1 to T2 Norm by BMI", "ERINTB_T1_T2_UNIT", "Plasma/Blood/Serum", "253",
      "ERINTD", "Excret Rate from T1 to T2 Norm by Dose", "ERINTD_T1_T2_UNIT", "Plasma/Blood/Serum", "254",
      "ERINTS", "Excret Rate from T1 to T2 Norm by SA", "ERINTS_T1_T2_UNIT", "Plasma/Blood/Serum", "255",
      "ERINTW", "Excret Rate from T1 to T2 Norm by WT", "ERINTW_T1_T2_UNIT", "Plasma/Blood/Serum", "256",
      "ERLSTB", "Last Meas Excretion Rate Norm by BMI", "ERLSTB", "Plasma/Blood/Serum", "257",
      "ERLSTD", "Last Meas Excretion Rate Norm by Dose", "ERLSTD", "Plasma/Blood/Serum", "258",
      "ERLSTS", "Last Meas Excretion Rate Norm by SA", "ERLSTS", "Plasma/Blood/Serum", "259",
      "ERLSTW", "Last Meas Excretion Rate Norm by WT", "ERLSTW", "Plasma/Blood/Serum", "260",
      "ERMAXB", "Max Excretion Rate Norm by BMI", "ERMAXB", "Plasma/Blood/Serum", "261",
      "ERMAXD", "Max Excretion Rate Norm by Dose", "ERMAXD", "Plasma/Blood/Serum", "262",
      "ERMAXS", "Max Excretion Rate Norm by SA", "ERMAXS", "Plasma/Blood/Serum", "263",
      "ERMAXW", "Max Excretion Rate Norm by WT", "ERMAXW", "Plasma/Blood/Serum", "264",
      "ERTLST", "Midpoint of Interval of Last Nonzero ER", "ERTLST", "Plasma/Blood/Serum", "265",
      "FABS", "Absolute Bioavailability", "FABS", "Plasma/Blood/Serum", "266",
      "FB", "Fraction Bound", "FB", "Plasma/Blood/Serum", "267",
      "FREL", "Relative Bioavailability", "FREL", "Plasma/Blood/Serum", "268",
      "FREXINT", "Fract Excr from T1 to T2", "FREXINT_T1_T2_UNIT", "Plasma/Blood/Serum", "269",
      "FU", "Fraction Unbound", "FU", "Plasma/Blood/Serum", "270",
      "HDCL", "Hemodialysis Clearance", "HDCL", "Plasma/Blood/Serum", "271",
      "HDER", "Hemodialysis Extraction Ratio", "HDER", "Plasma/Blood/Serum", "272",
      "HTMAX", "Half Tmax", "HTMAX", "Plasma/Blood/Serum", "273",
      "LAMZLTAU", "Lambda z Lower Limit TAU", "LAMZLTAU", "Plasma/Blood/Serum", "274",
      "LAMZNTAU", "Number of Points for Lambda z TAU", "LAMZNTAU", "Plasma/Blood/Serum", "275",
      "LAMZSPN", "Lambda z Span", "LAMZSPN", "Plasma/Blood/Serum", "276",
      "LAMZTAU", "Lambda z TAU", "LAMZTAU", "Plasma/Blood/Serum", "277",
      "LAMZUTAU", "Lambda z Upper Limit TAU", "LAMZUTAU", "Plasma/Blood/Serum", "278",
      "MAT", "Mean Absorption Time", "MAT", "Plasma/Blood/Serum", "279",
      "MRAUCIFO", "Metabolite Ratio for AUC Infinity Obs", "MRAUCIFO", "Plasma/Blood/Serum", "280",
      "MRAUCIFP", "Metabolite Ratio for AUC Infinity Pred", "MRAUCIFP", "Plasma/Blood/Serum", "281",
      "MRAUCINT", "Metabolite Ratio AUC from T1 to T2", "MRAUCINT_T1_T2_UNIT", "Plasma/Blood/Serum", "282",
      "MRAUCLST", "Metabolite Ratio AUC Last Nonzero Conc", "MRAUCLST", "Plasma/Blood/Serum", "283",
      "MRAUCTAU", "Metabolite Ratio for AUC Dosing Interval", "MRAUCTAU", "Plasma/Blood/Serum", "284",
      "MRCMAX", "Metabolite Ratio for Max Conc", "MRCMAX", "Plasma/Blood/Serum", "285",
      "MRTEVIFO", "MRT Extravasc Infinity Obs", "MRTEVIFO", "Plasma/Blood/Serum", "286",
      "MRTEVIFP", "MRT Extravasc Infinity Pred", "MRTEVIFP", "Plasma/Blood/Serum", "287",
      "MRTEVLST", "MRT Extravasc to Last Nonzero Conc", "MRTEVLST", "Plasma/Blood/Serum", "288",
      "MRTIVIFO", "MRT Intravasc Infinity Obs", "MRTIVIFO", "Plasma/Blood/Serum", "289",
      "MRTIVIFP", "MRT Intravasc Infinity Pred", "MRTIVIFP", "Plasma/Blood/Serum", "290",
      "MRTIVLST", "MRT Intravasc to Last Nonzero Conc", "MRTIVLST", "Plasma/Blood/Serum", "291",
      "NRENALCL", "Nonrenal CL", "NRENALCL", "Urine", "292",
      "NRENLCLB", "Nonrenal CL Norm by BMI", "NRENLCLB", "Urine", "293",
      "NRENLCLD", "Nonrenal CL Norm by Dose", "NRENLCLD", "Urine", "294",
      "NRENLCLS", "Nonrenal CL Norm by SA", "NRENLCLS", "Urine", "295",
      "NRENLCLW", "Nonrenal CL Norm by WT", "NRENLCLW", "Urine", "296",
      "PTROUGHR", "Peak Trough Ratio", "PTROUGHR", "Plasma/Blood/Serum", "297",
      "RAAUC", "Ratio AUC", "RAAUC", "Plasma/Blood/Serum", "298",
      "RAAUCIFO", "Ratio AUC Infinity Obs", "RAAUCIFO", "Plasma/Blood/Serum", "299",
      "RAAUCIFP", "Ratio AUC Infinity Pred", "RAAUCIFP", "Plasma/Blood/Serum", "300",
      "RACMAX", "Ratio CMAX", "RACMAX", "Plasma/Blood/Serum", "301",
      "RAMAXMIN", "Ratio of CMAX to CMIN", "RAMAXMIN", "Plasma/Blood/Serum", "302",
      "RCAMIFO", "Amt Rec Infinity Obs", "RCAMIFO", "Plasma/Blood/Serum", "303",
      "RCAMIFOB", "Amt Rec Infinity Obs Norm by BMI", "RCAMIFOB", "Plasma/Blood/Serum", "304",
      "RCAMIFOS", "Amt Rec Infinity Obs Norm by SA", "RCAMIFOS", "Plasma/Blood/Serum", "305",
      "RCAMIFOW", "Amt Rec Infinity Obs Norm by WT", "RCAMIFOW", "Plasma/Blood/Serum", "306",
      "RCAMIFP", "Amt Rec Infinity Pred", "RCAMIFP", "Plasma/Blood/Serum", "307",
      "RCAMIFPB", "Amt Rec Infinity Pred Norm by BMI", "RCAMIFPB", "Plasma/Blood/Serum", "308",
      "RCAMIFPS", "Amt Rec Infinity Pred Norm by SA", "RCAMIFPS", "Plasma/Blood/Serum", "309",
      "RCAMIFPW", "Amt Rec Infinity Pred Norm by WT", "RCAMIFPW", "Plasma/Blood/Serum", "310",
      "RCAMINTB", "Amt Rec from T1 to T2 Norm by BMI", "RCAMINTB_T1_T2_UNIT", "Plasma/Blood/Serum", "311",
      "RCAMINTS", "Amt Rec from T1 to T2 Norm by SA", "RCAMINTS_T1_T2_UNIT", "Plasma/Blood/Serum", "312",
      "RCAMINTW", "Amt Rec from T1 to T2 Norm by WT", "RCAMINTW_T1_T2_UNIT", "Plasma/Blood/Serum", "313",
      "RCAMTAU", "Amt Rec Over Dosing Interval", "RCAMTAU", "Plasma/Blood/Serum", "314",
      "RCAMTAUB", "Amt Rec Over Dosing Interval Norm by BMI", "RCAMTAUB", "Plasma/Blood/Serum", "315",
      "RCAMTAUS", "Amt Rec Over Dosing Interval Norm by SA", "RCAMTAUS", "Plasma/Blood/Serum", "316",
      "RCAMTAUW", "Amt Rec Over Dosing Interval Norm by WT", "RCAMTAUW", "Plasma/Blood/Serum", "317",
      "RCPCIFO", "Pct Rec Infinity Obs", "RCPCIFO", "Plasma/Blood/Serum", "318",
      "RCPCIFOB", "Pct Rec Infinity Obs Norm by BMI", "RCPCIFOB", "Plasma/Blood/Serum", "319",
      "RCPCIFOS", "Pct Rec Infinity Obs Norm by SA", "RCPCIFOS", "Plasma/Blood/Serum", "320",
      "RCPCIFOW", "Pct Rec Infinity Obs Norm by WT", "RCPCIFOW", "Plasma/Blood/Serum", "321",
      "RCPCIFP", "Pct Rec Infinity Pred", "RCPCIFP", "Plasma/Blood/Serum", "322",
      "RCPCIFPB", "Pct Rec Infinity Pred Norm by BMI", "RCPCIFPB", "Plasma/Blood/Serum", "323",
      "RCPCIFPS", "Pct Rec Infinity Pred Norm by SA", "RCPCIFPS", "Plasma/Blood/Serum", "324",
      "RCPCIFPW", "Pct Rec Infinity Pred Norm by WT", "RCPCIFPW", "Plasma/Blood/Serum", "325",
      "RCPCINTB", "Pct Rec from T1 to T2 Norm by BMI", "RCPCINTB_T1_T2_UNIT", "Plasma/Blood/Serum", "326",
      "RCPCINTS", "Pct Rec from T1 to T2 Norm by SA", "RCPCINTS_T1_T2_UNIT", "Plasma/Blood/Serum", "327",
      "RCPCINTW", "Pct Rec from T1 to T2 Norm by WT", "RCPCINTW_T1_T2_UNIT", "Plasma/Blood/Serum", "328",
      "RCPCLST", "Pct Rec to Last Nonzero Conc", "RCPCLST", "Plasma/Blood/Serum", "329",
      "RCPCTAU", "Pct Rec Over Dosing Interval", "RCPCTAU", "Plasma/Blood/Serum", "330",
      "RCPCTAUB", "Pct Rec Over Dosing Interval Norm by BMI", "RCPCTAUB", "Plasma/Blood/Serum", "331",
      "RCPCTAUS", "Pct Rec Over Dosing Interval Norm by SA", "RCPCTAUS", "Plasma/Blood/Serum", "332",
      "RCPCTAUW", "Pct Rec Over Dosing Interval Norm by WT", "RCPCTAUW", "Plasma/Blood/Serum", "333",
      "RENALCLB", "Renal CL Norm by BMI", "RENALCLB", "Urine", "334",
      "RENALCLD", "Renal CL Norm by Dose", "RENALCLD", "Urine", "335",
      "RENALCLS", "Renal CL Norm by SA", "RENALCLS", "Urine", "336",
      "RENALCLW", "Renal CL Norm by WT", "RENALCLW", "Urine", "337",
      "RENCLTAU", "Renal CL for Dose Int", "RENCLTAU", "Urine", "338",
      "RNCLINT", "Renal CL from T1 to T2", "RNCLINT_T1_T2_UNIT", "Urine", "339",
      "RNCLINTB", "Renal CL from T1 to T2 Norm by BMI", "RNCLINTB_T1_T2_UNIT", "Urine", "340",
      "RNCLINTD", "Renal CL from T1 to T2 Norm by Dose", "RNCLINTD_T1_T2_UNIT", "Urine", "341",
      "RNCLINTS", "Renal CL from T1 to T2 Norm by SA", "RNCLINTS_T1_T2_UNIT", "Urine", "342",
      "RNCLINTW", "Renal CL from T1 to T2 Norm by WT", "RNCLINTW_T1_T2_UNIT", "Urine", "343",
      "RNCLTAUB", "Renal CL for Dose Int Norm by BMI", "RNCLTAUB", "Urine", "344",
      "RNCLTAUD", "Renal CL for Dose Int Norm by Dose", "RNCLTAUD", "Urine", "345",
      "RNCLTAUS", "Renal CL for Dose Int Norm by SA", "RNCLTAUS", "Urine", "346",
      "RNCLTAUW", "Renal CL for Dose Int Norm by WT", "RNCLTAUW", "Urine", "347",
      "RNCLUB", "Renal CL for Unbound Drug", "RNCLUB", "Urine", "348",
      "SRAUC", "Stationarity Ratio AUC", "SRAUC", "Plasma/Blood/Serum", "349",
      "SWING", "Swing", "SWING", "Plasma/Blood/Serum", "350",
      "TAUHL", "Half-Life TAU", "TAUHL", "Plasma/Blood/Serum", "351",
      "TBBL", "Time Below Baseline", "Time_Below_B", "Plasma/Blood/Serum", "352",
      "TROUGHPR", "Trough Peak Ratio", "TROUGHPR", "Plasma/Blood/Serum", "353",
      "V0", "Vol Dist Initial", "V0", "Plasma/Blood/Serum", "354",
      "V0B", "Vol Dist Initial Norm by BMI", "V0B", "Plasma/Blood/Serum", "355",
      "V0D", "Vol Dist Initial Norm by Dose", "V0D", "Plasma/Blood/Serum", "356",
      "V0S", "Vol Dist Initial Norm by SA", "V0S", "Plasma/Blood/Serum", "357",
      "V0W", "Vol Dist Initial Norm by WT", "V0W", "Plasma/Blood/Serum", "358",
      "VSSOB", "Vol Dist Steady State Obs Norm by BMI", "VSSOB", "Plasma/Blood/Serum", "359",
      "VSSOBD", "Vol Dist Steady State Obs by B", "VSSOBD", "Plasma/Blood/Serum", "360",
      "VSSOD", "Vol Dist Steady State Obs Norm by Dose", "VSSOD", "Plasma/Blood/Serum", "361",
      "VSSOF", "Vol Dist Steady State Obs by F", "VSSOF", "Plasma/Blood/Serum", "362",
      "VSSOS", "Vol Dist Steady State Obs Norm by SA", "VSSOS", "Plasma/Blood/Serum", "363",
      "VSSOUB", "Vol Dist Steady State Obs by UB", "VSSOUB", "Plasma/Blood/Serum", "364",
      "VSSOW", "Vol Dist Steady State Obs Norm by WT", "VSSOW", "Plasma/Blood/Serum", "365",
      "VSSPB", "Vol Dist Steady State Pred Norm by BMI", "VSSPB", "Plasma/Blood/Serum", "366",
      "VSSPBD", "Vol Dist Steady State Pred by B", "VSSPBD", "Plasma/Blood/Serum", "367",
      "VSSPD", "Vol Dist Steady State Pred Norm by Dose", "VSSPD", "Plasma/Blood/Serum", "368",
      "VSSPF", "Vol Dist Steady State Pred by F", "VSSPF", "Plasma/Blood/Serum", "369",
      "VSSPS", "Vol Dist Steady State Pred Norm by SA", "VSSPS", "Plasma/Blood/Serum", "370",
      "VSSPUB", "Vol Dist Steady State Pred by UB", "VSSPUB", "Plasma/Blood/Serum", "371",
      "VSSPW", "Vol Dist Steady State Pred Norm by WT", "VSSPW", "Plasma/Blood/Serum", "372",
      "VZ", "Vol Z", "Vz", "Plasma/Blood/Serum", "373",
      "VZF", "Vol Z by F", "Vz_F", "Plasma/Blood/Serum", "374",
      "VZFOB", "Vz Obs by F Norm by BMI", "VZFOB", "Plasma/Blood/Serum", "375",
      "VZFOD", "Vz Obs by F Norm by Dose", "VZFOD", "Plasma/Blood/Serum", "376",
      "VZFOS", "Vz Obs by F Norm by SA", "VZFOS", "Plasma/Blood/Serum", "377",
      "VZFOUB", "Vz Obs by F for UB", "VZFOUB", "Plasma/Blood/Serum", "378",
      "VZFOW", "Vz Obs by F Norm by WT", "VZFOW", "Plasma/Blood/Serum", "379",
      "VZFPB", "Vz Pred by F Norm by BMI", "VZFPB", "Plasma/Blood/Serum", "380",
      "VZFPD", "Vz Pred by F Norm by Dose", "VZFPD", "Plasma/Blood/Serum", "381",
      "VZFPS", "Vz Pred by F Norm by SA", "VZFPS", "Plasma/Blood/Serum", "382",
      "VZFPUB", "Vz Pred by F for UB", "VZFPUB", "Plasma/Blood/Serum", "383",
      "VZFPW", "Vz Pred by F Norm by WT", "VZFPW", "Plasma/Blood/Serum", "384",
      "VZFTAU", "Vz for Dose Int by F", "VZFTAU", "Plasma/Blood/Serum", "385",
      "VZFTAUB", "Vz for Dose Int by F Norm by BMI", "VZFTAUB", "Plasma/Blood/Serum", "386",
      "VZFTAUD", "Vz for Dose Int by F Norm by Dose", "VZFTAUD", "Plasma/Blood/Serum", "387",
      "VZFTAUS", "Vz for Dose Int by F Norm by SA", "VZFTAUS", "Plasma/Blood/Serum", "388",
      "VZFTAUW", "Vz for Dose Int by F Norm by WT", "VZFTAUW", "Plasma/Blood/Serum", "389",
      "VZOB", "Vz Obs Norm by BMI", "VZOB", "Plasma/Blood/Serum", "390",
      "VZOD", "Vz Obs Norm by Dose", "VZOD", "Plasma/Blood/Serum", "391",
      "VZOS", "Vz Obs Norm by SA", "VZOS", "Plasma/Blood/Serum", "392",
      "VZOUB", "Vz Obs for UB", "VZOUB", "Plasma/Blood/Serum", "393",
      "VZOW", "Vz Obs Norm by WT", "VZOW", "Plasma/Blood/Serum", "394",
      "VZPB", "Vz Pred Norm by BMI", "VZPB", "Plasma/Blood/Serum", "395",
      "VZPD", "Vz Pred Norm by Dose", "VZPD", "Plasma/Blood/Serum", "396",
      "VZPS", "Vz Pred Norm by SA", "VZPS", "Plasma/Blood/Serum", "397",
      "VZPUB", "Vz Pred for UB", "VZPUB", "Plasma/Blood/Serum", "398"
    ),
    ncol = 5,
    byrow = TRUE
  ))
  colnames(pk_dataset) <- c("PARAMCD", "PARAM", "TLG_DISPLAY", "MATRIX", "TLG_ORDER")
  pk_dataset
}

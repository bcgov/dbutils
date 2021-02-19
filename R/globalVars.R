#### global variables ----
## https://r-pkgs.org/package-within.html#echo-a-working-package
## When using dplyr and unquoted variable names inside a package, a note is generated that:
## fun.name: no visible binding for global variable 'myVar'
## To address this, the global variables have been set here.


# Region IDs ----
RD_id <- c(0, 1, 3, 5, 7, 9, 15, 17, 19, 21, 23, 24, 26, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57, 59)
CA_id <- c(0, 11, 12, 13, 21, 22, 23, 24, 25, 31, 32, 41, 42, 43)
SD_id <- c(0, 5, 6, 8, 10, 19, 20, 22, 23, 27, 28, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 57, 58, 59, 60, 61, 62, 63, 64, 67, 68, 69, 70, 71, 72, 73, 74, 75, 78, 79, 81, 82, 83, 84, 85, 87, 91, 92)
HY_id <- c(0, 1, 2, 3, 4, 5)
CF_id <- c(0, 1, 2, 3, 4)
CL_id <- c(0, 1101, 1102, 1203, 1204, 1205, 1306, 1307, 1308, 1309, 2110, 2111, 2112, 2113, 2214, 2215, 2216, 2317, 2318, 2319, 2320, 2321, 2422, 2423, 2424, 2525, 2526, 2527, 2528, 2529, 3130, 3131, 3132, 3133, 3234, 3235, 3236, 3237, 3238, 3239, 4140, 4141, 4142, 4243, 4244, 4245, 4346, 4347)
HS_id <- c(0, 11, 12, 13, 14, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53)
HA_id <- c(0,111,112,113,114,115,116,121,122,123,124,125,126,127,131,132,133,134,135,136,137,138,139,141,142,143,144,145,146,147,148,149,211,212,213,214,215,221,222,223,224,231,232,233,234,311,321,322,323,324,325,326,331,332,333,334,335,336,337,411,412,413,414,421,422,423,424,425,426,431,432,433,434,510,511,512,513,514,515,516,517,518,519,521,522,523,524,531,532,533)
DR_id <- c(0, 1, 2, 5, 7, 6, 3, 4, 8)
PS_id <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
SR_id <- c(0, 905, 907, 913, 915, 918, 920, 925, 930, 932, 933, 934, 935, 937, 938, 939, 940, 943, 944, 945, 950, 952, 955, 965, 970, 975, 977, 400, 410)
CH_id <- c(0, 1110, 1120, 1130, 1140, 1150, 1160, 1210, 1220, 1230, 1240, 1250, 1260, 1270, 1310, 1320, 1330, 1340, 1350, 1361, 1362, 1371, 1372, 1373, 1374, 1375, 1376, 1377, 1380, 1390, 1410, 1420, 1431, 1432, 1433, 1440, 1450, 1461, 1462, 1470, 1480, 1490, 2110, 2121, 2122, 2131, 2132, 2133, 2134, 2141, 2142, 2150, 2210, 2221, 2222, 2223, 2224, 2231, 2232, 2233, 2241, 2242, 2243, 2244, 2245, 2311, 2312, 2313, 2314, 2315, 2316, 2317, 2321, 2322, 2323, 2331, 2332, 2333, 2334, 2335, 2336, 2337, 2338, 2341, 2342, 3111, 3112, 3113, 3114, 3115, 3116, 3117, 3211, 3212, 3213, 3221, 3222, 3223, 3231, 3232, 3233, 3241, 3242, 3243, 3244, 3251, 3252, 3253, 3261, 3262, 3263, 3264, 3311, 3312, 3313, 3314, 3315, 3321, 3322, 3323, 3331, 3332, 3333, 3341, 3342, 3351, 3352, 3353, 3360, 3370, 4111, 4112, 4113, 4114, 4115, 4116, 4117, 4118, 4119, 4121, 4122, 4123, 4124, 4125, 4131, 4132, 4133, 4134, 4141, 4142, 4211, 4212, 4220, 4231, 4232, 4233, 4234, 4241, 4242, 4243, 4244, 4245, 4246, 4247, 4251, 4252, 4253, 4261, 4262, 4263, 4311, 4312, 4313, 4321, 4322, 4330, 4341, 4342, 4343, 5101, 5102, 5110, 5121, 5122, 5130, 5141, 5142, 5143, 5150, 5160, 5171, 5172, 5180, 5190, 5211, 5212, 5221, 5222, 5223, 5231, 5232, 5233, 5234, 5241, 5242, 5243, 5244, 5245, 5246, 5247, 5311, 5312, 5313, 5314, 5321, 5322, 5323, 5331, 5332)

regionIDs <- list(RD_id, CA_id, SD_id, HY_id, CF_id, CL_id, HS_id, HA_id, DR_id, PS_id, SR_id, CH_id)
names(regionIDs) <- c("RD", "CA", "SD", "HY", "CF", "CL", "HS", "HA", "DR", "PS", "SR", "CH")
rm(RD_id, CA_id, SD_id, HY_id, CF_id, CL_id, HS_id, HA_id, DR_id, PS_id, SR_id, CH_id)


# Age vectors ----
popall <- c(seq(0, 109), seq(-4, -109, -5), -90, -100, -110, -999)
pop1yr90 <- c(seq(0, 89), -90, -999)
pop1yr100 <- c(seq(0, 99), -100, -999)

pop5yr90 <- c(seq(-4, -89, -5), -90, -999)
pop5yr100 <- c(seq(-4, -99, -5), -100, -999)

deaall <- c(seq(0, 119), seq(-4, -119, -5), -90, -120, -999)
dea1yr120 <- c(seq(0, 119), -120, -999)
dea1yr110 <- c(seq(0, 109), -110, -999)
dea1yr90 <- c(seq(0, 89), -90, -999)

dea5yr120 <- c(seq(-4, -119, -5), -120, -999)
dea5yr110 <- c(seq(-4, -109, -5), -110, -999)
dea5yr90 <- c(seq(-4, -89, -5), -90, -999)

birall <- c(seq(15, 64), seq(-19, -64, -5), -65, -999)
bir1yr65 <- c(seq(15, 64), -65, -999)
bir5yr65 <- c(seq(-19, -64, -5), -65, -999)

ageLists <- list(popall, dea1yr110, pop1yr90, pop1yr100, dea5yr110, pop5yr90, pop5yr100, deaall, dea1yr120, dea1yr90, dea5yr120, dea5yr90, birall, bir1yr65, bir5yr65)
names(ageLists) <- c("popall", "dea1yr110", "pop1yr90", "pop1yr100", "dea5yr110", "pop5yr90", "pop5yr100", "deaall", "dea1yr120", "dea1yr90", "dea5yr120", "dea5yr90", "birall", "bir1yr65", "bir5yr65")
rm(popall, dea1yr110, pop1yr90, pop1yr100, dea5yr110, pop5yr90, pop5yr100, deaall, dea1yr120, dea1yr90, dea5yr120, dea5yr90, birall, bir1yr65, bir5yr65)


# Databases paths ----
drive_path <- "//SFP.IDIR.BCGOV/S152/S52007/"
est_path <- paste0(drive_path, "PopulationR/Database/Estimates/")
proj_path <- paste0(drive_path, "PopulationR/Database/Projections/")
dea_path <- paste0(drive_path, "VITAL/Database/Deaths/")
bir_path <- paste0(drive_path, "VITAL/Database/Births/")

# est_path <- "I:/PopulationR/Database/Estimates/"
# proj_path <- "I:/PopulationR/Database/Projections/"
# dea_path <- "I:/VITAL/Database/Deaths/"
# bir_path <- "I:/VITAL/Database/Births/"

dbPaths <- list(est_path, proj_path, dea_path, bir_path)
names(dbPaths) <- c("est_path", "proj_path", "dea_path", "bir_path")
rm(est_path, proj_path, dea_path, bir_path, drive_path)


# Access functions variables ----

# in dbRead()
Age <- Female <- Male <- N <- Total <- TypeID <- Year <- NULL



# Raking variables ----

# in calc.cols()
TOTAL <- Ctrl_TOTAL <- Sum <- Diff <- NULL

# in dbRake()
Sex <- Region <- VarRow <- row_order <- TotalOldest <- NULL

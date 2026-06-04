## Function for predicting least-squares mean of log.Sale.Price
## Code produced by GUIDE 46.2 on 4/28/26 at 1:56
guide_predict <- function(){
 if(is.na(PropClassSqFt)){
   if(is.na(Census.Tract)){
     catvalues <- c("12","22","41","140","180")
     if(Neighborhood.Code %in% catvalues){
       nodeid <- 8
       predict <- -5.64290967882
       x <- ifelse(is.na(Number.of.Units), 94.4347826087, Number.of.Units)
       predict <- predict +  -0.512843664557E-02 * x
       x <- ifelse(is.na(HoodSqFt), 2030.09735127, HoodSqFt)
       predict <- predict +  0.881485133183E-02 * x
       predict <- min(max(predict, 9.21094019205),15.8169906933)
     } else {
       catvalues <- c("10","11","21","40","42","63","65","81","84","152")
       if(Neighborhood.Code %in% catvalues){
         catvalues <- c("19","73","74","76")
         if(Town.Code %in% catvalues){
           nodeid <- 36
           predict <- 11.9787921213
           x <- ifelse(is.na(Age), 69.0923076923, Age)
           predict <- predict +  0.530837647275E-02 * x
           x <- ifelse(is.na(Percent.Ownership), 0.420000000000E-01, Percent.Ownership)
           predict <- predict +  1.85497793534 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           nodeid <- 37
           predict <- -345.448488679
           x <- ifelse(is.na(Age), 57.5714285714, Age)
           predict <- predict +  -0.771906660092E-02 * x
           x <- ifelse(is.na(Sale.Year), 2014.60714286, Sale.Year)
           predict <- predict +  0.177261659173 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         }
       } else {
         catvalues <- c("30","32","91","93","100")
         if(Neighborhood.Code %in% catvalues){
           nodeid <- 38
           predict <- 15.0241195746
           x <- ifelse(is.na(log.Land.Square.Feet), 10.1614838757, log.Land.Square.Feet)
           predict <- predict +  -0.318889954660 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           if(!is.na(Number.of.Units) & Number.of.Units <= 116.500000000 ){
             nodeid <- 78
             predict <- -255.105088416
             x <- ifelse(is.na(log.Land.Square.Feet), 9.18469403186, log.Land.Square.Feet)
             predict <- predict +  0.279647303598 * x
             x <- ifelse(is.na(Percent.Ownership), 0.937323943662E-01, Percent.Ownership)
             predict <- predict +  1.79693001372 * x
             x <- ifelse(is.na(Sale.Year), 2015.32394366, Sale.Year)
             predict <- predict +  0.132125815701 * x
             x <- ifelse(is.na(HoodSqFt), 1872.53424981, HoodSqFt)
             predict <- predict +  -0.100523337022E-02 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             nodeid <- 79
             predict <- 64.0089554664
             x <- ifelse(is.na(log.Land.Square.Feet), 10.3281026877, log.Land.Square.Feet)
             predict <- predict +  -1.74889838816 * x
             x <- ifelse(is.na(HoodSqFt), 1985.54922496, HoodSqFt)
             predict <- predict +  -0.168926119328E-01 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           }
         }
       }
     }
   } else {
     catvalues <- c("12","13","19","20","23","30","34","36","37","74")
     if(Town.Code %in% catvalues){
       catvalues <- c("11","12","13","22","30","43","91","92","93","120","150","171")
       if(Neighborhood.Code %in% catvalues){
         if(!is.na(Age) & Age <= 6.50000000000 ){
           nodeid <- 40
           predict <- 10.4513181968
           x <- ifelse(is.na(log.Land.Square.Feet), 10.3471280722, log.Land.Square.Feet)
           predict <- predict +  0.212391952566 * x
           x <- ifelse(is.na(Percent.Ownership), 0.237500000000E-01, Percent.Ownership)
           predict <- predict +  4.50938388074 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           if(!is.na(Age) & Age <= 33.5000000000 ){
             nodeid <- 82
             predict <- 441.334845754
             x <- ifelse(is.na(log.Land.Square.Feet), 10.2409913065, log.Land.Square.Feet)
             predict <- predict +  0.984220260942E-01 * x
             x <- ifelse(is.na(Longitude), -87.6514225314, Longitude)
             predict <- predict +  6.21000096776 * x
             x <- ifelse(is.na(Latitude), 41.8978466239, Latitude)
             predict <- predict +  2.77707605042 * x
             x <- ifelse(is.na(Number.of.Units), 290.852197071, Number.of.Units)
             predict <- predict +  -0.145114429298E-02 * x
             x <- ifelse(is.na(Percent.Ownership), 0.369507323569E-01, Percent.Ownership)
             predict <- predict +  1.95283192264 * x
             x <- ifelse(is.na(HoodSqFt), 1902.68924212, HoodSqFt)
             predict <- predict +  -0.719187446715E-03 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             if(!is.na(Percent.Ownership) & Percent.Ownership <= 0.500000000000E-02 ){
               if(!is.na(Latitude) & Latitude <= 41.9026180939 ){
                 nodeid <- 332
                 predict <- 10.6886893602
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.6263845848, log.Land.Square.Feet)
                 predict <- predict +  0.193723487060 * x
                 x <- ifelse(is.na(Census.Tract), 89516.7192513, Census.Tract)
                 predict <- predict +  -0.268237943246E-05 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 333
                 predict <- -111.913556709
                 x <- ifelse(is.na(log.Land.Square.Feet), 11.0082229706, log.Land.Square.Feet)
                 predict <- predict +  0.230988345927 * x
                 x <- ifelse(is.na(Longitude), -87.6321725938, Longitude)
                 predict <- predict +  20.4622409310 * x
                 x <- ifelse(is.na(Latitude), 41.9103258140, Latitude)
                 predict <- predict +  45.6867614582 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               if(!is.na(Latitude) & Latitude <= 41.7259476450 ){
                 nodeid <- 334
                 predict <- -166.329295019
                 x <- ifelse(is.na(Latitude), 41.6187013056, Latitude)
                 predict <- predict +  4.25551036304 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 335
                 predict <- 921.510956204
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.55931481682, log.Land.Square.Feet)
                 predict <- predict +  0.325965339415 * x
                 x <- ifelse(is.na(Longitude), -87.6428335660, Longitude)
                 predict <- predict +  10.4041920210 * x
                 x <- ifelse(is.na(Number.of.Units), 84.9434523810, Number.of.Units)
                 predict <- predict +  -0.218939623680E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.506696428571E-01, Percent.Ownership)
                 predict <- predict +  2.84393301400 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         }
       } else {
         catvalues <- c("10","40","41","42","53","54","61","70","100","130","141","160","170","210")
         if(Neighborhood.Code %in% catvalues){
           nodeid <- 42
           predict <- -343.558552732
           x <- ifelse(is.na(log.Land.Square.Feet), 11.8027011413, log.Land.Square.Feet)
           predict <- predict +  0.239379566395 * x
           x <- ifelse(is.na(Longitude), -87.7973923314, Longitude)
           predict <- predict +  -1.60749813519 * x
           x <- ifelse(is.na(Latitude), 41.6925963844, Latitude)
           predict <- predict +  1.40549688564 * x
           x <- ifelse(is.na(Age), 30.4782608696, Age)
           predict <- predict +  -0.118895801841E-01 * x
           x <- ifelse(is.na(Number.of.Units), 112.462450593, Number.of.Units)
           predict <- predict +  -0.124711477377E-02 * x
           x <- ifelse(is.na(Percent.Ownership), 0.268379446640E-01, Percent.Ownership)
           predict <- predict +  3.17191936123 * x
           x <- ifelse(is.na(Sale.Year), 2015.09090909, Sale.Year)
           predict <- predict +  0.751956264848E-01 * x
           x <- ifelse(is.na(HoodSqFt), 1888.94156421, HoodSqFt)
           predict <- predict +  0.821607589629E-03 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           nodeid <- 43
           predict <- -442.862059474
           x <- ifelse(is.na(Longitude), -87.7335524531, Longitude)
           predict <- predict +  -3.32179990886 * x
           x <- ifelse(is.na(Age), 34.3490759754, Age)
           predict <- predict +  -0.172251933236E-01 * x
           x <- ifelse(is.na(Percent.Ownership), 0.387474332649E-01, Percent.Ownership)
           predict <- predict +  2.86258112240 * x
           x <- ifelse(is.na(Sale.Year), 2015.05338809, Sale.Year)
           predict <- predict +  0.807875877769E-01 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         }
       }
     } else {
       catvalues <- c("11","14","15","29","32","35","39")
       if(Town.Code %in% catvalues){
         catvalues <- c("10","20","40","50","55","65","70","71","74","80","100","109","120","122","130","131","140","151","160","175","181","210","211","222","224","250","255","290","310","340")
         if(Neighborhood.Code %in% catvalues){
           nodeid <- 44
           predict <- -305.653734610
           x <- ifelse(is.na(log.Land.Square.Feet), 11.8536580929, log.Land.Square.Feet)
           predict <- predict +  0.819060633479E-01 * x
           x <- ifelse(is.na(Latitude), 41.8687980381, Latitude)
           predict <- predict +  1.03016762855 * x
           x <- ifelse(is.na(Age), 34.7596348884, Age)
           predict <- predict +  -0.169089800520E-01 * x
           x <- ifelse(is.na(Number.of.Units), 155.992900609, Number.of.Units)
           predict <- predict +  -0.111435484184E-02 * x
           x <- ifelse(is.na(Sale.Year), 2015.06389452, Sale.Year)
           predict <- predict +  0.135429667598 * x
           x <- ifelse(is.na(Sale.Month), 6.79310344828, Sale.Month)
           predict <- predict +  0.143638457448E-01 * x
           x <- ifelse(is.na(HoodSqFt), 1751.44823624, HoodSqFt)
           predict <- predict +  0.227157997207E-03 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           catvalues <- c("12","41","72","82","83","103","112","113","180","200","400","410")
           if(Neighborhood.Code %in% catvalues){
             nodeid <- 90
             predict <- -269.995246208
             x <- ifelse(is.na(log.Land.Square.Feet), 13.2490059502, log.Land.Square.Feet)
             predict <- predict +  0.170494467696 * x
             x <- ifelse(is.na(Latitude), 41.9532028188, Latitude)
             predict <- predict +  4.88985371610 * x
             x <- ifelse(is.na(Census.Tract), 809165.706250, Census.Tract)
             predict <- predict +  0.930116014608E-04 * x
             x <- ifelse(is.na(Age), 21.1500000000, Age)
             predict <- predict +  -0.117491704645E-01 * x
             x <- ifelse(is.na(Number.of.Units), 213.581250000, Number.of.Units)
             predict <- predict +  -0.132626037764E-02 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             nodeid <- 91
             predict <- -146.721181075
             x <- ifelse(is.na(Longitude), -88.0191995771, Longitude)
             predict <- predict +  -0.459215031053 * x
             x <- ifelse(is.na(Latitude), 42.0055829848, Latitude)
             predict <- predict +  -2.02183789544 * x
             x <- ifelse(is.na(Census.Tract), 807416.062169, Census.Tract)
             predict <- predict +  -0.503504532600E-04 * x
             x <- ifelse(is.na(Age), 26.8240740741, Age)
             predict <- predict +  -0.222387113090E-01 * x
             x <- ifelse(is.na(Sale.Year), 2015.00000000, Sale.Year)
             predict <- predict +  0.121142850649 * x
             x <- ifelse(is.na(Sale.Month), 6.65608465608, Sale.Month)
             predict <- predict +  0.122788482295E-01 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           }
         }
       } else {
         catvalues <- c("16","28","70","75")
         if(Town.Code %in% catvalues){
           catvalues <- c("22","23","31","32","45","60","88","103","120","121","140","180")
           if(Neighborhood.Code %in% catvalues){
             nodeid <- 92
             predict <- -816.301886788
             x <- ifelse(is.na(log.Land.Square.Feet), 9.85468894173, log.Land.Square.Feet)
             predict <- predict +  0.191832088045 * x
             x <- ifelse(is.na(Longitude), -87.7018740929, Longitude)
             predict <- predict +  -4.76719390904 * x
             x <- ifelse(is.na(Latitude), 41.9356773702, Latitude)
             predict <- predict +  2.48314313088 * x
             x <- ifelse(is.na(Percent.Ownership), 0.748625792812E-01, Percent.Ownership)
             predict <- predict +  3.22720775051 * x
             x <- ifelse(is.na(Sale.Year), 2015.10359408, Sale.Year)
             predict <- predict +  0.151255429660 * x
             x <- ifelse(is.na(Sale.Month), 6.83298097252, Sale.Month)
             predict <- predict +  0.260896997558E-01 * x
             x <- ifelse(is.na(HoodSqFt), 2009.55129522, HoodSqFt)
             predict <- predict +  -0.725094044500E-03 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             catvalues <- c("12","50","51","62","80","83","85","111","150")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 186
               predict <- -364.587291348
               x <- ifelse(is.na(Longitude), -87.7661867286, Longitude)
               predict <- predict +  -1.22367407081 * x
               x <- ifelse(is.na(Census.Tract), 691331.161137, Census.Tract)
               predict <- predict +  0.103324684148E-05 * x
               x <- ifelse(is.na(Sale.Year), 2015.12322275, Sale.Year)
               predict <- predict +  0.132666146986 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               catvalues <- c("10","15","21","30","33","36","43","70")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 374
                 predict <- -352.191292630
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.8440324405, log.Land.Square.Feet)
                 predict <- predict +  0.254129862739 * x
                 x <- ifelse(is.na(Longitude), -87.7478228348, Longitude)
                 predict <- predict +  -0.630294482123 * x
                 x <- ifelse(is.na(Latitude), 41.7991176313, Latitude)
                 predict <- predict +  0.639791370081 * x
                 x <- ifelse(is.na(Census.Tract), 631869.607908, Census.Tract)
                 predict <- predict +  -0.587838348217E-06 * x
                 x <- ifelse(is.na(Age), 47.6342668863, Age)
                 predict <- predict +  -0.494888322015E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 74.4200988468, Number.of.Units)
                 predict <- predict +  -0.188766064734E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.717957166392E-01, Percent.Ownership)
                 predict <- predict +  2.73725317349 * x
                 x <- ifelse(is.na(Sale.Year), 2015.06425041, Sale.Year)
                 predict <- predict +  0.138726945654 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 375
                 predict <- -181.478943125
                 x <- ifelse(is.na(log.Land.Square.Feet), 11.3783281805, log.Land.Square.Feet)
                 predict <- predict +  0.156065836436 * x
                 x <- ifelse(is.na(Latitude), 41.8957624805, Latitude)
                 predict <- predict +  -1.05005144238 * x
                 x <- ifelse(is.na(Number.of.Units), 153.111635220, Number.of.Units)
                 predict <- predict +  -0.787993605623E-03 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.306918238994E-01, Percent.Ownership)
                 predict <- predict +  4.75897634190 * x
                 x <- ifelse(is.na(Sale.Year), 2015.02358491, Sale.Year)
                 predict <- predict +  0.117290192267 * x
                 x <- ifelse(is.na(HoodSqFt), 1778.75736176, HoodSqFt)
                 predict <- predict +  -0.544796231782E-03 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         } else {
           if(!is.na(Age) & Age <= 27.5000000000 ){
             catvalues <- c("12","24","30","32","42","44","51","52","53","60","62","63","70","71","80","81","84","92","93","110","120","131","132","150","151","152","160","170","200")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 188
               predict <- 68.0883364070
               x <- ifelse(is.na(log.Land.Square.Feet), 9.30283946466, log.Land.Square.Feet)
               predict <- predict +  0.222812893569 * x
               x <- ifelse(is.na(Longitude), -87.6974964891, Longitude)
               predict <- predict +  1.92260858485 * x
               x <- ifelse(is.na(Census.Tract), 338699.205227, Census.Tract)
               predict <- predict +  -0.252935409452E-06 * x
               x <- ifelse(is.na(Age), 11.7964867181, Age)
               predict <- predict +  -0.105836992638E-01 * x
               x <- ifelse(is.na(Number.of.Units), 42.5638389032, Number.of.Units)
               predict <- predict +  -0.139784813203E-02 * x
               x <- ifelse(is.na(Percent.Ownership), 0.159035989717, Percent.Ownership)
               predict <- predict +  1.97507653517 * x
               x <- ifelse(is.na(Sale.Year), 2015.02399314, Sale.Year)
               predict <- predict +  0.548473231129E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.75192802057, Sale.Month)
               predict <- predict +  0.970481616471E-02 * x
               x <- ifelse(is.na(HoodSqFt), 1997.09552126, HoodSqFt)
               predict <- predict +  0.306561518569E-03 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               catvalues <- c("10","11","14","20","22","31","35","40","41","43","50","56","61","72","74","82","90","91","111","141","174","210","430")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 378
                 predict <- 15.0159829788
                 x <- ifelse(is.na(log.Land.Square.Feet), 11.3225505464, log.Land.Square.Feet)
                 predict <- predict +  0.733851528283E-01 * x
                 x <- ifelse(is.na(Longitude), -87.8120946630, Longitude)
                 predict <- predict +  1.49318793551 * x
                 x <- ifelse(is.na(Age), 14.7151567944, Age)
                 predict <- predict +  -0.100639240437E-01 * x
                 x <- ifelse(is.na(Number.of.Units), 215.911149826, Number.of.Units)
                 predict <- predict +  -0.548702687324E-03 * x
                 x <- ifelse(is.na(Sale.Year), 2014.89285714, Sale.Year)
                 predict <- predict +  0.634401857315E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 379
                 predict <- -171.002347887
                 x <- ifelse(is.na(log.Land.Square.Feet), 11.4367392065, log.Land.Square.Feet)
                 predict <- predict +  0.131258511216 * x
                 x <- ifelse(is.na(Longitude), -87.8982715082, Longitude)
                 predict <- predict +  1.26857006925 * x
                 x <- ifelse(is.na(Latitude), 41.9980424538, Latitude)
                 predict <- predict +  1.34947496450 * x
                 x <- ifelse(is.na(Number.of.Units), 123.254870130, Number.of.Units)
                 predict <- predict +  -0.111517342750E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.403896103896E-01, Percent.Ownership)
                 predict <- predict +  -0.784396828948 * x
                 x <- ifelse(is.na(Sale.Year), 2014.96753247, Sale.Year)
                 predict <- predict +  0.117358503682 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           } else {
             catvalues <- c("15","20","23","32","34","41","42","51","56","62","64","72","80","82","91","92","100","101","102","112","115","132","140","152","160","162","166","171","180","220","250","260","270","320","361","371","390","402","430","440")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 190
               predict <- 3.04896728744
               x <- ifelse(is.na(log.Land.Square.Feet), 10.0624321605, log.Land.Square.Feet)
               predict <- predict +  0.216625892473 * x
               x <- ifelse(is.na(Longitude), -87.7417974287, Longitude)
               predict <- predict +  3.12346665061 * x
               x <- ifelse(is.na(Latitude), 41.9594733304, Latitude)
               predict <- predict +  1.47180377482 * x
               x <- ifelse(is.na(Census.Tract), 413616.268235, Census.Tract)
               predict <- predict +  -0.312768010264E-06 * x
               x <- ifelse(is.na(Age), 62.7905882353, Age)
               predict <- predict +  0.312125650246E-02 * x
               x <- ifelse(is.na(Number.of.Units), 94.4058823529, Number.of.Units)
               predict <- predict +  -0.472249337232E-03 * x
               x <- ifelse(is.na(Percent.Ownership), 0.565117647059E-01, Percent.Ownership)
               predict <- predict +  2.04063039038 * x
               x <- ifelse(is.na(Sale.Year), 2015.11117647, Sale.Year)
               predict <- predict +  0.108582426652 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               catvalues <- c("10","14","40","70","90","103","104","121","130","141","410")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 382
                 predict <- -84.4945929743
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.7737533551, log.Land.Square.Feet)
                 predict <- predict +  -0.167667277787 * x
                 x <- ifelse(is.na(Longitude), -87.7766704503, Longitude)
                 predict <- predict +  2.41814821258 * x
                 x <- ifelse(is.na(Latitude), 41.9622294447, Latitude)
                 predict <- predict +  2.79756716619 * x
                 x <- ifelse(is.na(Number.of.Units), 201.302021403, Number.of.Units)
                 predict <- predict +  0.561233841117E-03 * x
                 x <- ifelse(is.na(Sale.Year), 2015.11058264, Sale.Year)
                 predict <- predict +  0.965587242820E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1874.47652363, HoodSqFt)
                 predict <- predict +  -0.857847831864E-03 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 383
                 predict <- 58.1916250395
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.1906646800, log.Land.Square.Feet)
                 predict <- predict +  0.159601231058 * x
                 x <- ifelse(is.na(Longitude), -87.7281771899, Longitude)
                 predict <- predict +  2.41205372179 * x
                 x <- ifelse(is.na(Age), 67.3604320637, Age)
                 predict <- predict +  0.472267456600E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 145.723990904, Number.of.Units)
                 predict <- predict +  -0.505919831838E-03 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.598720864127E-01, Percent.Ownership)
                 predict <- predict +  2.54365490396 * x
                 x <- ifelse(is.na(Sale.Year), 2015.10886868, Sale.Year)
                 predict <- predict +  0.806005607082E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.71944286526, Sale.Month)
                 predict <- predict +  0.141072551396E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1955.90114238, HoodSqFt)
                 predict <- predict +  0.447546328143E-03 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         }
       }
     }
   }
 } else {
   if(is.na(Total.Building.Square.Feet)){
     if(is.na(Census.Tract)){
       catvalues <- c("10","17","21","22","24","25","28","38","73","74","77")
       if(Town.Code %in% catvalues){
         nodeid <- 24
         predict <- -233.500111513
         x <- ifelse(is.na(Sale.Year), 2014.85365854, Sale.Year)
         predict <- predict +  0.122433671145 * x
         predict <- min(max(predict, 9.21094019205),15.8169906933)
       } else {
         nodeid <- 25
         predict <- 12.1175981791
       }
     } else {
       catvalues <- c("11","12","13","14","15","31","32","36","37","39","70","72")
       if(Town.Code %in% catvalues){
         if(is.na(Percent.Ownership)){
           catvalues <- c("15","36","46","50","55","64","81","82","93","103","104","122","152","160","161","173","174","182","193","211","224","227","241","255","257","271","274","290","300","315","316","341","342","344","350","360","380","400","410","420","422","423","431","432")
           if(Neighborhood.Code %in% catvalues){
             catvalues <- c("46","93","315","341","344","350","422")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 208
               predict <- -223.508154393
               x <- ifelse(is.na(log.Building.Square.Feet), 7.09604150526, log.Building.Square.Feet)
               predict <- predict +  0.318762887063 * x
               x <- ifelse(is.na(Longitude), -87.7106148705, Longitude)
               predict <- predict +  -1.02236897521 * x
               x <- ifelse(is.na(Sale.Year), 2015.08444444, Sale.Year)
               predict <- predict +  0.715284296306E-01 * x
               x <- ifelse(is.na(HoodSqFt), 1331.89151604, HoodSqFt)
               predict <- predict +  -0.372268260159E-03 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               catvalues <- c("15","50","55","152","211","255")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 418
                 predict <- -643.461166787
                 x <- ifelse(is.na(Fireplaces), 0.148020654045, Fireplaces)
                 predict <- predict +  0.148206867661 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.21191733612, log.Building.Square.Feet)
                 predict <- predict +  0.475262562631 * x
                 x <- ifelse(is.na(Longitude), -87.7557724986, Longitude)
                 predict <- predict +  -5.83957431114 * x
                 x <- ifelse(is.na(Census.Tract), 820801.931153, Census.Tract)
                 predict <- predict +  0.265936300531E-04 * x
                 x <- ifelse(is.na(Sale.Year), 2015.14285714, Sale.Year)
                 predict <- predict +  0.583607008727E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 419
                 predict <- -236.380127709
                 x <- ifelse(is.na(Fireplaces), 0.270163934426, Fireplaces)
                 predict <- predict +  0.122890874802 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.23051638376, log.Building.Square.Feet)
                 predict <- predict +  0.624724486709 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.459016393443E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.851708618098 * x
                 x <- ifelse(is.na(Latitude), 41.7306616719, Latitude)
                 predict <- predict +  2.51628323088 * x
                 x <- ifelse(is.na(Sale.Year), 2015.10295082, Sale.Year)
                 predict <- predict +  0.689691480895E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           } else {
             catvalues <- c("1.0")
             if(Most.Recent.Sale %in% catvalues){
               catvalues <- c("11","23","24","45","63","84","101","121","133","134","162","226","251","293")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 420
                 predict <- -342.440347436
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.61153347756, log.Land.Square.Feet)
                 predict <- predict +  0.455727812703 * x
                 x <- ifelse(is.na(Fireplaces), 0.914285714286E-01, Fireplaces)
                 predict <- predict +  0.469702562548 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.08728628140, log.Building.Square.Feet)
                 predict <- predict +  0.623693047427 * x
                 x <- ifelse(is.na(Latitude), 41.6035483644, Latitude)
                 predict <- predict +  1.60196448569 * x
                 x <- ifelse(is.na(Census.Tract), 787110.874286, Census.Tract)
                 predict <- predict +  -0.123509691781E-05 * x
                 x <- ifelse(is.na(Age), 63.9619047619, Age)
                 predict <- predict +  -0.792752818963E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.08380952, Sale.Year)
                 predict <- predict +  0.138673794487 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 421
                 predict <- -437.505855964
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.58320103647, log.Land.Square.Feet)
                 predict <- predict +  0.259050717993 * x
                 x <- ifelse(is.na(Fireplaces), 0.171753097751, Fireplaces)
                 predict <- predict +  0.155936455969 * x
                 x <- ifelse(is.na(Half.Baths), 0.358765488756, Half.Baths)
                 predict <- predict +  0.703912902086E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.24046225065, log.Building.Square.Feet)
                 predict <- predict +  0.766424468438 * x
                 x <- ifelse(is.na(Longitude), -87.6958768889, Longitude)
                 predict <- predict +  -1.40062846333 * x
                 x <- ifelse(is.na(Latitude), 41.7117622139, Latitude)
                 predict <- predict +  2.32779345086 * x
                 x <- ifelse(is.na(Census.Tract), 742935.503442, Census.Tract)
                 predict <- predict +  -0.363608664870E-06 * x
                 x <- ifelse(is.na(Age), 67.1759981643, Age)
                 predict <- predict +  -0.833811159622E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.16268931, Sale.Year)
                 predict <- predict +  0.110427030845 * x
                 x <- ifelse(is.na(Sale.Month), 6.81941257458, Sale.Month)
                 predict <- predict +  0.133197007869E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1724.68698218, HoodSqFt)
                 predict <- predict +  -0.835793156966E-04 * x
                 x <- ifelse(is.na(PropClassSqFt), 1594.12643173, PropClassSqFt)
                 predict <- predict +  -0.208747751720E-03 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               catvalues <- c("10","11","12","20","22","25","41","42","73","87","90","110","141","151","175","194","200","201","210","232","262")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 422
                 predict <- -90.6064092761
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.61929634762, log.Land.Square.Feet)
                 predict <- predict +  0.179030428258 * x
                 x <- ifelse(is.na(Fireplaces), 0.195822454308, Fireplaces)
                 predict <- predict +  0.236005602361 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.26128421314, log.Building.Square.Feet)
                 predict <- predict +  0.851892783995 * x
                 x <- ifelse(is.na(Latitude), 41.7427992612, Latitude)
                 predict <- predict +  2.27482236282 * x
                 x <- ifelse(is.na(Age), 65.4503916449, Age)
                 predict <- predict +  -0.446256790569E-02 * x
                 x <- ifelse(is.na(PropClassSqFt), 1588.90034406, PropClassSqFt)
                 predict <- predict +  -0.190149083642E-03 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 423
                 predict <- -331.687107027
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.51469324453, log.Land.Square.Feet)
                 predict <- predict +  0.262915047956 * x
                 x <- ifelse(is.na(Fireplaces), 0.151186639320, Fireplaces)
                 predict <- predict +  0.113957672604 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.25791202615, log.Building.Square.Feet)
                 predict <- predict +  0.707671783135 * x
                 x <- ifelse(is.na(Longitude), -87.6675827185, Longitude)
                 predict <- predict +  -2.30162452339 * x
                 x <- ifelse(is.na(Latitude), 41.7073755076, Latitude)
                 predict <- predict +  1.80855025840 * x
                 x <- ifelse(is.na(Age), 71.7910928802, Age)
                 predict <- predict +  -0.731878621382E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2014.98095517, Sale.Year)
                 predict <- predict +  0.292087614880E-01 * x
                 x <- ifelse(is.na(PropClassSqFt), 1625.16366960, PropClassSqFt)
                 predict <- predict +  -0.154646483358E-03 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         } else {
           nodeid <- 53
           predict <- 10.6302362585
           x <- ifelse(is.na(PropClassSqFt), 1595.20390454, PropClassSqFt)
           predict <- predict +  0.431785253087E-03 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         }
       } else {
         catvalues <- c("16","18","20","22","26","28","30","35","71","77")
         if(Town.Code %in% catvalues){
           catvalues <- c("11","13","20","22","23","31","32","40","42","43","46","50","60","61","62","80","81","91","92","101","102","103","104","105","107","108","109","115","130","141","160","171","180","200","210","250","260","270","280","371","520")
           if(Neighborhood.Code %in% catvalues){
             catvalues <- c("40","92","101","102","103","104","115","160","200","210","520")
             if(Neighborhood.Code %in% catvalues){
               catvalues <- c("18","77")
               if(Town.Code %in% catvalues){
                 nodeid <- 432
                 predict <- -293.138232238
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.55409515470, log.Building.Square.Feet)
                 predict <- predict +  0.393687474171 * x
                 x <- ifelse(is.na(Census.Tract), 443732.460292, Census.Tract)
                 predict <- predict +  0.413823801382E-06 * x
                 x <- ifelse(is.na(Age), 102.952998379, Age)
                 predict <- predict +  -0.585153207379E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.15721232, Sale.Year)
                 predict <- predict +  0.150141096543 * x
                 x <- ifelse(is.na(HoodSqFt), 2002.56236359, HoodSqFt)
                 predict <- predict +  -0.385225526174E-03 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 433
                 predict <- -108.341112858
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.62655376668, log.Land.Square.Feet)
                 predict <- predict +  0.214701268293 * x
                 x <- ifelse(is.na(Fireplaces), 0.311737089202, Fireplaces)
                 predict <- predict +  0.104016870121 * x
                 x <- ifelse(is.na(Half.Baths), 0.437558685446, Half.Baths)
                 predict <- predict +  0.104936227277 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.32031815154, log.Building.Square.Feet)
                 predict <- predict +  0.680399549984 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.469483568075E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.674987767692 * x
                 x <- ifelse(is.na(Longitude), -87.8812498002, Longitude)
                 predict <- predict +  1.26794517817 * x
                 x <- ifelse(is.na(Latitude), 41.9196174901, Latitude)
                 predict <- predict +  1.31415638784 * x
                 x <- ifelse(is.na(Age), 59.3840375587, Age)
                 predict <- predict +  -0.558630902921E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.04600939, Sale.Year)
                 predict <- predict +  0.847138833880E-01 * x
                 x <- ifelse(is.na(PropClassSqFt), 1680.13878936, PropClassSqFt)
                 predict <- predict +  -0.179201591796E-03 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               catvalues <- c("22","31","43","46","61","62","171")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 434
                 predict <- -56.5624859620
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.83756345332, log.Land.Square.Feet)
                 predict <- predict +  0.204670010682 * x
                 x <- ifelse(is.na(Fireplaces), 0.314785373609, Fireplaces)
                 predict <- predict +  0.957487132959E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.34636041985, log.Building.Square.Feet)
                 predict <- predict +  0.545373259760 * x
                 x <- ifelse(is.na(Longitude), -87.8750446763, Longitude)
                 predict <- predict +  1.25727508118 * x
                 x <- ifelse(is.na(Latitude), 41.8468497520, Latitude)
                 predict <- predict +  0.912601001622 * x
                 x <- ifelse(is.na(Census.Tract), 675238.421304, Census.Tract)
                 predict <- predict +  -0.375954043840E-06 * x
                 x <- ifelse(is.na(Age), 50.8744038156, Age)
                 predict <- predict +  -0.720418608919E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.16375199, Sale.Year)
                 predict <- predict +  0.675530316893E-01 * x
                 x <- ifelse(is.na(PropClassSqFt), 1645.24024465, PropClassSqFt)
                 predict <- predict +  -0.960060362181E-04 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 435
                 predict <- -134.328447571
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.48531715180, log.Land.Square.Feet)
                 predict <- predict +  0.252020373347 * x
                 x <- ifelse(is.na(Fireplaces), 0.168862859932, Fireplaces)
                 predict <- predict +  0.204436524812 * x
                 x <- ifelse(is.na(Half.Baths), 0.381893606637, Half.Baths)
                 predict <- predict +  0.134217564918 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.34074126651, log.Building.Square.Feet)
                 predict <- predict +  0.494461033363 * x
                 x <- ifelse(is.na(Longitude), -87.8734508653, Longitude)
                 predict <- predict +  1.71944003338 * x
                 x <- ifelse(is.na(Latitude), 41.9430280458, Latitude)
                 predict <- predict +  1.64511630462 * x
                 x <- ifelse(is.na(Census.Tract), 573094.255490, Census.Tract)
                 predict <- predict +  -0.122065838494E-06 * x
                 x <- ifelse(is.na(Age), 69.0971205466, Age)
                 predict <- predict +  -0.686455367592E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.05124451, Sale.Year)
                 predict <- predict +  0.111251509051 * x
                 x <- ifelse(is.na(Sale.Month), 6.79062957540, Sale.Month)
                 predict <- predict +  0.149626768980E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1754.85173961, HoodSqFt)
                 predict <- predict +  -0.522227067958E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 1769.91872286, PropClassSqFt)
                 predict <- predict +  -0.111674629298E-03 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           } else {
             catvalues <- c("77")
             if(Town.Code %in% catvalues){
               catvalues <- c("51","52","120","152","170")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 436
                 predict <- -638.497417093
                 x <- ifelse(is.na(log.Land.Square.Feet), 7.77665357735, log.Land.Square.Feet)
                 predict <- predict +  0.402376445750 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.73308201261, log.Building.Square.Feet)
                 predict <- predict +  0.591200514913 * x
                 x <- ifelse(is.na(Latitude), 41.9033022928, Latitude)
                 predict <- predict +  11.6669289266 * x
                 x <- ifelse(is.na(Age), 71.5067873303, Age)
                 predict <- predict +  -0.386240944884E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2014.92307692, Sale.Year)
                 predict <- predict +  0.773401641010E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.90950226244, Sale.Month)
                 predict <- predict +  0.227041411389E-01 * x
                 x <- ifelse(is.na(PropClassSqFt), 2487.27278443, PropClassSqFt)
                 predict <- predict +  -0.132559211801E-03 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 437
                 predict <- 1199.27015273
                 x <- ifelse(is.na(log.Land.Square.Feet), 7.86635331701, log.Land.Square.Feet)
                 predict <- predict +  0.394535653660 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.63141840329, log.Building.Square.Feet)
                 predict <- predict +  0.546688746597 * x
                 x <- ifelse(is.na(Longitude), -87.6894664534, Longitude)
                 predict <- predict +  17.1802099757 * x
                 x <- ifelse(is.na(Latitude), 41.9091058105, Latitude)
                 predict <- predict +  3.58397360004 * x
                 x <- ifelse(is.na(Age), 83.6245059289, Age)
                 predict <- predict +  -0.389975269920E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2014.88932806, Sale.Year)
                 predict <- predict +  0.818618553842E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1837.67214729, HoodSqFt)
                 predict <- predict +  -0.828679903615E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 2318.38194866, PropClassSqFt)
                 predict <- predict +  -0.150025790351E-03 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               catvalues <- c("18","21","35","47","63","72","95","96","110","112","113","114","117","142","143","151","420","461","463","580","600")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 438
                 predict <- -12.6165850619
                 x <- ifelse(is.na(Fireplaces), 0.635802469136, Fireplaces)
                 predict <- predict +  0.124171401627 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.62525968687, log.Building.Square.Feet)
                 predict <- predict +  0.487490721668 * x
                 x <- ifelse(is.na(Longitude), -87.8645541379, Longitude)
                 predict <- predict +  1.41381301377 * x
                 x <- ifelse(is.na(Latitude), 41.9806548523, Latitude)
                 predict <- predict +  1.34002622677 * x
                 x <- ifelse(is.na(Sale.Year), 2015.03292181, Sale.Year)
                 predict <- predict +  0.445082614865E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.87242798354, Sale.Month)
                 predict <- predict +  0.141397770644E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 439
                 predict <- -40.4498460430
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.66234578110, log.Land.Square.Feet)
                 predict <- predict +  0.188719207998 * x
                 x <- ifelse(is.na(Fireplaces), 0.338817677444, Fireplaces)
                 predict <- predict +  0.125447345198 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.39204321185, log.Building.Square.Feet)
                 predict <- predict +  0.484500629990 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.726994451884E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.258131548346 * x
                 x <- ifelse(is.na(Longitude), -87.8708435538, Longitude)
                 predict <- predict +  1.64748767716 * x
                 x <- ifelse(is.na(Latitude), 41.9472396816, Latitude)
                 predict <- predict +  1.11852123000 * x
                 x <- ifelse(is.na(Census.Tract), 536489.697532, Census.Tract)
                 predict <- predict +  -0.148006288295E-06 * x
                 x <- ifelse(is.na(Age), 57.8777501435, Age)
                 predict <- predict +  -0.395925551993E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.08303042, Sale.Year)
                 predict <- predict +  0.723719296931E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.90242969198, Sale.Month)
                 predict <- predict +  0.743996246334E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         } else {
           catvalues <- c("24","38")
           if(Town.Code %in% catvalues){
             nodeid <- 110
             predict <- -184.651790617
             x <- ifelse(is.na(log.Land.Square.Feet), 8.86871354864, log.Land.Square.Feet)
             predict <- predict +  0.213705246864 * x
             x <- ifelse(is.na(Fireplaces), 0.450442851118, Fireplaces)
             predict <- predict +  0.105754727446 * x
             x <- ifelse(is.na(log.Building.Square.Feet), 7.37897705240, log.Building.Square.Feet)
             predict <- predict +  0.517415173682 * x
             x <- ifelse(is.na(Longitude), -87.8739838732, Longitude)
             predict <- predict +  -1.42481300217 * x
             x <- ifelse(is.na(Latitude), 42.0764498131, Latitude)
             predict <- predict +  -2.44508915320 * x
             x <- ifelse(is.na(Census.Tract), 804808.319275, Census.Tract)
             predict <- predict +  0.527666831202E-04 * x
             x <- ifelse(is.na(Age), 52.0189793336, Age)
             predict <- predict +  -0.395693600020E-02 * x
             x <- ifelse(is.na(Sale.Year), 2015.04048925, Sale.Year)
             predict <- predict +  0.630985205588E-01 * x
             x <- ifelse(is.na(Sale.Month), 6.88064107971, Sale.Month)
             predict <- predict +  0.109534943720E-01 * x
             x <- ifelse(is.na(HoodSqFt), 1898.01002642, HoodSqFt)
             predict <- predict +  -0.166887158857E-03 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             catvalues <- c("21","23","25")
             if(Town.Code %in% catvalues){
               catvalues <- c("43","44","45","50","53","62","80","92","93","94","110","141","170","171","180","190","200","210","220","240","300","320","420")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 444
                 predict <- 385.009385212
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.36270392279, log.Land.Square.Feet)
                 predict <- predict +  0.185869431489 * x
                 x <- ifelse(is.na(Half.Baths), 0.896153846154, Half.Baths)
                 predict <- predict +  0.686387731566E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.93342716558, log.Building.Square.Feet)
                 predict <- predict +  0.830599160169 * x
                 x <- ifelse(is.na(Longitude), -87.7479690142, Longitude)
                 predict <- predict +  4.32493305886 * x
                 x <- ifelse(is.na(Age), 63.2923076923, Age)
                 predict <- predict +  -0.215352948633E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 445
                 predict <- -146.438162718
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.05943734243, log.Land.Square.Feet)
                 predict <- predict +  0.137274968814 * x
                 x <- ifelse(is.na(Fireplaces), 0.703265499290, Fireplaces)
                 predict <- predict +  0.244384351477 * x
                 x <- ifelse(is.na(Half.Baths), 0.676289635589, Half.Baths)
                 predict <- predict +  0.836682924608E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.56471049904, log.Building.Square.Feet)
                 predict <- predict +  0.756979953260 * x
                 x <- ifelse(is.na(Longitude), -87.8310046578, Longitude)
                 predict <- predict +  -1.22004557481 * x
                 x <- ifelse(is.na(Census.Tract), 810005.525319, Census.Tract)
                 predict <- predict +  -0.251367554103E-04 * x
                 x <- ifelse(is.na(Sale.Year), 2015.01703739, Sale.Year)
                 predict <- predict +  0.323891180261E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.83861807856, Sale.Month)
                 predict <- predict +  0.110534969239E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               catvalues <- c("33","73","74")
               if(Town.Code %in% catvalues){
                 nodeid <- 446
                 predict <- 948.914192896
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.02855995262, log.Land.Square.Feet)
                 predict <- predict +  0.471492968388 * x
                 x <- ifelse(is.na(Fireplaces), 0.603521126761, Fireplaces)
                 predict <- predict +  0.935865163281E-01 * x
                 x <- ifelse(is.na(Half.Baths), 0.604929577465, Half.Baths)
                 predict <- predict +  0.570711614878E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.77777871130, log.Building.Square.Feet)
                 predict <- predict +  0.417741643574 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.316901408451E-01, Number.of.Commercial.Units)
                 predict <- predict +  -0.277199470783 * x
                 x <- ifelse(is.na(Longitude), -87.6791466582, Longitude)
                 predict <- predict +  8.44077647018 * x
                 x <- ifelse(is.na(Latitude), 41.9403191165, Latitude)
                 predict <- predict +  -7.26639875583 * x
                 x <- ifelse(is.na(Age), 76.7683098592, Age)
                 predict <- predict +  -0.196138171983E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2014.94647887, Sale.Year)
                 predict <- predict +  0.508458940045E-01 * x
                 x <- ifelse(is.na(PropClassSqFt), 2480.99097694, PropClassSqFt)
                 predict <- predict +  0.466642647488E-04 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 447
                 predict <- -13.3054427393
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.77415321045, log.Land.Square.Feet)
                 predict <- predict +  0.146450153185 * x
                 x <- ifelse(is.na(Fireplaces), 0.526796589525, Fireplaces)
                 predict <- predict +  0.202320516267 * x
                 x <- ifelse(is.na(Half.Baths), 0.592570036541, Half.Baths)
                 predict <- predict +  0.803900984601E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.54887882133, log.Building.Square.Feet)
                 predict <- predict +  0.627376081093 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.700365408039E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.425102802058 * x
                 x <- ifelse(is.na(Longitude), -87.8656431221, Longitude)
                 predict <- predict +  1.20909329769 * x
                 x <- ifelse(is.na(Latitude), 41.9732126126, Latitude)
                 predict <- predict +  0.522076982690 * x
                 x <- ifelse(is.na(Census.Tract), 731217.102010, Census.Tract)
                 predict <- predict +  0.257466400737E-06 * x
                 x <- ifelse(is.na(Age), 64.1071863581, Age)
                 predict <- predict +  -0.172517503722E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.06729598, Sale.Year)
                 predict <- predict +  0.515936247857E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.96802679659, Sale.Month)
                 predict <- predict +  0.856369456474E-02 * x
                 x <- ifelse(is.na(HoodSqFt), 1877.13629810, HoodSqFt)
                 predict <- predict +  0.920385678979E-04 * x
                 x <- ifelse(is.na(PropClassSqFt), 2034.32151396, PropClassSqFt)
                 predict <- predict +  -0.506535039047E-04 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         }
       }
     }
   } else {
     if(is.na(Census.Tract)){
       nodeid <- 14
       predict <- -334.412293477
       x <- ifelse(is.na(Fireplaces), 0.251928020566, Fireplaces)
       predict <- predict +  0.335386958776 * x
       x <- ifelse(is.na(Half.Baths), 0.789203084833, Half.Baths)
       predict <- predict +  0.251671893485 * x
       x <- ifelse(is.na(log.Building.Square.Feet), 7.56566516936, log.Building.Square.Feet)
       predict <- predict +  0.910890310047 * x
       x <- ifelse(is.na(Total.Building.Square.Feet), 16461.2904884, Total.Building.Square.Feet)
       predict <- predict +  -0.434337621981E-04 * x
       x <- ifelse(is.na(Age), 8.64524421594, Age)
       predict <- predict +  0.644458049299E-02 * x
       x <- ifelse(is.na(Sale.Year), 2014.98714653, Sale.Year)
       predict <- predict +  0.167878974609 * x
       x <- ifelse(is.na(PropClassSqFt), 1832.78857643, PropClassSqFt)
       predict <- predict +  0.385428948291E-03 * x
       predict <- min(max(predict, 9.21094019205),15.8169906933)
     } else {
       catvalues <- c("12","22","30","42","43","51","52","70","74","81","82","84","90","93","111","120","141","150","170","362")
       if(Neighborhood.Code %in% catvalues){
         nodeid <- 30
         predict <- -92.5163778149
         x <- ifelse(is.na(log.Building.Square.Feet), 7.38062745621, log.Building.Square.Feet)
         predict <- predict +  0.932888947171 * x
         x <- ifelse(is.na(Number.of.Commercial.Units), 0.359712230216E-01, Number.of.Commercial.Units)
         predict <- predict +  -0.651211405483 * x
         x <- ifelse(is.na(Longitude), -87.6965603488, Longitude)
         predict <- predict +  4.26519613505 * x
         x <- ifelse(is.na(Latitude), 41.9006261150, Latitude)
         predict <- predict +  6.83281300194 * x
         x <- ifelse(is.na(Sale.Year), 2014.99640288, Sale.Year)
         predict <- predict +  0.922049509014E-01 * x
         predict <- min(max(predict, 9.21094019205),15.8169906933)
       } else {
         catvalues <- c("10","50","80")
         if(Neighborhood.Code %in% catvalues){
           nodeid <- 62
           predict <- 4.16880281108
           x <- ifelse(is.na(log.Land.Square.Feet), 7.30490641547, log.Land.Square.Feet)
           predict <- predict +  0.959457108290 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           catvalues <- c("11","31","32","41","71","92","101","132","180","380","430")
           if(Neighborhood.Code %in% catvalues){
             nodeid <- 126
             predict <- 5.24341827007
             x <- ifelse(is.na(log.Land.Square.Feet), 7.98419414282, log.Land.Square.Feet)
             predict <- predict +  -0.286683257554 * x
             x <- ifelse(is.na(log.Building.Square.Feet), 7.19358505622, log.Building.Square.Feet)
             predict <- predict +  1.20698203895 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             nodeid <- 127
             predict <- -210.524582762
             x <- ifelse(is.na(log.Land.Square.Feet), 7.65243794684, log.Land.Square.Feet)
             predict <- predict +  0.753158465495 * x
             x <- ifelse(is.na(Latitude), 41.8182723183, Latitude)
             predict <- predict +  5.14259729325 * x
             x <- ifelse(is.na(Sale.Month), 6.90654205607, Sale.Month)
             predict <- predict +  0.739375330726E-01 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           }
         }
       }
     }
   }
 }
 return(c(nodeid,predict))
}
## end of function
##
##
## If desired, replace "cook_county_test_v3.txt" with name of file containing new data
## New file must have at least the same variables with same names
## (but not necessarily the same order) as in the training data file
## Missing value code is converted to NA if not already NA
newdata <- read.table("cook_county_test_v3.txt",header=TRUE,colClasses="character")
## node contains terminal node ID of each case
## pred contains predicted value of each case
node <- NULL
pred <- NULL
for(i in 1:nrow(newdata)){
    Neighborhood.Code <- as.character(newdata$Neighborhood.Code[i])
    log.Land.Square.Feet <- as.numeric(newdata$log.Land.Square.Feet[i])
    Town.Code <- as.character(newdata$Town.Code[i])
    Fireplaces <- as.numeric(newdata$Fireplaces[i])
    Half.Baths <- as.numeric(newdata$Half.Baths[i])
    log.Building.Square.Feet <- as.numeric(newdata$log.Building.Square.Feet[i])
    Number.of.Commercial.Units <- as.numeric(newdata$Number.of.Commercial.Units[i])
    Longitude <- as.numeric(newdata$Longitude[i])
    Latitude <- as.numeric(newdata$Latitude[i])
    Census.Tract <- as.numeric(newdata$Census.Tract[i])
    Total.Building.Square.Feet <- as.numeric(newdata$Total.Building.Square.Feet[i])
    Age <- as.numeric(newdata$Age[i])
    Number.of.Units <- as.numeric(newdata$Number.of.Units[i])
    Percent.Ownership <- as.numeric(newdata$Percent.Ownership[i])
    Sale.Year <- as.numeric(newdata$Sale.Year[i])
    Most.Recent.Sale <- as.character(newdata$Most.Recent.Sale[i])
    Sale.Month <- as.numeric(newdata$Sale.Month[i])
    HoodSqFt <- as.numeric(newdata$HoodSqFt[i])
    PropClassSqFt <- as.numeric(newdata$PropClassSqFt[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred <- c(pred,tmp[2])
}

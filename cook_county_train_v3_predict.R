## Function for predicting least-squares mean of log.Sale.Price
## Code produced by GUIDE 46.2 on 4/22/26 at 21:45
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
           predict <- 12.0036242636
           x <- ifelse(is.na(Age), 69.0923076923, Age)
           predict <- predict +  0.607657966039E-02 * x
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
         catvalues <- c("20","43","70","74","83","92","102","120","150")
         if(Neighborhood.Code %in% catvalues){
           nodeid <- 38
           predict <- -425.230443871
           x <- ifelse(is.na(Percent.Ownership), 0.151785714286, Percent.Ownership)
           predict <- predict +  1.24496977655 * x
           x <- ifelse(is.na(Sale.Year), 2015.10714286, Sale.Year)
           predict <- predict +  0.216907149179 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           catvalues <- c("32","80","93","110","131","160")
           if(Neighborhood.Code %in% catvalues){
             nodeid <- 78
             predict <- 9.66362190929
             x <- ifelse(is.na(log.Land.Square.Feet), 9.63942857121, log.Land.Square.Feet)
             predict <- predict +  0.283830763086 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             catvalues <- c("33","60","62","100")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 158
               predict <- 12.7675877111
               x <- ifelse(is.na(Number.of.Units), 82.6041666667, Number.of.Units)
               predict <- predict +  -0.835475121139E-02 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               nodeid <- 159
               predict <- -461.000343153
               x <- ifelse(is.na(log.Land.Square.Feet), 10.2553783216, log.Land.Square.Feet)
               predict <- predict +  -0.203349773869 * x
               x <- ifelse(is.na(Sale.Year), 2014.80000000, Sale.Year)
               predict <- predict +  0.235555371195 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             }
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
           predict <- 11.1576857019
           x <- ifelse(is.na(log.Land.Square.Feet), 10.3471280722, log.Land.Square.Feet)
           predict <- predict +  0.154475433729 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           if(!is.na(Age) & Age <= 33.5000000000 ){
             catvalues <- c("12","30","43","120","150")
             if(Neighborhood.Code %in% catvalues){
               if(!is.na(Number.of.Units) & Number.of.Units <= 290.500000000 ){
                 nodeid <- 328
                 predict <- 422.771035640
                 x <- ifelse(is.na(Longitude), -87.6631647492, Longitude)
                 predict <- predict +  5.89223001076 * x
                 x <- ifelse(is.na(Latitude), 41.9081081111, Latitude)
                 predict <- predict +  2.50861664515 * x
                 x <- ifelse(is.na(Census.Tract), 344809.622047, Census.Tract)
                 predict <- predict +  -0.315630162432E-06 * x
                 x <- ifelse(is.na(HoodSqFt), 1909.22866125, HoodSqFt)
                 predict <- predict +  0.931523456686E-03 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 329
                 predict <- 15.6907471571
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.4405351641, log.Land.Square.Feet)
                 predict <- predict +  -0.316508838026 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               if(!is.na(HoodSqFt) & HoodSqFt <= 2038.12903892 ){
                 nodeid <- 330
                 predict <- -76.0037123481
                 x <- ifelse(is.na(Latitude), 41.8458791012, Latitude)
                 predict <- predict +  2.11694061996 * x
                 x <- ifelse(is.na(Census.Tract), 336814.558442, Census.Tract)
                 predict <- predict +  -0.819319371431E-06 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.102727272727, Percent.Ownership)
                 predict <- predict +  2.25876952744 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 331
                 predict <- 722.986197987
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.2343403523, log.Land.Square.Feet)
                 predict <- predict +  0.189479424380 * x
                 x <- ifelse(is.na(Longitude), -87.6541468604, Longitude)
                 predict <- predict +  8.12113222194 * x
                 x <- ifelse(is.na(Number.of.Units), 198.932098765, Number.of.Units)
                 predict <- predict +  -0.253865596613E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           } else {
             if(!is.na(Percent.Ownership) & Percent.Ownership <= 0.500000000000E-02 ){
               if(!is.na(Longitude) & Longitude <= -87.6252474236 ){
                 nodeid <- 332
                 predict <- -1974.41809111
                 x <- ifelse(is.na(Longitude), -87.6311393652, Longitude)
                 predict <- predict +  -22.6884168336 * x
                 x <- ifelse(is.na(Census.Tract), 86091.8758782, Census.Tract)
                 predict <- predict +  -0.242215707859E-05 * x
                 x <- ifelse(is.na(HoodSqFt), 2007.42596249, HoodSqFt)
                 predict <- predict +  -0.644198788778E-03 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 333
                 predict <- 8.91464070286
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.6205525909, log.Land.Square.Feet)
                 predict <- predict +  0.348529396611 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               if(!is.na(Latitude) & Latitude <= 41.8905945609 ){
                 nodeid <- 334
                 predict <- -592.631554056
                 x <- ifelse(is.na(Longitude), -87.7349197941, Longitude)
                 predict <- predict +  -3.12579210572 * x
                 x <- ifelse(is.na(Census.Tract), 747810.395833, Census.Tract)
                 predict <- predict +  -0.398459868394E-05 * x
                 x <- ifelse(is.na(Sale.Year), 2015.00000000, Sale.Year)
                 predict <- predict +  0.164963037086 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 335
                 predict <- 849.757803801
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.56162208396, log.Land.Square.Feet)
                 predict <- predict +  0.327260492669 * x
                 x <- ifelse(is.na(Longitude), -87.6421551042, Longitude)
                 predict <- predict +  9.59736010642 * x
                 x <- ifelse(is.na(Number.of.Units), 85.4307228916, Number.of.Units)
                 predict <- predict +  -0.193418064607E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.505722891566E-01, Percent.Ownership)
                 predict <- predict +  2.89313131568 * x
                 x <- ifelse(is.na(HoodSqFt), 2036.83059060, HoodSqFt)
                 predict <- predict +  0.492390312961E-03 * x
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
             catvalues <- c("23","45","60","88","103")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 184
               predict <- -265.984634621
               x <- ifelse(is.na(Sale.Year), 2015.12698413, Sale.Year)
               predict <- predict +  0.137940596525 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               if(!is.na(Age) & Age <= 29.5000000000 ){
                 nodeid <- 370
                 predict <- -162.408136611
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.3225240630, log.Land.Square.Feet)
                 predict <- predict +  0.524498341146 * x
                 x <- ifelse(is.na(Latitude), 41.8228248706, Latitude)
                 predict <- predict +  4.02857558802 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.966071428571E-01, Percent.Ownership)
                 predict <- predict +  4.57996160679 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 371
                 predict <- -214.838719697
                 x <- ifelse(is.na(Age), 72.5451977401, Age)
                 predict <- predict +  0.849760399595E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.761299435028E-01, Percent.Ownership)
                 predict <- predict +  1.97761411211 * x
                 x <- ifelse(is.na(Sale.Year), 2015.06779661, Sale.Year)
                 predict <- predict +  0.111809596656 * x
                 x <- ifelse(is.na(Sale.Month), 6.70903954802, Sale.Month)
                 predict <- predict +  0.309438905564E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
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
               catvalues <- c("18","21","76")
               if(Town.Code %in% catvalues){
                 nodeid <- 380
                 predict <- -255.734257570
                 x <- ifelse(is.na(Age), 48.8285714286, Age)
                 predict <- predict +  0.105900079432E-01 * x
                 x <- ifelse(is.na(Sale.Year), 2014.91428571, Sale.Year)
                 predict <- predict +  0.132251705064 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 381
                 predict <- 60.6557497802
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.0107839677, log.Land.Square.Feet)
                 predict <- predict +  0.241301551379 * x
                 x <- ifelse(is.na(Longitude), -87.7341858944, Longitude)
                 predict <- predict +  3.89532191966 * x
                 x <- ifelse(is.na(Latitude), 41.9700066169, Latitude)
                 predict <- predict +  1.60082421782 * x
                 x <- ifelse(is.na(Census.Tract), 386758.336050, Census.Tract)
                 predict <- predict +  -0.244418466268E-06 * x
                 x <- ifelse(is.na(Age), 63.7097178683, Age)
                 predict <- predict +  0.245458963503E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 94.6733542320, Number.of.Units)
                 predict <- predict +  -0.608393751833E-03 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.578495297806E-01, Percent.Ownership)
                 predict <- predict +  2.11776031474 * x
                 x <- ifelse(is.na(Sale.Year), 2015.12413793, Sale.Year)
                 predict <- predict +  0.110800946775 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
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
               predict <- -253.782600998
               x <- ifelse(is.na(Garage.1.Area), 1.52444444444, Garage.1.Area)
               predict <- predict +  0.102049706296 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.09604150526, log.Building.Square.Feet)
               predict <- predict +  0.300742366637 * x
               x <- ifelse(is.na(Longitude), -87.7106148705, Longitude)
               predict <- predict +  -1.27686458323 * x
               x <- ifelse(is.na(Sale.Year), 2015.08444444, Sale.Year)
               predict <- predict +  0.754608476960E-01 * x
               x <- ifelse(is.na(HoodSqFt), 1331.89151604, HoodSqFt)
               predict <- predict +  -0.371794404864E-03 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               catvalues <- c("15","50","55","152","211","255")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 418
                 predict <- -675.852231119
                 x <- ifelse(is.na(Garage.1.Area), 1.70395869191, Garage.1.Area)
                 predict <- predict +  0.909431563752E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.21191733612, log.Building.Square.Feet)
                 predict <- predict +  0.546232761823 * x
                 x <- ifelse(is.na(Longitude), -87.7557724986, Longitude)
                 predict <- predict +  -6.11348969209 * x
                 x <- ifelse(is.na(Census.Tract), 820801.931153, Census.Tract)
                 predict <- predict +  0.329989461578E-04 * x
                 x <- ifelse(is.na(Sale.Year), 2015.14285714, Sale.Year)
                 predict <- predict +  0.595770194707E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 419
                 predict <- -237.752415090
                 x <- ifelse(is.na(Fireplaces), 0.270163934426, Fireplaces)
                 predict <- predict +  0.109250429349 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.69114754098, Garage.1.Area)
                 predict <- predict +  0.607884322764E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.23051638376, log.Building.Square.Feet)
                 predict <- predict +  0.601733418183 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.459016393443E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.795080249843 * x
                 x <- ifelse(is.na(Latitude), 41.7306616719, Latitude)
                 predict <- predict +  2.54004224584 * x
                 x <- ifelse(is.na(Census.Tract), 719695.257049, Census.Tract)
                 predict <- predict +  -0.636865195953E-06 * x
                 x <- ifelse(is.na(Sale.Year), 2015.10295082, Sale.Year)
                 predict <- predict +  0.693081844790E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1559.12460844, HoodSqFt)
                 predict <- predict +  0.142916037030E-03 * x
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
                 predict <- -434.084610650
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.58320103647, log.Land.Square.Feet)
                 predict <- predict +  0.236396663138 * x
                 x <- ifelse(is.na(Fireplaces), 0.171753097751, Fireplaces)
                 predict <- predict +  0.151967876191 * x
                 x <- ifelse(is.na(Half.Baths), 0.358765488756, Half.Baths)
                 predict <- predict +  0.679960280367E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.63240018357, Garage.1.Area)
                 predict <- predict +  0.870808514096E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.24046225065, log.Building.Square.Feet)
                 predict <- predict +  0.779563149513 * x
                 x <- ifelse(is.na(Longitude), -87.6958768889, Longitude)
                 predict <- predict +  -1.37591603616 * x
                 x <- ifelse(is.na(Latitude), 41.7117622139, Latitude)
                 predict <- predict +  2.29436822350 * x
                 x <- ifelse(is.na(Census.Tract), 742935.503442, Census.Tract)
                 predict <- predict +  -0.381876790832E-06 * x
                 x <- ifelse(is.na(Age), 67.1759981643, Age)
                 predict <- predict +  -0.833242936926E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.16268931, Sale.Year)
                 predict <- predict +  0.110477613178 * x
                 x <- ifelse(is.na(Sale.Month), 6.81941257458, Sale.Month)
                 predict <- predict +  0.132170360782E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1724.68698218, HoodSqFt)
                 predict <- predict +  -0.849315744076E-04 * x
                 x <- ifelse(is.na(PropClassSqFt), 1594.12643173, PropClassSqFt)
                 predict <- predict +  -0.200496733352E-03 * x
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
                 predict <- -330.643057798
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.51469324453, log.Land.Square.Feet)
                 predict <- predict +  0.243717140097 * x
                 x <- ifelse(is.na(Fireplaces), 0.151186639320, Fireplaces)
                 predict <- predict +  0.109143285580 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.58570172868, Garage.1.Area)
                 predict <- predict +  0.559776311594E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.25791202615, log.Building.Square.Feet)
                 predict <- predict +  0.716353214599 * x
                 x <- ifelse(is.na(Longitude), -87.6675827185, Longitude)
                 predict <- predict +  -2.29522543547 * x
                 x <- ifelse(is.na(Latitude), 41.7073755076, Latitude)
                 predict <- predict +  1.77831352339 * x
                 x <- ifelse(is.na(Age), 71.7910928802, Age)
                 predict <- predict +  -0.729461151682E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2014.98095517, Sale.Year)
                 predict <- predict +  0.295946278850E-01 * x
                 x <- ifelse(is.na(PropClassSqFt), 1625.16366960, PropClassSqFt)
                 predict <- predict +  -0.147749635006E-03 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         } else {
           nodeid <- 53
           predict <- 10.3436911659
           x <- ifelse(is.na(Full.Baths), 1.46428571429, Full.Baths)
           predict <- predict +  0.666079443906 * x
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
                 predict <- -271.709898935
                 x <- ifelse(is.na(Census.Tract), 443732.460292, Census.Tract)
                 predict <- predict +  0.361276765014E-06 * x
                 x <- ifelse(is.na(Full.Baths), 2.13290113452, Full.Baths)
                 predict <- predict +  0.188527316096 * x
                 x <- ifelse(is.na(Age), 102.952998379, Age)
                 predict <- predict +  -0.557524196214E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.15721232, Sale.Year)
                 predict <- predict +  0.140398404073 * x
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
                 predict <- -50.9868179180
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.83756345332, log.Land.Square.Feet)
                 predict <- predict +  0.200903397180 * x
                 x <- ifelse(is.na(Fireplaces), 0.314785373609, Fireplaces)
                 predict <- predict +  0.871251241974E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.73608903021, Garage.1.Area)
                 predict <- predict +  0.564264143300E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.34636041985, log.Building.Square.Feet)
                 predict <- predict +  0.435797267472 * x
                 x <- ifelse(is.na(Longitude), -87.8750446763, Longitude)
                 predict <- predict +  1.35054004034 * x
                 x <- ifelse(is.na(Latitude), 41.8468497520, Latitude)
                 predict <- predict +  0.945435486593 * x
                 x <- ifelse(is.na(Census.Tract), 675238.421304, Census.Tract)
                 predict <- predict +  -0.344137349135E-06 * x
                 x <- ifelse(is.na(Age), 50.8744038156, Age)
                 predict <- predict +  -0.737260592714E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.16375199, Sale.Year)
                 predict <- predict +  0.684552570180E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 435
                 predict <- -129.552680493
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.48531715180, log.Land.Square.Feet)
                 predict <- predict +  0.231579942387 * x
                 x <- ifelse(is.na(Rooms), 6.94899951196, Rooms)
                 predict <- predict +  0.240753160641E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.49023914104, Bedrooms)
                 predict <- predict +  -0.839380705383E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.168862859932, Fireplaces)
                 predict <- predict +  0.187959376037 * x
                 x <- ifelse(is.na(Half.Baths), 0.381893606637, Half.Baths)
                 predict <- predict +  0.155688865721 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.55441678868, Garage.1.Area)
                 predict <- predict +  0.544427460488E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.34074126651, log.Building.Square.Feet)
                 predict <- predict +  0.452275074296 * x
                 x <- ifelse(is.na(Longitude), -87.8734508653, Longitude)
                 predict <- predict +  1.76911594930 * x
                 x <- ifelse(is.na(Latitude), 41.9430280458, Latitude)
                 predict <- predict +  1.67059021647 * x
                 x <- ifelse(is.na(Full.Baths), 1.62201073694, Full.Baths)
                 predict <- predict +  0.101442276961 * x
                 x <- ifelse(is.na(Age), 69.0971205466, Age)
                 predict <- predict +  -0.628726745009E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.05124451, Sale.Year)
                 predict <- predict +  0.110656737681 * x
                 x <- ifelse(is.na(Sale.Month), 6.79062957540, Sale.Month)
                 predict <- predict +  0.152487096948E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1754.85173961, HoodSqFt)
                 predict <- predict +  -0.533969983856E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 1769.91872286, PropClassSqFt)
                 predict <- predict +  -0.121568116599E-03 * x
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
                 predict <- -38.0345293012
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.66234578110, log.Land.Square.Feet)
                 predict <- predict +  0.197694119028 * x
                 x <- ifelse(is.na(Bedrooms), 3.39602066195, Bedrooms)
                 predict <- predict +  -0.467599121039E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.338817677444, Fireplaces)
                 predict <- predict +  0.106700899843 * x
                 x <- ifelse(is.na(Half.Baths), 0.520374976086, Half.Baths)
                 predict <- predict +  0.397849898955E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.39204321185, log.Building.Square.Feet)
                 predict <- predict +  0.495913482682 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.726994451884E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.347041301988 * x
                 x <- ifelse(is.na(Longitude), -87.8708435538, Longitude)
                 predict <- predict +  1.66334020592 * x
                 x <- ifelse(is.na(Latitude), 41.9472396816, Latitude)
                 predict <- predict +  1.14072189208 * x
                 x <- ifelse(is.na(Census.Tract), 536489.697532, Census.Tract)
                 predict <- predict +  -0.145878127560E-06 * x
                 x <- ifelse(is.na(Full.Baths), 1.64224220394, Full.Baths)
                 predict <- predict +  0.600482008354E-01 * x
                 x <- ifelse(is.na(Age), 57.8777501435, Age)
                 predict <- predict +  -0.334224847744E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.08303042, Sale.Year)
                 predict <- predict +  0.713268065479E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.90242969198, Sale.Month)
                 predict <- predict +  0.742978856937E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         } else {
           catvalues <- c("24","38")
           if(Town.Code %in% catvalues){
             nodeid <- 110
             predict <- -180.945329767
             x <- ifelse(is.na(log.Land.Square.Feet), 8.86871354864, log.Land.Square.Feet)
             predict <- predict +  0.198875314710 * x
             x <- ifelse(is.na(Fireplaces), 0.450442851118, Fireplaces)
             predict <- predict +  0.100244064237 * x
             x <- ifelse(is.na(Garage.1.Area), 1.62842682412, Garage.1.Area)
             predict <- predict +  0.391871461953E-01 * x
             x <- ifelse(is.na(log.Building.Square.Feet), 7.37897705240, log.Building.Square.Feet)
             predict <- predict +  0.532403494048 * x
             x <- ifelse(is.na(Longitude), -87.8739838732, Longitude)
             predict <- predict +  -1.38766764738 * x
             x <- ifelse(is.na(Latitude), 42.0764498131, Latitude)
             predict <- predict +  -2.46374300677 * x
             x <- ifelse(is.na(Census.Tract), 804808.319275, Census.Tract)
             predict <- predict +  0.523396660411E-04 * x
             x <- ifelse(is.na(Age), 52.0189793336, Age)
             predict <- predict +  -0.397901543531E-02 * x
             x <- ifelse(is.na(Sale.Year), 2015.04048925, Sale.Year)
             predict <- predict +  0.634271407190E-01 * x
             x <- ifelse(is.na(Sale.Month), 6.88064107971, Sale.Month)
             predict <- predict +  0.105953190417E-01 * x
             x <- ifelse(is.na(HoodSqFt), 1898.01002642, HoodSqFt)
             predict <- predict +  -0.173616747830E-03 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             catvalues <- c("21","23","25")
             if(Town.Code %in% catvalues){
               catvalues <- c("43","44","45","50","53","62","80","92","93","94","110","141","170","171","180","190","200","210","220","240","300","320","420")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 444
                 predict <- 392.044601931
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.36270392279, log.Land.Square.Feet)
                 predict <- predict +  0.184324173648 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.93342716558, log.Building.Square.Feet)
                 predict <- predict +  0.856142389373 * x
                 x <- ifelse(is.na(Longitude), -87.7479690142, Longitude)
                 predict <- predict +  4.40641877073 * x
                 x <- ifelse(is.na(Age), 63.2923076923, Age)
                 predict <- predict +  -0.233803302179E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 445
                 predict <- -149.177653168
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.05943734243, log.Land.Square.Feet)
                 predict <- predict +  0.146532372103 * x
                 x <- ifelse(is.na(Rooms), 7.28632276384, Rooms)
                 predict <- predict +  -0.291247334044E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.703265499290, Fireplaces)
                 predict <- predict +  0.224355085722 * x
                 x <- ifelse(is.na(Half.Baths), 0.676289635589, Half.Baths)
                 predict <- predict +  0.935028580413E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.56471049904, log.Building.Square.Feet)
                 predict <- predict +  0.756996321621 * x
                 x <- ifelse(is.na(Longitude), -87.8310046578, Longitude)
                 predict <- predict +  -1.25888708834 * x
                 x <- ifelse(is.na(Census.Tract), 810005.525319, Census.Tract)
                 predict <- predict +  -0.247490871752E-04 * x
                 x <- ifelse(is.na(Full.Baths), 1.91292001893, Full.Baths)
                 predict <- predict +  0.766462361538E-01 * x
                 x <- ifelse(is.na(Sale.Year), 2015.01703739, Sale.Year)
                 predict <- predict +  0.318945939478E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.83861807856, Sale.Month)
                 predict <- predict +  0.109804710975E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               catvalues <- c("33","73","74")
               if(Town.Code %in% catvalues){
                 nodeid <- 446
                 predict <- 915.204804872
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.02855995262, log.Land.Square.Feet)
                 predict <- predict +  0.464850516501 * x
                 x <- ifelse(is.na(Rooms), 9.49436619718, Rooms)
                 predict <- predict +  -0.342011969186E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.603521126761, Fireplaces)
                 predict <- predict +  0.775268343551E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.77777871130, log.Building.Square.Feet)
                 predict <- predict +  0.615072338579 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.316901408451E-01, Number.of.Commercial.Units)
                 predict <- predict +  -0.330706487622 * x
                 x <- ifelse(is.na(Longitude), -87.6791466582, Longitude)
                 predict <- predict +  8.32078405031 * x
                 x <- ifelse(is.na(Latitude), 41.9403191165, Latitude)
                 predict <- predict +  -6.76947381920 * x
                 x <- ifelse(is.na(Full.Baths), 2.48943661972, Full.Baths)
                 predict <- predict +  0.733196015082E-01 * x
                 x <- ifelse(is.na(Age), 76.7683098592, Age)
                 predict <- predict +  -0.126669238112E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2014.94647887, Sale.Year)
                 predict <- predict +  0.513999777969E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 447
                 predict <- -16.1678442171
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.77415321045, log.Land.Square.Feet)
                 predict <- predict +  0.140493464242 * x
                 x <- ifelse(is.na(Bedrooms), 3.74695493301, Bedrooms)
                 predict <- predict +  -0.727540145329E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.526796589525, Fireplaces)
                 predict <- predict +  0.163086371490 * x
                 x <- ifelse(is.na(Half.Baths), 0.592570036541, Half.Baths)
                 predict <- predict +  0.892188293093E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.54902557856, Garage.1.Area)
                 predict <- predict +  0.717223966975E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.54887882133, log.Building.Square.Feet)
                 predict <- predict +  0.781968028882 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.700365408039E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.521361689451 * x
                 x <- ifelse(is.na(Longitude), -87.8656431221, Longitude)
                 predict <- predict +  1.24349521092 * x
                 x <- ifelse(is.na(Latitude), 41.9732126126, Latitude)
                 predict <- predict +  0.607225678000 * x
                 x <- ifelse(is.na(Census.Tract), 731217.102010, Census.Tract)
                 predict <- predict +  0.204089963682E-06 * x
                 x <- ifelse(is.na(Full.Baths), 1.87515225335, Full.Baths)
                 predict <- predict +  0.506470434258E-01 * x
                 x <- ifelse(is.na(Age), 64.1071863581, Age)
                 predict <- predict +  -0.109703380725E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.06729598, Sale.Year)
                 predict <- predict +  0.523252032766E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.96802679659, Sale.Month)
                 predict <- predict +  0.841891844269E-02 * x
                 x <- ifelse(is.na(PropClassSqFt), 2034.32151396, PropClassSqFt)
                 predict <- predict +  -0.617344028675E-04 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         }
       }
     }
   } else {
     if(is.na(Census.Tract)){
       catvalues <- c("40","70","180")
       if(Neighborhood.Code %in% catvalues){
         nodeid <- 28
         predict <- 25.1710188577
         x <- ifelse(is.na(Fireplaces), 0.127659574468, Fireplaces)
         predict <- predict +  0.855027744598 * x
         x <- ifelse(is.na(log.Building.Square.Feet), 7.47118816432, log.Building.Square.Feet)
         predict <- predict +  1.00060608374 * x
         x <- ifelse(is.na(Total.Building.Square.Feet), 8282.38297872, Total.Building.Square.Feet)
         predict <- predict +  -0.214275793897E-03 * x
         x <- ifelse(is.na(Sale.Month), 7.04255319149, Sale.Month)
         predict <- predict +  -0.986695623241E-02 * x
         x <- ifelse(is.na(HoodSqFt), 1766.30952186, HoodSqFt)
         predict <- predict +  -0.112226840416E-01 * x
         predict <- min(max(predict, 9.21094019205),15.8169906933)
       } else {
         catvalues <- c("16","19","25","36","37","70")
         if(Town.Code %in% catvalues){
           nodeid <- 58
           predict <- 12.3567703873
           x <- ifelse(is.na(Half.Baths), 0.739130434783, Half.Baths)
           predict <- predict +  -0.943934232202 * x
           x <- ifelse(is.na(Age), 2.06521739130, Age)
           predict <- predict +  -0.876505943273E-01 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           catvalues <- c("28","39","72","76")
           if(Town.Code %in% catvalues){
             nodeid <- 118
             predict <- 0.360049450814
             x <- ifelse(is.na(Rooms), 5.88359788360, Rooms)
             predict <- predict +  0.148615278031 * x
             x <- ifelse(is.na(Garage.1.Area), 1.63492063492, Garage.1.Area)
             predict <- predict +  -0.135999911686 * x
             x <- ifelse(is.na(log.Building.Square.Feet), 7.49298504365, log.Building.Square.Feet)
             predict <- predict +  1.48595944325 * x
             x <- ifelse(is.na(Total.Building.Square.Feet), 23215.3015873, Total.Building.Square.Feet)
             predict <- predict +  -0.389328387736E-04 * x
             x <- ifelse(is.na(HoodSqFt), 1855.51803263, HoodSqFt)
             predict <- predict +  -0.371538502312E-03 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             nodeid <- 119
             predict <- 10.3943963588
             x <- ifelse(is.na(Bedrooms), 3.03738317757, Bedrooms)
             predict <- predict +  0.289472424651 * x
             x <- ifelse(is.na(Fireplaces), 0.485981308411, Fireplaces)
             predict <- predict +  0.324476990973 * x
             x <- ifelse(is.na(Garage.1.Area), 1.08411214953, Garage.1.Area)
             predict <- predict +  0.194562737583 * x
             x <- ifelse(is.na(Total.Building.Square.Feet), 11122.7850467, Total.Building.Square.Feet)
             predict <- predict +  -0.485904854984E-04 * x
             x <- ifelse(is.na(PropClassSqFt), 1899.56138191, PropClassSqFt)
             predict <- predict +  0.392868537668E-03 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           }
         }
       }
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
         catvalues <- c("10","11","31","32","41","50","71","91","92","132","180","380","430")
         if(Neighborhood.Code %in% catvalues){
           nodeid <- 62
           predict <- -390.954277653
           x <- ifelse(is.na(log.Building.Square.Feet), 7.25345030071, log.Building.Square.Feet)
           predict <- predict +  1.00641757578 * x
           x <- ifelse(is.na(Latitude), 41.8844172910, Latitude)
           predict <- predict +  2.43509031443 * x
           x <- ifelse(is.na(Sale.Year), 2014.65432099, Sale.Year)
           predict <- predict +  0.145574746564 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           nodeid <- 63
           predict <- -199.433570469
           x <- ifelse(is.na(log.Land.Square.Feet), 7.66763421532, log.Land.Square.Feet)
           predict <- predict +  0.713639523586 * x
           x <- ifelse(is.na(Latitude), 41.8075768718, Latitude)
           predict <- predict +  4.88570222645 * x
           x <- ifelse(is.na(Sale.Month), 6.74336283186, Sale.Month)
           predict <- predict +  0.610582932609E-01 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
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
    Rooms <- as.numeric(newdata$Rooms[i])
    Bedrooms <- as.numeric(newdata$Bedrooms[i])
    Fireplaces <- as.numeric(newdata$Fireplaces[i])
    Half.Baths <- as.numeric(newdata$Half.Baths[i])
    Garage.1.Area <- as.numeric(newdata$Garage.1.Area[i])
    log.Building.Square.Feet <- as.numeric(newdata$log.Building.Square.Feet[i])
    Number.of.Commercial.Units <- as.numeric(newdata$Number.of.Commercial.Units[i])
    Longitude <- as.numeric(newdata$Longitude[i])
    Latitude <- as.numeric(newdata$Latitude[i])
    Census.Tract <- as.numeric(newdata$Census.Tract[i])
    Total.Building.Square.Feet <- as.numeric(newdata$Total.Building.Square.Feet[i])
    Full.Baths <- as.numeric(newdata$Full.Baths[i])
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

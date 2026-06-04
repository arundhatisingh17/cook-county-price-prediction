## Function for predicting least-squares mean of log.Sale.Price
## Code produced by GUIDE 46.2 on 4/26/26 at 21:34
guide_predict <- function(){
 if(is.na(PropClassPriceSqFt)){
   if(is.na(Census.Tract)){
     catvalues <- c("12","22","41","140","180")
     if(Neighborhood.Code %in% catvalues){
       nodeid <- 8
       predict <- 6.81102228455
       x <- ifelse(is.na(HoodPriceSqFt), 221.507060908, HoodPriceSqFt)
       predict <- predict +  0.223774879102E-01 * x
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
           predict <- -308.949246138
           x <- ifelse(is.na(Age), 57.5714285714, Age)
           predict <- predict +  -0.127138077285E-01 * x
           x <- ifelse(is.na(Sale.Year), 2014.60714286, Sale.Year)
           predict <- predict +  0.158561534624 * x
           x <- ifelse(is.na(HoodPriceSqFt), 145.355369236, HoodPriceSqFt)
           predict <- predict +  0.100561564157E-01 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         }
       } else {
         nodeid <- 19
         predict <- -325.518193901
         x <- ifelse(is.na(Age), 68.0829493088, Age)
         predict <- predict +  -0.435539336116E-02 * x
         x <- ifelse(is.na(Sale.Year), 2015.16129032, Sale.Year)
         predict <- predict +  0.167013448924 * x
         x <- ifelse(is.na(Sale.Month), 6.45161290323, Sale.Month)
         predict <- predict +  0.448488830911E-01 * x
         x <- ifelse(is.na(HoodPriceSqFt), 173.568380700, HoodPriceSqFt)
         predict <- predict +  0.587743900127E-02 * x
         predict <- min(max(predict, 9.21094019205),15.8169906933)
       }
     }
   } else {
     catvalues <- c("12","13","19","20","23","30","34","36","37","74")
     if(Town.Code %in% catvalues){
       catvalues <- c("11","12","13","22","30","43","91","92","93","120","150","171")
       if(Neighborhood.Code %in% catvalues){
         catvalues <- c("11","12","13","91")
         if(Neighborhood.Code %in% catvalues){
           nodeid <- 40
           predict <- 448.824934402
           x <- ifelse(is.na(log.Land.Square.Feet), 9.77971774176, log.Land.Square.Feet)
           predict <- predict +  0.225425367907 * x
           x <- ifelse(is.na(Longitude), -87.6557074603, Longitude)
           predict <- predict +  6.41545356145 * x
           x <- ifelse(is.na(Latitude), 41.9114544788, Latitude)
           predict <- predict +  2.95489780752 * x
           x <- ifelse(is.na(Age), 51.0798668885, Age)
           predict <- predict +  -0.300626845926E-02 * x
           x <- ifelse(is.na(Number.of.Units), 107.890183028, Number.of.Units)
           predict <- predict +  -0.207290471784E-02 * x
           x <- ifelse(is.na(Percent.Ownership), 0.831946755408E-01, Percent.Ownership)
           predict <- predict +  2.51904067343 * x
           x <- ifelse(is.na(HoodPriceSqFt), 241.170081105, HoodPriceSqFt)
           predict <- predict +  0.164327139893E-02 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           if(!is.na(Census.Tract) & Census.Tract <= 806350.500000 ){
             nodeid <- 82
             predict <- 666.733562727
             x <- ifelse(is.na(log.Land.Square.Feet), 10.3372149840, log.Land.Square.Feet)
             predict <- predict +  0.208446119873 * x
             x <- ifelse(is.na(Longitude), -87.6316245245, Longitude)
             predict <- predict +  7.47982440111 * x
             x <- ifelse(is.na(Age), 41.1641991925, Age)
             predict <- predict +  -0.314816241227E-02 * x
             x <- ifelse(is.na(Number.of.Units), 360.763795424, Number.of.Units)
             predict <- predict +  -0.100683018763E-02 * x
             x <- ifelse(is.na(Percent.Ownership), 0.782637954240E-02, Percent.Ownership)
             predict <- predict +  6.50694197228 * x
             x <- ifelse(is.na(HoodPriceSqFt), 179.278099364, HoodPriceSqFt)
             predict <- predict +  -0.223960691916E-02 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             nodeid <- 83
             predict <- -481.607507445
             x <- ifelse(is.na(Longitude), -87.7676285947, Longitude)
             predict <- predict +  -2.97556518549 * x
             x <- ifelse(is.na(Age), 31.7500000000, Age)
             predict <- predict +  -0.262329443398E-01 * x
             x <- ifelse(is.na(Sale.Year), 2014.87500000, Sale.Year)
             predict <- predict +  0.115327384769 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           }
         }
       } else {
         catvalues <- c("10","40","41","42","53","54","61","70","100","130","141","160","170","210")
         if(Neighborhood.Code %in% catvalues){
           catvalues <- c("10","41","53","70","141","160","210")
           if(Neighborhood.Code %in% catvalues){
             nodeid <- 84
             predict <- -143.900259095
             x <- ifelse(is.na(log.Land.Square.Feet), 11.1616674962, log.Land.Square.Feet)
             predict <- predict +  0.220420649619 * x
             x <- ifelse(is.na(Latitude), 41.7152062148, Latitude)
             predict <- predict +  0.926436123123 * x
             x <- ifelse(is.na(Age), 32.6176470588, Age)
             predict <- predict +  -0.749278550207E-02 * x
             x <- ifelse(is.na(Percent.Ownership), 0.326470588235E-01, Percent.Ownership)
             predict <- predict +  3.08860505570 * x
             x <- ifelse(is.na(Sale.Year), 2015.10784314, Sale.Year)
             predict <- predict +  0.568419644759E-01 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             nodeid <- 85
             predict <- -364.500733928
             x <- ifelse(is.na(Longitude), -87.7955298438, Longitude)
             predict <- predict +  -6.81889405698 * x
             x <- ifelse(is.na(Latitude), 41.6773235188, Latitude)
             predict <- predict +  -5.80431702046 * x
             x <- ifelse(is.na(Census.Tract), 823078.649007, Census.Tract)
             predict <- predict +  -0.178262878823E-03 * x
             x <- ifelse(is.na(Age), 29.0331125828, Age)
             predict <- predict +  -0.106224024439E-01 * x
             x <- ifelse(is.na(Sale.Year), 2015.07947020, Sale.Year)
             predict <- predict +  0.829211789402E-01 * x
             x <- ifelse(is.na(HoodPriceSqFt), 145.716275272, HoodPriceSqFt)
             predict <- predict +  -0.426965231807E-02 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           }
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
           predict <- -391.153839462
           x <- ifelse(is.na(log.Land.Square.Feet), 11.8536580929, log.Land.Square.Feet)
           predict <- predict +  0.961042008039E-01 * x
           x <- ifelse(is.na(Latitude), 41.8687980381, Latitude)
           predict <- predict +  2.51291203201 * x
           x <- ifelse(is.na(Census.Tract), 814467.128803, Census.Tract)
           predict <- predict +  0.332441594009E-04 * x
           x <- ifelse(is.na(Age), 34.7596348884, Age)
           predict <- predict +  -0.169012979025E-01 * x
           x <- ifelse(is.na(Number.of.Units), 155.992900609, Number.of.Units)
           predict <- predict +  -0.118061801612E-02 * x
           x <- ifelse(is.na(Sale.Year), 2015.06389452, Sale.Year)
           predict <- predict +  0.133735304730 * x
           x <- ifelse(is.na(Sale.Month), 6.79310344828, Sale.Month)
           predict <- predict +  0.139190457117E-01 * x
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
             predict <- -343.521648620
             x <- ifelse(is.na(Longitude), -88.0191995771, Longitude)
             predict <- predict +  -1.36148895890 * x
             x <- ifelse(is.na(Age), 26.8240740741, Age)
             predict <- predict +  -0.196683138234E-01 * x
             x <- ifelse(is.na(Sale.Year), 2015.00000000, Sale.Year)
             predict <- predict +  0.117164855028 * x
             x <- ifelse(is.na(Sale.Month), 6.65608465608, Sale.Month)
             predict <- predict +  0.111772338291E-01 * x
             x <- ifelse(is.na(HoodPriceSqFt), 160.593388836, HoodPriceSqFt)
             predict <- predict +  -0.152659148557E-02 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           }
         }
       } else {
         catvalues <- c("16","28","70","75")
         if(Town.Code %in% catvalues){
           catvalues <- c("22","23","31","32","45","60","88","103","120","121","140","180")
           if(Neighborhood.Code %in% catvalues){
             nodeid <- 92
             predict <- -1039.48415292
             x <- ifelse(is.na(log.Land.Square.Feet), 9.85468894173, log.Land.Square.Feet)
             predict <- predict +  0.279712614790 * x
             x <- ifelse(is.na(Longitude), -87.7018740929, Longitude)
             predict <- predict +  -6.69111503683 * x
             x <- ifelse(is.na(Latitude), 41.9356773702, Latitude)
             predict <- predict +  3.84904321821 * x
             x <- ifelse(is.na(Number.of.Units), 31.8520084567, Number.of.Units)
             predict <- predict +  -0.509828593729E-02 * x
             x <- ifelse(is.na(Percent.Ownership), 0.748625792812E-01, Percent.Ownership)
             predict <- predict +  3.24223618882 * x
             x <- ifelse(is.na(Sale.Year), 2015.10359408, Sale.Year)
             predict <- predict +  0.149751945652 * x
             x <- ifelse(is.na(Sale.Month), 6.83298097252, Sale.Month)
             predict <- predict +  0.255953801439E-01 * x
             x <- ifelse(is.na(HoodPriceSqFt), 186.995694372, HoodPriceSqFt)
             predict <- predict +  -0.104708154565E-01 * x
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
                 predict <- -317.406747825
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.8440324405, log.Land.Square.Feet)
                 predict <- predict +  0.223414754447 * x
                 x <- ifelse(is.na(Longitude), -87.7478228348, Longitude)
                 predict <- predict +  -0.356876145594 * x
                 x <- ifelse(is.na(Latitude), 41.7991176313, Latitude)
                 predict <- predict +  0.631891245675 * x
                 x <- ifelse(is.na(Age), 47.6342668863, Age)
                 predict <- predict +  -0.447849170663E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 74.4200988468, Number.of.Units)
                 predict <- predict +  -0.188354585389E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.717957166392E-01, Percent.Ownership)
                 predict <- predict +  2.68469632762 * x
                 x <- ifelse(is.na(Sale.Year), 2015.06425041, Sale.Year)
                 predict <- predict +  0.134274238985 * x
                 x <- ifelse(is.na(HoodPriceSqFt), 149.021235453, HoodPriceSqFt)
                 predict <- predict +  -0.103829093118E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 375
                 predict <- -183.010480077
                 x <- ifelse(is.na(log.Land.Square.Feet), 11.3783281805, log.Land.Square.Feet)
                 predict <- predict +  0.110751777338 * x
                 x <- ifelse(is.na(Latitude), 41.8957624805, Latitude)
                 predict <- predict +  -0.886815968675 * x
                 x <- ifelse(is.na(Number.of.Units), 153.111635220, Number.of.Units)
                 predict <- predict +  -0.550256670109E-03 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.306918238994E-01, Percent.Ownership)
                 predict <- predict +  4.84821176786 * x
                 x <- ifelse(is.na(Sale.Year), 2015.02358491, Sale.Year)
                 predict <- predict +  0.114706639172 * x
                 x <- ifelse(is.na(HoodPriceSqFt), 146.736416552, HoodPriceSqFt)
                 predict <- predict +  -0.404820041916E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         } else {
           if(!is.na(Age) & Age <= 27.5000000000 ){
             catvalues <- c("12","24","30","32","42","44","51","52","53","60","62","63","70","71","80","81","84","92","93","110","120","131","132","150","151","152","160","170","200")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 188
               predict <- 77.4867267170
               x <- ifelse(is.na(log.Land.Square.Feet), 9.30283946466, log.Land.Square.Feet)
               predict <- predict +  0.219505318176 * x
               x <- ifelse(is.na(Longitude), -87.6974964891, Longitude)
               predict <- predict +  1.98995081495 * x
               x <- ifelse(is.na(Census.Tract), 338699.205227, Census.Tract)
               predict <- predict +  -0.245150731860E-06 * x
               x <- ifelse(is.na(Age), 11.7964867181, Age)
               predict <- predict +  -0.997800703750E-02 * x
               x <- ifelse(is.na(Number.of.Units), 42.5638389032, Number.of.Units)
               predict <- predict +  -0.138103506081E-02 * x
               x <- ifelse(is.na(Percent.Ownership), 0.159035989717, Percent.Ownership)
               predict <- predict +  2.00923388895 * x
               x <- ifelse(is.na(Sale.Year), 2015.02399314, Sale.Year)
               predict <- predict +  0.533191732936E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.75192802057, Sale.Month)
               predict <- predict +  0.934314980741E-02 * x
               x <- ifelse(is.na(HoodPriceSqFt), 200.457745673, HoodPriceSqFt)
               predict <- predict +  0.107817271167E-02 * x
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
               predict <- 25.1181950754
               x <- ifelse(is.na(log.Land.Square.Feet), 10.0624321605, log.Land.Square.Feet)
               predict <- predict +  0.219979192713 * x
               x <- ifelse(is.na(Longitude), -87.7417974287, Longitude)
               predict <- predict +  3.35726689682 * x
               x <- ifelse(is.na(Latitude), 41.9594733304, Latitude)
               predict <- predict +  1.50295873918 * x
               x <- ifelse(is.na(Census.Tract), 413616.268235, Census.Tract)
               predict <- predict +  -0.239062799305E-06 * x
               x <- ifelse(is.na(Age), 62.7905882353, Age)
               predict <- predict +  0.303589450884E-02 * x
               x <- ifelse(is.na(Number.of.Units), 94.4058823529, Number.of.Units)
               predict <- predict +  -0.516445481497E-03 * x
               x <- ifelse(is.na(Percent.Ownership), 0.565117647059E-01, Percent.Ownership)
               predict <- predict +  2.10806874895 * x
               x <- ifelse(is.na(Sale.Year), 2015.11117647, Sale.Year)
               predict <- predict +  0.106965838673 * x
               x <- ifelse(is.na(HoodPriceSqFt), 168.038236631, HoodPriceSqFt)
               predict <- predict +  0.200357896607E-02 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               catvalues <- c("18","22","24","26","31","38")
               if(Town.Code %in% catvalues){
                 nodeid <- 382
                 predict <- -232.665799526
                 x <- ifelse(is.na(Longitude), -87.8981386238, Longitude)
                 predict <- predict +  0.827868137115 * x
                 x <- ifelse(is.na(Age), 40.8984455959, Age)
                 predict <- predict +  -0.143571211470E-01 * x
                 x <- ifelse(is.na(Sale.Year), 2015.05906736, Sale.Year)
                 predict <- predict +  0.157386669104 * x
                 x <- ifelse(is.na(HoodPriceSqFt), 173.684584861, HoodPriceSqFt)
                 predict <- predict +  0.220113001269E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 383
                 predict <- 144.133131416
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.86160588821, log.Land.Square.Feet)
                 predict <- predict +  0.154208307432 * x
                 x <- ifelse(is.na(Longitude), -87.6918690132, Longitude)
                 predict <- predict +  3.43853055891 * x
                 x <- ifelse(is.na(Latitude), 41.9530861623, Latitude)
                 predict <- predict +  0.975646005345 * x
                 x <- ifelse(is.na(Census.Tract), 306495.188568, Census.Tract)
                 predict <- predict +  0.336433417157E-06 * x
                 x <- ifelse(is.na(Age), 74.1263995286, Age)
                 predict <- predict +  0.413012685780E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 151.108426635, Number.of.Units)
                 predict <- predict +  -0.320851904055E-03 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.634855627578E-01, Percent.Ownership)
                 predict <- predict +  2.66337273322 * x
                 x <- ifelse(is.na(Sale.Year), 2015.12345315, Sale.Year)
                 predict <- predict +  0.625316923360E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.79375368297, Sale.Month)
                 predict <- predict +  0.144022061802E-01 * x
                 x <- ifelse(is.na(HoodPriceSqFt), 186.023593539, HoodPriceSqFt)
                 predict <- predict +  0.229947796161E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         }
       }
     }
   }
 } else {
   if(is.na(Census.Tract)){
     if(is.na(Total.Building.Square.Feet)){
       nodeid <- 12
       predict <- -243.327414880
       x <- ifelse(is.na(log.Building.Square.Feet), 7.83609456556, log.Building.Square.Feet)
       predict <- predict +  0.263842741260 * x
       x <- ifelse(is.na(Sale.Year), 2014.75229358, Sale.Year)
       predict <- predict +  0.125652459689 * x
       x <- ifelse(is.na(HoodPriceSqFt), 184.058666380, HoodPriceSqFt)
       predict <- predict +  0.555556829267E-02 * x
       predict <- min(max(predict, 9.21094019205),15.8169906933)
     } else {
       catvalues <- c("22","28","29","39","71","72","77")
       if(Town.Code %in% catvalues){
         nodeid <- 26
         predict <- -250.639938384
         x <- ifelse(is.na(log.Land.Square.Feet), 7.72453422280, log.Land.Square.Feet)
         predict <- predict +  -0.453591842907E-01 * x
         x <- ifelse(is.na(Rooms), 5.86475409836, Rooms)
         predict <- predict +  0.971323215183E-01 * x
         x <- ifelse(is.na(Bedrooms), 2.51639344262, Bedrooms)
         predict <- predict +  -0.244473233480 * x
         x <- ifelse(is.na(Half.Baths), 0.852459016393, Half.Baths)
         predict <- predict +  0.454490871318 * x
         x <- ifelse(is.na(Garage.1.Area), 1.45491803279, Garage.1.Area)
         predict <- predict +  -0.181599463804 * x
         x <- ifelse(is.na(log.Building.Square.Feet), 7.50676575172, log.Building.Square.Feet)
         predict <- predict +  0.734547570029 * x
         x <- ifelse(is.na(Total.Building.Square.Feet), 9874.78688525, Total.Building.Square.Feet)
         predict <- predict +  -0.737406500118E-04 * x
         x <- ifelse(is.na(Full.Baths), 2.06557377049, Full.Baths)
         predict <- predict +  0.304209161363 * x
         x <- ifelse(is.na(Age), 8.09426229508, Age)
         predict <- predict +  0.899234118862E-02 * x
         x <- ifelse(is.na(Sale.Year), 2014.59426230, Sale.Year)
         predict <- predict +  0.126244294639 * x
         x <- ifelse(is.na(HoodPriceSqFt), 149.972762172, HoodPriceSqFt)
         predict <- predict +  0.570348672027E-02 * x
         x <- ifelse(is.na(PropClassPriceSqFt), 148.831008790, PropClassPriceSqFt)
         predict <- predict +  0.911842679977E-02 * x
         predict <- min(max(predict, 9.21094019205),15.8169906933)
       } else {
         nodeid <- 27
         predict <- -416.527804138
         x <- ifelse(is.na(log.Land.Square.Feet), 8.48893054155, log.Land.Square.Feet)
         predict <- predict +  -0.281163961601 * x
         x <- ifelse(is.na(Fireplaces), 0.193103448276, Fireplaces)
         predict <- predict +  0.417028084252 * x
         x <- ifelse(is.na(Half.Baths), 0.682758620690, Half.Baths)
         predict <- predict +  -0.269976605539 * x
         x <- ifelse(is.na(log.Building.Square.Feet), 7.66477867215, log.Building.Square.Feet)
         predict <- predict +  1.22147412561 * x
         x <- ifelse(is.na(Total.Building.Square.Feet), 27544.7862069, Total.Building.Square.Feet)
         predict <- predict +  -0.496990065557E-04 * x
         x <- ifelse(is.na(Full.Baths), 2.50344827586, Full.Baths)
         predict <- predict +  -0.177793285886 * x
         x <- ifelse(is.na(Sale.Year), 2015.64827586, Sale.Year)
         predict <- predict +  0.209102220097 * x
         x <- ifelse(is.na(HoodPriceSqFt), 154.340065294, HoodPriceSqFt)
         predict <- predict +  0.660204091721E-02 * x
         predict <- min(max(predict, 9.21094019205),15.8169906933)
       }
     }
   } else {
     catvalues <- c("71","72")
     if(Town.Code %in% catvalues){
       if(is.na(Total.Building.Square.Feet)){
         catvalues <- c("30","40","51","52","61","71","80","91","92","120","121","130","151","170","191","192","194","210","212","221","222","223","230","251","270","280","281","282","293","310","312","321","323","330","345","520")
         if(Neighborhood.Code %in% catvalues){
           catvalues <- c("0.0")
           if(Most.Recent.Sale %in% catvalues){
             catvalues <- c("30","40","61","71","191","192","194","210","230","270","280","520")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 224
               predict <- -307.336591225
               x <- ifelse(is.na(log.Land.Square.Feet), 8.27105334695, log.Land.Square.Feet)
               predict <- predict +  0.472027484570 * x
               x <- ifelse(is.na(Latitude), 41.8302614944, Latitude)
               predict <- predict +  2.74619830021 * x
               x <- ifelse(is.na(Census.Tract), 527011.000000, Census.Tract)
               predict <- predict +  -0.673837607487E-06 * x
               x <- ifelse(is.na(Full.Baths), 1.46097560976, Full.Baths)
               predict <- predict +  0.110699661839 * x
               x <- ifelse(is.na(Age), 77.0170731707, Age)
               predict <- predict +  -0.111175741436E-01 * x
               x <- ifelse(is.na(Sale.Year), 2014.77317073, Sale.Year)
               predict <- predict +  0.994854687733E-01 * x
               x <- ifelse(is.na(HoodPriceSqFt), 135.164429031, HoodPriceSqFt)
               predict <- predict +  0.554280321705E-02 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               if(!is.na(Census.Tract) & Census.Tract <= 680950.000000 ){
                 nodeid <- 450
                 predict <- -379.171371338
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.23874655870, log.Building.Square.Feet)
                 predict <- predict +  0.403122960011 * x
                 x <- ifelse(is.na(Longitude), -87.6635473438, Longitude)
                 predict <- predict +  -2.46518743322 * x
                 x <- ifelse(is.na(Latitude), 41.7644883414, Latitude)
                 predict <- predict +  4.12377287617 * x
                 x <- ifelse(is.na(Census.Tract), 546042.175258, Census.Tract)
                 predict <- predict +  -0.137655961368E-05 * x
                 x <- ifelse(is.na(Age), 86.4123711340, Age)
                 predict <- predict +  -0.884457286294E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 451
                 predict <- -877.789017504
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.29741991764, log.Land.Square.Feet)
                 predict <- predict +  0.360637957075 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.30201725112, log.Building.Square.Feet)
                 predict <- predict +  0.407291402225 * x
                 x <- ifelse(is.na(Longitude), -87.6526043226, Longitude)
                 predict <- predict +  -8.51716377483 * x
                 x <- ifelse(is.na(Latitude), 41.7420935894, Latitude)
                 predict <- predict +  3.27073665785 * x
                 x <- ifelse(is.na(Age), 82.5130641330, Age)
                 predict <- predict +  -0.591162136897E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           } else {
             catvalues <- c("51","80","91","92","121","130","170","251","293","310","312","330","345")
             if(Neighborhood.Code %in% catvalues){
               if(!is.na(log.Land.Square.Feet) & log.Land.Square.Feet <= 8.01496276508 ){
                 nodeid <- 452
                 predict <- -355.376481077
                 x <- ifelse(is.na(Bedrooms), 3.58139534884, Bedrooms)
                 predict <- predict +  0.234903241732 * x
                 x <- ifelse(is.na(Age), 94.0116279070, Age)
                 predict <- predict +  -0.895091930276E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.06976744, Sale.Year)
                 predict <- predict +  0.181429501884 * x
                 x <- ifelse(is.na(HoodPriceSqFt), 115.173827419, HoodPriceSqFt)
                 predict <- predict +  0.506588355206E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 453
                 predict <- -263.572012806
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.23399468287, log.Land.Square.Feet)
                 predict <- predict +  0.571065405765 * x
                 x <- ifelse(is.na(Bedrooms), 3.56682027650, Bedrooms)
                 predict <- predict +  0.138495914041 * x
                 x <- ifelse(is.na(Age), 87.9055299539, Age)
                 predict <- predict +  -0.961509987846E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.22811060, Sale.Year)
                 predict <- predict +  0.133812658545 * x
                 x <- ifelse(is.na(HoodPriceSqFt), 81.0528504646, HoodPriceSqFt)
                 predict <- predict +  0.222044031434E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               catvalues <- c("40","120","151","191","194","221","222","230","323")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 454
                 predict <- -1977.02107374
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.30288888403, log.Building.Square.Feet)
                 predict <- predict +  0.478635277032 * x
                 x <- ifelse(is.na(Longitude), -87.6985396617, Longitude)
                 predict <- predict +  -18.5805188554 * x
                 x <- ifelse(is.na(Latitude), 41.8081437810, Latitude)
                 predict <- predict +  2.58838863302 * x
                 x <- ifelse(is.na(Age), 79.1711111111, Age)
                 predict <- predict +  -0.521043829323E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.14444444, Sale.Year)
                 predict <- predict +  0.123620550553 * x
                 x <- ifelse(is.na(HoodPriceSqFt), 130.765929375, HoodPriceSqFt)
                 predict <- predict +  -0.893311689949E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 455
                 predict <- -356.135961986
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.27698707504, log.Land.Square.Feet)
                 predict <- predict +  0.397633310608 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.18944985655, log.Building.Square.Feet)
                 predict <- predict +  0.287892852201 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.134408602151E-01, Number.of.Commercial.Units)
                 predict <- predict +  -0.438248272531 * x
                 x <- ifelse(is.na(Latitude), 41.8061855486, Latitude)
                 predict <- predict +  3.42968216972 * x
                 x <- ifelse(is.na(Age), 75.8503584229, Age)
                 predict <- predict +  -0.841856177590E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.09587814, Sale.Year)
                 predict <- predict +  0.108846927333 * x
                 x <- ifelse(is.na(HoodPriceSqFt), 115.566584892, HoodPriceSqFt)
                 predict <- predict +  0.434609900161E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         } else {
           nodeid <- 57
           predict <- -228.300841725
           x <- ifelse(is.na(log.Land.Square.Feet), 8.32909912516, log.Land.Square.Feet)
           predict <- predict +  0.342103147919 * x
           x <- ifelse(is.na(Fireplaces), 0.120346479912, Fireplaces)
           predict <- predict +  0.864041261114E-01 * x
           x <- ifelse(is.na(log.Building.Square.Feet), 7.28160573170, log.Building.Square.Feet)
           predict <- predict +  0.364431184227 * x
           x <- ifelse(is.na(Number.of.Commercial.Units), 0.140066347217E-01, Number.of.Commercial.Units)
           predict <- predict +  -0.185113649999 * x
           x <- ifelse(is.na(Longitude), -87.7464412599, Longitude)
           predict <- predict +  0.749675650567 * x
           x <- ifelse(is.na(Latitude), 41.8883271688, Latitude)
           predict <- predict +  2.32061897472 * x
           x <- ifelse(is.na(Census.Tract), 349027.529672, Census.Tract)
           predict <- predict +  -0.324649615977E-06 * x
           x <- ifelse(is.na(Full.Baths), 1.51437523037, Full.Baths)
           predict <- predict +  0.455171375055E-01 * x
           x <- ifelse(is.na(Age), 76.2594913380, Age)
           predict <- predict +  -0.421668791694E-02 * x
           x <- ifelse(is.na(Sale.Year), 2015.07316624, Sale.Year)
           predict <- predict +  0.100545505471 * x
           x <- ifelse(is.na(Sale.Month), 6.87338739403, Sale.Month)
           predict <- predict +  0.985659348248E-02 * x
           x <- ifelse(is.na(HoodPriceSqFt), 179.615904200, HoodPriceSqFt)
           predict <- predict +  0.159937549798E-02 * x
           x <- ifelse(is.na(PropClassPriceSqFt), 148.432069004, PropClassPriceSqFt)
           predict <- predict +  0.331204459468E-02 * x
           predict <- predict +  0.596060197052 * Percent.Ownership.NA
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         }
       } else {
         nodeid <- 29
         predict <- -166.283636639
         x <- ifelse(is.na(log.Land.Square.Feet), 7.54969611694, log.Land.Square.Feet)
         predict <- predict +  0.768061367960 * x
         x <- ifelse(is.na(Latitude), 41.8704500031, Latitude)
         predict <- predict +  4.07149233706 * x
         x <- ifelse(is.na(HoodPriceSqFt), 155.732984837, HoodPriceSqFt)
         predict <- predict +  0.680935889136E-02 * x
         predict <- min(max(predict, 9.21094019205),15.8169906933)
       }
     } else {
       catvalues <- c("10","16","17","19","23","24","25","27","29","33","38","73","74","75")
       if(Town.Code %in% catvalues){
         catvalues <- c("73","74")
         if(Town.Code %in% catvalues){
           nodeid <- 60
           predict <- 658.136215634
           x <- ifelse(is.na(log.Land.Square.Feet), 7.88233493836, log.Land.Square.Feet)
           predict <- predict +  0.461402107128 * x
           x <- ifelse(is.na(Rooms), 9.56976744186, Rooms)
           predict <- predict +  -0.191049391423E-01 * x
           x <- ifelse(is.na(Fireplaces), 0.545784883721, Fireplaces)
           predict <- predict +  0.523522715453E-01 * x
           x <- ifelse(is.na(log.Building.Square.Feet), 7.75738893135, log.Building.Square.Feet)
           predict <- predict +  0.547435455956 * x
           x <- ifelse(is.na(Number.of.Commercial.Units), 0.327034883721E-01, Number.of.Commercial.Units)
           predict <- predict +  -0.195818285995 * x
           x <- ifelse(is.na(Longitude), -87.6654335406, Longitude)
           predict <- predict +  4.59209908076 * x
           x <- ifelse(is.na(Latitude), 41.9443788765, Latitude)
           predict <- predict +  -8.68734786055 * x
           x <- ifelse(is.na(Full.Baths), 2.52470930233, Full.Baths)
           predict <- predict +  0.583490934880E-01 * x
           x <- ifelse(is.na(Age), 78.6845930233, Age)
           predict <- predict +  -0.120401278478E-02 * x
           x <- ifelse(is.na(Sale.Year), 2014.92514535, Sale.Year)
           predict <- predict +  0.567438630894E-01 * x
           x <- ifelse(is.na(PropClassPriceSqFt), 155.308295167, PropClassPriceSqFt)
           predict <- predict +  0.202000023055E-02 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           nodeid <- 61
           predict <- 24.2127393332
           x <- ifelse(is.na(log.Land.Square.Feet), 8.98647606686, log.Land.Square.Feet)
           predict <- predict +  0.135894354565 * x
           x <- ifelse(is.na(Bedrooms), 3.66988468758, Bedrooms)
           predict <- predict +  -0.410004083293E-01 * x
           x <- ifelse(is.na(Fireplaces), 0.615446500402, Fireplaces)
           predict <- predict +  0.137042202776 * x
           x <- ifelse(is.na(Half.Baths), 0.683963529096, Half.Baths)
           predict <- predict +  0.304996242362E-01 * x
           x <- ifelse(is.na(Garage.1.Area), 1.62563689997, Garage.1.Area)
           predict <- predict +  0.640415217295E-01 * x
           x <- ifelse(is.na(log.Building.Square.Feet), 7.53898585516, log.Building.Square.Feet)
           predict <- predict +  0.562710386492 * x
           x <- ifelse(is.na(Longitude), -87.8727007321, Longitude)
           predict <- predict +  1.31363593118 * x
           x <- ifelse(is.na(Latitude), 42.0497067413, Latitude)
           predict <- predict +  0.369925898683 * x
           x <- ifelse(is.na(Census.Tract), 779621.529364, Census.Tract)
           predict <- predict +  0.248758614300E-06 * x
           x <- ifelse(is.na(Full.Baths), 1.84714400644, Full.Baths)
           predict <- predict +  0.799843103885E-01 * x
           x <- ifelse(is.na(Sale.Year), 2015.04679539, Sale.Year)
           predict <- predict +  0.405950322049E-01 * x
           x <- ifelse(is.na(Sale.Month), 6.91338160365, Sale.Month)
           predict <- predict +  0.778475699651E-02 * x
           x <- ifelse(is.na(HoodPriceSqFt), 169.971104717, HoodPriceSqFt)
           predict <- predict +  0.683740069078E-03 * x
           x <- ifelse(is.na(PropClassPriceSqFt), 173.002583659, PropClassPriceSqFt)
           predict <- predict +  0.280006252842E-02 * x
           predict <- predict +  0.186538142202 * Total.Building.Square.Feet.NA
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         }
       } else {
         catvalues <- c("13","15","28","32")
         if(Town.Code %in% catvalues){
           catvalues <- c("41","45","88","180","201","222","257","314")
           if(Neighborhood.Code %in% catvalues){
             nodeid <- 124
             predict <- -434.491980467
             x <- ifelse(is.na(log.Land.Square.Feet), 8.97662391755, log.Land.Square.Feet)
             predict <- predict +  0.377313928770 * x
             x <- ifelse(is.na(log.Building.Square.Feet), 7.34087736026, log.Building.Square.Feet)
             predict <- predict +  0.654551379194 * x
             x <- ifelse(is.na(Longitude), -87.7421176848, Longitude)
             predict <- predict +  -3.63901988248 * x
             x <- ifelse(is.na(Census.Tract), 826453.183673, Census.Tract)
             predict <- predict +  -0.803068956964E-04 * x
             x <- ifelse(is.na(Age), 42.9183673469, Age)
             predict <- predict +  -0.886606657007E-02 * x
             x <- ifelse(is.na(Sale.Year), 2014.87755102, Sale.Year)
             predict <- predict +  0.920049396097E-01 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             catvalues <- c("60","110","120","226","240","260","340")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 250
               predict <- -1534.28841530
               x <- ifelse(is.na(Half.Baths), 0.480961923848, Half.Baths)
               predict <- predict +  0.174524546624 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.13976569036, log.Building.Square.Feet)
               predict <- predict +  0.445217700136 * x
               x <- ifelse(is.na(Longitude), -87.6978499121, Longitude)
               predict <- predict +  -15.9683148878 * x
               x <- ifelse(is.na(Age), 50.1563126253, Age)
               predict <- predict +  -0.102338168170E-01 * x
               x <- ifelse(is.na(Sale.Year), 2014.98396794, Sale.Year)
               predict <- predict +  0.704542458003E-01 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               nodeid <- 251
               predict <- -496.807347104
               x <- ifelse(is.na(log.Land.Square.Feet), 8.90532023982, log.Land.Square.Feet)
               predict <- predict +  0.110751945121 * x
               x <- ifelse(is.na(Rooms), 6.82020057307, Rooms)
               predict <- predict +  0.180124203477E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.439111747851, Fireplaces)
               predict <- predict +  0.105251966093 * x
               x <- ifelse(is.na(Half.Baths), 0.587034383954, Half.Baths)
               predict <- predict +  0.672210918705E-01 * x
               x <- ifelse(is.na(Garage.1.Area), 1.68230659026, Garage.1.Area)
               predict <- predict +  0.859471090871E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.39852569521, log.Building.Square.Feet)
               predict <- predict +  0.311646452447 * x
               x <- ifelse(is.na(Longitude), -87.7729400686, Longitude)
               predict <- predict +  -4.02831736060 * x
               x <- ifelse(is.na(Latitude), 41.6252408409, Latitude)
               predict <- predict +  0.767012959455 * x
               x <- ifelse(is.na(Full.Baths), 1.67836676218, Full.Baths)
               predict <- predict +  0.451125705736E-01 * x
               x <- ifelse(is.na(Age), 45.3391833811, Age)
               predict <- predict +  -0.602120983330E-02 * x
               x <- ifelse(is.na(Sale.Year), 2015.11138968, Sale.Year)
               predict <- predict +  0.591928016321E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.69878223496, Sale.Month)
               predict <- predict +  0.921242977987E-02 * x
               x <- ifelse(is.na(HoodPriceSqFt), 137.390330405, HoodPriceSqFt)
               predict <- predict +  0.110748621672E-02 * x
               x <- ifelse(is.na(PropClassPriceSqFt), 160.008893783, PropClassPriceSqFt)
               predict <- predict +  0.178617986656E-02 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             }
           }
         } else {
           catvalues <- c("26")
           if(Town.Code %in% catvalues){
             nodeid <- 126
             predict <- -1126.60675394
             x <- ifelse(is.na(log.Building.Square.Feet), 7.23584384614, log.Building.Square.Feet)
             predict <- predict +  0.509302058074 * x
             x <- ifelse(is.na(Longitude), -87.8184415056, Longitude)
             predict <- predict +  -7.18045617282 * x
             x <- ifelse(is.na(Latitude), 41.9698142091, Latitude)
             predict <- predict +  9.40365386508 * x
             x <- ifelse(is.na(Age), 56.4917491749, Age)
             predict <- predict +  -0.493823220845E-02 * x
             x <- ifelse(is.na(Sale.Year), 2015.04620462, Sale.Year)
             predict <- predict +  0.548266239791E-01 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             catvalues <- c("12","14","37","70")
             if(Town.Code %in% catvalues){
               catvalues <- c("10","22","23","24","30","31","32","40","41","53","54","63","84","86","88","101","102","120","142","162","163","164","170","181","191","200","210","230","250","280")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 508
                 predict <- -327.157638703
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.59150472571, log.Land.Square.Feet)
                 predict <- predict +  0.284368768951 * x
                 x <- ifelse(is.na(Fireplaces), 0.184556407448, Fireplaces)
                 predict <- predict +  0.155660222196 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.30253261434, log.Building.Square.Feet)
                 predict <- predict +  0.714369071107 * x
                 x <- ifelse(is.na(Latitude), 41.6412231918, Latitude)
                 predict <- predict +  2.82736067127 * x
                 x <- ifelse(is.na(Age), 69.9069003286, Age)
                 predict <- predict +  -0.621448054801E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.10898138, Sale.Year)
                 predict <- predict +  0.105504726028 * x
                 x <- ifelse(is.na(PropClassPriceSqFt), 146.913093139, PropClassPriceSqFt)
                 predict <- predict +  0.461901136308E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 509
                 predict <- -266.054293315
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.59093256690, log.Land.Square.Feet)
                 predict <- predict +  0.337810908138 * x
                 x <- ifelse(is.na(Fireplaces), 0.197709666554, Fireplaces)
                 predict <- predict +  0.177370648596 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.57157292018, Garage.1.Area)
                 predict <- predict +  0.804399236302E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.26908538119, log.Building.Square.Feet)
                 predict <- predict +  0.665073753759 * x
                 x <- ifelse(is.na(Latitude), 41.6575333373, Latitude)
                 predict <- predict +  1.80733846538 * x
                 x <- ifelse(is.na(Census.Tract), 680511.730886, Census.Tract)
                 predict <- predict +  -0.629468108730E-06 * x
                 x <- ifelse(is.na(Age), 66.7598518020, Age)
                 predict <- predict +  -0.790296687268E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.17985854, Sale.Year)
                 predict <- predict +  0.962861543567E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.81980464803, Sale.Month)
                 predict <- predict +  0.130359310662E-01 * x
                 x <- ifelse(is.na(HoodPriceSqFt), 132.294579600, HoodPriceSqFt)
                 predict <- predict +  0.116406320015E-02 * x
                 x <- ifelse(is.na(PropClassPriceSqFt), 150.133106326, PropClassPriceSqFt)
                 predict <- predict +  0.466340173324E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               catvalues <- c("14","19","23","31","33","36","42","62","70","72","87","92","93","113","115","116","121","122","130","132","133","140","141","143","150","151","166","174","211","260","270","290","310","330","410")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 510
                 predict <- -100.958597019
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.54707814854, log.Land.Square.Feet)
                 predict <- predict +  0.130270428448 * x
                 x <- ifelse(is.na(Rooms), 7.09817729909, Rooms)
                 predict <- predict +  0.258049351571E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.322700911350, Fireplaces)
                 predict <- predict +  0.171345430975 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.52402651201, Garage.1.Area)
                 predict <- predict +  0.568407046977E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.38871026820, log.Building.Square.Feet)
                 predict <- predict +  0.425147972138 * x
                 x <- ifelse(is.na(Longitude), -87.8295866876, Longitude)
                 predict <- predict +  1.49890213377 * x
                 x <- ifelse(is.na(Latitude), 41.8683921954, Latitude)
                 predict <- predict +  1.47121219071 * x
                 x <- ifelse(is.na(Age), 68.1193040597, Age)
                 predict <- predict +  -0.516895120715E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.08492129, Sale.Year)
                 predict <- predict +  0.877284895372E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.87241093621, Sale.Month)
                 predict <- predict +  0.104878341345E-01 * x
                 x <- ifelse(is.na(HoodPriceSqFt), 158.910465482, HoodPriceSqFt)
                 predict <- predict +  0.437803233872E-02 * x
                 x <- ifelse(is.na(PropClassPriceSqFt), 153.068742292, PropClassPriceSqFt)
                 predict <- predict +  0.346369808283E-02 * x
                 predict <- predict +  0.243064514771 * Total.Building.Square.Feet.NA
                 predict <- predict +  0.712684830899 * Percent.Ownership.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 511
                 predict <- -71.4015848373
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.58911796249, log.Land.Square.Feet)
                 predict <- predict +  0.147021185672 * x
                 x <- ifelse(is.na(Rooms), 6.95115925504, Rooms)
                 predict <- predict +  0.314061958289E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.46208665906, Bedrooms)
                 predict <- predict +  -0.794310685806E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.265773470163, Fireplaces)
                 predict <- predict +  0.170673895679 * x
                 x <- ifelse(is.na(Half.Baths), 0.435290763968, Half.Baths)
                 predict <- predict +  0.146250987569 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.57915241353, Garage.1.Area)
                 predict <- predict +  0.448416473101E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.34962072542, log.Building.Square.Feet)
                 predict <- predict +  0.390487730109 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.147282402128E-01, Number.of.Commercial.Units)
                 predict <- predict +  -0.149425370982 * x
                 x <- ifelse(is.na(Longitude), -87.8592690108, Longitude)
                 predict <- predict +  1.90565300484 * x
                 x <- ifelse(is.na(Latitude), 41.8892117735, Latitude)
                 predict <- predict +  2.11420820982 * x
                 x <- ifelse(is.na(Full.Baths), 1.64861269479, Full.Baths)
                 predict <- predict +  0.133788157744 * x
                 x <- ifelse(is.na(Age), 63.1342645382, Age)
                 predict <- predict +  -0.522761872062E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.04950589, Sale.Year)
                 predict <- predict +  0.778639789584E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.80653743824, Sale.Month)
                 predict <- predict +  0.184511545592E-01 * x
                 x <- ifelse(is.na(HoodPriceSqFt), 154.034862906, HoodPriceSqFt)
                 predict <- predict +  0.450095250833E-02 * x
                 x <- ifelse(is.na(PropClassPriceSqFt), 152.259332973, PropClassPriceSqFt)
                 predict <- predict +  0.359488755926E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
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
## If desired, replace "cook_county_test_v2.txt" with name of file containing new data
## New file must have at least the same variables with same names
## (but not necessarily the same order) as in the training data file
## Missing value code is converted to NA if not already NA
newdata <- read.table("cook_county_test_v2.txt",header=TRUE,colClasses="character")
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
    HoodPriceSqFt <- as.numeric(newdata$HoodPriceSqFt[i])
    PropClassPriceSqFt <- as.numeric(newdata$PropClassPriceSqFt[i])
    Total.Building.Square.Feet.NA <- ifelse(is.na(Total.Building.Square.Feet),1,0)
    Percent.Ownership.NA <- ifelse(is.na(Percent.Ownership),1,0)
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred <- c(pred,tmp[2])
}

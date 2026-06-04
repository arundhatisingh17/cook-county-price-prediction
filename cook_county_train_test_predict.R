## Function for predicting least-squares mean of log.Sale.Price
## Code produced by GUIDE 46.2 on 4/22/26 at 17:10
guide_predict <- function(){
 if(is.na(Number.of.Units)){
   if(is.na(Total.Building.Square.Feet)){
     if(is.na(Census.Tract)){
       nodeid <- 8
       predict <- -285.547961250
       x <- ifelse(is.na(log.Building.Square.Feet), 7.83609456556, log.Building.Square.Feet)
       predict <- predict +  0.283344656766 * x
       x <- ifelse(is.na(Sale.Year), 2014.75229358, Sale.Year)
       predict <- predict +  0.146167405762 * x
       predict <- predict +  1.77401875444 * Percent.Ownership.NA
       predict <- min(max(predict, 9.21094019205),15.8169906933)
     } else {
       catvalues <- c("11","12","13","14","15","31","32","36","37","39","70","72")
       if(Town.Code %in% catvalues){
         if(is.na(Percent.Ownership)){
           catvalues <- c("15","36","46","50","55","64","81","82","93","103","104","122","152","160","161","173","174","182","193","211","224","227","241","255","257","271","274","290","300","315","316","341","342","344","350","360","380","400","410","420","422","423","431","432")
           if(Neighborhood.Code %in% catvalues){
             nodeid <- 72
             predict <- -209.838939366
             x <- ifelse(is.na(log.Land.Square.Feet), 8.64630319408, log.Land.Square.Feet)
             predict <- predict +  0.845850121494E-01 * x
             x <- ifelse(is.na(Fireplaces), 0.232518232518, Fireplaces)
             predict <- predict +  0.149963194253 * x
             x <- ifelse(is.na(Garage.1.Area), 1.67824967825, Garage.1.Area)
             predict <- predict +  0.690822667752E-01 * x
             x <- ifelse(is.na(log.Building.Square.Feet), 7.21290038447, log.Building.Square.Feet)
             predict <- predict +  0.529347602219 * x
             x <- ifelse(is.na(Number.of.Commercial.Units), 0.557700557701E-02, Number.of.Commercial.Units)
             predict <- predict +  -0.576216865963 * x
             x <- ifelse(is.na(Latitude), 41.7308769348, Latitude)
             predict <- predict +  1.84876460645 * x
             x <- ifelse(is.na(Census.Tract), 742705.398112, Census.Tract)
             predict <- predict +  -0.107462609405E-05 * x
             x <- ifelse(is.na(Age), 58.8755898756, Age)
             predict <- predict +  -0.250952027964E-02 * x
             x <- ifelse(is.na(Sale.Year), 2015.11111111, Sale.Year)
             predict <- predict +  0.699776950531E-01 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             catvalues <- c("1.0")
             if(Most.Recent.Sale %in% catvalues){
               catvalues <- c("11","23","24","45","63","84","101","121","133","134","162","226","251","293")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 292
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
                 nodeid <- 293
                 predict <- -432.409737203
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.58320103647, log.Land.Square.Feet)
                 predict <- predict +  0.264896155190 * x
                 x <- ifelse(is.na(Fireplaces), 0.171753097751, Fireplaces)
                 predict <- predict +  0.149193824558 * x
                 x <- ifelse(is.na(Half.Baths), 0.358765488756, Half.Baths)
                 predict <- predict +  0.761802623058E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.63240018357, Garage.1.Area)
                 predict <- predict +  0.912346121056E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.24046225065, log.Building.Square.Feet)
                 predict <- predict +  0.503101821846 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.814593850390E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.255676809257 * x
                 x <- ifelse(is.na(Longitude), -87.6958768889, Longitude)
                 predict <- predict +  -1.36879005181 * x
                 x <- ifelse(is.na(Latitude), 41.7117622139, Latitude)
                 predict <- predict +  2.27454835335 * x
                 x <- ifelse(is.na(Census.Tract), 742935.503442, Census.Tract)
                 predict <- predict +  -0.421493062773E-06 * x
                 x <- ifelse(is.na(Age), 67.1759981643, Age)
                 predict <- predict +  -0.867088776101E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.16268931, Sale.Year)
                 predict <- predict +  0.111074539931 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               catvalues <- c("10","11","12","20","22","25","41","42","73","87","90","110","141","151","175","194","200","201","210","232","262")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 294
                 predict <- -87.5662605900
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.61929634762, log.Land.Square.Feet)
                 predict <- predict +  0.187313151690 * x
                 x <- ifelse(is.na(Fireplaces), 0.195822454308, Fireplaces)
                 predict <- predict +  0.233917230207 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.26128421314, log.Building.Square.Feet)
                 predict <- predict +  0.591943558966 * x
                 x <- ifelse(is.na(Latitude), 41.7427992612, Latitude)
                 predict <- predict +  2.23813803226 * x
                 x <- ifelse(is.na(Age), 65.4503916449, Age)
                 predict <- predict +  -0.437680948137E-02 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 295
                 predict <- -330.634651972
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.51469324453, log.Land.Square.Feet)
                 predict <- predict +  0.277928061175 * x
                 x <- ifelse(is.na(Fireplaces), 0.151186639320, Fireplaces)
                 predict <- predict +  0.110637718717 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.58570172868, Garage.1.Area)
                 predict <- predict +  0.599501392755E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.25791202615, log.Building.Square.Feet)
                 predict <- predict +  0.522031789631 * x
                 x <- ifelse(is.na(Longitude), -87.6675827185, Longitude)
                 predict <- predict +  -2.23337044910 * x
                 x <- ifelse(is.na(Latitude), 41.7073755076, Latitude)
                 predict <- predict +  1.82563213607 * x
                 x <- ifelse(is.na(Age), 71.7910928802, Age)
                 predict <- predict +  -0.763072565180E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2014.98095517, Sale.Year)
                 predict <- predict +  0.317471524378E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         } else {
           nodeid <- 37
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
             catvalues <- c("18","77")
             if(Town.Code %in% catvalues){
               catvalues <- c("13","20","40","92","101")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 304
                 predict <- -581.453387393
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.35856361113, log.Building.Square.Feet)
                 predict <- predict +  0.541017377067 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.264672036824E-01, Number.of.Commercial.Units)
                 predict <- predict +  -0.449956457434 * x
                 x <- ifelse(is.na(Longitude), -87.9340089785, Longitude)
                 predict <- predict +  2.65915686449 * x
                 x <- ifelse(is.na(Latitude), 41.9512633996, Latitude)
                 predict <- predict +  13.0728705632 * x
                 x <- ifelse(is.na(Age), 76.3567318757, Age)
                 predict <- predict +  -0.596159062071E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.06098964, Sale.Year)
                 predict <- predict +  0.136420783363 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 305
                 predict <- -452.415921242
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.15233288147, log.Land.Square.Feet)
                 predict <- predict +  0.206454508909 * x
                 x <- ifelse(is.na(Half.Baths), 0.228546409807, Half.Baths)
                 predict <- predict +  0.377271235174 * x
                 x <- ifelse(is.na(Longitude), -87.8071374733, Longitude)
                 predict <- predict +  5.60154888310 * x
                 x <- ifelse(is.na(Latitude), 41.8982259312, Latitude)
                 predict <- predict +  16.5995183345 * x
                 x <- ifelse(is.na(Full.Baths), 2.06217162872, Full.Baths)
                 predict <- predict +  0.161166273389 * x
                 x <- ifelse(is.na(Age), 89.2740805604, Age)
                 predict <- predict +  -0.332554486824E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.15148862, Sale.Year)
                 predict <- predict +  0.128289461239 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               catvalues <- c("104","105","107","108","109","210","250","270","280","520")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 306
                 predict <- -266.766712266
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.01318828516, log.Land.Square.Feet)
                 predict <- predict +  0.255459989284 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.29472344767, log.Building.Square.Feet)
                 predict <- predict +  0.314550136418 * x
                 x <- ifelse(is.na(Census.Tract), 442824.006329, Census.Tract)
                 predict <- predict +  -0.303873658531E-06 * x
                 x <- ifelse(is.na(Age), 69.0232067511, Age)
                 predict <- predict +  -0.602649916423E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2014.94514768, Sale.Year)
                 predict <- predict +  0.136393498054 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 307
                 predict <- -115.949745349
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.66072740874, log.Land.Square.Feet)
                 predict <- predict +  0.155851181949 * x
                 x <- ifelse(is.na(Fireplaces), 0.246687054027, Fireplaces)
                 predict <- predict +  0.113154215109 * x
                 x <- ifelse(is.na(Half.Baths), 0.444699286442, Half.Baths)
                 predict <- predict +  0.590249822215E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.66488277268, Garage.1.Area)
                 predict <- predict +  0.326306530154E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.31129052276, log.Building.Square.Feet)
                 predict <- predict +  0.406690924519 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.790010193680E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.177751950910 * x
                 x <- ifelse(is.na(Longitude), -87.8594282811, Longitude)
                 predict <- predict +  1.02132935505 * x
                 x <- ifelse(is.na(Latitude), 41.9204042202, Latitude)
                 predict <- predict +  0.907028626299 * x
                 x <- ifelse(is.na(Census.Tract), 614419.451835, Census.Tract)
                 predict <- predict +  -0.274109607595E-06 * x
                 x <- ifelse(is.na(Full.Baths), 1.54510703364, Full.Baths)
                 predict <- predict +  0.563073454747E-01 * x
                 x <- ifelse(is.na(Age), 61.3924566769, Age)
                 predict <- predict +  -0.482765799303E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.06600408, Sale.Year)
                 predict <- predict +  0.873044965346E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           } else {
             catvalues <- c("77")
             if(Town.Code %in% catvalues){
               catvalues <- c("51","52","120","152","170")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 308
                 predict <- -133.480803792
                 x <- ifelse(is.na(log.Land.Square.Feet), 7.77665357735, log.Land.Square.Feet)
                 predict <- predict +  0.408515973434 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.73308201261, log.Building.Square.Feet)
                 predict <- predict +  0.462432719007 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.610859728507E-01, Number.of.Commercial.Units)
                 predict <- predict +  -0.226040423614 * x
                 x <- ifelse(is.na(Longitude), -87.6744784722, Longitude)
                 predict <- predict +  6.91110124841 * x
                 x <- ifelse(is.na(Latitude), 41.9033022928, Latitude)
                 predict <- predict +  14.0900142956 * x
                 x <- ifelse(is.na(Age), 71.5067873303, Age)
                 predict <- predict +  -0.387634220553E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2014.92307692, Sale.Year)
                 predict <- predict +  0.774222020475E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 309
                 predict <- 804.316361299
                 x <- ifelse(is.na(log.Land.Square.Feet), 7.86635331701, log.Land.Square.Feet)
                 predict <- predict +  0.370338162402 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.63141840329, log.Building.Square.Feet)
                 predict <- predict +  0.374007012519 * x
                 x <- ifelse(is.na(Longitude), -87.6894664534, Longitude)
                 predict <- predict +  13.5911997879 * x
                 x <- ifelse(is.na(Latitude), 41.9091058105, Latitude)
                 predict <- predict +  5.47532797581 * x
                 x <- ifelse(is.na(Age), 83.6245059289, Age)
                 predict <- predict +  -0.434190990333E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2014.88932806, Sale.Year)
                 predict <- predict +  0.821819230901E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               catvalues <- c("18","21","35","47","63","72","95","96","110","112","113","114","117","142","143","151","420","461","463","580","600")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 310
                 predict <- -3.20838358680
                 x <- ifelse(is.na(Fireplaces), 0.635802469136, Fireplaces)
                 predict <- predict +  0.119418214076 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.62525968687, log.Building.Square.Feet)
                 predict <- predict +  0.492316079077 * x
                 x <- ifelse(is.na(Longitude), -87.8645541379, Longitude)
                 predict <- predict +  1.41018874821 * x
                 x <- ifelse(is.na(Latitude), 41.9806548523, Latitude)
                 predict <- predict +  1.32557418786 * x
                 x <- ifelse(is.na(Sale.Year), 2015.03292181, Sale.Year)
                 predict <- predict +  0.400137752118E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 311
                 predict <- -37.6122464008
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.66234578110, log.Land.Square.Feet)
                 predict <- predict +  0.199470013037 * x
                 x <- ifelse(is.na(Bedrooms), 3.39602066195, Bedrooms)
                 predict <- predict +  -0.469190703242E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.338817677444, Fireplaces)
                 predict <- predict +  0.106521171934 * x
                 x <- ifelse(is.na(Half.Baths), 0.520374976086, Half.Baths)
                 predict <- predict +  0.401314632239E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.39204321185, log.Building.Square.Feet)
                 predict <- predict +  0.497058081180 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.726994451884E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.346566592174 * x
                 x <- ifelse(is.na(Longitude), -87.8708435538, Longitude)
                 predict <- predict +  1.66137548623 * x
                 x <- ifelse(is.na(Latitude), 41.9472396816, Latitude)
                 predict <- predict +  1.13775263439 * x
                 x <- ifelse(is.na(Census.Tract), 536489.697532, Census.Tract)
                 predict <- predict +  -0.149422229373E-06 * x
                 x <- ifelse(is.na(Full.Baths), 1.64224220394, Full.Baths)
                 predict <- predict +  0.594790265546E-01 * x
                 x <- ifelse(is.na(Age), 57.8777501435, Age)
                 predict <- predict +  -0.332989269398E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.08303042, Sale.Year)
                 predict <- predict +  0.711082575263E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         } else {
           catvalues <- c("24","38")
           if(Town.Code %in% catvalues){
             nodeid <- 78
             predict <- -149.442200930
             x <- ifelse(is.na(log.Land.Square.Feet), 8.86871354864, log.Land.Square.Feet)
             predict <- predict +  0.197512579002 * x
             x <- ifelse(is.na(Fireplaces), 0.450442851118, Fireplaces)
             predict <- predict +  0.100223661213 * x
             x <- ifelse(is.na(Garage.1.Area), 1.62842682412, Garage.1.Area)
             predict <- predict +  0.399689621340E-01 * x
             x <- ifelse(is.na(log.Building.Square.Feet), 7.37897705240, log.Building.Square.Feet)
             predict <- predict +  0.519296609308 * x
             x <- ifelse(is.na(Longitude), -87.8739838732, Longitude)
             predict <- predict +  -1.34478652585 * x
             x <- ifelse(is.na(Latitude), 42.0764498131, Latitude)
             predict <- predict +  -2.93347594885 * x
             x <- ifelse(is.na(Census.Tract), 804808.319275, Census.Tract)
             predict <- predict +  0.438784384455E-04 * x
             x <- ifelse(is.na(Age), 52.0189793336, Age)
             predict <- predict +  -0.390182296111E-02 * x
             x <- ifelse(is.na(Sale.Year), 2015.04048925, Sale.Year)
             predict <- predict +  0.627751761665E-01 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             catvalues <- c("21","23","25")
             if(Town.Code %in% catvalues){
               catvalues <- c("43","44","45","50","53","62","80","92","93","94","110","141","170","171","180","190","200","210","220","240","300","320","420")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 316
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
                 nodeid <- 317
                 predict <- -144.377271570
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.05943734243, log.Land.Square.Feet)
                 predict <- predict +  0.148177573907 * x
                 x <- ifelse(is.na(Rooms), 7.28632276384, Rooms)
                 predict <- predict +  -0.294220399031E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.703265499290, Fireplaces)
                 predict <- predict +  0.222913053697 * x
                 x <- ifelse(is.na(Half.Baths), 0.676289635589, Half.Baths)
                 predict <- predict +  0.948676355315E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.56471049904, log.Building.Square.Feet)
                 predict <- predict +  0.756363273407 * x
                 x <- ifelse(is.na(Longitude), -87.8310046578, Longitude)
                 predict <- predict +  -1.23609807114 * x
                 x <- ifelse(is.na(Census.Tract), 810005.525319, Census.Tract)
                 predict <- predict +  -0.247330744721E-04 * x
                 x <- ifelse(is.na(Full.Baths), 1.91292001893, Full.Baths)
                 predict <- predict +  0.764905096144E-01 * x
                 x <- ifelse(is.na(Sale.Year), 2015.01703739, Sale.Year)
                 predict <- predict +  0.305327002925E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               catvalues <- c("33","73","74")
               if(Town.Code %in% catvalues){
                 nodeid <- 318
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
                 nodeid <- 319
                 predict <- -15.7022704236
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.77415321045, log.Land.Square.Feet)
                 predict <- predict +  0.134092866903 * x
                 x <- ifelse(is.na(Bedrooms), 3.74695493301, Bedrooms)
                 predict <- predict +  -0.686604439964E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.526796589525, Fireplaces)
                 predict <- predict +  0.162407097733 * x
                 x <- ifelse(is.na(Half.Baths), 0.592570036541, Half.Baths)
                 predict <- predict +  0.889551753522E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.54902557856, Garage.1.Area)
                 predict <- predict +  0.737996939249E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.54887882133, log.Building.Square.Feet)
                 predict <- predict +  0.689914453764 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.700365408039E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.569593903732 * x
                 x <- ifelse(is.na(Longitude), -87.8656431221, Longitude)
                 predict <- predict +  1.23133911260 * x
                 x <- ifelse(is.na(Latitude), 41.9732126126, Latitude)
                 predict <- predict +  0.606553955575 * x
                 x <- ifelse(is.na(Census.Tract), 731217.102010, Census.Tract)
                 predict <- predict +  0.198145646207E-06 * x
                 x <- ifelse(is.na(Full.Baths), 1.87515225335, Full.Baths)
                 predict <- predict +  0.437646603664E-01 * x
                 x <- ifelse(is.na(Age), 64.1071863581, Age)
                 predict <- predict +  -0.122838745375E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.06729598, Sale.Year)
                 predict <- predict +  0.519215554012E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         }
       }
     }
   } else {
     if(is.na(Census.Tract)){
       nodeid <- 10
       predict <- -365.413569658
       x <- ifelse(is.na(Rooms), 6.57583547558, Rooms)
       predict <- predict +  0.791550989007E-01 * x
       x <- ifelse(is.na(Fireplaces), 0.251928020566, Fireplaces)
       predict <- predict +  0.399612581285 * x
       x <- ifelse(is.na(Half.Baths), 0.789203084833, Half.Baths)
       predict <- predict +  0.230750988776 * x
       x <- ifelse(is.na(log.Building.Square.Feet), 7.56566516936, log.Building.Square.Feet)
       predict <- predict +  0.725799159957 * x
       x <- ifelse(is.na(Total.Building.Square.Feet), 16461.2904884, Total.Building.Square.Feet)
       predict <- predict +  -0.412888637710E-04 * x
       x <- ifelse(is.na(Age), 8.64524421594, Age)
       predict <- predict +  0.678148034882E-02 * x
       x <- ifelse(is.na(Sale.Year), 2014.98714653, Sale.Year)
       predict <- predict +  0.184032736683 * x
       predict <- min(max(predict, 9.21094019205),15.8169906933)
     } else {
       catvalues <- c("14","18","20","36","37","39","72")
       if(Town.Code %in% catvalues){
         nodeid <- 22
         predict <- 11.6784242828
         x <- ifelse(is.na(Age), 92.2962962963, Age)
         predict <- predict +  -0.126434071040E-01 * x
         predict <- min(max(predict, 9.21094019205),15.8169906933)
       } else {
         catvalues <- c("11","12","21","29","30","73","77")
         if(Town.Code %in% catvalues){
           catvalues <- c("51","52","93","170")
           if(Neighborhood.Code %in% catvalues){
             nodeid <- 92
             predict <- -198.465095445
             x <- ifelse(is.na(log.Building.Square.Feet), 7.51505628178, log.Building.Square.Feet)
             predict <- predict +  0.911375275731 * x
             x <- ifelse(is.na(Number.of.Commercial.Units), 0.476190476190E-01, Number.of.Commercial.Units)
             predict <- predict +  -0.787097421994 * x
             x <- ifelse(is.na(Sale.Year), 2015.28571429, Sale.Year)
             predict <- predict +  0.101512828753 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             catvalues <- c("12","70","81","84","120","150")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 186
               predict <- 705.669628239
               x <- ifelse(is.na(log.Building.Square.Feet), 7.34426724995, log.Building.Square.Feet)
               predict <- predict +  0.932658079963 * x
               x <- ifelse(is.na(Number.of.Commercial.Units), 0.465116279070E-01, Number.of.Commercial.Units)
               predict <- predict +  -0.803469393146 * x
               x <- ifelse(is.na(Longitude), -87.6801348484, Longitude)
               predict <- predict +  10.0076726396 * x
               x <- ifelse(is.na(Census.Tract), 189402.372093, Census.Tract)
               predict <- predict +  0.431721207241E-06 * x
               x <- ifelse(is.na(Sale.Year), 2014.81395349, Sale.Year)
               predict <- predict +  0.881521835366E-01 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               catvalues <- c("11","40","50","85","102","115")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 374
                 predict <- -671.020877296
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.27178816150, log.Building.Square.Feet)
                 predict <- predict +  0.713955697645 * x
                 x <- ifelse(is.na(Longitude), -87.7355354004, Longitude)
                 predict <- predict +  -7.71091710879 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 375
                 predict <- -262.626195590
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.42878445195, log.Building.Square.Feet)
                 predict <- predict +  0.877779430277 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.886075949367E-01, Number.of.Commercial.Units)
                 predict <- predict +  -0.845631277283 * x
                 x <- ifelse(is.na(Census.Tract), 471882.822785, Census.Tract)
                 predict <- predict +  -0.820083702448E-06 * x
                 x <- ifelse(is.na(Sale.Year), 2014.96202532, Sale.Year)
                 predict <- predict +  0.133257538259 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         } else {
           catvalues <- c("17","27","71")
           if(Town.Code %in% catvalues){
             nodeid <- 94
             predict <- -611.266385876
             x <- ifelse(is.na(log.Land.Square.Feet), 7.87942425704, log.Land.Square.Feet)
             predict <- predict +  1.11997828783 * x
             x <- ifelse(is.na(Sale.Year), 2015.37500000, Sale.Year)
             predict <- predict +  0.304904014315 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             catvalues <- c("15","22","70","76")
             if(Town.Code %in% catvalues){
               nodeid <- 190
               predict <- 52.6940145957
               x <- ifelse(is.na(log.Building.Square.Feet), 7.28165694894, log.Building.Square.Feet)
               predict <- predict +  0.947735458941 * x
               x <- ifelse(is.na(Longitude), -87.6425116680, Longitude)
               predict <- predict +  6.58718743142 * x
               x <- ifelse(is.na(Latitude), 41.8128752852, Latitude)
               predict <- predict +  12.6503975034 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               nodeid <- 191
               predict <- 5.91237965508
               x <- ifelse(is.na(log.Building.Square.Feet), 7.36252910660, log.Building.Square.Feet)
               predict <- predict +  1.05509124453 * x
               x <- ifelse(is.na(Census.Tract), 593668.638298, Census.Tract)
               predict <- predict +  -0.172950415536E-05 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             }
           }
         }
       }
     }
   }
 } else {
   if(is.na(Census.Tract)){
     nodeid <- 6
     predict <- -290.218909067
     x <- ifelse(is.na(log.Land.Square.Feet), 9.70516067951, log.Land.Square.Feet)
     predict <- predict +  -0.130890226381 * x
     x <- ifelse(is.na(Sale.Year), 2015.10937500, Sale.Year)
     predict <- predict +  0.150587208788 * x
     predict <- min(max(predict, 9.21094019205),15.8169906933)
   } else {
     catvalues <- c("12","13","19","20","23","30","34","36","37","74")
     if(Town.Code %in% catvalues){
       catvalues <- c("11","12","13","22","30","43","91","92","93","120","150","171")
       if(Neighborhood.Code %in% catvalues){
         catvalues <- c("11","12","13","91")
         if(Neighborhood.Code %in% catvalues){
           nodeid <- 56
           predict <- 501.475740445
           x <- ifelse(is.na(log.Land.Square.Feet), 9.77971774176, log.Land.Square.Feet)
           predict <- predict +  0.212201288349 * x
           x <- ifelse(is.na(Longitude), -87.6557074603, Longitude)
           predict <- predict +  6.84151479745 * x
           x <- ifelse(is.na(Latitude), 41.9114544788, Latitude)
           predict <- predict +  2.60142453743 * x
           x <- ifelse(is.na(Age), 51.0798668885, Age)
           predict <- predict +  -0.228460307475E-02 * x
           x <- ifelse(is.na(Number.of.Units), 107.890183028, Number.of.Units)
           predict <- predict +  -0.204307979320E-02 * x
           x <- ifelse(is.na(Percent.Ownership), 0.831946755408E-01, Percent.Ownership)
           predict <- predict +  2.47128716468 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           if(!is.na(Census.Tract) & Census.Tract <= 806350.500000 ){
             if(!is.na(Census.Tract) & Census.Tract <= 81050.0000000 ){
               if(!is.na(Number.of.Units) & Number.of.Units <= 211.000000000 ){
                 nodeid <- 456
                 predict <- 7145.00071992
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.46369995367, log.Land.Square.Feet)
                 predict <- predict +  0.547945736614 * x
                 x <- ifelse(is.na(Longitude), -87.6294372905, Longitude)
                 predict <- predict +  81.4460344959 * x
                 x <- ifelse(is.na(Number.of.Units), 89.1588235294, Number.of.Units)
                 predict <- predict +  -0.513517270076E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.270588235294E-01, Percent.Ownership)
                 predict <- predict +  4.91061666192 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 457
                 predict <- 10.4664621253
                 x <- ifelse(is.na(log.Land.Square.Feet), 11.0268547630, log.Land.Square.Feet)
                 predict <- predict +  0.150024024627 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.994764397906E-03, Percent.Ownership)
                 predict <- predict +  55.0407970629 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               nodeid <- 229
               predict <- 712.699255832
               x <- ifelse(is.na(log.Land.Square.Feet), 10.3521273016, log.Land.Square.Feet)
               predict <- predict +  0.261168364502 * x
               x <- ifelse(is.na(Longitude), -87.6320237523, Longitude)
               predict <- predict +  8.01467281104 * x
               x <- ifelse(is.na(Age), 38.7208888889, Age)
               predict <- predict +  -0.320795066991E-02 * x
               x <- ifelse(is.na(Number.of.Units), 388.002666667, Number.of.Units)
               predict <- predict +  -0.941688692008E-03 * x
               x <- ifelse(is.na(Percent.Ownership), 0.608000000000E-02, Percent.Ownership)
               predict <- predict +  6.55104462788 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             }
           } else {
             nodeid <- 115
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
           nodeid <- 58
           predict <- -385.434856032
           x <- ifelse(is.na(log.Land.Square.Feet), 11.8027011413, log.Land.Square.Feet)
           predict <- predict +  0.177413965981 * x
           x <- ifelse(is.na(Longitude), -87.7973923314, Longitude)
           predict <- predict +  -2.18471064584 * x
           x <- ifelse(is.na(Latitude), 41.6925963844, Latitude)
           predict <- predict +  1.31684891052 * x
           x <- ifelse(is.na(Age), 30.4782608696, Age)
           predict <- predict +  -0.104580589551E-01 * x
           x <- ifelse(is.na(Percent.Ownership), 0.268379446640E-01, Percent.Ownership)
           predict <- predict +  3.29329390080 * x
           x <- ifelse(is.na(Sale.Year), 2015.09090909, Sale.Year)
           predict <- predict +  0.737022463042E-01 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           nodeid <- 59
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
           nodeid <- 60
           predict <- -393.470594885
           x <- ifelse(is.na(log.Land.Square.Feet), 11.8536580929, log.Land.Square.Feet)
           predict <- predict +  0.964763919113E-01 * x
           x <- ifelse(is.na(Latitude), 41.8687980381, Latitude)
           predict <- predict +  2.57236605016 * x
           x <- ifelse(is.na(Census.Tract), 814467.128803, Census.Tract)
           predict <- predict +  0.343404865422E-04 * x
           x <- ifelse(is.na(Age), 34.7596348884, Age)
           predict <- predict +  -0.171058294884E-01 * x
           x <- ifelse(is.na(Number.of.Units), 155.992900609, Number.of.Units)
           predict <- predict +  -0.119166196349E-02 * x
           x <- ifelse(is.na(Sale.Year), 2015.06389452, Sale.Year)
           predict <- predict +  0.133255686508 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           nodeid <- 61
           predict <- -90.5149433909
           x <- ifelse(is.na(log.Land.Square.Feet), 12.9275831650, log.Land.Square.Feet)
           predict <- predict +  0.104912423165 * x
           x <- ifelse(is.na(Longitude), -88.0171428068, Longitude)
           predict <- predict +  -0.265227615368 * x
           x <- ifelse(is.na(Latitude), 41.9964336108, Latitude)
           predict <- predict +  -2.32214339007 * x
           x <- ifelse(is.na(Census.Tract), 807721.676856, Census.Tract)
           predict <- predict +  -0.608142612609E-04 * x
           x <- ifelse(is.na(Age), 25.8329694323, Age)
           predict <- predict +  -0.228048176492E-01 * x
           x <- ifelse(is.na(Number.of.Units), 231.242358079, Number.of.Units)
           predict <- predict +  -0.103328964309E-02 * x
           x <- ifelse(is.na(Sale.Year), 2015.00436681, Sale.Year)
           predict <- predict +  0.111694606237 * x
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         }
       } else {
         catvalues <- c("16","28","70","75")
         if(Town.Code %in% catvalues){
           catvalues <- c("22","23","31","32","45","60","88","103","120","121","140","180")
           if(Neighborhood.Code %in% catvalues){
             nodeid <- 124
             predict <- -761.393788781
             x <- ifelse(is.na(log.Land.Square.Feet), 9.85468894173, log.Land.Square.Feet)
             predict <- predict +  0.202263061397 * x
             x <- ifelse(is.na(Longitude), -87.7018740929, Longitude)
             predict <- predict +  -4.20968287581 * x
             x <- ifelse(is.na(Latitude), 41.9356773702, Latitude)
             predict <- predict +  2.35429141879 * x
             x <- ifelse(is.na(Percent.Ownership), 0.748625792812E-01, Percent.Ownership)
             predict <- predict +  3.22054933295 * x
             x <- ifelse(is.na(Sale.Year), 2015.10359408, Sale.Year)
             predict <- predict +  0.150267395829 * x
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             catvalues <- c("12","50","51","62","80","83","85","111","150")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 250
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
                 nodeid <- 502
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
                 nodeid <- 503
                 predict <- -175.786908449
                 x <- ifelse(is.na(log.Land.Square.Feet), 11.3783281805, log.Land.Square.Feet)
                 predict <- predict +  0.104922001578 * x
                 x <- ifelse(is.na(Latitude), 41.8957624805, Latitude)
                 predict <- predict +  -1.02994956272 * x
                 x <- ifelse(is.na(Number.of.Units), 153.111635220, Number.of.Units)
                 predict <- predict +  -0.542230471841E-03 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.306918238994E-01, Percent.Ownership)
                 predict <- predict +  4.78742321435 * x
                 x <- ifelse(is.na(Sale.Year), 2015.02358491, Sale.Year)
                 predict <- predict +  0.113836213090 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         } else {
           if(!is.na(Age) & Age <= 27.5000000000 ){
             catvalues <- c("12","24","30","32","42","44","51","52","53","60","62","63","70","71","80","81","84","92","93","110","120","131","132","150","151","152","160","170","200")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 252
               predict <- 96.6121395956
               x <- ifelse(is.na(log.Land.Square.Feet), 9.30283946466, log.Land.Square.Feet)
               predict <- predict +  0.222103526840 * x
               x <- ifelse(is.na(Longitude), -87.6974964891, Longitude)
               predict <- predict +  2.11864802753 * x
               x <- ifelse(is.na(Census.Tract), 338699.205227, Census.Tract)
               predict <- predict +  -0.259637157408E-06 * x
               x <- ifelse(is.na(Age), 11.7964867181, Age)
               predict <- predict +  -0.789171653689E-02 * x
               x <- ifelse(is.na(Number.of.Units), 42.5638389032, Number.of.Units)
               predict <- predict +  -0.141780713646E-02 * x
               x <- ifelse(is.na(Percent.Ownership), 0.159035989717, Percent.Ownership)
               predict <- predict +  1.98894454549 * x
               x <- ifelse(is.na(Sale.Year), 2015.02399314, Sale.Year)
               predict <- predict +  0.495480716147E-01 * x
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               catvalues <- c("10","11","14","20","22","31","35","40","41","43","50","56","61","72","74","82","90","91","111","141","174","210","430")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 506
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
                 nodeid <- 507
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
               nodeid <- 254
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
                 nodeid <- 510
                 predict <- -109.899825144
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.7737533551, log.Land.Square.Feet)
                 predict <- predict +  -0.223479116183 * x
                 x <- ifelse(is.na(Longitude), -87.7766704503, Longitude)
                 predict <- predict +  1.93880030145 * x
                 x <- ifelse(is.na(Latitude), 41.9622294447, Latitude)
                 predict <- predict +  2.25511089825 * x
                 x <- ifelse(is.na(Number.of.Units), 201.302021403, Number.of.Units)
                 predict <- predict +  0.634073380292E-03 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.349346016647E-01, Percent.Ownership)
                 predict <- predict +  -1.47785168889 * x
                 x <- ifelse(is.na(Sale.Year), 2015.11058264, Sale.Year)
                 predict <- predict +  0.991008158871E-01 * x
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 511
                 predict <- 97.6141529442
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.1906646800, log.Land.Square.Feet)
                 predict <- predict +  0.177734275010 * x
                 x <- ifelse(is.na(Longitude), -87.7281771899, Longitude)
                 predict <- predict +  2.78960710443 * x
                 x <- ifelse(is.na(Age), 67.3604320637, Age)
                 predict <- predict +  0.453526092901E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 145.723990904, Number.of.Units)
                 predict <- predict +  -0.611199677627E-03 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.598720864127E-01, Percent.Ownership)
                 predict <- predict +  2.62071630150 * x
                 x <- ifelse(is.na(Sale.Year), 2015.10886868, Sale.Year)
                 predict <- predict +  0.778752766287E-01 * x
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
## If desired, replace "cook_county_test.txt" with name of file containing new data
## New file must have at least the same variables with same names
## (but not necessarily the same order) as in the training data file
## Missing value code is converted to NA if not already NA
newdata <- read.table("cook_county_test.txt",header=TRUE,colClasses="character")
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
    Percent.Ownership.NA <- ifelse(is.na(Percent.Ownership),1,0)
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred <- c(pred,tmp[2])
}

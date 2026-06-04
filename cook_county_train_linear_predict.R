## Function for predicting least-squares mean of log.Sale.Price
## Code produced by GUIDE 46.2 on 4/23/26 at 14:57
guide_predict <- function(){
 catvalues <- c("16","22","23","25","29","30","33","38","71","72","76")
 if(Town.Code %in% catvalues){
   catvalues <- c("21","44","45","48","63","84","87","93","94","95","132","141","142","190","220","240","320","400","420","461","463","580")
   if(Neighborhood.Code %in% catvalues){
     nodeid <- 4
     predict <- 137.835158299
     x <- ifelse(is.na(log.Land.Square.Feet), 9.20329789205, log.Land.Square.Feet)
     predict <- predict +  0.133730553339 * x
     x <- ifelse(is.na(Rooms), 7.65055248619, Rooms)
     predict <- predict +  0.145441077181E-01 * x
     x <- ifelse(is.na(Bedrooms), 3.84944751381, Bedrooms)
     predict <- predict +  -0.165401166266E-01 * x
     x <- ifelse(is.na(Fireplaces), 0.872928176796, Fireplaces)
     predict <- predict +  0.972596017053E-01 * x
     x <- ifelse(is.na(Half.Baths), 0.751381215470, Half.Baths)
     predict <- predict +  0.279768898198E-01 * x
     x <- ifelse(is.na(Garage.1.Area), 1.74861878453, Garage.1.Area)
     predict <- predict +  -0.148449896688E-02 * x
     x <- ifelse(is.na(Garage.2.Area), 0.124309392265E-01, Garage.2.Area)
     predict <- predict +  -0.127624248258 * x
     x <- ifelse(is.na(log.Building.Square.Feet), 7.70021829617, log.Building.Square.Feet)
     predict <- predict +  0.532924567235 * x
     x <- ifelse(is.na(Number.of.Commercial.Units), 0.138121546961E-02, Number.of.Commercial.Units)
     predict <- predict +  -0.224500618692 * x
     x <- ifelse(is.na(Longitude), -87.8049316546, Longitude)
     predict <- predict +  2.74053978860 * x
     x <- ifelse(is.na(Latitude), 42.0698437106, Latitude)
     predict <- predict +  1.33066907670 * x
     x <- ifelse(is.na(Census.Tract), 666777.372396, Census.Tract)
     predict <- predict +  -0.233592600275E-07 * x
     x <- ifelse(is.na(Full.Baths), 1.98895027624, Full.Baths)
     predict <- predict +  0.389923840257E-01 * x
     x <- ifelse(is.na(Age), 57.0661478599, Age)
     predict <- predict +  -0.315818894079E-03 * x
     x <- ifelse(is.na(Number.of.Units), 32.8723404255, Number.of.Units)
     predict <- predict +  -0.102654461935E-01 * x
     x <- ifelse(is.na(Percent.Ownership), 0.468085106383E-01, Percent.Ownership)
     predict <- predict +  -0.160831844144 * x
     x <- ifelse(is.na(Sale.Year), 2014.98054475, Sale.Year)
     predict <- predict +  0.271704991736E-01 * x
     x <- ifelse(is.na(Sale.Month), 7.00259403372, Sale.Month)
     predict <- predict +  0.799388310527E-02 * x
     x <- ifelse(is.na(HoodSqFt), 2169.69092367, HoodSqFt)
     predict <- predict +  0.468525243247E-04 * x
     x <- ifelse(is.na(PropClassSqFt), 2253.47292522, PropClassSqFt)
     predict <- predict +  0.146929787878E-04 * x
     predict <- predict +  -1.30822955898 * Rooms.NA
     predict <- predict +  0.613811714679 * Longitude.NA
     predict <- min(max(predict, 9.21094019205),15.8169906933)
   } else {
     catvalues <- c("14","15","30","31","33","43","50","53","74","82","92","100","101","110","111","131","140","143","150","180","194","210","222","251","270","271","274","280","282","293","300","310","312","330","345","361","362","371","410","422","423","431","432","440")
     if(Neighborhood.Code %in% catvalues){
       catvalues <- c("74","111","131","282","293","300")
       if(Neighborhood.Code %in% catvalues){
         catvalues <- c("0.0")
         if(Most.Recent.Sale %in% catvalues){
           nodeid <- 40
           predict <- -645.770870951
           x <- ifelse(is.na(log.Land.Square.Feet), 8.71358895229, log.Land.Square.Feet)
           predict <- predict +  0.299228508404 * x
           x <- ifelse(is.na(Rooms), 6.11309523810, Rooms)
           predict <- predict +  0.270996805411E-01 * x
           x <- ifelse(is.na(Bedrooms), 3.16666666667, Bedrooms)
           predict <- predict +  -0.306353947629E-01 * x
           x <- ifelse(is.na(Fireplaces), 0.238095238095, Fireplaces)
           predict <- predict +  0.184072761415E-01 * x
           x <- ifelse(is.na(Half.Baths), 0.339285714286, Half.Baths)
           predict <- predict +  0.142575273183E-01 * x
           x <- ifelse(is.na(Garage.1.Area), 1.54761904762, Garage.1.Area)
           predict <- predict +  0.201167192344E-01 * x
           x <- ifelse(is.na(log.Building.Square.Feet), 7.22592895882, log.Building.Square.Feet)
           predict <- predict +  0.666055027311 * x
           x <- ifelse(is.na(Longitude), -87.7052504713, Longitude)
           predict <- predict +  -6.40777155539 * x
           x <- ifelse(is.na(Latitude), 41.8367280077, Latitude)
           predict <- predict +  0.926741162600 * x
           x <- ifelse(is.na(Census.Tract), 634790.791878, Census.Tract)
           predict <- predict +  -0.931160573125E-06 * x
           x <- ifelse(is.na(Full.Baths), 1.40476190476, Full.Baths)
           predict <- predict +  0.433537435607E-01 * x
           x <- ifelse(is.na(Age), 65.9849246231, Age)
           predict <- predict +  -0.335728135749E-02 * x
           x <- ifelse(is.na(Number.of.Units), 33.1935483871, Number.of.Units)
           predict <- predict +  0.438663437507E-02 * x
           x <- ifelse(is.na(Percent.Ownership), 0.738709677419E-01, Percent.Ownership)
           predict <- predict +  5.16703417633 * x
           x <- ifelse(is.na(Sale.Year), 2014.99497487, Sale.Year)
           predict <- predict +  0.240146712269E-01 * x
           x <- ifelse(is.na(Sale.Month), 6.31658291457, Sale.Month)
           predict <- predict +  0.646603745855E-02 * x
           x <- ifelse(is.na(HoodSqFt), 1511.16653670, HoodSqFt)
           predict <- predict +  0.686309455932E-03 * x
           x <- ifelse(is.na(PropClassSqFt), 1576.18782646, PropClassSqFt)
           predict <- predict +  -0.105233247106E-03 * x
           predict <- predict +  -1.35784761211 * Rooms.NA
           predict <- predict +  0.104623717288 * Longitude.NA
           predict <- predict +  0.110345777461 * Total.Building.Square.Feet.NA
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           nodeid <- 41
           predict <- -762.739432748
           x <- ifelse(is.na(log.Land.Square.Feet), 8.75306817999, log.Land.Square.Feet)
           predict <- predict +  0.399896743650 * x
           x <- ifelse(is.na(Rooms), 6.17694369973, Rooms)
           predict <- predict +  -0.348170780873E-01 * x
           x <- ifelse(is.na(Bedrooms), 3.23056300268, Bedrooms)
           predict <- predict +  0.153190514010 * x
           x <- ifelse(is.na(Fireplaces), 0.292225201072, Fireplaces)
           predict <- predict +  -0.380702603388E-01 * x
           x <- ifelse(is.na(Half.Baths), 0.383378016086, Half.Baths)
           predict <- predict +  -0.287192047674E-01 * x
           x <- ifelse(is.na(Garage.1.Area), 1.53887399464, Garage.1.Area)
           predict <- predict +  0.159294024259 * x
           x <- ifelse(is.na(log.Building.Square.Feet), 7.26060085713, log.Building.Square.Feet)
           predict <- predict +  0.621002029180 * x
           x <- ifelse(is.na(Longitude), -87.7272785452, Longitude)
           predict <- predict +  -6.62169688490 * x
           x <- ifelse(is.na(Latitude), 41.8722588205, Latitude)
           predict <- predict +  -1.14653426610 * x
           x <- ifelse(is.na(Census.Tract), 641359.188073, Census.Tract)
           predict <- predict +  -0.119009483448E-05 * x
           x <- ifelse(is.na(Full.Baths), 1.40214477212, Full.Baths)
           predict <- predict +  -0.480148770851E-01 * x
           x <- ifelse(is.na(Age), 65.8063781321, Age)
           predict <- predict +  -0.678712855595E-02 * x
           x <- ifelse(is.na(Number.of.Units), 41.6666666667, Number.of.Units)
           predict <- predict +  0.233834455533E-02 * x
           x <- ifelse(is.na(Percent.Ownership), 0.613636363636E-01, Percent.Ownership)
           predict <- predict +  6.83297437638 * x
           x <- ifelse(is.na(Sale.Year), 2015.15717540, Sale.Year)
           predict <- predict +  0.116004468182 * x
           x <- ifelse(is.na(Sale.Month), 6.83143507973, Sale.Month)
           predict <- predict +  0.222368281782E-01 * x
           x <- ifelse(is.na(HoodSqFt), 1566.68119178, HoodSqFt)
           predict <- predict +  0.644866592950E-03 * x
           x <- ifelse(is.na(PropClassSqFt), 1602.26674184, PropClassSqFt)
           predict <- predict +  -0.888422073081E-04 * x
           predict <- predict +  -1.62354856575 * Rooms.NA
           predict <- predict +  0.164174889224 * Longitude.NA
           predict <- predict +  -0.340493693914 * Total.Building.Square.Feet.NA
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         }
       } else {
         catvalues <- c("30","50","53","180","194","210","222","251","270","274","310","312","330","345","423")
         if(Neighborhood.Code %in% catvalues){
           catvalues <- c("30","180","194","210","312","330")
           if(Neighborhood.Code %in% catvalues){
             catvalues <- c("312","330")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 168
               predict <- -120.594692993
               x <- ifelse(is.na(log.Land.Square.Feet), 8.28630720027, log.Land.Square.Feet)
               predict <- predict +  0.529524594756 * x
               x <- ifelse(is.na(Rooms), 6.31578947368, Rooms)
               predict <- predict +  0.360014310950E-01 * x
               x <- ifelse(is.na(Bedrooms), 3.28229665072, Bedrooms)
               predict <- predict +  -0.427015795289E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.669856459330E-01, Fireplaces)
               predict <- predict +  0.347410471187 * x
               x <- ifelse(is.na(Half.Baths), 0.200956937799, Half.Baths)
               predict <- predict +  0.161828907174 * x
               x <- ifelse(is.na(Garage.1.Area), 1.11961722488, Garage.1.Area)
               predict <- predict +  0.472828928032E-01 * x
               x <- ifelse(is.na(Garage.2.Area), 0.191387559809E-01, Garage.2.Area)
               predict <- predict +  0.198205795903 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.12664754594, log.Building.Square.Feet)
               predict <- predict +  0.230415026594 * x
               x <- ifelse(is.na(Number.of.Commercial.Units), 0.478468899522E-02, Number.of.Commercial.Units)
               predict <- predict +  0.343870145735 * x
               x <- ifelse(is.na(Longitude), -87.6395231617, Longitude)
               predict <- predict +  -5.35214492316 * x
               x <- ifelse(is.na(Latitude), 41.6865850522, Latitude)
               predict <- predict +  -12.3841752682 * x
               x <- ifelse(is.na(Census.Tract), 587891.377990, Census.Tract)
               predict <- predict +  -0.780599195911E-05 * x
               x <- ifelse(is.na(Total.Building.Square.Feet), 2775.00000000, Total.Building.Square.Feet)
               predict <- predict +  -0.192481851806E-03 * x
               x <- ifelse(is.na(Full.Baths), 1.39712918660, Full.Baths)
               predict <- predict +  0.163866915277 * x
               x <- ifelse(is.na(Age), 78.7799043062, Age)
               predict <- predict +  -0.105608980988E-01 * x
               x <- ifelse(is.na(Sale.Year), 2015.11961722, Sale.Year)
               predict <- predict +  0.120074167280 * x
               x <- ifelse(is.na(Sale.Month), 6.82296650718, Sale.Month)
               predict <- predict +  0.125075263747E-01 * x
               x <- ifelse(is.na(HoodSqFt), 1401.96816853, HoodSqFt)
               predict <- predict +  -0.458796211003E-01 * x
               x <- ifelse(is.na(PropClassSqFt), 1503.33762175, PropClassSqFt)
               predict <- predict +  -0.146193194653E-03 * x
               predict <- predict +  0.210653119157E-01 * Total.Building.Square.Feet.NA
               predict <- predict +  0.163715712334 * Percent.Ownership.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               catvalues <- c("1.0")
               if(Most.Recent.Sale %in% catvalues){
                 nodeid <- 338
                 predict <- 27.3850593278
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.86536908555, log.Land.Square.Feet)
                 predict <- predict +  0.255324971514 * x
                 x <- ifelse(is.na(Rooms), 6.72402597403, Rooms)
                 predict <- predict +  -0.381588652689E-02 * x
                 x <- ifelse(is.na(Bedrooms), 3.36904761905, Bedrooms)
                 predict <- predict +  0.264594595103E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.129870129870, Fireplaces)
                 predict <- predict +  0.209940078659 * x
                 x <- ifelse(is.na(Half.Baths), 0.316017316017, Half.Baths)
                 predict <- predict +  0.820570810223E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.59090909091, Garage.1.Area)
                 predict <- predict +  -0.283034287299E-02 * x
                 x <- ifelse(is.na(Garage.2.Area), 0.865800865801E-02, Garage.2.Area)
                 predict <- predict +  0.495506921504E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.28932768973, log.Building.Square.Feet)
                 predict <- predict +  0.501199755742 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.173160173160E-01, Number.of.Commercial.Units)
                 predict <- predict +  -0.264137287843 * x
                 x <- ifelse(is.na(Longitude), -87.7746194147, Longitude)
                 predict <- predict +  4.22090155635 * x
                 x <- ifelse(is.na(Latitude), 41.9017884539, Latitude)
                 predict <- predict +  3.07215037241 * x
                 x <- ifelse(is.na(Census.Tract), 617618.042500, Census.Tract)
                 predict <- predict +  0.292940378506E-06 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 5533.47058824, Total.Building.Square.Feet)
                 predict <- predict +  -0.470735831759E-04 * x
                 x <- ifelse(is.na(Full.Baths), 1.58116883117, Full.Baths)
                 predict <- predict +  -0.139856247080E-02 * x
                 x <- ifelse(is.na(Age), 63.4975247525, Age)
                 predict <- predict +  -0.363985867263E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 96.1875000000, Number.of.Units)
                 predict <- predict +  -0.127683727926E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.456701030928E-01, Percent.Ownership)
                 predict <- predict +  2.56585890548 * x
                 x <- ifelse(is.na(Sale.Year), 2015.08085809, Sale.Year)
                 predict <- predict +  0.108734499877 * x
                 x <- ifelse(is.na(Sale.Month), 6.79537953795, Sale.Month)
                 predict <- predict +  0.134985205233E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1719.10326952, HoodSqFt)
                 predict <- predict +  0.113831815311E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 1739.37427920, PropClassSqFt)
                 predict <- predict +  -0.948386824809E-04 * x
                 predict <- predict +  0.415432624564 * Rooms.NA
                 predict <- predict +  -0.543361344984 * Longitude.NA
                 predict <- predict +  0.374275218770 * Total.Building.Square.Feet.NA
                 predict <- predict +  1.52271173279 * Percent.Ownership.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 339
                 predict <- 123.061965562
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.68815322887, log.Land.Square.Feet)
                 predict <- predict +  0.339466840433 * x
                 x <- ifelse(is.na(Rooms), 6.83135391924, Rooms)
                 predict <- predict +  0.130619420389E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.42280285036, Bedrooms)
                 predict <- predict +  -0.113217615848E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.926365795724E-01, Fireplaces)
                 predict <- predict +  0.130732984675 * x
                 x <- ifelse(is.na(Half.Baths), 0.356294536817, Half.Baths)
                 predict <- predict +  0.508009962544E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.58432304038, Garage.1.Area)
                 predict <- predict +  -0.653358748664E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.28348304606, log.Building.Square.Feet)
                 predict <- predict +  0.701711197993 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.142517814727E-01, Number.of.Commercial.Units)
                 predict <- predict +  0.249784040596 * x
                 x <- ifelse(is.na(Longitude), -87.7654811478, Longitude)
                 predict <- predict +  5.36027597005 * x
                 x <- ifelse(is.na(Latitude), 41.8943281148, Latitude)
                 predict <- predict +  4.34146181940 * x
                 x <- ifelse(is.na(Census.Tract), 550299.675510, Census.Tract)
                 predict <- predict +  0.516888985245E-06 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 7444.21052632, Total.Building.Square.Feet)
                 predict <- predict +  -0.103913945976E-03 * x
                 x <- ifelse(is.na(Full.Baths), 1.60095011876, Full.Baths)
                 predict <- predict +  0.411195213844E-01 * x
                 x <- ifelse(is.na(Age), 67.7196078431, Age)
                 predict <- predict +  -0.394873025229E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 88.7415730337, Number.of.Units)
                 predict <- predict +  -0.324735941855E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.452808988764E-01, Percent.Ownership)
                 predict <- predict +  2.97535510089 * x
                 x <- ifelse(is.na(Sale.Year), 2014.69215686, Sale.Year)
                 predict <- predict +  0.845605944146E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.62156862745, Sale.Month)
                 predict <- predict +  -0.657590866377E-02 * x
                 x <- ifelse(is.na(HoodSqFt), 1717.48121771, HoodSqFt)
                 predict <- predict +  0.175434140617E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 1701.78034266, PropClassSqFt)
                 predict <- predict +  -0.233608741267E-03 * x
                 predict <- predict +  -1.15101141241 * Rooms.NA
                 predict <- predict +  -0.636041260376 * Longitude.NA
                 predict <- predict +  0.221842643952 * Total.Building.Square.Feet.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           } else {
             catvalues <- c("22","29","72","76")
             if(Town.Code %in% catvalues){
               catvalues <- c("22","29")
               if(Town.Code %in% catvalues){
                 nodeid <- 340
                 predict <- 813.072527052
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.7577160080, log.Land.Square.Feet)
                 predict <- predict +  0.972124420146E-01 * x
                 x <- ifelse(is.na(Rooms), 6.48951048951, Rooms)
                 predict <- predict +  0.129021246452E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.05594405594, Bedrooms)
                 predict <- predict +  -0.538697773922E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.426573426573, Fireplaces)
                 predict <- predict +  0.114208578445 * x
                 x <- ifelse(is.na(Half.Baths), 0.769230769231, Half.Baths)
                 predict <- predict +  -0.465913977981E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.06993006993, Garage.1.Area)
                 predict <- predict +  0.994964139829E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.37657447554, log.Building.Square.Feet)
                 predict <- predict +  0.975251452121 * x
                 x <- ifelse(is.na(Longitude), -87.9099371857, Longitude)
                 predict <- predict +  5.99203979839 * x
                 x <- ifelse(is.na(Latitude), 42.0712031922, Latitude)
                 predict <- predict +  -1.97084677184 * x
                 x <- ifelse(is.na(Census.Tract), 805381.884712, Census.Tract)
                 predict <- predict +  -0.537250658865E-03 * x
                 x <- ifelse(is.na(Full.Baths), 1.50349650350, Full.Baths)
                 predict <- predict +  -0.831151847302E-01 * x
                 x <- ifelse(is.na(Age), 38.4402985075, Age)
                 predict <- predict +  -0.777975195374E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 287.795366795, Number.of.Units)
                 predict <- predict +  -0.932939318554E-03 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.179536679537E-01, Percent.Ownership)
                 predict <- predict +  -0.855250910879 * x
                 x <- ifelse(is.na(Sale.Year), 2014.98009950, Sale.Year)
                 predict <- predict +  0.116285978055 * x
                 x <- ifelse(is.na(Sale.Month), 6.94527363184, Sale.Month)
                 predict <- predict +  0.172505146322E-01 * x
                 x <- ifelse(is.na(PropClassSqFt), 1744.20057445, PropClassSqFt)
                 predict <- predict +  -0.113242129620E-03 * x
                 predict <- predict +  -1.31369308035 * Rooms.NA
                 predict <- predict +  -0.400305501514 * Longitude.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 341
                 predict <- -1776.91162587
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.37385880326, log.Land.Square.Feet)
                 predict <- predict +  0.566637787525 * x
                 x <- ifelse(is.na(Rooms), 6.71243042672, Rooms)
                 predict <- predict +  -0.149951593003E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.48608534323, Bedrooms)
                 predict <- predict +  0.212395903643E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.153988868275, Fireplaces)
                 predict <- predict +  0.821922155899E-01 * x
                 x <- ifelse(is.na(Half.Baths), 0.302411873840, Half.Baths)
                 predict <- predict +  0.342188239811E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.47495361781, Garage.1.Area)
                 predict <- predict +  0.607562102005E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.29399383605, log.Building.Square.Feet)
                 predict <- predict +  0.489600579023 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.111317254174E-01, Number.of.Commercial.Units)
                 predict <- predict +  -0.229520776539 * x
                 x <- ifelse(is.na(Longitude), -87.6655813188, Longitude)
                 predict <- predict +  -12.3403285338 * x
                 x <- ifelse(is.na(Latitude), 41.7306868169, Latitude)
                 predict <- predict +  11.3025970942 * x
                 x <- ifelse(is.na(Census.Tract), 687107.134694, Census.Tract)
                 predict <- predict +  0.115860952759E-05 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 56072.9830508, Total.Building.Square.Feet)
                 predict <- predict +  -0.575276179569E-04 * x
                 x <- ifelse(is.na(Full.Baths), 1.64007421150, Full.Baths)
                 predict <- predict +  0.390486158079E-01 * x
                 x <- ifelse(is.na(Age), 68.4146788991, Age)
                 predict <- predict +  -0.533029544111E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 95.5000000000, Number.of.Units)
                 predict <- predict +  0.432785648791E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.106666666667, Percent.Ownership)
                 predict <- predict +  7.96259745725 * x
                 x <- ifelse(is.na(Sale.Year), 2015.23119266, Sale.Year)
                 predict <- predict +  0.112676441692 * x
                 x <- ifelse(is.na(Sale.Month), 6.24587155963, Sale.Month)
                 predict <- predict +  0.152083039595E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1560.95226659, HoodSqFt)
                 predict <- predict +  -0.996177857535E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 1734.69181794, PropClassSqFt)
                 predict <- predict +  -0.104936386555E-03 * x
                 predict <- predict +  -1.33902965295 * Rooms.NA
                 predict <- predict +  0.479192285511 * Longitude.NA
                 predict <- predict +  2.90072949739 * Total.Building.Square.Feet.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               nodeid <- 171
               predict <- -4.03152757897
               x <- ifelse(is.na(log.Land.Square.Feet), 9.36789679145, log.Land.Square.Feet)
               predict <- predict +  0.317917108546 * x
               x <- ifelse(is.na(Rooms), 7.31518624642, Rooms)
               predict <- predict +  -0.117847785176E-01 * x
               x <- ifelse(is.na(Bedrooms), 3.67621776504, Bedrooms)
               predict <- predict +  -0.795848950599E-02 * x
               x <- ifelse(is.na(Fireplaces), 0.366762177650, Fireplaces)
               predict <- predict +  0.217126754374 * x
               x <- ifelse(is.na(Half.Baths), 0.581661891117, Half.Baths)
               predict <- predict +  -0.387125155920E-01 * x
               x <- ifelse(is.na(Garage.1.Area), 1.62750716332, Garage.1.Area)
               predict <- predict +  0.240835549684E-01 * x
               x <- ifelse(is.na(Garage.2.Area), 0.573065902579E-02, Garage.2.Area)
               predict <- predict +  -0.300241848246 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.48429715869, log.Building.Square.Feet)
               predict <- predict +  0.400718370802 * x
               x <- ifelse(is.na(Number.of.Commercial.Units), 0.143266475645E-01, Number.of.Commercial.Units)
               predict <- predict +  -0.186686842475 * x
               x <- ifelse(is.na(Longitude), -87.8310312834, Longitude)
               predict <- predict +  2.25965913578 * x
               x <- ifelse(is.na(Latitude), 41.9962392462, Latitude)
               predict <- predict +  1.38314483024 * x
               x <- ifelse(is.na(Census.Tract), 583141.331096, Census.Tract)
               predict <- predict +  -0.230136167637E-07 * x
               x <- ifelse(is.na(Total.Building.Square.Feet), 2489.33333333, Total.Building.Square.Feet)
               predict <- predict +  0.908342423069E-03 * x
               x <- ifelse(is.na(Full.Baths), 1.76217765043, Full.Baths)
               predict <- predict +  0.742076983099E-01 * x
               x <- ifelse(is.na(Age), 57.2527716186, Age)
               predict <- predict +  -0.415206494553E-02 * x
               x <- ifelse(is.na(Number.of.Units), 185.990196078, Number.of.Units)
               predict <- predict +  -0.252089553747E-02 * x
               x <- ifelse(is.na(Percent.Ownership), 0.310784313725E-01, Percent.Ownership)
               predict <- predict +  1.62192160045 * x
               x <- ifelse(is.na(Sale.Year), 2014.98447894, Sale.Year)
               predict <- predict +  0.771328315458E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.65631929047, Sale.Month)
               predict <- predict +  0.937907875179E-02 * x
               x <- ifelse(is.na(HoodSqFt), 1726.16612312, HoodSqFt)
               predict <- predict +  -0.412695818599E-02 * x
               x <- ifelse(is.na(PropClassSqFt), 1988.35911335, PropClassSqFt)
               predict <- predict +  0.477176149855E-04 * x
               predict <- predict +  -1.82418919884 * Rooms.NA
               predict <- predict +  0.205009260183 * Longitude.NA
               predict <- predict +  1.10704858714 * Total.Building.Square.Feet.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             }
           }
         } else {
           catvalues <- c("14","33","43","82","101","110","280","362","422","431","432")
           if(Neighborhood.Code %in% catvalues){
             catvalues <- c("43","82","101","110","432")
             if(Neighborhood.Code %in% catvalues){
               if(is.na(Longitude) | Longitude <= -87.7598274165 ){
                 nodeid <- 344
                 predict <- 450.101082719
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.8233409477, log.Land.Square.Feet)
                 predict <- predict +  0.196146563799 * x
                 x <- ifelse(is.na(Rooms), 7.58823529412, Rooms)
                 predict <- predict +  -0.357881674499E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.82352941176, Bedrooms)
                 predict <- predict +  -0.347167240113E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.954248366013, Fireplaces)
                 predict <- predict +  0.663019159885E-01 * x
                 x <- ifelse(is.na(Half.Baths), 0.777777777778, Half.Baths)
                 predict <- predict +  0.760881841916E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.68627450980, Garage.1.Area)
                 predict <- predict +  0.512375520435E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.64722333516, log.Building.Square.Feet)
                 predict <- predict +  0.743708078757 * x
                 x <- ifelse(is.na(Longitude), -87.9170061237, Longitude)
                 predict <- predict +  0.492384137637 * x
                 x <- ifelse(is.na(Latitude), 42.0870512460, Latitude)
                 predict <- predict +  -8.44001373065 * x
                 x <- ifelse(is.na(Census.Tract), 803589.520492, Census.Tract)
                 predict <- predict +  -0.175454984352E-03 * x
                 x <- ifelse(is.na(Full.Baths), 2.11111111111, Full.Baths)
                 predict <- predict +  0.123573970317 * x
                 x <- ifelse(is.na(Age), 37.1680000000, Age)
                 predict <- predict +  -0.100028634911E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 297.670103093, Number.of.Units)
                 predict <- predict +  -0.569144363667E-03 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.139175257732E-01, Percent.Ownership)
                 predict <- predict +  18.5770130318 * x
                 x <- ifelse(is.na(Sale.Year), 2015.02800000, Sale.Year)
                 predict <- predict +  0.467888311754E-01 * x
                 x <- ifelse(is.na(Sale.Month), 7.09600000000, Sale.Month)
                 predict <- predict +  0.160042985241E-02 * x
                 x <- ifelse(is.na(HoodSqFt), 1938.07453464, HoodSqFt)
                 predict <- predict +  0.206520978649E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 2148.65960008, PropClassSqFt)
                 predict <- predict +  0.357574323729E-04 * x
                 predict <- predict +  -1.60729740430 * Rooms.NA
                 predict <- predict +  0.710095640071E-01 * Longitude.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 345
                 predict <- 873.682639782
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.51559348536, log.Land.Square.Feet)
                 predict <- predict +  0.267544779341 * x
                 x <- ifelse(is.na(Rooms), 7.65283018868, Rooms)
                 predict <- predict +  -0.233320728980E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.71509433962, Bedrooms)
                 predict <- predict +  0.250716259260E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.230188679245, Fireplaces)
                 predict <- predict +  0.444564830577E-01 * x
                 x <- ifelse(is.na(Half.Baths), 0.349056603774, Half.Baths)
                 predict <- predict +  0.202938351342E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.63018867925, Garage.1.Area)
                 predict <- predict +  -0.290136231183E-01 * x
                 x <- ifelse(is.na(Garage.2.Area), 0.754716981132E-02, Garage.2.Area)
                 predict <- predict +  0.566974244584E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.48934939024, log.Building.Square.Feet)
                 predict <- predict +  0.418713357411 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.207547169811E-01, Number.of.Commercial.Units)
                 predict <- predict +  -0.168000709749 * x
                 x <- ifelse(is.na(Longitude), -87.7089017494, Longitude)
                 predict <- predict +  15.2382518562 * x
                 x <- ifelse(is.na(Latitude), 41.9422327781, Latitude)
                 predict <- predict +  6.72315736195 * x
                 x <- ifelse(is.na(Census.Tract), 242898.860372, Census.Tract)
                 predict <- predict +  -0.201591294538E-06 * x
                 x <- ifelse(is.na(Full.Baths), 1.85660377358, Full.Baths)
                 predict <- predict +  -0.350842031135E-01 * x
                 x <- ifelse(is.na(Age), 77.1343085106, Age)
                 predict <- predict +  -0.447778929465E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 16.5630630631, Number.of.Units)
                 predict <- predict +  -0.665054596781E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.107668161435, Percent.Ownership)
                 predict <- predict +  2.14231081452 * x
                 x <- ifelse(is.na(Sale.Year), 2015.07579787, Sale.Year)
                 predict <- predict +  0.946197033575E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.93351063830, Sale.Month)
                 predict <- predict +  0.129958494967E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1862.88152005, HoodSqFt)
                 predict <- predict +  -0.208766210739E-02 * x
                 x <- ifelse(is.na(PropClassSqFt), 2099.95903964, PropClassSqFt)
                 predict <- predict +  -0.457749053784E-04 * x
                 predict <- predict +  0.898680771186 * Rooms.NA
                 predict <- predict +  0.571285467996E-01 * Total.Building.Square.Feet.NA
                 predict <- predict +  1.99664009313 * Percent.Ownership.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               nodeid <- 173
               predict <- -85.9170094354
               x <- ifelse(is.na(log.Land.Square.Feet), 8.99173485607, log.Land.Square.Feet)
               predict <- predict +  0.423162661171 * x
               x <- ifelse(is.na(Rooms), 6.51533742331, Rooms)
               predict <- predict +  -0.312689509684E-01 * x
               x <- ifelse(is.na(Bedrooms), 3.26687116564, Bedrooms)
               predict <- predict +  0.332237382705E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.306748466258, Fireplaces)
               predict <- predict +  0.677289825947E-01 * x
               x <- ifelse(is.na(Half.Baths), 0.395705521472, Half.Baths)
               predict <- predict +  0.738952817344E-01 * x
               x <- ifelse(is.na(Garage.1.Area), 1.61963190184, Garage.1.Area)
               predict <- predict +  0.691554997637E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.31191134924, log.Building.Square.Feet)
               predict <- predict +  0.420264371914 * x
               x <- ifelse(is.na(Number.of.Commercial.Units), 0.153374233129E-01, Number.of.Commercial.Units)
               predict <- predict +  -0.180544205992 * x
               x <- ifelse(is.na(Longitude), -87.8755408878, Longitude)
               predict <- predict +  1.47657817108 * x
               x <- ifelse(is.na(Latitude), 41.9601015913, Latitude)
               predict <- predict +  0.887077769055 * x
               x <- ifelse(is.na(Census.Tract), 554227.847545, Census.Tract)
               predict <- predict +  -0.753858161167E-06 * x
               x <- ifelse(is.na(Full.Baths), 1.52453987730, Full.Baths)
               predict <- predict +  0.514366739289E-01 * x
               x <- ifelse(is.na(Age), 54.5743589744, Age)
               predict <- predict +  -0.107495887135E-01 * x
               x <- ifelse(is.na(Number.of.Units), 73.8281250000, Number.of.Units)
               predict <- predict +  -0.512125235625E-02 * x
               x <- ifelse(is.na(Percent.Ownership), 0.423437500000E-01, Percent.Ownership)
               predict <- predict +  6.28543672980 * x
               x <- ifelse(is.na(Sale.Year), 2014.94358974, Sale.Year)
               predict <- predict +  0.921470739257E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.86410256410, Sale.Month)
               predict <- predict +  0.127838992668E-01 * x
               x <- ifelse(is.na(HoodSqFt), 1569.09689096, HoodSqFt)
               predict <- predict +  -0.148358975890E-03 * x
               x <- ifelse(is.na(PropClassSqFt), 1682.00248904, PropClassSqFt)
               predict <- predict +  -0.104807301421E-03 * x
               predict <- predict +  -2.24602485124 * Rooms.NA
               predict <- predict +  0.340698193529 * Longitude.NA
               predict <- predict +  -0.129381686000 * Total.Building.Square.Feet.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             }
           } else {
             catvalues <- c("16","29","38","71","72")
             if(Town.Code %in% catvalues){
               catvalues <- c("92")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 348
                 predict <- 1440.56247335
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.22634390420, log.Land.Square.Feet)
                 predict <- predict +  -0.896529155826E-01 * x
                 x <- ifelse(is.na(Rooms), 8.41071428571, Rooms)
                 predict <- predict +  -0.268681533598E-01 * x
                 x <- ifelse(is.na(Bedrooms), 4.53571428571, Bedrooms)
                 predict <- predict +  0.108871866287 * x
                 x <- ifelse(is.na(Fireplaces), 0.535714285714E-01, Fireplaces)
                 predict <- predict +  0.358878896169E-02 * x
                 x <- ifelse(is.na(Half.Baths), 0.303571428571, Half.Baths)
                 predict <- predict +  0.210220504871 * x
                 x <- ifelse(is.na(Garage.1.Area), 0.642857142857, Garage.1.Area)
                 predict <- predict +  -0.690060933919E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.53528666950, log.Building.Square.Feet)
                 predict <- predict +  0.248571554207 * x
                 x <- ifelse(is.na(Longitude), -87.6345626340, Longitude)
                 predict <- predict +  -0.496026309608 * x
                 x <- ifelse(is.na(Latitude), 41.7849815248, Latitude)
                 predict <- predict +  -39.3645173761 * x
                 x <- ifelse(is.na(Census.Tract), 781170.000000, Census.Tract)
                 predict <- predict +  0.303014573808E-05 * x
                 x <- ifelse(is.na(Full.Baths), 2.00000000000, Full.Baths)
                 predict <- predict +  -0.250035504913 * x
                 x <- ifelse(is.na(Age), 97.5178571429, Age)
                 predict <- predict +  -0.502627560290E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.08928571, Sale.Year)
                 predict <- predict +  0.827535005070E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.64285714286, Sale.Month)
                 predict <- predict +  0.246780927181E-01 * x
                 x <- ifelse(is.na(PropClassSqFt), 2249.40390425, PropClassSqFt)
                 predict <- predict +  0.697135567813E-04 * x
                 predict <- predict +  1.72944190862 * Longitude.NA
                 predict <- predict +  1.54817182118 * Total.Building.Square.Feet.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 349
                 predict <- -148.201158923
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.90683828447, log.Land.Square.Feet)
                 predict <- predict +  0.280534060487 * x
                 x <- ifelse(is.na(Rooms), 6.31157635468, Rooms)
                 predict <- predict +  -0.128661451082E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.20566502463, Bedrooms)
                 predict <- predict +  -0.170307467960E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.190886699507, Fireplaces)
                 predict <- predict +  0.185199815902 * x
                 x <- ifelse(is.na(Half.Baths), 0.370689655172, Half.Baths)
                 predict <- predict +  0.990997778151E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.70566502463, Garage.1.Area)
                 predict <- predict +  0.192111052450E-01 * x
                 x <- ifelse(is.na(Garage.2.Area), 0.738916256158E-02, Garage.2.Area)
                 predict <- predict +  -0.510702459508E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.26940453001, log.Building.Square.Feet)
                 predict <- predict +  0.449430994271 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.554187192118E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.566738833174 * x
                 x <- ifelse(is.na(Longitude), -87.8376507317, Longitude)
                 predict <- predict +  0.966379168260 * x
                 x <- ifelse(is.na(Latitude), 41.9746646231, Latitude)
                 predict <- predict +  1.61007825336 * x
                 x <- ifelse(is.na(Census.Tract), 407117.801846, Census.Tract)
                 predict <- predict +  -0.664314662043E-06 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 8177.73913043, Total.Building.Square.Feet)
                 predict <- predict +  -0.119462824469E-03 * x
                 x <- ifelse(is.na(Full.Baths), 1.45812807882, Full.Baths)
                 predict <- predict +  0.414722529493E-01 * x
                 x <- ifelse(is.na(Age), 62.0031813362, Age)
                 predict <- predict +  -0.329683924880E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 94.7213740458, Number.of.Units)
                 predict <- predict +  -0.355224681005E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.402661596958E-01, Percent.Ownership)
                 predict <- predict +  1.11706191466 * x
                 x <- ifelse(is.na(Sale.Year), 2015.09862142, Sale.Year)
                 predict <- predict +  0.851565022619E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.95493107105, Sale.Month)
                 predict <- predict +  0.105379127895E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1735.64767504, HoodSqFt)
                 predict <- predict +  0.213098482261E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 1575.89073282, PropClassSqFt)
                 predict <- predict +  -0.385341579989E-04 * x
                 predict <- predict +  -0.818079033350 * Rooms.NA
                 predict <- predict +  -0.304437370777 * Longitude.NA
                 predict <- predict +  1.19834264634 * Total.Building.Square.Feet.NA
                 predict <- predict +  0.795947006459 * Percent.Ownership.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               nodeid <- 175
               predict <- 269.341357391
               x <- ifelse(is.na(log.Land.Square.Feet), 9.27343459730, log.Land.Square.Feet)
               predict <- predict +  0.114947011118 * x
               x <- ifelse(is.na(Rooms), 7.25192307692, Rooms)
               predict <- predict +  0.553592332892E-02 * x
               x <- ifelse(is.na(Bedrooms), 3.68076923077, Bedrooms)
               predict <- predict +  0.124900247158E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.798076923077, Fireplaces)
               predict <- predict +  0.120525115712 * x
               x <- ifelse(is.na(Half.Baths), 0.680769230769, Half.Baths)
               predict <- predict +  0.541456228658E-01 * x
               x <- ifelse(is.na(Garage.1.Area), 1.69807692308, Garage.1.Area)
               predict <- predict +  0.729736547405E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.57165701530, log.Building.Square.Feet)
               predict <- predict +  0.611863807358 * x
               x <- ifelse(is.na(Number.of.Commercial.Units), 0.192307692308E-02, Number.of.Commercial.Units)
               predict <- predict +  -0.375751511182 * x
               x <- ifelse(is.na(Longitude), -87.8011801471, Longitude)
               predict <- predict +  2.09431297966 * x
               x <- ifelse(is.na(Latitude), 42.0296889478, Latitude)
               predict <- predict +  -2.46017709976 * x
               x <- ifelse(is.na(Census.Tract), 804498.558923, Census.Tract)
               predict <- predict +  -0.545596307141E-04 * x
               x <- ifelse(is.na(Full.Baths), 1.92500000000, Full.Baths)
               predict <- predict +  0.745200550214E-02 * x
               x <- ifelse(is.na(Age), 55.1789297659, Age)
               predict <- predict +  -0.140568674856E-02 * x
               x <- ifelse(is.na(Number.of.Units), 68.8717948718, Number.of.Units)
               predict <- predict +  0.516582090735E-02 * x
               x <- ifelse(is.na(Percent.Ownership), 0.334615384615E-01, Percent.Ownership)
               predict <- predict +  2.74332451112 * x
               x <- ifelse(is.na(Sale.Year), 2015.01839465, Sale.Year)
               predict <- predict +  0.338665398462E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.76588628763, Sale.Month)
               predict <- predict +  -0.567177221871E-03 * x
               x <- ifelse(is.na(HoodSqFt), 1931.72656378, HoodSqFt)
               predict <- predict +  0.129952366535E-03 * x
               x <- ifelse(is.na(PropClassSqFt), 2008.66622748, PropClassSqFt)
               predict <- predict +  0.184243004725E-04 * x
               predict <- predict +  -1.00470689536 * Rooms.NA
               predict <- predict +  -0.450916546795 * Longitude.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             }
           }
         }
       }
     } else {
       catvalues <- c("72")
       if(Town.Code %in% catvalues){
         catvalues <- c("51","71","80","91","120","121","130","170","171","223","281","321")
         if(Neighborhood.Code %in% catvalues){
           catvalues <- c("91","120","121","130","170","171")
           if(Neighborhood.Code %in% catvalues){
             if(!is.na(Age) & Age <= 115.500000000 ){
               nodeid <- 176
               predict <- 397.867550896
               x <- ifelse(is.na(log.Land.Square.Feet), 8.13655161262, log.Land.Square.Feet)
               predict <- predict +  0.376263223069 * x
               x <- ifelse(is.na(Rooms), 8.12876712329, Rooms)
               predict <- predict +  0.603993938360E-01 * x
               x <- ifelse(is.na(Bedrooms), 4.07397260274, Bedrooms)
               predict <- predict +  -0.919377388998E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.438356164384E-01, Fireplaces)
               predict <- predict +  0.261796149051 * x
               x <- ifelse(is.na(Half.Baths), 0.156164383562, Half.Baths)
               predict <- predict +  -0.224323598277 * x
               x <- ifelse(is.na(Garage.1.Area), 1.18904109589, Garage.1.Area)
               predict <- predict +  -0.423127812520E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.41729613535, log.Building.Square.Feet)
               predict <- predict +  0.452386854991 * x
               x <- ifelse(is.na(Number.of.Commercial.Units), 0.136986301370E-01, Number.of.Commercial.Units)
               predict <- predict +  0.271471202625 * x
               x <- ifelse(is.na(Longitude), -87.6631025510, Longitude)
               predict <- predict +  6.33545032783 * x
               x <- ifelse(is.na(Latitude), 41.7780609195, Latitude)
               predict <- predict +  1.50280897322 * x
               x <- ifelse(is.na(Census.Tract), 673813.698630, Census.Tract)
               predict <- predict +  -0.596235577052E-06 * x
               x <- ifelse(is.na(Total.Building.Square.Feet), 3738.00000000, Total.Building.Square.Feet)
               predict <- predict +  -0.795959552238E-04 * x
               x <- ifelse(is.na(Full.Baths), 1.89041095890, Full.Baths)
               predict <- predict +  -0.249814553825E-01 * x
               x <- ifelse(is.na(Age), 92.2876712329, Age)
               predict <- predict +  -0.877151479802E-02 * x
               x <- ifelse(is.na(Sale.Year), 2015.16712329, Sale.Year)
               predict <- predict +  0.498824640816E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.82465753425, Sale.Month)
               predict <- predict +  0.275523214183E-01 * x
               x <- ifelse(is.na(HoodSqFt), 1788.25062406, HoodSqFt)
               predict <- predict +  -0.107053129158E-03 * x
               x <- ifelse(is.na(PropClassSqFt), 2094.59129681, PropClassSqFt)
               predict <- predict +  0.366557112781E-04 * x
               predict <- predict +  -0.300547768976 * Total.Building.Square.Feet.NA
               predict <- predict +  0.235147795174E-01 * Percent.Ownership.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               nodeid <- 177
               predict <- -1056.86615838
               x <- ifelse(is.na(log.Land.Square.Feet), 8.09917612211, log.Land.Square.Feet)
               predict <- predict +  0.957245930794E-01 * x
               x <- ifelse(is.na(Rooms), 8.46067415730, Rooms)
               predict <- predict +  0.477161511805E-01 * x
               x <- ifelse(is.na(Bedrooms), 4.06741573034, Bedrooms)
               predict <- predict +  0.866302424379E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.123595505618, Fireplaces)
               predict <- predict +  -0.187988359797 * x
               x <- ifelse(is.na(Half.Baths), 0.123595505618, Half.Baths)
               predict <- predict +  -0.123939892628 * x
               x <- ifelse(is.na(Garage.1.Area), 0.921348314607, Garage.1.Area)
               predict <- predict +  0.821873406483E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.45124605932, log.Building.Square.Feet)
               predict <- predict +  0.381234555734 * x
               x <- ifelse(is.na(Number.of.Commercial.Units), 0.224719101124E-01, Number.of.Commercial.Units)
               predict <- predict +  -0.504871886352 * x
               x <- ifelse(is.na(Longitude), -87.6545564493, Longitude)
               predict <- predict +  -4.96758223798 * x
               x <- ifelse(is.na(Latitude), 41.7823892136, Latitude)
               predict <- predict +  9.44498065494 * x
               x <- ifelse(is.na(Census.Tract), 673374.157303, Census.Tract)
               predict <- predict +  0.480086654731E-05 * x
               x <- ifelse(is.na(Total.Building.Square.Feet), 3227.00000000, Total.Building.Square.Feet)
               predict <- predict +  -0.682555289701E-03 * x
               x <- ifelse(is.na(Full.Baths), 1.94382022472, Full.Baths)
               predict <- predict +  -0.295663495954E-01 * x
               x <- ifelse(is.na(Age), 122.943820225, Age)
               predict <- predict +  -0.880531480071E-02 * x
               x <- ifelse(is.na(Sale.Year), 2015.30337079, Sale.Year)
               predict <- predict +  0.114764762097 * x
               x <- ifelse(is.na(Sale.Month), 6.30337078652, Sale.Month)
               predict <- predict +  -0.638930060806E-01 * x
               x <- ifelse(is.na(HoodSqFt), 1838.01346606, HoodSqFt)
               predict <- predict +  0.707173777623E-03 * x
               x <- ifelse(is.na(PropClassSqFt), 2292.38587661, PropClassSqFt)
               predict <- predict +  0.374738956341E-04 * x
               predict <- predict +  0.579119520364 * Total.Building.Square.Feet.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             }
           } else {
             catvalues <- c("1.0")
             if(Most.Recent.Sale %in% catvalues){
               nodeid <- 178
               predict <- -459.697027555
               x <- ifelse(is.na(log.Land.Square.Feet), 8.21174726650, log.Land.Square.Feet)
               predict <- predict +  0.498349178849 * x
               x <- ifelse(is.na(Rooms), 7.14624505929, Rooms)
               predict <- predict +  -0.439078753319E-01 * x
               x <- ifelse(is.na(Bedrooms), 3.64031620553, Bedrooms)
               predict <- predict +  0.110633150469 * x
               x <- ifelse(is.na(Fireplaces), 0.711462450593E-01, Fireplaces)
               predict <- predict +  0.487300839361E-01 * x
               x <- ifelse(is.na(Half.Baths), 0.213438735178, Half.Baths)
               predict <- predict +  0.191507286011 * x
               x <- ifelse(is.na(Garage.1.Area), 1.45454545455, Garage.1.Area)
               predict <- predict +  -0.518654046569E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.29443704725, log.Building.Square.Feet)
               predict <- predict +  0.220918310255 * x
               x <- ifelse(is.na(Number.of.Commercial.Units), 0.237154150198E-01, Number.of.Commercial.Units)
               predict <- predict +  -0.132975152554 * x
               x <- ifelse(is.na(Longitude), -87.6664563067, Longitude)
               predict <- predict +  -0.896552774908 * x
               x <- ifelse(is.na(Latitude), 41.7625109372, Latitude)
               predict <- predict +  3.31916809879 * x
               x <- ifelse(is.na(Census.Tract), 686571.484127, Census.Tract)
               predict <- predict +  0.973849314525E-06 * x
               x <- ifelse(is.na(Total.Building.Square.Feet), 3476.54545455, Total.Building.Square.Feet)
               predict <- predict +  0.156900327895E-03 * x
               x <- ifelse(is.na(Full.Baths), 1.64822134387, Full.Baths)
               predict <- predict +  0.590247449211E-01 * x
               x <- ifelse(is.na(Age), 93.4268774704, Age)
               predict <- predict +  -0.652447095911E-02 * x
               x <- ifelse(is.na(Sale.Year), 2015.16205534, Sale.Year)
               predict <- predict +  0.122736728244 * x
               x <- ifelse(is.na(Sale.Month), 6.72727272727, Sale.Month)
               predict <- predict +  0.239238449457E-02 * x
               x <- ifelse(is.na(HoodSqFt), 1619.90804852, HoodSqFt)
               predict <- predict +  -0.316191302128E-03 * x
               x <- ifelse(is.na(PropClassSqFt), 1849.95225504, PropClassSqFt)
               predict <- predict +  -0.131580666859E-04 * x
               predict <- predict +  -0.503364537991 * Longitude.NA
               predict <- predict +  0.354269786701 * Total.Building.Square.Feet.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               nodeid <- 179
               predict <- -1452.45570785
               x <- ifelse(is.na(log.Land.Square.Feet), 8.25274269840, log.Land.Square.Feet)
               predict <- predict +  0.334796053730 * x
               x <- ifelse(is.na(Rooms), 6.60509554140, Rooms)
               predict <- predict +  -0.444108465789E-03 * x
               x <- ifelse(is.na(Bedrooms), 3.42038216561, Bedrooms)
               predict <- predict +  -0.413740387393E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.828025477707E-01, Fireplaces)
               predict <- predict +  0.154896883846 * x
               x <- ifelse(is.na(Half.Baths), 0.273885350318, Half.Baths)
               predict <- predict +  0.106511967722E-01 * x
               x <- ifelse(is.na(Garage.1.Area), 1.47133757962, Garage.1.Area)
               predict <- predict +  -0.907745287825E-02 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.21896117150, log.Building.Square.Feet)
               predict <- predict +  0.319736985821 * x
               x <- ifelse(is.na(Number.of.Commercial.Units), 0.636942675159E-02, Number.of.Commercial.Units)
               predict <- predict +  0.394455957238 * x
               x <- ifelse(is.na(Longitude), -87.6642216897, Longitude)
               predict <- predict +  -12.0273373971 * x
               x <- ifelse(is.na(Latitude), 41.7524139605, Latitude)
               predict <- predict +  5.57539458707 * x
               x <- ifelse(is.na(Census.Tract), 690794.369427, Census.Tract)
               predict <- predict +  0.787570391299E-06 * x
               x <- ifelse(is.na(Full.Baths), 1.45222929936, Full.Baths)
               predict <- predict +  0.105868293405 * x
               x <- ifelse(is.na(Age), 86.5095541401, Age)
               predict <- predict +  -0.172022958161E-02 * x
               x <- ifelse(is.na(Sale.Year), 2014.92993631, Sale.Year)
               predict <- predict +  0.851400478792E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.65605095541, Sale.Month)
               predict <- predict +  -0.351652866015E-02 * x
               x <- ifelse(is.na(HoodSqFt), 1550.80036321, HoodSqFt)
               predict <- predict +  -0.906889625635E-03 * x
               x <- ifelse(is.na(PropClassSqFt), 1623.51983120, PropClassSqFt)
               predict <- predict +  0.969679315625E-04 * x
               predict <- predict +  0.230359072881 * Total.Building.Square.Feet.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             }
           }
         } else {
           catvalues <- c("1.0")
           if(Most.Recent.Sale %in% catvalues){
             nodeid <- 90
             predict <- -756.528859220
             x <- ifelse(is.na(log.Land.Square.Feet), 8.29038703439, log.Land.Square.Feet)
             predict <- predict +  0.174041697616 * x
             x <- ifelse(is.na(Rooms), 5.82009724473, Rooms)
             predict <- predict +  -0.629297108065E-02 * x
             x <- ifelse(is.na(Bedrooms), 3.07212317666, Bedrooms)
             predict <- predict +  -0.208878701730E-01 * x
             x <- ifelse(is.na(Fireplaces), 0.599675850891E-01, Fireplaces)
             predict <- predict +  -0.264638746025E-01 * x
             x <- ifelse(is.na(Half.Baths), 0.217990275527, Half.Baths)
             predict <- predict +  0.225440250501E-01 * x
             x <- ifelse(is.na(Garage.1.Area), 1.60453808752, Garage.1.Area)
             predict <- predict +  0.547149484103E-02 * x
             x <- ifelse(is.na(Garage.2.Area), 0.162074554295E-02, Garage.2.Area)
             predict <- predict +  -0.814163913831E-01 * x
             x <- ifelse(is.na(log.Building.Square.Feet), 7.13923670574, log.Building.Square.Feet)
             predict <- predict +  0.224874358768 * x
             x <- ifelse(is.na(Number.of.Commercial.Units), 0.972447325770E-02, Number.of.Commercial.Units)
             predict <- predict +  -0.555284930752 * x
             x <- ifelse(is.na(Longitude), -87.7224924468, Longitude)
             predict <- predict +  -4.02087356006 * x
             x <- ifelse(is.na(Latitude), 41.7741179895, Latitude)
             predict <- predict +  3.84829254750 * x
             x <- ifelse(is.na(Census.Tract), 652918.918831, Census.Tract)
             predict <- predict +  0.742601131555E-06 * x
             x <- ifelse(is.na(Total.Building.Square.Feet), 2832.33333333, Total.Building.Square.Feet)
             predict <- predict +  0.106585560948E-03 * x
             x <- ifelse(is.na(Full.Baths), 1.32982171799, Full.Baths)
             predict <- predict +  0.856359619954E-01 * x
             x <- ifelse(is.na(Age), 69.6280388979, Age)
             predict <- predict +  -0.714050518262E-02 * x
             x <- ifelse(is.na(Percent.Ownership), 0.580000000000, Percent.Ownership)
             predict <- predict +  -1.40286529420 * x
             x <- ifelse(is.na(Sale.Year), 2015.15721232, Sale.Year)
             predict <- predict +  0.124739348529 * x
             x <- ifelse(is.na(Sale.Month), 6.85818476499, Sale.Month)
             predict <- predict +  0.478515408995E-02 * x
             x <- ifelse(is.na(HoodSqFt), 1489.98006340, HoodSqFt)
             predict <- predict +  -0.159958666820E-03 * x
             x <- ifelse(is.na(PropClassSqFt), 1504.13581896, PropClassSqFt)
             predict <- predict +  0.312535468638E-04 * x
             predict <- predict +  0.458634500175 * Longitude.NA
             predict <- predict +  0.311767879836 * Total.Building.Square.Feet.NA
             predict <- predict +  0.849502030608 * Percent.Ownership.NA
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             nodeid <- 91
             predict <- -805.435914427
             x <- ifelse(is.na(log.Land.Square.Feet), 8.30204388114, log.Land.Square.Feet)
             predict <- predict +  0.154978700817 * x
             x <- ifelse(is.na(Rooms), 6.03565062389, Rooms)
             predict <- predict +  -0.214387040688E-01 * x
             x <- ifelse(is.na(Bedrooms), 3.13903743316, Bedrooms)
             predict <- predict +  0.328355834308E-01 * x
             x <- ifelse(is.na(Fireplaces), 0.980392156863E-01, Fireplaces)
             predict <- predict +  -0.425763272269E-01 * x
             x <- ifelse(is.na(Half.Baths), 0.210338680927, Half.Baths)
             predict <- predict +  0.532181618765E-01 * x
             x <- ifelse(is.na(Garage.1.Area), 1.59536541889, Garage.1.Area)
             predict <- predict +  0.681981463888E-01 * x
             x <- ifelse(is.na(log.Building.Square.Feet), 7.15607657148, log.Building.Square.Feet)
             predict <- predict +  0.316147416634 * x
             x <- ifelse(is.na(Number.of.Commercial.Units), 0.891265597148E-02, Number.of.Commercial.Units)
             predict <- predict +  -0.320897871916 * x
             x <- ifelse(is.na(Longitude), -87.7095575857, Longitude)
             predict <- predict +  -4.58694499854 * x
             x <- ifelse(is.na(Latitude), 41.7688445552, Latitude)
             predict <- predict +  6.11349832220 * x
             x <- ifelse(is.na(Census.Tract), 669371.400722, Census.Tract)
             predict <- predict +  -0.329272336450E-06 * x
             x <- ifelse(is.na(Total.Building.Square.Feet), 3478.00000000, Total.Building.Square.Feet)
             predict <- predict +  -0.233708286098E-03 * x
             x <- ifelse(is.na(Full.Baths), 1.33689839572, Full.Baths)
             predict <- predict +  0.163816969938E-01 * x
             x <- ifelse(is.na(Age), 72.6167557932, Age)
             predict <- predict +  -0.867744948004E-02 * x
             x <- ifelse(is.na(Sale.Year), 2014.87344029, Sale.Year)
             predict <- predict +  0.775562023730E-01 * x
             x <- ifelse(is.na(Sale.Month), 6.51158645276, Sale.Month)
             predict <- predict +  0.186580182149E-01 * x
             x <- ifelse(is.na(HoodSqFt), 1518.99009593, HoodSqFt)
             predict <- predict +  -0.234575437964E-03 * x
             x <- ifelse(is.na(PropClassSqFt), 1511.60161339, PropClassSqFt)
             predict <- predict +  0.326271786409E-04 * x
             predict <- predict +  0.654954586019 * Longitude.NA
             predict <- predict +  0.951203964664 * Total.Building.Square.Feet.NA
             predict <- predict +  0.916770621507E-01 * Percent.Ownership.NA
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           }
         }
       } else {
         catvalues <- c("11","52","60","61","65","72","80","85","91","160","430","520")
         if(Neighborhood.Code %in% catvalues){
           if(!is.na(Age) & Age <= 60.5000000000 ){
             catvalues <- c("16","30")
             if(Town.Code %in% catvalues){
               nodeid <- 184
               predict <- -196.063964390
               x <- ifelse(is.na(log.Land.Square.Feet), 11.3849442980, log.Land.Square.Feet)
               predict <- predict +  0.182132370622 * x
               x <- ifelse(is.na(Rooms), 7.07194244604, Rooms)
               predict <- predict +  0.845293604236E-02 * x
               x <- ifelse(is.na(Bedrooms), 3.62589928058, Bedrooms)
               predict <- predict +  0.632549722133E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.410071942446, Fireplaces)
               predict <- predict +  0.149564180934 * x
               x <- ifelse(is.na(Half.Baths), 0.640287769784, Half.Baths)
               predict <- predict +  -0.478449852409E-01 * x
               x <- ifelse(is.na(Garage.1.Area), 1.59712230216, Garage.1.Area)
               predict <- predict +  0.159793385784 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.48374239578, log.Building.Square.Feet)
               predict <- predict +  0.346026297760 * x
               x <- ifelse(is.na(Longitude), -87.9546777755, Longitude)
               predict <- predict +  0.256478807704 * x
               x <- ifelse(is.na(Latitude), 41.9404106103, Latitude)
               predict <- predict +  0.123363006515 * x
               x <- ifelse(is.na(Census.Tract), 797915.136598, Census.Tract)
               predict <- predict +  -0.697916031648E-05 * x
               x <- ifelse(is.na(Full.Baths), 1.82014388489, Full.Baths)
               predict <- predict +  0.653080686002E-03 * x
               x <- ifelse(is.na(Age), 34.4675000000, Age)
               predict <- predict +  -0.195263502037E-01 * x
               x <- ifelse(is.na(Number.of.Units), 173.919540230, Number.of.Units)
               predict <- predict +  -0.130000838199E-02 * x
               x <- ifelse(is.na(Percent.Ownership), 0.934865900383E-02, Percent.Ownership)
               predict <- predict +  8.10136703412 * x
               x <- ifelse(is.na(Sale.Year), 2015.04000000, Sale.Year)
               predict <- predict +  0.112401969604 * x
               x <- ifelse(is.na(Sale.Month), 7.07250000000, Sale.Month)
               predict <- predict +  0.179129283184E-01 * x
               x <- ifelse(is.na(HoodSqFt), 1816.48433333, HoodSqFt)
               predict <- predict +  0.511774449964E-04 * x
               x <- ifelse(is.na(PropClassSqFt), 1898.54849511, PropClassSqFt)
               predict <- predict +  0.966062993516E-05 * x
               predict <- predict +  -1.74119308805 * Rooms.NA
               predict <- predict +  -0.556691636815 * Longitude.NA
               predict <- predict +  0.866730563237 * Total.Building.Square.Feet.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               if(!is.na(Age) & Age <= 20.5000000000 ){
                 nodeid <- 370
                 predict <- 35.8918413224
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.61010734799, log.Land.Square.Feet)
                 predict <- predict +  0.114086406340 * x
                 x <- ifelse(is.na(Rooms), 7.70319634703, Rooms)
                 predict <- predict +  -0.605167879898E-03 * x
                 x <- ifelse(is.na(Bedrooms), 3.60730593607, Bedrooms)
                 predict <- predict +  -0.642171074127E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.872146118721, Fireplaces)
                 predict <- predict +  0.612935176737E-01 * x
                 x <- ifelse(is.na(Half.Baths), 0.977168949772, Half.Baths)
                 predict <- predict +  0.526867817887E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.41095890411, Garage.1.Area)
                 predict <- predict +  -0.289301778814E-01 * x
                 x <- ifelse(is.na(Garage.2.Area), 0.182648401826E-01, Garage.2.Area)
                 predict <- predict +  -0.114873260807 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.83469777298, log.Building.Square.Feet)
                 predict <- predict +  0.794210501688 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.456621004566E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.483371483438 * x
                 x <- ifelse(is.na(Longitude), -87.8366240917, Longitude)
                 predict <- predict +  2.31261874507 * x
                 x <- ifelse(is.na(Latitude), 42.0271360799, Latitude)
                 predict <- predict +  2.87141230546 * x
                 x <- ifelse(is.na(Census.Tract), 662622.941748, Census.Tract)
                 predict <- predict +  -0.219296636133E-06 * x
                 x <- ifelse(is.na(Full.Baths), 2.50684931507, Full.Baths)
                 predict <- predict +  0.318719901686E-01 * x
                 x <- ifelse(is.na(Age), 10.3419203747, Age)
                 predict <- predict +  -0.898204026916E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 143.706730769, Number.of.Units)
                 predict <- predict +  -0.197725159733E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.345933014354E-01, Percent.Ownership)
                 predict <- predict +  0.549408263221 * x
                 x <- ifelse(is.na(Sale.Year), 2015.11007026, Sale.Year)
                 predict <- predict +  0.255224904818E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.80327868852, Sale.Month)
                 predict <- predict +  -0.114312773665E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1842.75207251, HoodSqFt)
                 predict <- predict +  0.281553707278E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 2433.35320646, PropClassSqFt)
                 predict <- predict +  -0.472843337947E-05 * x
                 predict <- predict +  -1.65614372843 * Rooms.NA
                 predict <- predict +  0.510563339457 * Longitude.NA
                 predict <- predict +  1.96545190175 * Total.Building.Square.Feet.NA
                 predict <- predict +  -0.565286069932 * Percent.Ownership.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 371
                 predict <- 33.8773913394
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.68243807625, log.Land.Square.Feet)
                 predict <- predict +  0.132313519232 * x
                 x <- ifelse(is.na(Rooms), 6.74379084967, Rooms)
                 predict <- predict +  0.889007903453E-02 * x
                 x <- ifelse(is.na(Bedrooms), 3.44313725490, Bedrooms)
                 predict <- predict +  -0.325006638621E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.423529411765, Fireplaces)
                 predict <- predict +  0.799327955486E-01 * x
                 x <- ifelse(is.na(Half.Baths), 0.594771241830, Half.Baths)
                 predict <- predict +  0.523705912478E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.67450980392, Garage.1.Area)
                 predict <- predict +  0.102833509181 * x
                 x <- ifelse(is.na(Garage.2.Area), 0.156862745098E-01, Garage.2.Area)
                 predict <- predict +  0.107020757126E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.38744670519, log.Building.Square.Feet)
                 predict <- predict +  0.543200645772 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.261437908497E-02, Number.of.Commercial.Units)
                 predict <- predict +  0.390147959380 * x
                 x <- ifelse(is.na(Longitude), -87.8694249343, Longitude)
                 predict <- predict +  1.90892781226 * x
                 x <- ifelse(is.na(Latitude), 42.0638095585, Latitude)
                 predict <- predict +  0.306821209565E-01 * x
                 x <- ifelse(is.na(Census.Tract), 731420.340827, Census.Tract)
                 predict <- predict +  0.222311086362E-06 * x
                 x <- ifelse(is.na(Full.Baths), 1.62745098039, Full.Baths)
                 predict <- predict +  0.372268442926E-01 * x
                 x <- ifelse(is.na(Age), 45.0527256479, Age)
                 predict <- predict +  -0.756128643955E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 112.262711864, Number.of.Units)
                 predict <- predict +  -0.260400156975E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.256179775281E-01, Percent.Ownership)
                 predict <- predict +  1.19923119629 * x
                 x <- ifelse(is.na(Sale.Year), 2014.95531725, Sale.Year)
                 predict <- predict +  0.692729948276E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.89365504915, Sale.Month)
                 predict <- predict +  0.760579524660E-02 * x
                 x <- ifelse(is.na(HoodSqFt), 1781.80755513, HoodSqFt)
                 predict <- predict +  0.109457632373E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 1746.60551304, PropClassSqFt)
                 predict <- predict +  0.138003627131E-04 * x
                 predict <- predict +  -0.824441466347 * Rooms.NA
                 predict <- predict +  -0.757546572896 * Longitude.NA
                 predict <- predict +  0.435290620620 * Percent.Ownership.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           } else {
             catvalues <- c("22","23","25","38")
             if(Town.Code %in% catvalues){
               nodeid <- 186
               predict <- 243.229699352
               x <- ifelse(is.na(log.Land.Square.Feet), 9.18938214997, log.Land.Square.Feet)
               predict <- predict +  0.227624825835 * x
               x <- ifelse(is.na(Rooms), 6.69421487603, Rooms)
               predict <- predict +  0.170969414319E-01 * x
               x <- ifelse(is.na(Bedrooms), 3.39256198347, Bedrooms)
               predict <- predict +  0.418716797369E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.595041322314, Fireplaces)
               predict <- predict +  0.125091257160 * x
               x <- ifelse(is.na(Half.Baths), 0.500000000000, Half.Baths)
               predict <- predict +  0.747694504923E-01 * x
               x <- ifelse(is.na(Garage.1.Area), 1.78512396694, Garage.1.Area)
               predict <- predict +  -0.162665258409E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.38743620733, log.Building.Square.Feet)
               predict <- predict +  0.295160599601 * x
               x <- ifelse(is.na(Longitude), -87.8673208021, Longitude)
               predict <- predict +  1.03162406479 * x
               x <- ifelse(is.na(Latitude), 42.0798455527, Latitude)
               predict <- predict +  -2.77460748244 * x
               x <- ifelse(is.na(Census.Tract), 803033.253061, Census.Tract)
               predict <- predict +  -0.819301573687E-04 * x
               x <- ifelse(is.na(Full.Baths), 1.57024793388, Full.Baths)
               predict <- predict +  0.283035507426E-01 * x
               x <- ifelse(is.na(Age), 71.6612903226, Age)
               predict <- predict +  0.128952484946E-02 * x
               x <- ifelse(is.na(Number.of.Units), 63.0000000000, Number.of.Units)
               predict <- predict +  0.544703914040E-02 * x
               x <- ifelse(is.na(Percent.Ownership), 0.183333333333E-01, Percent.Ownership)
               predict <- predict +  31.6070779629 * x
               x <- ifelse(is.na(Sale.Year), 2015.29435484, Sale.Year)
               predict <- predict +  0.184651348776E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.72177419355, Sale.Month)
               predict <- predict +  0.535703137230E-02 * x
               x <- ifelse(is.na(HoodSqFt), 1825.62131134, HoodSqFt)
               predict <- predict +  -0.693452562306E-04 * x
               x <- ifelse(is.na(PropClassSqFt), 1721.31668810, PropClassSqFt)
               predict <- predict +  0.435955690278E-04 * x
               predict <- predict +  -1.44640729289 * Rooms.NA
               predict <- predict +  -0.562643232676E-01 * Longitude.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               nodeid <- 187
               predict <- -165.188173871
               x <- ifelse(is.na(log.Land.Square.Feet), 8.54867977727, log.Land.Square.Feet)
               predict <- predict +  0.241287619055 * x
               x <- ifelse(is.na(Rooms), 6.72606382979, Rooms)
               predict <- predict +  0.396996066716E-01 * x
               x <- ifelse(is.na(Bedrooms), 3.32446808511, Bedrooms)
               predict <- predict +  -0.572302969780E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.930851063830E-01, Fireplaces)
               predict <- predict +  0.796889689936E-01 * x
               x <- ifelse(is.na(Half.Baths), 0.180851063830, Half.Baths)
               predict <- predict +  -0.529387684030E-01 * x
               x <- ifelse(is.na(Garage.1.Area), 1.56117021277, Garage.1.Area)
               predict <- predict +  0.511770932301E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.22829554404, log.Building.Square.Feet)
               predict <- predict +  0.298362594419 * x
               x <- ifelse(is.na(Number.of.Commercial.Units), 0.186170212766E-01, Number.of.Commercial.Units)
               predict <- predict +  -0.129978717345 * x
               x <- ifelse(is.na(Longitude), -87.7744543505, Longitude)
               predict <- predict +  1.03692263800 * x
               x <- ifelse(is.na(Latitude), 41.9253152311, Latitude)
               predict <- predict +  0.982226692471 * x
               x <- ifelse(is.na(Census.Tract), 462188.198511, Census.Tract)
               predict <- predict +  -0.161543828587E-06 * x
               x <- ifelse(is.na(Total.Building.Square.Feet), 2826.90909091, Total.Building.Square.Feet)
               predict <- predict +  -0.129964019898E-04 * x
               x <- ifelse(is.na(Full.Baths), 1.51063829787, Full.Baths)
               predict <- predict +  -0.171275051890E-01 * x
               x <- ifelse(is.na(Age), 93.3086419753, Age)
               predict <- predict +  -0.618333760267E-02 * x
               x <- ifelse(is.na(Number.of.Units), 95.3793103448, Number.of.Units)
               predict <- predict +  -0.887785615661E-02 * x
               x <- ifelse(is.na(Percent.Ownership), 0.282758620690E-01, Percent.Ownership)
               predict <- predict +  -2.42585979262 * x
               x <- ifelse(is.na(Sale.Year), 2015.11358025, Sale.Year)
               predict <- predict +  0.111396563250 * x
               x <- ifelse(is.na(Sale.Month), 7.08888888889, Sale.Month)
               predict <- predict +  0.191186057698E-01 * x
               x <- ifelse(is.na(HoodSqFt), 1692.79812554, HoodSqFt)
               predict <- predict +  -0.430528832856E-03 * x
               x <- ifelse(is.na(PropClassSqFt), 1743.11029954, PropClassSqFt)
               predict <- predict +  -0.960634554525E-05 * x
               predict <- predict +  -0.816667318166E-01 * Rooms.NA
               predict <- predict +  0.961513063288 * Longitude.NA
               predict <- predict +  0.601087466369 * Total.Building.Square.Feet.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             }
           }
         } else {
           catvalues <- c("10","13","20","26","27","41","62","70","83","96","130","170","200","250","260","350","390","560")
           if(Neighborhood.Code %in% catvalues){
             catvalues <- c("70")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 188
               predict <- -65.0630697324
               x <- ifelse(is.na(log.Land.Square.Feet), 9.55847621909, log.Land.Square.Feet)
               predict <- predict +  0.790266505721E-01 * x
               x <- ifelse(is.na(Rooms), 7.45083207262, Rooms)
               predict <- predict +  -0.974002565051E-02 * x
               x <- ifelse(is.na(Bedrooms), 3.69440242057, Bedrooms)
               predict <- predict +  -0.407580949581E-02 * x
               x <- ifelse(is.na(Fireplaces), 0.291981845688, Fireplaces)
               predict <- predict +  0.607530603540E-01 * x
               x <- ifelse(is.na(Half.Baths), 0.462934947050, Half.Baths)
               predict <- predict +  0.117708055988E-01 * x
               x <- ifelse(is.na(Garage.1.Area), 1.70650529501, Garage.1.Area)
               predict <- predict +  0.917130334385E-02 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.42258757297, log.Building.Square.Feet)
               predict <- predict +  0.531230336972 * x
               x <- ifelse(is.na(Number.of.Commercial.Units), 0.166414523449E-01, Number.of.Commercial.Units)
               predict <- predict +  -0.586412885376 * x
               x <- ifelse(is.na(Longitude), -87.8815456783, Longitude)
               predict <- predict +  1.53387876796 * x
               x <- ifelse(is.na(Latitude), 42.0193727036, Latitude)
               predict <- predict +  0.794196197603 * x
               x <- ifelse(is.na(Census.Tract), 607360.320526, Census.Tract)
               predict <- predict +  -0.611892878405E-07 * x
               x <- ifelse(is.na(Total.Building.Square.Feet), 9174.80000000, Total.Building.Square.Feet)
               predict <- predict +  -0.249460139918E-03 * x
               x <- ifelse(is.na(Full.Baths), 1.85930408472, Full.Baths)
               predict <- predict +  0.788834679873E-03 * x
               x <- ifelse(is.na(Age), 60.2888665998, Age)
               predict <- predict +  -0.352631737552E-02 * x
               x <- ifelse(is.na(Number.of.Units), 123.642857143, Number.of.Units)
               predict <- predict +  -0.183223385049E-02 * x
               x <- ifelse(is.na(Percent.Ownership), 0.383086053412E-01, Percent.Ownership)
               predict <- predict +  3.06322847589 * x
               x <- ifelse(is.na(Sale.Year), 2015.12236710, Sale.Year)
               predict <- predict +  0.861978131834E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.92176529589, Sale.Month)
               predict <- predict +  0.149936942074E-01 * x
               x <- ifelse(is.na(PropClassSqFt), 1935.41572452, PropClassSqFt)
               predict <- predict +  -0.547294837040E-04 * x
               predict <- predict +  0.487823807342 * Rooms.NA
               predict <- predict +  -0.742029457261E-01 * Longitude.NA
               predict <- predict +  1.81712051862 * Total.Building.Square.Feet.NA
               predict <- predict +  1.68691963611 * Percent.Ownership.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               catvalues <- c("23","25","29","33")
               if(Town.Code %in% catvalues){
                 nodeid <- 378
                 predict <- 101.599322396
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.0609293838, log.Land.Square.Feet)
                 predict <- predict +  0.175294695686 * x
                 x <- ifelse(is.na(Rooms), 8.01798561151, Rooms)
                 predict <- predict +  0.996921245140E-02 * x
                 x <- ifelse(is.na(Bedrooms), 3.96762589928, Bedrooms)
                 predict <- predict +  -0.114220358283E-01 * x
                 x <- ifelse(is.na(Fireplaces), 1.03237410072, Fireplaces)
                 predict <- predict +  0.689111340391E-01 * x
                 x <- ifelse(is.na(Half.Baths), 0.877697841727, Half.Baths)
                 predict <- predict +  0.824106787288E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.65107913669, Garage.1.Area)
                 predict <- predict +  0.611623501068E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.82519563155, log.Building.Square.Feet)
                 predict <- predict +  0.654052684308 * x
                 x <- ifelse(is.na(Longitude), -87.8923867645, Longitude)
                 predict <- predict +  2.20095033515 * x
                 x <- ifelse(is.na(Latitude), 42.1008793871, Latitude)
                 predict <- predict +  1.07979101042 * x
                 x <- ifelse(is.na(Census.Tract), 802572.425641, Census.Tract)
                 predict <- predict +  0.368652735604E-04 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 6555.50000000, Total.Building.Square.Feet)
                 predict <- predict +  0.391112249843E-04 * x
                 x <- ifelse(is.na(Full.Baths), 2.32014388489, Full.Baths)
                 predict <- predict +  0.183102455400 * x
                 x <- ifelse(is.na(Age), 40.6377551020, Age)
                 predict <- predict +  -0.195279873027E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 91.6228070175, Number.of.Units)
                 predict <- predict +  -0.355321676525E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.302631578947E-01, Percent.Ownership)
                 predict <- predict +  0.640994170457 * x
                 x <- ifelse(is.na(Sale.Year), 2014.93112245, Sale.Year)
                 predict <- predict +  0.111760033598E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.69132653061, Sale.Month)
                 predict <- predict +  0.781845363965E-02 * x
                 x <- ifelse(is.na(HoodSqFt), 1993.96702251, HoodSqFt)
                 predict <- predict +  0.152824956692E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 2474.17338823, PropClassSqFt)
                 predict <- predict +  -0.895106282085E-04 * x
                 predict <- predict +  -1.19301889548 * Rooms.NA
                 predict <- predict +  0.217930856514 * Longitude.NA
                 predict <- predict +  0.773781331236E-01 * Total.Building.Square.Feet.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 379
                 predict <- 27.6685651345
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.59947215048, log.Land.Square.Feet)
                 predict <- predict +  0.195310812991E-01 * x
                 x <- ifelse(is.na(Rooms), 6.86884003033, Rooms)
                 predict <- predict +  -0.224063288762E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.50341167551, Bedrooms)
                 predict <- predict +  0.520048328337E-02 * x
                 x <- ifelse(is.na(Fireplaces), 0.218347232752, Fireplaces)
                 predict <- predict +  0.150015885064 * x
                 x <- ifelse(is.na(Half.Baths), 0.438968915845, Half.Baths)
                 predict <- predict +  0.963789256490E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.70432145565, Garage.1.Area)
                 predict <- predict +  0.733839298952E-01 * x
                 x <- ifelse(is.na(Garage.2.Area), 0.379075056861E-02, Garage.2.Area)
                 predict <- predict +  -0.429175480974E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.36296021302, log.Building.Square.Feet)
                 predict <- predict +  0.536609357865 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.128885519333E-01, Number.of.Commercial.Units)
                 predict <- predict +  -0.245748271341 * x
                 x <- ifelse(is.na(Longitude), -87.8014827566, Longitude)
                 predict <- predict +  2.57598757442 * x
                 x <- ifelse(is.na(Latitude), 41.9708690028, Latitude)
                 predict <- predict +  0.789953208646 * x
                 x <- ifelse(is.na(Census.Tract), 523070.788479, Census.Tract)
                 predict <- predict +  0.908808531548E-07 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 2726.50000000, Total.Building.Square.Feet)
                 predict <- predict +  0.495911627772E-03 * x
                 x <- ifelse(is.na(Full.Baths), 1.61865049280, Full.Baths)
                 predict <- predict +  0.916929276422E-01 * x
                 x <- ifelse(is.na(Age), 55.2816525988, Age)
                 predict <- predict +  -0.462586819966E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 278.993562232, Number.of.Units)
                 predict <- predict +  0.339095161529E-03 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.245815450644E-01, Percent.Ownership)
                 predict <- predict +  -1.57904963098 * x
                 x <- ifelse(is.na(Sale.Year), 2015.05775211, Sale.Year)
                 predict <- predict +  0.851943794804E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.84007107952, Sale.Month)
                 predict <- predict +  0.120853686850E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1839.29972253, HoodSqFt)
                 predict <- predict +  -0.658913491661E-05 * x
                 x <- ifelse(is.na(PropClassSqFt), 1724.04046438, PropClassSqFt)
                 predict <- predict +  -0.666224367962E-04 * x
                 predict <- predict +  -0.733264060283 * Rooms.NA
                 predict <- predict +  0.271750215612 * Longitude.NA
                 predict <- predict +  0.725038914528 * Total.Building.Square.Feet.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           } else {
             catvalues <- c("32","102","230")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 190
               predict <- -698.287713850
               x <- ifelse(is.na(log.Land.Square.Feet), 9.98523982653, log.Land.Square.Feet)
               predict <- predict +  0.379153462478 * x
               x <- ifelse(is.na(Rooms), 7.69369369369, Rooms)
               predict <- predict +  0.944280506670E-01 * x
               x <- ifelse(is.na(Bedrooms), 3.90990990991, Bedrooms)
               predict <- predict +  -0.113965788052E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.810810810811, Fireplaces)
               predict <- predict +  0.777812651452E-01 * x
               x <- ifelse(is.na(Half.Baths), 0.864864864865, Half.Baths)
               predict <- predict +  -0.738314372775E-01 * x
               x <- ifelse(is.na(Garage.1.Area), 1.65765765766, Garage.1.Area)
               predict <- predict +  0.444175052703E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.72147789580, log.Building.Square.Feet)
               predict <- predict +  0.268426279377E-01 * x
               x <- ifelse(is.na(Longitude), -87.9402414146, Longitude)
               predict <- predict +  7.60573297264 * x
               x <- ifelse(is.na(Latitude), 42.0923689630, Latitude)
               predict <- predict +  16.5457017466 * x
               x <- ifelse(is.na(Census.Tract), 802626.301471, Census.Tract)
               predict <- predict +  0.773665930711E-03 * x
               x <- ifelse(is.na(Full.Baths), 1.98198198198, Full.Baths)
               predict <- predict +  0.199995630915E-02 * x
               x <- ifelse(is.na(Age), 39.0441176471, Age)
               predict <- predict +  -0.171981033264E-02 * x
               x <- ifelse(is.na(Number.of.Units), 153.080000000, Number.of.Units)
               predict <- predict +  -0.161220161771E-02 * x
               x <- ifelse(is.na(Percent.Ownership), 0.840000000000E-02, Percent.Ownership)
               predict <- predict +  15.0198594721 * x
               x <- ifelse(is.na(Sale.Year), 2015.00735294, Sale.Year)
               predict <- predict +  0.286831220281E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.96323529412, Sale.Month)
               predict <- predict +  -0.150330635185E-02 * x
               x <- ifelse(is.na(HoodSqFt), 2112.59765112, HoodSqFt)
               predict <- predict +  0.197749064463E-03 * x
               x <- ifelse(is.na(PropClassSqFt), 2320.20079429, PropClassSqFt)
               predict <- predict +  0.105617996253E-03 * x
               predict <- predict +  -2.13614490731 * Rooms.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               if(!is.na(Age) & Age <= 33.5000000000 ){
                 nodeid <- 382
                 predict <- 111.055308895
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.6544199759, log.Land.Square.Feet)
                 predict <- predict +  0.166006744840 * x
                 x <- ifelse(is.na(Rooms), 7.45731707317, Rooms)
                 predict <- predict +  -0.342807643954E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.36280487805, Bedrooms)
                 predict <- predict +  0.297186003817E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.814024390244, Fireplaces)
                 predict <- predict +  0.199959767073 * x
                 x <- ifelse(is.na(Half.Baths), 0.932926829268, Half.Baths)
                 predict <- predict +  0.146616815559 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.35365853659, Garage.1.Area)
                 predict <- predict +  0.108605728627 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.73534991686, log.Building.Square.Feet)
                 predict <- predict +  0.557888785659 * x
                 x <- ifelse(is.na(Longitude), -87.8702347102, Longitude)
                 predict <- predict +  3.12533924654 * x
                 x <- ifelse(is.na(Latitude), 42.0085379731, Latitude)
                 predict <- predict +  0.965391407244 * x
                 x <- ifelse(is.na(Census.Tract), 656948.514954, Census.Tract)
                 predict <- predict +  0.314526323388E-06 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 9299.54929577, Total.Building.Square.Feet)
                 predict <- predict +  0.768457039295E-05 * x
                 x <- ifelse(is.na(Full.Baths), 2.23780487805, Full.Baths)
                 predict <- predict +  0.452130374144E-01 * x
                 x <- ifelse(is.na(Age), 18.4420549582, Age)
                 predict <- predict +  -0.139824835696E-01 * x
                 x <- ifelse(is.na(Number.of.Units), 149.803536346, Number.of.Units)
                 predict <- predict +  -0.109807166757E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.293516699411E-01, Percent.Ownership)
                 predict <- predict +  1.85860519289 * x
                 x <- ifelse(is.na(Sale.Year), 2014.93906810, Sale.Year)
                 predict <- predict +  0.640475458905E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.69653524492, Sale.Month)
                 predict <- predict +  0.107949505677E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1987.35066058, HoodSqFt)
                 predict <- predict +  -0.125978257105E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 2282.58716045, PropClassSqFt)
                 predict <- predict +  0.707630039650E-04 * x
                 predict <- predict +  -1.38438047068 * Rooms.NA
                 predict <- predict +  -0.556256709899 * Longitude.NA
                 predict <- predict +  0.821577987637 * Total.Building.Square.Feet.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 383
                 predict <- 3.15841189203
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.49108210693, log.Land.Square.Feet)
                 predict <- predict +  0.204114972015 * x
                 x <- ifelse(is.na(Rooms), 7.13295774648, Rooms)
                 predict <- predict +  -0.683436885321E-02 * x
                 x <- ifelse(is.na(Bedrooms), 3.62140845070, Bedrooms)
                 predict <- predict +  -0.324922191720E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.345352112676, Fireplaces)
                 predict <- predict +  0.108308601195 * x
                 x <- ifelse(is.na(Half.Baths), 0.495774647887, Half.Baths)
                 predict <- predict +  0.798323774799E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.69183098592, Garage.1.Area)
                 predict <- predict +  0.270826212009E-01 * x
                 x <- ifelse(is.na(Garage.2.Area), 0.225352112676E-02, Garage.2.Area)
                 predict <- predict +  0.403095321763E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.39759516829, log.Building.Square.Feet)
                 predict <- predict +  0.676965387013 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.957746478873E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.252706769578 * x
                 x <- ifelse(is.na(Longitude), -87.8393821861, Longitude)
                 predict <- predict +  2.82068421853 * x
                 x <- ifelse(is.na(Latitude), 42.0051168388, Latitude)
                 predict <- predict +  1.32259041281 * x
                 x <- ifelse(is.na(Census.Tract), 583405.457955, Census.Tract)
                 predict <- predict +  0.276207857448E-06 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 3504.80000000, Total.Building.Square.Feet)
                 predict <- predict +  0.174930433189E-03 * x
                 x <- ifelse(is.na(Full.Baths), 1.65352112676, Full.Baths)
                 predict <- predict +  0.679350309662E-01 * x
                 x <- ifelse(is.na(Age), 63.8620123203, Age)
                 predict <- predict +  -0.544918868549E-04 * x
                 x <- ifelse(is.na(Number.of.Units), 161.883333333, Number.of.Units)
                 predict <- predict +  -0.194315940503E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.429864253394E-01, Percent.Ownership)
                 predict <- predict +  2.72680705070 * x
                 x <- ifelse(is.na(Sale.Year), 2015.04106776, Sale.Year)
                 predict <- predict +  0.955828014254E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.80451745380, Sale.Month)
                 predict <- predict +  0.107363903642E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1939.55325618, HoodSqFt)
                 predict <- predict +  -0.891614434756E-04 * x
                 x <- ifelse(is.na(PropClassSqFt), 1783.63457802, PropClassSqFt)
                 predict <- predict +  -0.757399407577E-04 * x
                 predict <- predict +  -0.419565064379 * Rooms.NA
                 predict <- predict +  0.110737892937 * Longitude.NA
                 predict <- predict +  0.608559282940 * Total.Building.Square.Feet.NA
                 predict <- predict +  1.18648540608 * Percent.Ownership.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         }
       }
     }
   }
 } else {
   catvalues <- c("10","17","19","24","26","73","74")
   if(Town.Code %in% catvalues){
     catvalues <- c("10","38","52","112","140","220")
     if(Neighborhood.Code %in% catvalues){
       nodeid <- 12
       predict <- -326.633994839
       x <- ifelse(is.na(log.Land.Square.Feet), 9.10916438288, log.Land.Square.Feet)
       predict <- predict +  0.219030346921 * x
       x <- ifelse(is.na(Rooms), 6.56728232190, Rooms)
       predict <- predict +  -0.674637623347E-01 * x
       x <- ifelse(is.na(Bedrooms), 3.37203166227, Bedrooms)
       predict <- predict +  0.753784456271E-01 * x
       x <- ifelse(is.na(Fireplaces), 0.184696569921, Fireplaces)
       predict <- predict +  0.168692185190E-01 * x
       x <- ifelse(is.na(Half.Baths), 0.403693931398, Half.Baths)
       predict <- predict +  -0.754653721579E-01 * x
       x <- ifelse(is.na(Garage.1.Area), 1.55672823219, Garage.1.Area)
       predict <- predict +  0.367673715001E-01 * x
       x <- ifelse(is.na(log.Building.Square.Feet), 7.24626352350, log.Building.Square.Feet)
       predict <- predict +  0.835164304106 * x
       x <- ifelse(is.na(Number.of.Commercial.Units), 0.527704485488E-02, Number.of.Commercial.Units)
       predict <- predict +  0.340737855451 * x
       x <- ifelse(is.na(Longitude), -87.7832797109, Longitude)
       predict <- predict +  -1.23396155876 * x
       x <- ifelse(is.na(Latitude), 41.9753416770, Latitude)
       predict <- predict +  1.76721236077 * x
       x <- ifelse(is.na(Census.Tract), 804090.001852, Census.Tract)
       predict <- predict +  0.658927801632E-06 * x
       x <- ifelse(is.na(Full.Baths), 1.48021108179, Full.Baths)
       predict <- predict +  0.839629579159E-02 * x
       x <- ifelse(is.na(Age), 53.2702205882, Age)
       predict <- predict +  -0.436258687255E-02 * x
       x <- ifelse(is.na(Number.of.Units), 57.7515151515, Number.of.Units)
       predict <- predict +  0.253560887734E-02 * x
       x <- ifelse(is.na(Percent.Ownership), 0.384939759036E-01, Percent.Ownership)
       predict <- predict +  4.21057774992 * x
       x <- ifelse(is.na(Sale.Year), 2014.99264706, Sale.Year)
       predict <- predict +  0.721775018795E-01 * x
       x <- ifelse(is.na(Sale.Month), 6.76102941176, Sale.Month)
       predict <- predict +  0.189760329155E-04 * x
       x <- ifelse(is.na(HoodSqFt), 1888.63927019, HoodSqFt)
       predict <- predict +  0.194301954280E-03 * x
       x <- ifelse(is.na(PropClassSqFt), 1573.74521908, PropClassSqFt)
       predict <- predict +  -0.862089511215E-04 * x
       predict <- predict +  1.33071700863 * Rooms.NA
       predict <- predict +  0.176301429756 * Longitude.NA
       predict <- predict +  2.51833516435 * Percent.Ownership.NA
       predict <- min(max(predict, 9.21094019205),15.8169906933)
     } else {
       catvalues <- c("20","23","24","34","81","93")
       if(Neighborhood.Code %in% catvalues){
         if(!is.na(Age) & Age <= 55.5000000000 ){
           nodeid <- 52
           predict <- -48.6143738850
           x <- ifelse(is.na(log.Land.Square.Feet), 9.05394421485, log.Land.Square.Feet)
           predict <- predict +  0.319363837345 * x
           x <- ifelse(is.na(Rooms), 7.83333333333, Rooms)
           predict <- predict +  -0.261394564044E-01 * x
           x <- ifelse(is.na(Bedrooms), 3.81140350877, Bedrooms)
           predict <- predict +  0.814614478796E-01 * x
           x <- ifelse(is.na(Fireplaces), 0.938596491228, Fireplaces)
           predict <- predict +  0.507186452042E-01 * x
           x <- ifelse(is.na(Half.Baths), 0.929824561404, Half.Baths)
           predict <- predict +  0.230434639979E-01 * x
           x <- ifelse(is.na(Garage.1.Area), 1.42982456140, Garage.1.Area)
           predict <- predict +  0.580426905522E-01 * x
           x <- ifelse(is.na(Garage.2.Area), 0.263157894737E-01, Garage.2.Area)
           predict <- predict +  0.354467829523E-01 * x
           x <- ifelse(is.na(log.Building.Square.Feet), 7.75429602908, log.Building.Square.Feet)
           predict <- predict +  -0.184987018445 * x
           x <- ifelse(is.na(Number.of.Commercial.Units), 0.438596491228E-02, Number.of.Commercial.Units)
           predict <- predict +  -0.148595127393 * x
           x <- ifelse(is.na(Longitude), -87.7523758417, Longitude)
           predict <- predict +  0.941689131548 * x
           x <- ifelse(is.na(Latitude), 41.9659130218, Latitude)
           predict <- predict +  0.802182471860 * x
           x <- ifelse(is.na(Census.Tract), 353607.644172, Census.Tract)
           predict <- predict +  -0.845355592617E-06 * x
           x <- ifelse(is.na(Full.Baths), 2.53070175439, Full.Baths)
           predict <- predict +  0.445173192582E-01 * x
           x <- ifelse(is.na(Age), 21.1801619433, Age)
           predict <- predict +  -0.200962728755E-01 * x
           x <- ifelse(is.na(Number.of.Units), 29.9285714286, Number.of.Units)
           predict <- predict +  -0.537031337158E-02 * x
           x <- ifelse(is.na(Percent.Ownership), 0.131879699248, Percent.Ownership)
           predict <- predict +  1.68882315501 * x
           x <- ifelse(is.na(Sale.Year), 2014.98380567, Sale.Year)
           predict <- predict +  0.542225352322E-01 * x
           x <- ifelse(is.na(Sale.Month), 6.50404858300, Sale.Month)
           predict <- predict +  0.751451331161E-02 * x
           x <- ifelse(is.na(HoodSqFt), 2055.42856554, HoodSqFt)
           predict <- predict +  0.299882319981E-03 * x
           x <- ifelse(is.na(PropClassSqFt), 2371.07509444, PropClassSqFt)
           predict <- predict +  0.104186149297E-03 * x
           predict <- predict +  -1.29044287957 * Rooms.NA
           predict <- predict +  -0.961380327680E-01 * Longitude.NA
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         } else {
           nodeid <- 53
           predict <- 28.8792708738
           x <- ifelse(is.na(log.Land.Square.Feet), 8.53135404466, log.Land.Square.Feet)
           predict <- predict +  0.376147986682 * x
           x <- ifelse(is.na(Rooms), 7.53975903614, Rooms)
           predict <- predict +  -0.400731087533E-02 * x
           x <- ifelse(is.na(Bedrooms), 3.66746987952, Bedrooms)
           predict <- predict +  -0.418348361054E-02 * x
           x <- ifelse(is.na(Fireplaces), 0.272289156627, Fireplaces)
           predict <- predict +  0.289513054232E-01 * x
           x <- ifelse(is.na(Half.Baths), 0.506024096386, Half.Baths)
           predict <- predict +  -0.713275217294E-02 * x
           x <- ifelse(is.na(Garage.1.Area), 1.40963855422, Garage.1.Area)
           predict <- predict +  0.181912131823E-01 * x
           x <- ifelse(is.na(Garage.2.Area), 0.481927710843E-02, Garage.2.Area)
           predict <- predict +  0.108849561565 * x
           x <- ifelse(is.na(log.Building.Square.Feet), 7.40092404372, log.Building.Square.Feet)
           predict <- predict +  0.417161888246 * x
           x <- ifelse(is.na(Number.of.Commercial.Units), 0.192771084337E-01, Number.of.Commercial.Units)
           predict <- predict +  -0.297405913021 * x
           x <- ifelse(is.na(Longitude), -87.7081775109, Longitude)
           predict <- predict +  2.08870248961 * x
           x <- ifelse(is.na(Latitude), 41.9861447907, Latitude)
           predict <- predict +  0.523877493127 * x
           x <- ifelse(is.na(Census.Tract), 472714.420000, Census.Tract)
           predict <- predict +  -0.460489614313E-06 * x
           x <- ifelse(is.na(Total.Building.Square.Feet), 3409.00000000, Total.Building.Square.Feet)
           predict <- predict +  -0.120529788047E-03 * x
           x <- ifelse(is.na(Full.Baths), 1.70602409639, Full.Baths)
           predict <- predict +  -0.497636093259E-01 * x
           x <- ifelse(is.na(Age), 87.0722433460, Age)
           predict <- predict +  0.882223156998E-02 * x
           x <- ifelse(is.na(Number.of.Units), 22.9729729730, Number.of.Units)
           predict <- predict +  -0.407585952852E-03 * x
           x <- ifelse(is.na(Percent.Ownership), 0.116371681416, Percent.Ownership)
           predict <- predict +  1.30078688789 * x
           x <- ifelse(is.na(Sale.Year), 2015.02091255, Sale.Year)
           predict <- predict +  0.680082657074E-01 * x
           x <- ifelse(is.na(Sale.Month), 6.75095057034, Sale.Month)
           predict <- predict +  0.186460845828E-01 * x
           x <- ifelse(is.na(HoodSqFt), 1919.19244374, HoodSqFt)
           predict <- predict +  0.265963261759E-03 * x
           x <- ifelse(is.na(PropClassSqFt), 1836.99596535, PropClassSqFt)
           predict <- predict +  0.123287894187E-03 * x
           predict <- predict +  -0.155855404895 * Rooms.NA
           predict <- predict +  -0.166550015713E-01 * Longitude.NA
           predict <- predict +  0.240430954884 * Total.Building.Square.Feet.NA
           predict <- predict +  0.765896207009 * Percent.Ownership.NA
           predict <- min(max(predict, 9.21094019205),15.8169906933)
         }
       } else {
         catvalues <- c("11","22","31","32","43","50","60","62","63","71","80","101","130")
         if(Neighborhood.Code %in% catvalues){
           catvalues <- c("31","32","43","50","71","101")
           if(Neighborhood.Code %in% catvalues){
             if(!is.na(Age) & Age <= 69.5000000000 ){
               nodeid <- 216
               predict <- -67.4546863501
               x <- ifelse(is.na(log.Land.Square.Feet), 9.69329329745, log.Land.Square.Feet)
               predict <- predict +  0.225102510125 * x
               x <- ifelse(is.na(Rooms), 6.95564516129, Rooms)
               predict <- predict +  0.110646301480E-01 * x
               x <- ifelse(is.na(Bedrooms), 3.42741935484, Bedrooms)
               predict <- predict +  0.181898028954E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.504032258065, Fireplaces)
               predict <- predict +  0.105613722148 * x
               x <- ifelse(is.na(Half.Baths), 0.643145161290, Half.Baths)
               predict <- predict +  0.530217968418E-01 * x
               x <- ifelse(is.na(Garage.1.Area), 1.45362903226, Garage.1.Area)
               predict <- predict +  0.989021567129E-01 * x
               x <- ifelse(is.na(Garage.2.Area), 0.806451612903E-02, Garage.2.Area)
               predict <- predict +  0.134246897251E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.44121449038, log.Building.Square.Feet)
               predict <- predict +  0.274078170913 * x
               x <- ifelse(is.na(Number.of.Commercial.Units), 0.201612903226E-02, Number.of.Commercial.Units)
               predict <- predict +  -0.434629144784E-01 * x
               x <- ifelse(is.na(Longitude), -87.7313764186, Longitude)
               predict <- predict +  1.84004089059 * x
               x <- ifelse(is.na(Latitude), 42.0055259155, Latitude)
               predict <- predict +  0.676246305453 * x
               x <- ifelse(is.na(Census.Tract), 511370.746206, Census.Tract)
               predict <- predict +  -0.283053408751E-06 * x
               x <- ifelse(is.na(Full.Baths), 1.75806451613, Full.Baths)
               predict <- predict +  0.531775301307E-02 * x
               x <- ifelse(is.na(Age), 40.7165551839, Age)
               predict <- predict +  -0.133190200515E-01 * x
               x <- ifelse(is.na(Number.of.Units), 195.682857143, Number.of.Units)
               predict <- predict +  -0.750519504511E-03 * x
               x <- ifelse(is.na(Percent.Ownership), 0.519971870605E-01, Percent.Ownership)
               predict <- predict +  2.86838571144 * x
               x <- ifelse(is.na(Sale.Year), 2015.05100334, Sale.Year)
               predict <- predict +  0.103550493869 * x
               x <- ifelse(is.na(Sale.Month), 6.74163879599, Sale.Month)
               predict <- predict +  0.109647672388E-01 * x
               x <- ifelse(is.na(HoodSqFt), 1945.80258559, HoodSqFt)
               predict <- predict +  -0.249810409330E-03 * x
               x <- ifelse(is.na(PropClassSqFt), 1879.01595396, PropClassSqFt)
               predict <- predict +  0.562255844856E-04 * x
               predict <- predict +  -0.114637137737 * Rooms.NA
               predict <- predict +  0.179273254403E-01 * Longitude.NA
               predict <- predict +  -0.150505585312E-01 * Total.Building.Square.Feet.NA
               predict <- predict +  1.42782526957 * Percent.Ownership.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               nodeid <- 217
               predict <- 504.625016268
               x <- ifelse(is.na(log.Land.Square.Feet), 8.94977050038, log.Land.Square.Feet)
               predict <- predict +  0.358942162821 * x
               x <- ifelse(is.na(Rooms), 10.3588516746, Rooms)
               predict <- predict +  -0.354529517041E-01 * x
               x <- ifelse(is.na(Bedrooms), 4.96172248804, Bedrooms)
               predict <- predict +  0.230133445621E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.401913875598, Fireplaces)
               predict <- predict +  0.732603695036E-01 * x
               x <- ifelse(is.na(Half.Baths), 0.435406698565, Half.Baths)
               predict <- predict +  -0.376222872505E-01 * x
               x <- ifelse(is.na(Garage.1.Area), 1.51674641148, Garage.1.Area)
               predict <- predict +  -0.130227862743E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.77237186867, log.Building.Square.Feet)
               predict <- predict +  0.504470229424 * x
               x <- ifelse(is.na(Number.of.Commercial.Units), 0.191387559809E-01, Number.of.Commercial.Units)
               predict <- predict +  0.112997622903 * x
               x <- ifelse(is.na(Longitude), -87.6737202535, Longitude)
               predict <- predict +  5.41756152465 * x
               x <- ifelse(is.na(Latitude), 41.9835238053, Latitude)
               predict <- predict +  -2.43512390071 * x
               x <- ifelse(is.na(Census.Tract), 197058.701400, Census.Tract)
               predict <- predict +  -0.299093238487E-07 * x
               x <- ifelse(is.na(Total.Building.Square.Feet), 4150.00000000, Total.Building.Square.Feet)
               predict <- predict +  0.215975768521E-03 * x
               x <- ifelse(is.na(Full.Baths), 2.24401913876, Full.Baths)
               predict <- predict +  0.105433545505 * x
               x <- ifelse(is.na(Age), 100.438988095, Age)
               predict <- predict +  0.215051676937E-02 * x
               x <- ifelse(is.na(Number.of.Units), 19.9935205184, Number.of.Units)
               predict <- predict +  -0.105983357257E-01 * x
               x <- ifelse(is.na(Percent.Ownership), 0.955075593952E-01, Percent.Ownership)
               predict <- predict +  2.96208606362 * x
               x <- ifelse(is.na(Sale.Year), 2015.09523810, Sale.Year)
               predict <- predict +  0.382153123392E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.92559523810, Sale.Month)
               predict <- predict +  0.524414926637E-02 * x
               x <- ifelse(is.na(HoodSqFt), 1899.77541077, HoodSqFt)
               predict <- predict +  0.310495399958E-03 * x
               x <- ifelse(is.na(PropClassSqFt), 2304.02432144, PropClassSqFt)
               predict <- predict +  -0.198672039043E-04 * x
               predict <- predict +  -1.32475564610 * Rooms.NA
               predict <- predict +  -0.523879396840E-01 * Longitude.NA
               predict <- predict +  0.219827581243 * Total.Building.Square.Feet.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             }
           } else {
             catvalues <- c("60","130")
             if(Neighborhood.Code %in% catvalues){
               if(!is.na(Latitude) & Latitude <= 41.9698050268 ){
                 nodeid <- 436
                 predict <- 75.9394389274
                 x <- ifelse(is.na(log.Land.Square.Feet), 10.4058200670, log.Land.Square.Feet)
                 predict <- predict +  0.108183957797 * x
                 x <- ifelse(is.na(Rooms), 7.58585858586, Rooms)
                 predict <- predict +  -0.713406479794E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.59595959596, Bedrooms)
                 predict <- predict +  0.837475986208E-01 * x
                 x <- ifelse(is.na(Fireplaces), 1.01010101010, Fireplaces)
                 predict <- predict +  0.169029790988 * x
                 x <- ifelse(is.na(Half.Baths), 0.838383838384, Half.Baths)
                 predict <- predict +  0.632131755452E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.43434343434, Garage.1.Area)
                 predict <- predict +  -0.517394313572E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.88568998055, log.Building.Square.Feet)
                 predict <- predict +  0.249180498703 * x
                 x <- ifelse(is.na(Longitude), -87.7188798017, Longitude)
                 predict <- predict +  1.04875767118 * x
                 x <- ifelse(is.na(Latitude), 41.8907549279, Latitude)
                 predict <- predict +  -0.587912589795 * x
                 x <- ifelse(is.na(Census.Tract), 214301.847032, Census.Tract)
                 predict <- predict +  -0.648201012862E-06 * x
                 x <- ifelse(is.na(Full.Baths), 2.13131313131, Full.Baths)
                 predict <- predict +  0.858430150948E-01 * x
                 x <- ifelse(is.na(Age), 49.4726027397, Age)
                 predict <- predict +  -0.130457804621E-03 * x
                 x <- ifelse(is.na(Number.of.Units), 476.725663717, Number.of.Units)
                 predict <- predict +  -0.273277859816E-03 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.181415929204E-01, Percent.Ownership)
                 predict <- predict +  6.16082988998 * x
                 x <- ifelse(is.na(Sale.Year), 2015.17808219, Sale.Year)
                 predict <- predict +  0.251693519435E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.85388127854, Sale.Month)
                 predict <- predict +  0.143424860001E-02 * x
                 x <- ifelse(is.na(PropClassSqFt), 2426.91192927, PropClassSqFt)
                 predict <- predict +  -0.233616101849E-04 * x
                 predict <- predict +  -1.64059142196 * Rooms.NA
                 predict <- predict +  0.489857420024 * Total.Building.Square.Feet.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 437
                 predict <- -4034.31147272
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.33711012696, log.Land.Square.Feet)
                 predict <- predict +  -0.409869548601E-01 * x
                 x <- ifelse(is.na(Rooms), 7.37234042553, Rooms)
                 predict <- predict +  -0.466565322026E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.63829787234, Bedrooms)
                 predict <- predict +  0.106880085412 * x
                 x <- ifelse(is.na(Fireplaces), 0.510638297872, Fireplaces)
                 predict <- predict +  -0.869933914886E-01 * x
                 x <- ifelse(is.na(Half.Baths), 0.521276595745, Half.Baths)
                 predict <- predict +  0.257292091699 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.37234042553, Garage.1.Area)
                 predict <- predict +  0.107403881754 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.40514403908, log.Building.Square.Feet)
                 predict <- predict +  0.351100691050 * x
                 x <- ifelse(is.na(Longitude), -87.6813530100, Longitude)
                 predict <- predict +  -31.8978472594 * x
                 x <- ifelse(is.na(Latitude), 42.0142790579, Latitude)
                 predict <- predict +  23.8579577009 * x
                 x <- ifelse(is.na(Census.Tract), 451988.823529, Census.Tract)
                 predict <- predict +  -0.580948591306E-05 * x
                 x <- ifelse(is.na(Full.Baths), 1.78723404255, Full.Baths)
                 predict <- predict +  0.316705767350 * x
                 x <- ifelse(is.na(Age), 72.5052083333, Age)
                 predict <- predict +  0.983312224418E-03 * x
                 x <- ifelse(is.na(Number.of.Units), 232.683673469, Number.of.Units)
                 predict <- predict +  -0.481029834635E-04 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.374489795918E-01, Percent.Ownership)
                 predict <- predict +  5.85874092329 * x
                 x <- ifelse(is.na(Sale.Year), 2015.33333333, Sale.Year)
                 predict <- predict +  0.123579494157 * x
                 x <- ifelse(is.na(Sale.Month), 6.96354166667, Sale.Month)
                 predict <- predict +  0.249632914173E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1750.11831317, HoodSqFt)
                 predict <- predict +  -0.115933941222E-02 * x
                 x <- ifelse(is.na(PropClassSqFt), 1860.09394555, PropClassSqFt)
                 predict <- predict +  -0.258019409537E-04 * x
                 predict <- predict +  -1.52069664569 * Rooms.NA
                 predict <- predict +  0.561874515371 * Longitude.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               if(!is.na(Longitude) & Longitude <= -87.6725732626 ){
                 nodeid <- 438
                 predict <- -158.928722698
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.35877817728, log.Land.Square.Feet)
                 predict <- predict +  0.976525588855E-01 * x
                 x <- ifelse(is.na(Rooms), 7.15887850467, Rooms)
                 predict <- predict +  0.312949523311E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.51401869159, Bedrooms)
                 predict <- predict +  -0.190481494026 * x
                 x <- ifelse(is.na(Fireplaces), 0.663551401869, Fireplaces)
                 predict <- predict +  -0.791023400874E-01 * x
                 x <- ifelse(is.na(Half.Baths), 0.457943925234, Half.Baths)
                 predict <- predict +  0.707936663085E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.59813084112, Garage.1.Area)
                 predict <- predict +  0.513608352406E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.49635626326, log.Building.Square.Feet)
                 predict <- predict +  0.681519259254 * x
                 x <- ifelse(is.na(Longitude), -87.7621316936, Longitude)
                 predict <- predict +  1.38309064423 * x
                 x <- ifelse(is.na(Latitude), 42.0192042629, Latitude)
                 predict <- predict +  0.692730668742 * x
                 x <- ifelse(is.na(Census.Tract), 456965.452055, Census.Tract)
                 predict <- predict +  0.702330096683E-06 * x
                 x <- ifelse(is.na(Full.Baths), 1.85981308411, Full.Baths)
                 predict <- predict +  0.152582584873 * x
                 x <- ifelse(is.na(Age), 62.2374429224, Age)
                 predict <- predict +  -0.586130814522E-03 * x
                 x <- ifelse(is.na(Number.of.Units), 42.0982142857, Number.of.Units)
                 predict <- predict +  -0.209376626823E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.656250000000E-01, Percent.Ownership)
                 predict <- predict +  2.20386144323 * x
                 x <- ifelse(is.na(Sale.Year), 2015.10045662, Sale.Year)
                 predict <- predict +  0.128126101131 * x
                 x <- ifelse(is.na(Sale.Month), 6.60273972603, Sale.Month)
                 predict <- predict +  0.142142413752E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1970.54935621, HoodSqFt)
                 predict <- predict +  -0.159320848594E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 1866.90962177, PropClassSqFt)
                 predict <- predict +  -0.106460322711E-03 * x
                 predict <- predict +  -1.24121247465 * Rooms.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 439
                 predict <- 3379.07032377
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.85931070543, log.Land.Square.Feet)
                 predict <- predict +  0.267146888530 * x
                 x <- ifelse(is.na(Rooms), 9.80733944954, Rooms)
                 predict <- predict +  -0.271851296594E-01 * x
                 x <- ifelse(is.na(Bedrooms), 4.53211009174, Bedrooms)
                 predict <- predict +  -0.297766998293E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.642201834862, Fireplaces)
                 predict <- predict +  0.877560205926E-01 * x
                 x <- ifelse(is.na(Half.Baths), 0.816513761468, Half.Baths)
                 predict <- predict +  0.996749410564E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 0.926605504587, Garage.1.Area)
                 predict <- predict +  0.127978966814 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.90188954972, log.Building.Square.Feet)
                 predict <- predict +  0.625693095025 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.458715596330E-01, Number.of.Commercial.Units)
                 predict <- predict +  -0.806750405387E-01 * x
                 x <- ifelse(is.na(Longitude), -87.6347736110, Longitude)
                 predict <- predict +  36.2343581657 * x
                 x <- ifelse(is.na(Latitude), 41.9197494064, Latitude)
                 predict <- predict +  -6.00368159727 * x
                 x <- ifelse(is.na(Census.Tract), 72688.7265018, Census.Tract)
                 predict <- predict +  -0.222024676823E-04 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 4832.50000000, Total.Building.Square.Feet)
                 predict <- predict +  0.888051255957E-04 * x
                 x <- ifelse(is.na(Full.Baths), 2.73394495413, Full.Baths)
                 predict <- predict +  0.645163367626E-01 * x
                 x <- ifelse(is.na(Age), 53.5242320819, Age)
                 predict <- predict +  -0.530127793224E-03 * x
                 x <- ifelse(is.na(Number.of.Units), 233.941740413, Number.of.Units)
                 predict <- predict +  -0.134399400663E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.242225497421E-01, Percent.Ownership)
                 predict <- predict +  5.47331600662 * x
                 x <- ifelse(is.na(Sale.Year), 2015.09010239, Sale.Year)
                 predict <- predict +  0.262724991856E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.86075085324, Sale.Month)
                 predict <- predict +  0.263161887490E-02 * x
                 x <- ifelse(is.na(HoodSqFt), 2104.83816526, HoodSqFt)
                 predict <- predict +  0.307969344418E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 2490.39794844, PropClassSqFt)
                 predict <- predict +  0.689891660413E-04 * x
                 predict <- predict +  1.16114591702 * Rooms.NA
                 predict <- predict +  -0.594638836952 * Longitude.NA
                 predict <- predict +  -0.706001383625 * Total.Building.Square.Feet.NA
                 predict <- predict +  3.31169923169 * Percent.Ownership.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         } else {
           catvalues <- c("73")
           if(Town.Code %in% catvalues){
             nodeid <- 110
             predict <- 491.447269165
             x <- ifelse(is.na(log.Land.Square.Feet), 8.52020273841, log.Land.Square.Feet)
             predict <- predict +  0.278831714295 * x
             x <- ifelse(is.na(Rooms), 9.59118541033, Rooms)
             predict <- predict +  -0.234411463572E-01 * x
             x <- ifelse(is.na(Bedrooms), 4.48176291793, Bedrooms)
             predict <- predict +  -0.114126039795E-01 * x
             x <- ifelse(is.na(Fireplaces), 0.457446808511, Fireplaces)
             predict <- predict +  0.593296882742E-01 * x
             x <- ifelse(is.na(Half.Baths), 0.454407294833, Half.Baths)
             predict <- predict +  0.348664820461E-01 * x
             x <- ifelse(is.na(Garage.1.Area), 1.35562310030, Garage.1.Area)
             predict <- predict +  -0.161519522755E-01 * x
             x <- ifelse(is.na(Garage.2.Area), 0.303951367781E-02, Garage.2.Area)
             predict <- predict +  0.141044490741 * x
             x <- ifelse(is.na(log.Building.Square.Feet), 7.71985345748, log.Building.Square.Feet)
             predict <- predict +  0.564082773163 * x
             x <- ifelse(is.na(Number.of.Commercial.Units), 0.364741641337E-01, Number.of.Commercial.Units)
             predict <- predict +  -0.368409217216 * x
             x <- ifelse(is.na(Longitude), -87.6676081460, Longitude)
             predict <- predict +  2.82097751593 * x
             x <- ifelse(is.na(Latitude), 41.9476008727, Latitude)
             predict <- predict +  -7.33828149806 * x
             x <- ifelse(is.na(Census.Tract), 129616.141074, Census.Tract)
             predict <- predict +  -0.108884166224E-06 * x
             x <- ifelse(is.na(Total.Building.Square.Feet), 3826.06666667, Total.Building.Square.Feet)
             predict <- predict +  0.364780588773E-04 * x
             x <- ifelse(is.na(Full.Baths), 2.50000000000, Full.Baths)
             predict <- predict +  0.134641477535E-01 * x
             x <- ifelse(is.na(Age), 60.6662728884, Age)
             predict <- predict +  -0.257797647499E-02 * x
             x <- ifelse(is.na(Number.of.Units), 18.7574879227, Number.of.Units)
             predict <- predict +  -0.499056481772E-02 * x
             x <- ifelse(is.na(Percent.Ownership), 0.160714975845, Percent.Ownership)
             predict <- predict +  2.01973261757 * x
             x <- ifelse(is.na(Sale.Year), 2015.01299468, Sale.Year)
             predict <- predict +  0.347514423395E-01 * x
             x <- ifelse(is.na(Sale.Month), 6.73301831069, Sale.Month)
             predict <- predict +  0.519661990950E-02 * x
             x <- ifelse(is.na(HoodSqFt), 2112.23506353, HoodSqFt)
             predict <- predict +  0.134127821331E-03 * x
             x <- ifelse(is.na(PropClassSqFt), 2466.86268399, PropClassSqFt)
             predict <- predict +  0.581421298016E-04 * x
             predict <- predict +  -1.14898279057 * Rooms.NA
             predict <- predict +  -0.243043368855 * Longitude.NA
             predict <- predict +  0.310416537833 * Total.Building.Square.Feet.NA
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             catvalues <- c("24")
             if(Town.Code %in% catvalues){
               nodeid <- 222
               predict <- -461.931640230
               x <- ifelse(is.na(log.Land.Square.Feet), 9.39714455552, log.Land.Square.Feet)
               predict <- predict +  0.670980538590E-01 * x
               x <- ifelse(is.na(Rooms), 7.31325301205, Rooms)
               predict <- predict +  -0.495640430401E-01 * x
               x <- ifelse(is.na(Bedrooms), 3.69277108434, Bedrooms)
               predict <- predict +  -0.755524923647E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.680722891566, Fireplaces)
               predict <- predict +  0.126670796409 * x
               x <- ifelse(is.na(Half.Baths), 0.813253012048, Half.Baths)
               predict <- predict +  0.282429400117E-01 * x
               x <- ifelse(is.na(Garage.1.Area), 1.51807228916, Garage.1.Area)
               predict <- predict +  0.238725460493E-01 * x
               x <- ifelse(is.na(Garage.2.Area), 0.481927710843E-01, Garage.2.Area)
               predict <- predict +  0.210497243200E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.54948860794, log.Building.Square.Feet)
               predict <- predict +  0.858797296825 * x
               x <- ifelse(is.na(Longitude), -87.7530010145, Longitude)
               predict <- predict +  0.615659370589 * x
               x <- ifelse(is.na(Latitude), 42.0293769321, Latitude)
               predict <- predict +  5.85398956019 * x
               x <- ifelse(is.na(Census.Tract), 807465.030435, Census.Tract)
               predict <- predict +  0.229960644166E-03 * x
               x <- ifelse(is.na(Full.Baths), 1.89759036145, Full.Baths)
               predict <- predict +  0.638391615942E-01 * x
               x <- ifelse(is.na(Age), 43.7434782609, Age)
               predict <- predict +  -0.412450599730E-02 * x
               x <- ifelse(is.na(Number.of.Units), 54.7500000000, Number.of.Units)
               predict <- predict +  0.110680866542E-01 * x
               x <- ifelse(is.na(Percent.Ownership), 0.193750000000E-01, Percent.Ownership)
               predict <- predict +  22.6645441821 * x
               x <- ifelse(is.na(Sale.Year), 2014.92173913, Sale.Year)
               predict <- predict +  0.443316844924E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.74782608696, Sale.Month)
               predict <- predict +  0.727990372461E-02 * x
               x <- ifelse(is.na(HoodSqFt), 1998.83935750, HoodSqFt)
               predict <- predict +  0.150327075688E-03 * x
               x <- ifelse(is.na(PropClassSqFt), 1984.52927421, PropClassSqFt)
               predict <- predict +  -0.880886014528E-04 * x
               predict <- predict +  -0.909215796019 * Rooms.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               catvalues <- c("19","74")
               if(Town.Code %in% catvalues){
                 nodeid <- 446
                 predict <- 45.5408399505
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.73496738142, log.Land.Square.Feet)
                 predict <- predict +  0.137115996672 * x
                 x <- ifelse(is.na(Rooms), 9.47138047138, Rooms)
                 predict <- predict +  -0.195930191115E-01 * x
                 x <- ifelse(is.na(Bedrooms), 4.42760942761, Bedrooms)
                 predict <- predict +  0.311390617542E-02 * x
                 x <- ifelse(is.na(Fireplaces), 0.774410774411, Fireplaces)
                 predict <- predict +  0.275598171123E-01 * x
                 x <- ifelse(is.na(Half.Baths), 0.730639730640, Half.Baths)
                 predict <- predict +  -0.642139745325E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.17845117845, Garage.1.Area)
                 predict <- predict +  0.129986180713 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.83890110259, log.Building.Square.Feet)
                 predict <- predict +  0.621039226505 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.303030303030E-01, Number.of.Commercial.Units)
                 predict <- predict +  -0.161269659923 * x
                 x <- ifelse(is.na(Longitude), -87.6492490902, Longitude)
                 predict <- predict +  1.89135374231 * x
                 x <- ifelse(is.na(Latitude), 41.8950155119, Latitude)
                 predict <- predict +  3.37359580397 * x
                 x <- ifelse(is.na(Census.Tract), 205963.646979, Census.Tract)
                 predict <- predict +  -0.454396947876E-07 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 4875.40000000, Total.Building.Square.Feet)
                 predict <- predict +  0.663309930016E-04 * x
                 x <- ifelse(is.na(Full.Baths), 2.70370370370, Full.Baths)
                 predict <- predict +  0.110875698054 * x
                 x <- ifelse(is.na(Age), 48.2411960133, Age)
                 predict <- predict +  -0.134325399834E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 324.616721854, Number.of.Units)
                 predict <- predict +  -0.817005465664E-03 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.370529801325E-01, Percent.Ownership)
                 predict <- predict +  2.50350452759 * x
                 x <- ifelse(is.na(Sale.Year), 2015.00398671, Sale.Year)
                 predict <- predict +  -0.634646941115E-02 * x
                 x <- ifelse(is.na(Sale.Month), 6.80465116279, Sale.Month)
                 predict <- predict +  -0.984111399778E-03 * x
                 x <- ifelse(is.na(HoodSqFt), 1909.21338351, HoodSqFt)
                 predict <- predict +  -0.483789370255E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 2671.20682261, PropClassSqFt)
                 predict <- predict +  0.548921508507E-04 * x
                 predict <- predict +  -1.72381609670 * Rooms.NA
                 predict <- predict +  -0.961316209039E-01 * Longitude.NA
                 predict <- predict +  0.222612786234 * Total.Building.Square.Feet.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 447
                 predict <- 1.31251208330
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.36096801523, log.Land.Square.Feet)
                 predict <- predict +  0.781001158669E-01 * x
                 x <- ifelse(is.na(Rooms), 7.38952164009, Rooms)
                 predict <- predict +  0.144065576768E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.65375854214, Bedrooms)
                 predict <- predict +  -0.301930100833E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.626423690205, Fireplaces)
                 predict <- predict +  0.128128276707 * x
                 x <- ifelse(is.na(Half.Baths), 0.624145785877, Half.Baths)
                 predict <- predict +  0.378245757507E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.58997722096, Garage.1.Area)
                 predict <- predict +  0.885507392550E-01 * x
                 x <- ifelse(is.na(Garage.2.Area), 0.911161731207E-02, Garage.2.Area)
                 predict <- predict +  0.152882719932 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.53009973615, log.Building.Square.Feet)
                 predict <- predict +  0.335043049209 * x
                 x <- ifelse(is.na(Longitude), -87.8169088659, Longitude)
                 predict <- predict +  1.25358783369 * x
                 x <- ifelse(is.na(Latitude), 42.0638969383, Latitude)
                 predict <- predict +  1.00811561391 * x
                 x <- ifelse(is.na(Census.Tract), 807617.450172, Census.Tract)
                 predict <- predict +  0.337658916951E-05 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 3728.28571429, Total.Building.Square.Feet)
                 predict <- predict +  0.514951059340E-04 * x
                 x <- ifelse(is.na(Full.Baths), 1.82915717540, Full.Baths)
                 predict <- predict +  0.731599742830E-01 * x
                 x <- ifelse(is.na(Age), 61.1231028668, Age)
                 predict <- predict +  -0.204485709344E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 79.9350649351, Number.of.Units)
                 predict <- predict +  0.162003916153E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.485806451613E-01, Percent.Ownership)
                 predict <- predict +  2.52481444605 * x
                 x <- ifelse(is.na(Sale.Year), 2015.18381113, Sale.Year)
                 predict <- predict +  0.347554209914E-01 * x
                 x <- ifelse(is.na(Sale.Month), 7.17537942664, Sale.Month)
                 predict <- predict +  0.666087955703E-02 * x
                 x <- ifelse(is.na(HoodSqFt), 1944.83942779, HoodSqFt)
                 predict <- predict +  0.663379532257E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 2062.19580610, PropClassSqFt)
                 predict <- predict +  0.639584585633E-04 * x
                 predict <- predict +  0.102822696038 * Rooms.NA
                 predict <- predict +  -0.946003178477E-01 * Longitude.NA
                 predict <- predict +  0.189633748023 * Total.Building.Square.Feet.NA
                 predict <- predict +  1.11565116454 * Percent.Ownership.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         }
       }
     }
   } else {
     catvalues <- c("34")
     if(Town.Code %in% catvalues){
       nodeid <- 14
       predict <- -88.6946659573
       x <- ifelse(is.na(log.Land.Square.Feet), 8.97800944370, log.Land.Square.Feet)
       predict <- predict +  0.286145117249 * x
       x <- ifelse(is.na(Rooms), 6.56069364162, Rooms)
       predict <- predict +  -0.543524667934E-01 * x
       x <- ifelse(is.na(Bedrooms), 3.38728323699, Bedrooms)
       predict <- predict +  0.141996792454E-01 * x
       x <- ifelse(is.na(Fireplaces), 0.554913294798, Fireplaces)
       predict <- predict +  0.105534506858 * x
       x <- ifelse(is.na(Half.Baths), 0.531791907514, Half.Baths)
       predict <- predict +  0.164846900584 * x
       x <- ifelse(is.na(Garage.1.Area), 1.86127167630, Garage.1.Area)
       predict <- predict +  0.474649250713E-01 * x
       x <- ifelse(is.na(log.Building.Square.Feet), 7.34932117785, log.Building.Square.Feet)
       predict <- predict +  0.226009952366 * x
       x <- ifelse(is.na(Number.of.Commercial.Units), 0.578034682081E-02, Number.of.Commercial.Units)
       predict <- predict +  -0.930344703168 * x
       x <- ifelse(is.na(Longitude), -87.8203782389, Longitude)
       predict <- predict +  2.27358871447 * x
       x <- ifelse(is.na(Latitude), 41.8354581034, Latitude)
       predict <- predict +  2.48747392817 * x
       x <- ifelse(is.na(Census.Tract), 815683.912371, Census.Tract)
       predict <- predict +  0.474319885176E-04 * x
       x <- ifelse(is.na(Full.Baths), 1.46242774566, Full.Baths)
       predict <- predict +  0.964221142604E-01 * x
       x <- ifelse(is.na(Age), 71.5051546392, Age)
       predict <- predict +  -0.114149787256E-02 * x
       x <- ifelse(is.na(Number.of.Units), 17.9523809524, Number.of.Units)
       predict <- predict +  -0.725794234921E-02 * x
       x <- ifelse(is.na(Percent.Ownership), 0.107142857143, Percent.Ownership)
       predict <- predict +  4.68003443786 * x
       x <- ifelse(is.na(Sale.Year), 2015.08247423, Sale.Year)
       predict <- predict +  0.771531005552E-01 * x
       x <- ifelse(is.na(Sale.Month), 7.11855670103, Sale.Month)
       predict <- predict +  0.358887870927E-02 * x
       x <- ifelse(is.na(HoodSqFt), 1728.13862687, HoodSqFt)
       predict <- predict +  -0.120372424540E-02 * x
       x <- ifelse(is.na(PropClassSqFt), 1700.42693860, PropClassSqFt)
       predict <- predict +  0.102553871914E-03 * x
       predict <- predict +  -1.27116314224 * Rooms.NA
       predict <- min(max(predict, 9.21094019205),15.8169906933)
     } else {
       catvalues <- c("12","13","14","32","37","70")
       if(Town.Code %in% catvalues){
         catvalues <- c("20","22","42","44","46","60","64","71","73","81","90","93","103","150","151","152","161","170","175","182","200","201","222","227","230","241","255","257","262","300","312","315","316","330","341","342","344","350","360")
         if(Neighborhood.Code %in% catvalues){
           catvalues <- c("0.0")
           if(Most.Recent.Sale %in% catvalues){
             catvalues <- c("20","46","71","152","182","241","255","316","341","350","360")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 240
               predict <- -372.345942281
               x <- ifelse(is.na(log.Land.Square.Feet), 9.76460655513, log.Land.Square.Feet)
               predict <- predict +  0.216470728478 * x
               x <- ifelse(is.na(Rooms), 7.84905660377, Rooms)
               predict <- predict +  -0.904611351865E-01 * x
               x <- ifelse(is.na(Bedrooms), 3.79245283019, Bedrooms)
               predict <- predict +  0.333013572905 * x
               x <- ifelse(is.na(Fireplaces), 0.547169811321, Fireplaces)
               predict <- predict +  0.987666401562E-02 * x
               x <- ifelse(is.na(Half.Baths), 0.528301886792, Half.Baths)
               predict <- predict +  0.389080116204 * x
               x <- ifelse(is.na(Garage.1.Area), 1.41509433962, Garage.1.Area)
               predict <- predict +  0.924317541473E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.56279614984, log.Building.Square.Feet)
               predict <- predict +  0.576450936580 * x
               x <- ifelse(is.na(Longitude), -87.6226298437, Longitude)
               predict <- predict +  0.246857248624 * x
               x <- ifelse(is.na(Latitude), 41.7309804112, Latitude)
               predict <- predict +  6.55528048299 * x
               x <- ifelse(is.na(Census.Tract), 550458.946970, Census.Tract)
               predict <- predict +  -0.259049790744E-06 * x
               x <- ifelse(is.na(Full.Baths), 1.92452830189, Full.Baths)
               predict <- predict +  -0.708762921727E-01 * x
               x <- ifelse(is.na(Age), 63.2030075188, Age)
               predict <- predict +  -0.409789164884E-02 * x
               x <- ifelse(is.na(Number.of.Units), 168.837500000, Number.of.Units)
               predict <- predict +  -0.149263581520E-02 * x
               x <- ifelse(is.na(Percent.Ownership), 0.580000000000E-01, Percent.Ownership)
               predict <- predict +  4.14666836568 * x
               x <- ifelse(is.na(Sale.Year), 2014.74436090, Sale.Year)
               predict <- predict +  0.622868923399E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.61654135338, Sale.Month)
               predict <- predict +  0.270818359632E-01 * x
               x <- ifelse(is.na(HoodSqFt), 1691.20849846, HoodSqFt)
               predict <- predict +  0.158637254754E-03 * x
               x <- ifelse(is.na(PropClassSqFt), 1938.14316277, PropClassSqFt)
               predict <- predict +  -0.131547934997E-03 * x
               predict <- predict +  -1.57462773289 * Rooms.NA
               predict <- predict +  -0.590448327635 * Longitude.NA
               predict <- predict +  0.847050791043 * Total.Building.Square.Feet.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               nodeid <- 241
               predict <- -325.198407376
               x <- ifelse(is.na(log.Land.Square.Feet), 9.41701413974, log.Land.Square.Feet)
               predict <- predict +  0.501111728671E-01 * x
               x <- ifelse(is.na(Rooms), 7.24056603774, Rooms)
               predict <- predict +  -0.299988715504E-01 * x
               x <- ifelse(is.na(Bedrooms), 3.64622641509, Bedrooms)
               predict <- predict +  0.486770493882E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.452830188679, Fireplaces)
               predict <- predict +  0.727284041689E-02 * x
               x <- ifelse(is.na(Half.Baths), 0.693396226415, Half.Baths)
               predict <- predict +  0.197372412641E-01 * x
               x <- ifelse(is.na(Garage.1.Area), 1.68867924528, Garage.1.Area)
               predict <- predict +  0.923317551778E-01 * x
               x <- ifelse(is.na(Garage.2.Area), 0.188679245283E-01, Garage.2.Area)
               predict <- predict +  0.326530390317E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.39953104385, log.Building.Square.Feet)
               predict <- predict +  0.586248561578 * x
               x <- ifelse(is.na(Longitude), -87.6764316681, Longitude)
               predict <- predict +  -2.16718913339 * x
               x <- ifelse(is.na(Latitude), 41.5766674594, Latitude)
               predict <- predict +  3.19811564938 * x
               x <- ifelse(is.na(Census.Tract), 791542.120833, Census.Tract)
               predict <- predict +  0.190791879511E-05 * x
               x <- ifelse(is.na(Full.Baths), 1.64622641509, Full.Baths)
               predict <- predict +  0.142396518764 * x
               x <- ifelse(is.na(Age), 43.4090909091, Age)
               predict <- predict +  -0.603951938079E-02 * x
               x <- ifelse(is.na(Number.of.Units), 96.0666666667, Number.of.Units)
               predict <- predict +  -0.251654733875E-02 * x
               x <- ifelse(is.na(Percent.Ownership), 0.210000000000E-01, Percent.Ownership)
               predict <- predict +  -2.10965281382 * x
               x <- ifelse(is.na(Sale.Year), 2014.75619835, Sale.Year)
               predict <- predict +  0.395585252370E-02 * x
               x <- ifelse(is.na(Sale.Month), 6.48347107438, Sale.Month)
               predict <- predict +  -0.100112933706E-01 * x
               x <- ifelse(is.na(HoodSqFt), 1835.50394494, HoodSqFt)
               predict <- predict +  -0.658161059394E-04 * x
               x <- ifelse(is.na(PropClassSqFt), 1721.28530188, PropClassSqFt)
               predict <- predict +  -0.134203197487E-03 * x
               predict <- predict +  -0.653278431074 * Rooms.NA
               predict <- predict +  -0.555066171916 * Longitude.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             }
           } else {
             catvalues <- c("12","14","37")
             if(Town.Code %in% catvalues){
               nodeid <- 242
               predict <- -245.573474733
               x <- ifelse(is.na(log.Land.Square.Feet), 9.45316202380, log.Land.Square.Feet)
               predict <- predict +  0.990539330332E-01 * x
               x <- ifelse(is.na(Rooms), 6.61967213115, Rooms)
               predict <- predict +  0.716753304229E-02 * x
               x <- ifelse(is.na(Bedrooms), 3.31803278689, Bedrooms)
               predict <- predict +  0.589232701199E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.475409836066, Fireplaces)
               predict <- predict +  0.583958609823E-01 * x
               x <- ifelse(is.na(Half.Baths), 0.632786885246, Half.Baths)
               predict <- predict +  0.109943103357 * x
               x <- ifelse(is.na(Garage.1.Area), 1.78032786885, Garage.1.Area)
               predict <- predict +  0.785043533550E-01 * x
               x <- ifelse(is.na(Garage.2.Area), 0.196721311475E-01, Garage.2.Area)
               predict <- predict +  0.228162609559E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.32080382892, log.Building.Square.Feet)
               predict <- predict +  0.286740765237 * x
               x <- ifelse(is.na(Number.of.Commercial.Units), 0.327868852459E-02, Number.of.Commercial.Units)
               predict <- predict +  -0.141998255196 * x
               x <- ifelse(is.na(Longitude), -87.5939444693, Longitude)
               predict <- predict +  0.180542508813E-01 * x
               x <- ifelse(is.na(Latitude), 41.5681632350, Latitude)
               predict <- predict +  1.77174213326 * x
               x <- ifelse(is.na(Census.Tract), 827479.800587, Census.Tract)
               predict <- predict +  0.651974531556E-04 * x
               x <- ifelse(is.na(Total.Building.Square.Feet), 3886.00000000, Total.Building.Square.Feet)
               predict <- predict +  0.188351942769E-03 * x
               x <- ifelse(is.na(Full.Baths), 1.50163934426, Full.Baths)
               predict <- predict +  0.765421585562E-01 * x
               x <- ifelse(is.na(Age), 46.6217008798, Age)
               predict <- predict +  -0.681804199813E-02 * x
               x <- ifelse(is.na(Number.of.Units), 147.444444444, Number.of.Units)
               predict <- predict +  0.401756237525E-03 * x
               x <- ifelse(is.na(Percent.Ownership), 0.278378378378E-01, Percent.Ownership)
               predict <- predict +  7.68631501717 * x
               x <- ifelse(is.na(Sale.Year), 2015.24046921, Sale.Year)
               predict <- predict +  0.604122076365E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.70087976540, Sale.Month)
               predict <- predict +  0.127030157116E-01 * x
               x <- ifelse(is.na(HoodSqFt), 1879.15635890, HoodSqFt)
               predict <- predict +  0.106658754515E-03 * x
               x <- ifelse(is.na(PropClassSqFt), 1604.14575324, PropClassSqFt)
               predict <- predict +  -0.178900752228E-04 * x
               predict <- predict +  3.11858470606 * Rooms.NA
               predict <- predict +  0.465837559027 * Total.Building.Square.Feet.NA
               predict <- predict +  4.48029020892 * Percent.Ownership.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               nodeid <- 243
               predict <- -664.059525110
               x <- ifelse(is.na(log.Land.Square.Feet), 9.60546300891, log.Land.Square.Feet)
               predict <- predict +  0.194648708156 * x
               x <- ifelse(is.na(Rooms), 7.15644820296, Rooms)
               predict <- predict +  -0.583150860579E-01 * x
               x <- ifelse(is.na(Bedrooms), 3.59196617336, Bedrooms)
               predict <- predict +  0.426339701023E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.551797040169, Fireplaces)
               predict <- predict +  0.148671843178 * x
               x <- ifelse(is.na(Half.Baths), 0.638477801268, Half.Baths)
               predict <- predict +  0.162122224591 * x
               x <- ifelse(is.na(Garage.1.Area), 1.57928118393, Garage.1.Area)
               predict <- predict +  0.493478020918E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.48623985000, log.Building.Square.Feet)
               predict <- predict +  1.13316149123 * x
               x <- ifelse(is.na(Number.of.Commercial.Units), 0.845665961945E-02, Number.of.Commercial.Units)
               predict <- predict +  -0.276698350210 * x
               x <- ifelse(is.na(Longitude), -87.6838312344, Longitude)
               predict <- predict +  -3.93311061236 * x
               x <- ifelse(is.na(Latitude), 41.6457099781, Latitude)
               predict <- predict +  4.71227827026 * x
               x <- ifelse(is.na(Census.Tract), 675480.432065, Census.Tract)
               predict <- predict +  -0.100341384493E-05 * x
               x <- ifelse(is.na(Full.Baths), 1.77801268499, Full.Baths)
               predict <- predict +  0.105234501946 * x
               x <- ifelse(is.na(Age), 49.9959349593, Age)
               predict <- predict +  -0.312701873499E-02 * x
               x <- ifelse(is.na(Number.of.Units), 123.698113208, Number.of.Units)
               predict <- predict +  -0.147901751851E-02 * x
               x <- ifelse(is.na(Percent.Ownership), 0.485283018868E-01, Percent.Ownership)
               predict <- predict +  3.73428274496 * x
               x <- ifelse(is.na(Sale.Year), 2015.20731707, Sale.Year)
               predict <- predict +  0.627121375077E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.87127371274, Sale.Month)
               predict <- predict +  -0.293739858781E-02 * x
               x <- ifelse(is.na(HoodSqFt), 1804.15180355, HoodSqFt)
               predict <- predict +  -0.142921179736E-03 * x
               x <- ifelse(is.na(PropClassSqFt), 1860.67940209, PropClassSqFt)
               predict <- predict +  -0.256656425116E-03 * x
               predict <- predict +  -1.14003744184 * Rooms.NA
               predict <- predict +  -0.121116473383 * Longitude.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             }
           }
         } else {
           catvalues <- c("1.0")
           if(Most.Recent.Sale %in% catvalues){
             catvalues <- c("10","32","72","92","160","185","250","270")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 244
               predict <- -214.189672948
               x <- ifelse(is.na(log.Land.Square.Feet), 9.05761161317, log.Land.Square.Feet)
               predict <- predict +  0.160716410092 * x
               x <- ifelse(is.na(Rooms), 7.42424242424, Rooms)
               predict <- predict +  -0.250774746885E-01 * x
               x <- ifelse(is.na(Bedrooms), 3.75959595960, Bedrooms)
               predict <- predict +  -0.333742924166E-02 * x
               x <- ifelse(is.na(Fireplaces), 0.282828282828, Fireplaces)
               predict <- predict +  0.109389962128 * x
               x <- ifelse(is.na(Half.Baths), 0.472727272727, Half.Baths)
               predict <- predict +  0.248940139684E-01 * x
               x <- ifelse(is.na(Garage.1.Area), 1.54747474747, Garage.1.Area)
               predict <- predict +  0.404883633090E-01 * x
               x <- ifelse(is.na(Garage.2.Area), 0.808080808081E-02, Garage.2.Area)
               predict <- predict +  0.408960530747E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.36438467645, log.Building.Square.Feet)
               predict <- predict +  0.681106159222 * x
               x <- ifelse(is.na(Longitude), -87.6431086053, Longitude)
               predict <- predict +  1.10067015214 * x
               x <- ifelse(is.na(Latitude), 41.6578134587, Latitude)
               predict <- predict +  2.80634475892 * x
               x <- ifelse(is.na(Census.Tract), 740568.480480, Census.Tract)
               predict <- predict +  -0.527803986050E-06 * x
               x <- ifelse(is.na(Full.Baths), 1.64848484848, Full.Baths)
               predict <- predict +  0.796997362847E-01 * x
               x <- ifelse(is.na(Age), 58.2588235294, Age)
               predict <- predict +  -0.472376335330E-02 * x
               x <- ifelse(is.na(Number.of.Units), 24.4216216216, Number.of.Units)
               predict <- predict +  -0.123195552398E-03 * x
               x <- ifelse(is.na(Percent.Ownership), 0.121210526316, Percent.Ownership)
               predict <- predict +  2.25222272402 * x
               x <- ifelse(is.na(Sale.Year), 2015.32352941, Sale.Year)
               predict <- predict +  0.987488124514E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.77500000000, Sale.Month)
               predict <- predict +  0.131265257274E-01 * x
               x <- ifelse(is.na(HoodSqFt), 1781.95567716, HoodSqFt)
               predict <- predict +  -0.129951475847E-03 * x
               x <- ifelse(is.na(PropClassSqFt), 1669.15943542, PropClassSqFt)
               predict <- predict +  -0.291572687695E-04 * x
               predict <- predict +  -0.321186574978 * Rooms.NA
               predict <- predict +  0.247747601032 * Longitude.NA
               predict <- predict +  0.735846413670 * Percent.Ownership.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               catvalues <- c("31","40","50","53","54","80","87","88","111","130","164","171","181","210","220","224","232","240","280","310","314","320","345")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 490
                 predict <- -392.998310013
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.92278042876, log.Land.Square.Feet)
                 predict <- predict +  0.237521993320 * x
                 x <- ifelse(is.na(Rooms), 6.44182825485, Rooms)
                 predict <- predict +  -0.148181446215E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.29639889197, Bedrooms)
                 predict <- predict +  0.720759932401E-03 * x
                 x <- ifelse(is.na(Fireplaces), 0.198753462604, Fireplaces)
                 predict <- predict +  0.120439386532 * x
                 x <- ifelse(is.na(Half.Baths), 0.438365650970, Half.Baths)
                 predict <- predict +  0.110787132368 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.63157894737, Garage.1.Area)
                 predict <- predict +  0.107530956935 * x
                 x <- ifelse(is.na(Garage.2.Area), 0.484764542936E-02, Garage.2.Area)
                 predict <- predict +  -0.333596591486E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.21441526951, log.Building.Square.Feet)
                 predict <- predict +  0.497674587889 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.484764542936E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.375320817906 * x
                 x <- ifelse(is.na(Longitude), -87.6417857984, Longitude)
                 predict <- predict +  -1.44605674092 * x
                 x <- ifelse(is.na(Latitude), 41.6330080148, Latitude)
                 predict <- predict +  2.12273479346 * x
                 x <- ifelse(is.na(Census.Tract), 704446.027095, Census.Tract)
                 predict <- predict +  -0.310247700159E-06 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 5500.28571429, Total.Building.Square.Feet)
                 predict <- predict +  -0.598815045114E-04 * x
                 x <- ifelse(is.na(Full.Baths), 1.42659279778, Full.Baths)
                 predict <- predict +  0.446967717919E-01 * x
                 x <- ifelse(is.na(Age), 57.7484316186, Age)
                 predict <- predict +  -0.822953981750E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 96.4066666667, Number.of.Units)
                 predict <- predict +  -0.250196638224E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.506535947712E-01, Percent.Ownership)
                 predict <- predict +  1.25690622082 * x
                 x <- ifelse(is.na(Sale.Year), 2015.21079046, Sale.Year)
                 predict <- predict +  0.908348424703E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.77227101631, Sale.Month)
                 predict <- predict +  0.141839269233E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1561.12138159, HoodSqFt)
                 predict <- predict +  -0.703311817594E-04 * x
                 x <- ifelse(is.na(PropClassSqFt), 1522.50073204, PropClassSqFt)
                 predict <- predict +  -0.395803521438E-04 * x
                 predict <- predict +  -0.405368201700 * Rooms.NA
                 predict <- predict +  0.611513810449 * Longitude.NA
                 predict <- predict +  0.654770479749 * Total.Building.Square.Feet.NA
                 predict <- predict +  1.05784635694 * Percent.Ownership.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 491
                 predict <- -575.798597488
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.89320808115, log.Land.Square.Feet)
                 predict <- predict +  0.280253426616 * x
                 x <- ifelse(is.na(Rooms), 6.77831325301, Rooms)
                 predict <- predict +  -0.226160807999E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.47036144578, Bedrooms)
                 predict <- predict +  0.126658966935E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.180240963855, Fireplaces)
                 predict <- predict +  0.202253361589 * x
                 x <- ifelse(is.na(Half.Baths), 0.379759036145, Half.Baths)
                 predict <- predict +  0.743711360914E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.57349397590, Garage.1.Area)
                 predict <- predict +  0.886373666604E-01 * x
                 x <- ifelse(is.na(Garage.2.Area), 0.963855421687E-02, Garage.2.Area)
                 predict <- predict +  0.445348500111E-02 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.23259558473, log.Building.Square.Feet)
                 predict <- predict +  0.841883405097 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.385542168675E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.121745556434 * x
                 x <- ifelse(is.na(Longitude), -87.6228917232, Longitude)
                 predict <- predict +  -2.02178815660 * x
                 x <- ifelse(is.na(Latitude), 41.6133416909, Latitude)
                 predict <- predict +  3.11342714431 * x
                 x <- ifelse(is.na(Census.Tract), 758595.825561, Census.Tract)
                 predict <- predict +  -0.355909972246E-08 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 2707.06666667, Total.Building.Square.Feet)
                 predict <- predict +  0.167214165498E-03 * x
                 x <- ifelse(is.na(Full.Baths), 1.48963855422, Full.Baths)
                 predict <- predict +  0.511569187910E-01 * x
                 x <- ifelse(is.na(Age), 61.0434408602, Age)
                 predict <- predict +  -0.827971818014E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 64.1280000000, Number.of.Units)
                 predict <- predict +  -0.370811488788E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.860526315789E-01, Percent.Ownership)
                 predict <- predict +  3.07662688687 * x
                 x <- ifelse(is.na(Sale.Year), 2015.13763441, Sale.Year)
                 predict <- predict +  0.133749028868 * x
                 x <- ifelse(is.na(Sale.Month), 6.78236559140, Sale.Month)
                 predict <- predict +  0.128854911223E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1810.04740105, HoodSqFt)
                 predict <- predict +  0.835344996584E-04 * x
                 x <- ifelse(is.na(PropClassSqFt), 1556.10484711, PropClassSqFt)
                 predict <- predict +  -0.133626506329E-03 * x
                 predict <- predict +  0.715811062800 * Rooms.NA
                 predict <- predict +  0.434715358955 * Longitude.NA
                 predict <- predict +  0.879841706901E-01 * Total.Building.Square.Feet.NA
                 predict <- predict +  1.78475904200 * Percent.Ownership.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           } else {
             catvalues <- c("21","23","24","41","61","63","75","84","85","86","88","91","101","102","110","121","134","164","226")
             if(Neighborhood.Code %in% catvalues){
               nodeid <- 246
               predict <- 264.371477434
               x <- ifelse(is.na(log.Land.Square.Feet), 8.71761818215, log.Land.Square.Feet)
               predict <- predict +  0.319395657307 * x
               x <- ifelse(is.na(Rooms), 5.89328063241, Rooms)
               predict <- predict +  -0.191898058382E-01 * x
               x <- ifelse(is.na(Bedrooms), 3.10671936759, Bedrooms)
               predict <- predict +  0.670982653481E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.106719367589, Fireplaces)
               predict <- predict +  0.254058608300E-01 * x
               x <- ifelse(is.na(Half.Baths), 0.343873517787, Half.Baths)
               predict <- predict +  0.110979321132E-01 * x
               x <- ifelse(is.na(Garage.1.Area), 1.68774703557, Garage.1.Area)
               predict <- predict +  -0.452314238943E-01 * x
               x <- ifelse(is.na(Garage.2.Area), 0.158102766798E-01, Garage.2.Area)
               predict <- predict +  -0.103549831441 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.08638245612, log.Building.Square.Feet)
               predict <- predict +  0.427626280185 * x
               x <- ifelse(is.na(Longitude), -87.6080946804, Longitude)
               predict <- predict +  2.90439447468 * x
               x <- ifelse(is.na(Latitude), 41.5938592425, Latitude)
               predict <- predict +  -0.808887255967E-01 * x
               x <- ifelse(is.na(Census.Tract), 813267.203125, Census.Tract)
               predict <- predict +  -0.486500185044E-06 * x
               x <- ifelse(is.na(Full.Baths), 1.26086956522, Full.Baths)
               predict <- predict +  0.663447404021E-01 * x
               x <- ifelse(is.na(Age), 57.2578125000, Age)
               predict <- predict +  -0.943363857355E-02 * x
               x <- ifelse(is.na(Number.of.Units), 25.0000000000, Number.of.Units)
               predict <- predict +  0.418460019769E-01 * x
               x <- ifelse(is.na(Percent.Ownership), 0.566666666667E-01, Percent.Ownership)
               predict <- predict +  44.3036391058 * x
               x <- ifelse(is.na(Sale.Year), 2015.15234375, Sale.Year)
               predict <- predict +  -0.232608521875E-02 * x
               x <- ifelse(is.na(Sale.Month), 6.83984375000, Sale.Month)
               predict <- predict +  -0.236053014270E-01 * x
               x <- ifelse(is.na(HoodSqFt), 1830.76141325, HoodSqFt)
               predict <- predict +  -0.209071392652E-04 * x
               x <- ifelse(is.na(PropClassSqFt), 1306.29732622, PropClassSqFt)
               predict <- predict +  -0.267853384389E-03 * x
               predict <- predict +  -1.08214229377 * Rooms.NA
               predict <- predict +  0.664169808103 * Total.Building.Square.Feet.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               catvalues <- c("30","33","40","51","52","53","80","100","111","120","140","141","162","163","180","181","191","220","250","260","340")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 494
                 predict <- -321.688149732
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.73168006943, log.Land.Square.Feet)
                 predict <- predict +  0.311555426335 * x
                 x <- ifelse(is.na(Rooms), 7.80360934183, Rooms)
                 predict <- predict +  0.459953761470E-02 * x
                 x <- ifelse(is.na(Bedrooms), 3.88110403397, Bedrooms)
                 predict <- predict +  -0.273153643880E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.202760084926, Fireplaces)
                 predict <- predict +  0.675311645170E-01 * x
                 x <- ifelse(is.na(Half.Baths), 0.363057324841, Half.Baths)
                 predict <- predict +  0.809632286472E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.58280254777, Garage.1.Area)
                 predict <- predict +  0.297149146976E-01 * x
                 x <- ifelse(is.na(Garage.2.Area), 0.530785562633E-02, Garage.2.Area)
                 predict <- predict +  -0.312926304951 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.33672706701, log.Building.Square.Feet)
                 predict <- predict +  0.994924598791 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.212314225053E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.767736093729 * x
                 x <- ifelse(is.na(Longitude), -87.6180443459, Longitude)
                 predict <- predict +  -2.24210392104 * x
                 x <- ifelse(is.na(Latitude), 41.6668410858, Latitude)
                 predict <- predict +  1.84830660812 * x
                 x <- ifelse(is.na(Census.Tract), 657946.325956, Census.Tract)
                 predict <- predict +  -0.127861411653E-06 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 7726.71428571, Total.Building.Square.Feet)
                 predict <- predict +  0.697083871194E-04 * x
                 x <- ifelse(is.na(Full.Baths), 1.63906581741, Full.Baths)
                 predict <- predict +  -0.299015419967E-02 * x
                 x <- ifelse(is.na(Age), 69.0159521436, Age)
                 predict <- predict +  -0.368589752783E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 104.442622951, Number.of.Units)
                 predict <- predict +  -0.997531627837E-03 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.324590163934E-01, Percent.Ownership)
                 predict <- predict +  5.33619945849 * x
                 x <- ifelse(is.na(Sale.Year), 2015.06979063, Sale.Year)
                 predict <- predict +  0.247064987285E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.48953140578, Sale.Month)
                 predict <- predict +  0.208696334758E-02 * x
                 x <- ifelse(is.na(HoodSqFt), 1680.69568786, HoodSqFt)
                 predict <- predict +  -0.493969405996E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 1688.18028180, PropClassSqFt)
                 predict <- predict +  -0.207530788277E-03 * x
                 predict <- predict +  -1.40369151583 * Rooms.NA
                 predict <- predict +  0.234611305513 * Longitude.NA
                 predict <- predict +  0.188910665949 * Total.Building.Square.Feet.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 495
                 predict <- -314.064221023
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.97446272266, log.Land.Square.Feet)
                 predict <- predict +  0.138854465758 * x
                 x <- ifelse(is.na(Rooms), 7.72382397572, Rooms)
                 predict <- predict +  -0.240477610326E-03 * x
                 x <- ifelse(is.na(Bedrooms), 3.85887708649, Bedrooms)
                 predict <- predict +  -0.486307506980E-02 * x
                 x <- ifelse(is.na(Fireplaces), 0.241274658574, Fireplaces)
                 predict <- predict +  0.105540536325 * x
                 x <- ifelse(is.na(Half.Baths), 0.470409711684, Half.Baths)
                 predict <- predict +  0.701578445432E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.53110773900, Garage.1.Area)
                 predict <- predict +  0.249561871874E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.36063431737, log.Building.Square.Feet)
                 predict <- predict +  0.692082039234 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.151745068285E-02, Number.of.Commercial.Units)
                 predict <- predict +  0.540342153346 * x
                 x <- ifelse(is.na(Longitude), -87.6450536418, Longitude)
                 predict <- predict +  -1.75528238331 * x
                 x <- ifelse(is.na(Latitude), 41.6469421085, Latitude)
                 predict <- predict +  1.91314125818 * x
                 x <- ifelse(is.na(Census.Tract), 723463.860229, Census.Tract)
                 predict <- predict +  -0.612674167629E-07 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 15634.2000000, Total.Building.Square.Feet)
                 predict <- predict +  -0.619336513073E-04 * x
                 x <- ifelse(is.na(Full.Baths), 1.71623672231, Full.Baths)
                 predict <- predict +  0.354228076749E-01 * x
                 x <- ifelse(is.na(Age), 60.9949685535, Age)
                 predict <- predict +  -0.579474996035E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 47.1323529412, Number.of.Units)
                 predict <- predict +  -0.322650871027E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.852941176471E-01, Percent.Ownership)
                 predict <- predict +  1.51151945590 * x
                 x <- ifelse(is.na(Sale.Year), 2014.86037736, Sale.Year)
                 predict <- predict +  0.431567544215E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.36981132075, Sale.Month)
                 predict <- predict +  0.340524623364E-02 * x
                 x <- ifelse(is.na(HoodSqFt), 1781.39906661, HoodSqFt)
                 predict <- predict +  -0.138774055407E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 1697.02513167, PropClassSqFt)
                 predict <- predict +  -0.621068088772E-04 * x
                 predict <- predict +  -0.641500619763 * Rooms.NA
                 predict <- predict +  -0.965217004902E-02 * Longitude.NA
                 predict <- predict +  0.153256055757 * Total.Building.Square.Feet.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           }
         }
       } else {
         catvalues <- c("12","14","23","25","31","41","47","62","67","80","101","108","115","152","161","165","180","190","200","211","220","400")
         if(Neighborhood.Code %in% catvalues){
           catvalues <- c("15","31","77")
           if(Town.Code %in% catvalues){
             catvalues <- c("0.0")
             if(Most.Recent.Sale %in% catvalues){
               if(!is.na(Longitude) & Longitude <= -87.7102196427 ){
                 nodeid <- 496
                 predict <- 6.61854842740
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.41486113083, log.Land.Square.Feet)
                 predict <- predict +  0.278472661861 * x
                 x <- ifelse(is.na(Rooms), 6.22485207101, Rooms)
                 predict <- predict +  -0.294746770172E-02 * x
                 x <- ifelse(is.na(Bedrooms), 3.20710059172, Bedrooms)
                 predict <- predict +  -0.985716694543E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.946745562130E-01, Fireplaces)
                 predict <- predict +  0.849490275575E-01 * x
                 x <- ifelse(is.na(Half.Baths), 0.171597633136, Half.Baths)
                 predict <- predict +  -0.877151177987E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.52071005917, Garage.1.Area)
                 predict <- predict +  0.983880307975E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.21887944018, log.Building.Square.Feet)
                 predict <- predict +  -0.149655689710E-01 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.591715976331E-02, Number.of.Commercial.Units)
                 predict <- predict +  0.275253657896 * x
                 x <- ifelse(is.na(Longitude), -87.8045721974, Longitude)
                 predict <- predict +  -2.33550747149 * x
                 x <- ifelse(is.na(Latitude), 41.8700296585, Latitude)
                 predict <- predict +  -8.66577004114 * x
                 x <- ifelse(is.na(Census.Tract), 611382.788235, Census.Tract)
                 predict <- predict +  0.178688886307E-06 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 2534.50000000, Total.Building.Square.Feet)
                 predict <- predict +  -0.308798796116E-03 * x
                 x <- ifelse(is.na(Full.Baths), 1.49112426036, Full.Baths)
                 predict <- predict +  0.123782680918 * x
                 x <- ifelse(is.na(Age), 81.9588235294, Age)
                 predict <- predict +  -0.931601460851E-02 * x
                 x <- ifelse(is.na(Sale.Year), 2015.15294118, Sale.Year)
                 predict <- predict +  0.788853111265E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.55294117647, Sale.Month)
                 predict <- predict +  -0.213586654717E-02 * x
                 x <- ifelse(is.na(HoodSqFt), 1786.73809988, HoodSqFt)
                 predict <- predict +  0.645193233898E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 1672.25986792, PropClassSqFt)
                 predict <- predict +  0.316090245763E-03 * x
                 predict <- predict +  -2.16183035017 * Rooms.NA
                 predict <- predict +  0.891613040414 * Total.Building.Square.Feet.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 497
                 predict <- 7024.83225654
                 x <- ifelse(is.na(log.Land.Square.Feet), 7.97633549525, log.Land.Square.Feet)
                 predict <- predict +  0.930524521772 * x
                 x <- ifelse(is.na(Rooms), 7.72500000000, Rooms)
                 predict <- predict +  -0.195029009665E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.95000000000, Bedrooms)
                 predict <- predict +  0.628049621228E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.500000000000E-01, Fireplaces)
                 predict <- predict +  0.273484970166 * x
                 x <- ifelse(is.na(Half.Baths), 0.500000000000E-01, Half.Baths)
                 predict <- predict +  1.05227952124 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.00000000000, Garage.1.Area)
                 predict <- predict +  0.105046544736 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.51505571117, log.Building.Square.Feet)
                 predict <- predict +  -0.942607477697E-01 * x
                 x <- ifelse(is.na(Longitude), -87.6966766059, Longitude)
                 predict <- predict +  80.1101048960 * x
                 x <- ifelse(is.na(Latitude), 41.8723995587, Latitude)
                 predict <- predict +  -6.27838086209 * x
                 x <- ifelse(is.na(Census.Tract), 481567.346939, Census.Tract)
                 predict <- predict +  0.803312592191E-07 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 2758.00000000, Total.Building.Square.Feet)
                 predict <- predict +  0.309189830878E-03 * x
                 x <- ifelse(is.na(Full.Baths), 2.22500000000, Full.Baths)
                 predict <- predict +  0.388711687102 * x
                 x <- ifelse(is.na(Age), 92.3061224490, Age)
                 predict <- predict +  -0.884978532474E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 8.22222222222, Number.of.Units)
                 predict <- predict +  0.261514415537E-01 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.148888888889, Percent.Ownership)
                 predict <- predict +  5.69573844874 * x
                 x <- ifelse(is.na(Sale.Year), 2015.00000000, Sale.Year)
                 predict <- predict +  0.135571398016 * x
                 x <- ifelse(is.na(Sale.Month), 7.12244897959, Sale.Month)
                 predict <- predict +  0.407616097301E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1780.87204936, HoodSqFt)
                 predict <- predict +  -0.348991472209E-02 * x
                 x <- ifelse(is.na(PropClassSqFt), 2488.50948889, PropClassSqFt)
                 predict <- predict +  -0.163728409095E-03 * x
                 predict <- predict +  -1.31668832428 * Rooms.NA
                 predict <- predict +  -0.613968923112 * Total.Building.Square.Feet.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               catvalues <- c("152","161")
               if(Neighborhood.Code %in% catvalues){
                 nodeid <- 498
                 predict <- -3668.36181736
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.61393542469, log.Land.Square.Feet)
                 predict <- predict +  0.825549180113E-01 * x
                 x <- ifelse(is.na(Rooms), 6.40000000000, Rooms)
                 predict <- predict +  0.566081686948E-02 * x
                 x <- ifelse(is.na(Bedrooms), 3.13333333333, Bedrooms)
                 predict <- predict +  0.717625281420E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.300000000000, Fireplaces)
                 predict <- predict +  -0.100254496988 * x
                 x <- ifelse(is.na(Half.Baths), 0.533333333333, Half.Baths)
                 predict <- predict +  0.181099450794 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.66666666667, Garage.1.Area)
                 predict <- predict +  0.127110382560 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.38584379781, log.Building.Square.Feet)
                 predict <- predict +  0.311845646149 * x
                 x <- ifelse(is.na(Longitude), -87.7437705948, Longitude)
                 predict <- predict +  -32.4567864580 * x
                 x <- ifelse(is.na(Latitude), 41.8814284051, Latitude)
                 predict <- predict +  16.6372508805 * x
                 x <- ifelse(is.na(Census.Tract), 448156.603774, Census.Tract)
                 predict <- predict +  -0.101269689695E-04 * x
                 x <- ifelse(is.na(Full.Baths), 1.63333333333, Full.Baths)
                 predict <- predict +  -0.908850987158E-01 * x
                 x <- ifelse(is.na(Age), 46.1111111111, Age)
                 predict <- predict +  -0.332659663469E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 13.7083333333, Number.of.Units)
                 predict <- predict +  -0.182046598934E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.178333333333, Percent.Ownership)
                 predict <- predict +  1.38812901993 * x
                 x <- ifelse(is.na(Sale.Year), 2015.07407407, Sale.Year)
                 predict <- predict +  0.672444077507E-01 * x
                 x <- ifelse(is.na(Sale.Month), 7.46296296296, Sale.Month)
                 predict <- predict +  0.425760334594E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1825.17021277, HoodSqFt)
                 predict <- predict +  0.100189617764E-02 * x
                 x <- ifelse(is.na(PropClassSqFt), 1770.85341600, PropClassSqFt)
                 predict <- predict +  0.234385619371E-03 * x
                 predict <- predict +  -0.853126155671 * Rooms.NA
                 predict <- predict +  0.117855093147 * Longitude.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 499
                 predict <- -293.497014912
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.38698469167, log.Land.Square.Feet)
                 predict <- predict +  0.467916637380 * x
                 x <- ifelse(is.na(Rooms), 6.68846153846, Rooms)
                 predict <- predict +  0.168286404789E-03 * x
                 x <- ifelse(is.na(Bedrooms), 3.43653846154, Bedrooms)
                 predict <- predict +  0.263543179918E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.480769230769E-01, Fireplaces)
                 predict <- predict +  -0.276546569960E-01 * x
                 x <- ifelse(is.na(Half.Baths), 0.192307692308, Half.Baths)
                 predict <- predict +  -0.769767760133E-02 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.50961538462, Garage.1.Area)
                 predict <- predict +  0.534507556196E-01 * x
                 x <- ifelse(is.na(Garage.2.Area), 0.230769230769E-01, Garage.2.Area)
                 predict <- predict +  -0.472856212176E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.23549031502, log.Building.Square.Feet)
                 predict <- predict +  0.967148127802E-01 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.269230769231E-01, Number.of.Commercial.Units)
                 predict <- predict +  -0.504196238898 * x
                 x <- ifelse(is.na(Longitude), -87.7984182483, Longitude)
                 predict <- predict +  -1.01058107692 * x
                 x <- ifelse(is.na(Latitude), 41.8688779513, Latitude)
                 predict <- predict +  -2.26278266924 * x
                 x <- ifelse(is.na(Census.Tract), 637857.529190, Census.Tract)
                 predict <- predict +  0.157728522365E-06 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 3119.57142857, Total.Building.Square.Feet)
                 predict <- predict +  -0.222306936112E-03 * x
                 x <- ifelse(is.na(Full.Baths), 1.56346153846, Full.Baths)
                 predict <- predict +  0.386550616646E-01 * x
                 x <- ifelse(is.na(Age), 82.1996233522, Age)
                 predict <- predict +  -0.397939188711E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 34.0909090909, Number.of.Units)
                 predict <- predict +  -0.430911823250E-01 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.180714285714, Percent.Ownership)
                 predict <- predict +  -3.53466051688 * x
                 x <- ifelse(is.na(Sale.Year), 2015.01506591, Sale.Year)
                 predict <- predict +  0.153220589962 * x
                 x <- ifelse(is.na(Sale.Month), 6.64218455744, Sale.Month)
                 predict <- predict +  0.178819315898E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1777.81216766, HoodSqFt)
                 predict <- predict +  0.483306462239E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 1787.30608176, PropClassSqFt)
                 predict <- predict +  0.124869754984E-03 * x
                 predict <- predict +  -2.60101292959 * Rooms.NA
                 predict <- predict +  0.231089653013 * Total.Building.Square.Feet.NA
                 predict <- predict +  -0.957387390979 * Percent.Ownership.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             }
           } else {
             catvalues <- c("21","28")
             if(Town.Code %in% catvalues){
               nodeid <- 250
               predict <- -961.033183815
               x <- ifelse(is.na(log.Land.Square.Feet), 9.79322381184, log.Land.Square.Feet)
               predict <- predict +  -0.256313849182E-01 * x
               x <- ifelse(is.na(Rooms), 7.39857651246, Rooms)
               predict <- predict +  0.859710719983E-02 * x
               x <- ifelse(is.na(Bedrooms), 3.54448398577, Bedrooms)
               predict <- predict +  0.346888477425E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.875444839858, Fireplaces)
               predict <- predict +  0.131286004518 * x
               x <- ifelse(is.na(Half.Baths), 0.747330960854, Half.Baths)
               predict <- predict +  -0.681578373919E-04 * x
               x <- ifelse(is.na(Garage.1.Area), 1.65480427046, Garage.1.Area)
               predict <- predict +  0.853457610402E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.69821690970, log.Building.Square.Feet)
               predict <- predict +  0.642210688371 * x
               x <- ifelse(is.na(Longitude), -87.8725159041, Longitude)
               predict <- predict +  -8.98082200288 * x
               x <- ifelse(is.na(Latitude), 41.7236000411, Latitude)
               predict <- predict +  2.45014230980 * x
               x <- ifelse(is.na(Census.Tract), 821457.969780, Census.Tract)
               predict <- predict +  0.664940716590E-04 * x
               x <- ifelse(is.na(Total.Building.Square.Feet), 4906.80000000, Total.Building.Square.Feet)
               predict <- predict +  -0.232303155655E-03 * x
               x <- ifelse(is.na(Full.Baths), 1.98220640569, Full.Baths)
               predict <- predict +  -0.165198425056E-01 * x
               x <- ifelse(is.na(Age), 37.6440217391, Age)
               predict <- predict +  -0.385348333499E-02 * x
               x <- ifelse(is.na(Number.of.Units), 81.5862068966, Number.of.Units)
               predict <- predict +  0.355998983503E-02 * x
               x <- ifelse(is.na(Percent.Ownership), 0.293103448276E-01, Percent.Ownership)
               predict <- predict +  -2.10888719687 * x
               x <- ifelse(is.na(Sale.Year), 2014.91847826, Sale.Year)
               predict <- predict +  0.110526157430E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.97010869565, Sale.Month)
               predict <- predict +  0.143981884467E-01 * x
               x <- ifelse(is.na(HoodSqFt), 2110.28012685, HoodSqFt)
               predict <- predict +  0.268283749436E-04 * x
               x <- ifelse(is.na(PropClassSqFt), 2283.24364789, PropClassSqFt)
               predict <- predict +  -0.168521843339E-04 * x
               predict <- predict +  -0.709986607397 * Rooms.NA
               predict <- predict +  0.817745760044 * Longitude.NA
               predict <- predict +  1.42370716264 * Total.Building.Square.Feet.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             } else {
               nodeid <- 251
               predict <- -137.885447337
               x <- ifelse(is.na(log.Land.Square.Feet), 9.59633621769, log.Land.Square.Feet)
               predict <- predict +  0.279393461716 * x
               x <- ifelse(is.na(Rooms), 6.62186379928, Rooms)
               predict <- predict +  -0.171616544483E-01 * x
               x <- ifelse(is.na(Bedrooms), 3.29480286738, Bedrooms)
               predict <- predict +  0.112440182753E-01 * x
               x <- ifelse(is.na(Fireplaces), 0.315412186380, Fireplaces)
               predict <- predict +  0.545473127956E-01 * x
               x <- ifelse(is.na(Half.Baths), 0.552867383513, Half.Baths)
               predict <- predict +  0.105133602961 * x
               x <- ifelse(is.na(Garage.1.Area), 1.68548387097, Garage.1.Area)
               predict <- predict +  0.130378736486E-01 * x
               x <- ifelse(is.na(Garage.2.Area), 0.107526881720E-01, Garage.2.Area)
               predict <- predict +  -0.289680196448E-01 * x
               x <- ifelse(is.na(log.Building.Square.Feet), 7.32909468802, log.Building.Square.Feet)
               predict <- predict +  0.596462888407 * x
               x <- ifelse(is.na(Longitude), -87.8623772837, Longitude)
               predict <- predict +  1.45698406383 * x
               x <- ifelse(is.na(Latitude), 41.8938775875, Latitude)
               predict <- predict +  2.58659064206 * x
               x <- ifelse(is.na(Census.Tract), 711109.256512, Census.Tract)
               predict <- predict +  0.200123695499E-06 * x
               x <- ifelse(is.na(Full.Baths), 1.54301075269, Full.Baths)
               predict <- predict +  0.603576436056E-01 * x
               x <- ifelse(is.na(Age), 52.4743372814, Age)
               predict <- predict +  -0.159207432550E-02 * x
               x <- ifelse(is.na(Number.of.Units), 126.707762557, Number.of.Units)
               predict <- predict +  -0.114460366663E-02 * x
               x <- ifelse(is.na(Percent.Ownership), 0.480030257186E-01, Percent.Ownership)
               predict <- predict +  4.78412526008 * x
               x <- ifelse(is.na(Sale.Year), 2015.04794134, Sale.Year)
               predict <- predict +  0.793832645182E-01 * x
               x <- ifelse(is.na(Sale.Month), 6.77946982516, Sale.Month)
               predict <- predict +  0.135349015411E-01 * x
               x <- ifelse(is.na(HoodSqFt), 1833.38210184, HoodSqFt)
               predict <- predict +  0.152555356800E-03 * x
               x <- ifelse(is.na(PropClassSqFt), 1667.69969006, PropClassSqFt)
               predict <- predict +  -0.633587241673E-04 * x
               predict <- predict +  0.642970032605 * Rooms.NA
               predict <- predict +  0.189794739938 * Longitude.NA
               predict <- predict +  0.405608686096 * Total.Building.Square.Feet.NA
               predict <- predict +  2.12864940099 * Percent.Ownership.NA
               predict <- min(max(predict, 9.21094019205),15.8169906933)
             }
           }
         } else {
           catvalues <- c("75","106","112","174")
           if(Neighborhood.Code %in% catvalues){
             nodeid <- 126
             predict <- 276.034698330
             x <- ifelse(is.na(log.Land.Square.Feet), 9.26399884180, log.Land.Square.Feet)
             predict <- predict +  0.157561487328 * x
             x <- ifelse(is.na(Rooms), 7.22033898305, Rooms)
             predict <- predict +  -0.184310632856E-01 * x
             x <- ifelse(is.na(Bedrooms), 3.32203389831, Bedrooms)
             predict <- predict +  0.111115220770 * x
             x <- ifelse(is.na(Fireplaces), 0.796610169492, Fireplaces)
             predict <- predict +  0.121724962519E-01 * x
             x <- ifelse(is.na(Half.Baths), 0.915254237288, Half.Baths)
             predict <- predict +  -0.530821591608E-01 * x
             x <- ifelse(is.na(Garage.1.Area), 1.57627118644, Garage.1.Area)
             predict <- predict +  0.325068308477E-01 * x
             x <- ifelse(is.na(log.Building.Square.Feet), 7.61041147267, log.Building.Square.Feet)
             predict <- predict +  0.331249390212 * x
             x <- ifelse(is.na(Longitude), -88.0392856344, Longitude)
             predict <- predict +  2.94256636044 * x
             x <- ifelse(is.na(Latitude), 41.9831966485, Latitude)
             predict <- predict +  -0.526719692256 * x
             x <- ifelse(is.na(Census.Tract), 807189.261538, Census.Tract)
             predict <- predict +  -0.247541082473E-04 * x
             x <- ifelse(is.na(Full.Baths), 1.88135593220, Full.Baths)
             predict <- predict +  -0.299753120096E-02 * x
             x <- ifelse(is.na(Age), 27.6000000000, Age)
             predict <- predict +  0.587456499261E-02 * x
             x <- ifelse(is.na(Number.of.Units), 80.6666666667, Number.of.Units)
             predict <- predict +  -0.287435560734E-02 * x
             x <- ifelse(is.na(Percent.Ownership), 0.116666666667E-01, Percent.Ownership)
             predict <- predict +  -18.3864285692 * x
             x <- ifelse(is.na(Sale.Year), 2015.09230769, Sale.Year)
             predict <- predict +  0.171744547801E-01 * x
             x <- ifelse(is.na(Sale.Month), 6.27692307692, Sale.Month)
             predict <- predict +  0.100911885006E-01 * x
             x <- ifelse(is.na(HoodSqFt), 2040.93120316, HoodSqFt)
             predict <- predict +  -0.319428666697E-03 * x
             x <- ifelse(is.na(PropClassSqFt), 2104.00837984, PropClassSqFt)
             predict <- predict +  -0.198548763596E-04 * x
             predict <- predict +  -0.907467678768 * Rooms.NA
             predict <- min(max(predict, 9.21094019205),15.8169906933)
           } else {
             catvalues <- c("34","102","105","109","140","210","250","310")
             if(Neighborhood.Code %in% catvalues){
               catvalues <- c("1.0")
               if(Most.Recent.Sale %in% catvalues){
                 nodeid <- 508
                 predict <- -297.864628376
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.30094316024, log.Land.Square.Feet)
                 predict <- predict +  0.218365130399 * x
                 x <- ifelse(is.na(Rooms), 7.24308300395, Rooms)
                 predict <- predict +  0.597328471891E-02 * x
                 x <- ifelse(is.na(Bedrooms), 3.62055335968, Bedrooms)
                 predict <- predict +  -0.512640109904E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.171936758893, Fireplaces)
                 predict <- predict +  0.400826607595E-01 * x
                 x <- ifelse(is.na(Half.Baths), 0.343873517787, Half.Baths)
                 predict <- predict +  0.141306555447E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.36363636364, Garage.1.Area)
                 predict <- predict +  -0.263910027122E-01 * x
                 x <- ifelse(is.na(Garage.2.Area), 0.237154150198E-01, Garage.2.Area)
                 predict <- predict +  -0.167685678560 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.35723543852, log.Building.Square.Feet)
                 predict <- predict +  0.863002471380 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.395256916996E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.506703596446 * x
                 x <- ifelse(is.na(Longitude), -87.8496608060, Longitude)
                 predict <- predict +  -1.21530404111 * x
                 x <- ifelse(is.na(Latitude), 41.8450216957, Latitude)
                 predict <- predict +  -0.555719339487 * x
                 x <- ifelse(is.na(Census.Tract), 745025.884993, Census.Tract)
                 predict <- predict +  0.901968822117E-06 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 3985.00000000, Total.Building.Square.Feet)
                 predict <- predict +  -0.111100998589E-03 * x
                 x <- ifelse(is.na(Full.Baths), 1.66205533597, Full.Baths)
                 predict <- predict +  0.549445941930E-01 * x
                 x <- ifelse(is.na(Age), 57.2100840336, Age)
                 predict <- predict +  -0.628958913783E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 138.418269231, Number.of.Units)
                 predict <- predict +  -0.595948980892E-03 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.545023696682E-01, Percent.Ownership)
                 predict <- predict +  2.66224002753 * x
                 x <- ifelse(is.na(Sale.Year), 2015.21148459, Sale.Year)
                 predict <- predict +  0.106819627362 * x
                 x <- ifelse(is.na(Sale.Month), 6.82072829132, Sale.Month)
                 predict <- predict +  0.120388535328E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1810.56882228, HoodSqFt)
                 predict <- predict +  0.195036220435E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 1790.43936299, PropClassSqFt)
                 predict <- predict +  -0.318001994604E-03 * x
                 predict <- predict +  0.882058328474 * Rooms.NA
                 predict <- predict +  -0.606651201127 * Longitude.NA
                 predict <- predict +  0.612646916240 * Total.Building.Square.Feet.NA
                 predict <- predict +  2.36165422418 * Percent.Ownership.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 509
                 predict <- -266.451872173
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.22877371866, log.Land.Square.Feet)
                 predict <- predict +  0.184481062514 * x
                 x <- ifelse(is.na(Rooms), 7.56363636364, Rooms)
                 predict <- predict +  -0.246443497952E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.75454545455, Bedrooms)
                 predict <- predict +  0.104923636858E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.122727272727, Fireplaces)
                 predict <- predict +  0.998689028987E-01 * x
                 x <- ifelse(is.na(Half.Baths), 0.286363636364, Half.Baths)
                 predict <- predict +  -0.463262925385E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.33181818182, Garage.1.Area)
                 predict <- predict +  0.753506485001E-01 * x
                 x <- ifelse(is.na(Garage.2.Area), 0.181818181818E-01, Garage.2.Area)
                 predict <- predict +  0.370137252459 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.39794378301, log.Building.Square.Feet)
                 predict <- predict +  0.760229615729 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.318181818182E-01, Number.of.Commercial.Units)
                 predict <- predict +  -0.268814605315 * x
                 x <- ifelse(is.na(Longitude), -87.8349803322, Longitude)
                 predict <- predict +  -2.90937061702 * x
                 x <- ifelse(is.na(Latitude), 41.8480961369, Latitude)
                 predict <- predict +  -1.55950165226 * x
                 x <- ifelse(is.na(Census.Tract), 716447.121711, Census.Tract)
                 predict <- predict +  0.917552619014E-06 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 8619.75000000, Total.Building.Square.Feet)
                 predict <- predict +  0.134870418432E-03 * x
                 x <- ifelse(is.na(Full.Baths), 1.78636363636, Full.Baths)
                 predict <- predict +  0.207153125185 * x
                 x <- ifelse(is.na(Age), 59.2955974843, Age)
                 predict <- predict +  -0.530238343103E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 128.132653061, Number.of.Units)
                 predict <- predict +  -0.185472400592E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.757142857143E-01, Percent.Ownership)
                 predict <- predict +  2.47816547576 * x
                 x <- ifelse(is.na(Sale.Year), 2014.92767296, Sale.Year)
                 predict <- predict +  0.389795690621E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.65094339623, Sale.Month)
                 predict <- predict +  0.185860469893E-02 * x
                 x <- ifelse(is.na(HoodSqFt), 1832.10658373, HoodSqFt)
                 predict <- predict +  0.313830136144E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 1872.79656547, PropClassSqFt)
                 predict <- predict +  -0.327753382278E-03 * x
                 predict <- predict +  -1.28992084905 * Rooms.NA
                 predict <- predict +  -1.43108708887 * Longitude.NA
                 predict <- predict +  0.173775122835 * Total.Building.Square.Feet.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               }
             } else {
               catvalues <- c("27","35","77")
               if(Town.Code %in% catvalues){
                 nodeid <- 510
                 predict <- -131.033209723
                 x <- ifelse(is.na(log.Land.Square.Feet), 8.82310366940, log.Land.Square.Feet)
                 predict <- predict +  0.136684097501 * x
                 x <- ifelse(is.na(Rooms), 8.16780923994, Rooms)
                 predict <- predict +  0.271834187350E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.97913561848, Bedrooms)
                 predict <- predict +  -0.102899607453 * x
                 x <- ifelse(is.na(Fireplaces), 0.264977645306, Fireplaces)
                 predict <- predict +  0.257527323124 * x
                 x <- ifelse(is.na(Half.Baths), 0.445901639344, Half.Baths)
                 predict <- predict +  0.302185875282 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.37645305514, Garage.1.Area)
                 predict <- predict +  0.331329424133E-01 * x
                 x <- ifelse(is.na(Garage.2.Area), 0.160953800298E-01, Garage.2.Area)
                 predict <- predict +  0.184426274772E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.55710231849, log.Building.Square.Feet)
                 predict <- predict +  0.658083204591 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.375558867362E-01, Number.of.Commercial.Units)
                 predict <- predict +  -0.231275004153 * x
                 x <- ifelse(is.na(Longitude), -87.7881463894, Longitude)
                 predict <- predict +  5.88187317727 * x
                 x <- ifelse(is.na(Latitude), 41.9227365306, Latitude)
                 predict <- predict +  11.5670879750 * x
                 x <- ifelse(is.na(Census.Tract), 538446.008935, Census.Tract)
                 predict <- predict +  0.399008623441E-06 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 6419.74857143, Total.Building.Square.Feet)
                 predict <- predict +  -0.144865198351E-03 * x
                 x <- ifelse(is.na(Full.Baths), 2.06080476900, Full.Baths)
                 predict <- predict +  0.633367723366E-01 * x
                 x <- ifelse(is.na(Age), 63.6977557027, Age)
                 predict <- predict +  -0.240726219417E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 92.0788082653, Number.of.Units)
                 predict <- predict +  -0.154442411330E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.130738255034, Percent.Ownership)
                 predict <- predict +  1.50242202246 * x
                 x <- ifelse(is.na(Sale.Year), 2015.05665931, Sale.Year)
                 predict <- predict +  0.841234717188E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.75901398087, Sale.Month)
                 predict <- predict +  0.185374626493E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1895.33999464, HoodSqFt)
                 predict <- predict +  -0.197028808903E-03 * x
                 x <- ifelse(is.na(PropClassSqFt), 2144.02010318, PropClassSqFt)
                 predict <- predict +  -0.151643914143E-03 * x
                 predict <- predict +  -0.228445959101 * Rooms.NA
                 predict <- predict +  0.299658105617 * Longitude.NA
                 predict <- predict +  0.272724427693 * Total.Building.Square.Feet.NA
                 predict <- predict +  0.381894993403 * Percent.Ownership.NA
                 predict <- min(max(predict, 9.21094019205),15.8169906933)
               } else {
                 nodeid <- 511
                 predict <- -158.448259984
                 x <- ifelse(is.na(log.Land.Square.Feet), 9.26989214867, log.Land.Square.Feet)
                 predict <- predict +  0.229900497447 * x
                 x <- ifelse(is.na(Rooms), 6.58089229383, Rooms)
                 predict <- predict +  -0.110685080664E-01 * x
                 x <- ifelse(is.na(Bedrooms), 3.26543488058, Bedrooms)
                 predict <- predict +  -0.126132759353E-01 * x
                 x <- ifelse(is.na(Fireplaces), 0.291572780532, Fireplaces)
                 predict <- predict +  0.184679237606 * x
                 x <- ifelse(is.na(Half.Baths), 0.466426318161, Half.Baths)
                 predict <- predict +  0.584555178775E-01 * x
                 x <- ifelse(is.na(Garage.1.Area), 1.70286916028, Garage.1.Area)
                 predict <- predict +  0.389864263907E-01 * x
                 x <- ifelse(is.na(Garage.2.Area), 0.141204746883E-01, Garage.2.Area)
                 predict <- predict +  -0.326727062760E-01 * x
                 x <- ifelse(is.na(log.Building.Square.Feet), 7.29868760149, log.Building.Square.Feet)
                 predict <- predict +  0.654422815776 * x
                 x <- ifelse(is.na(Number.of.Commercial.Units), 0.630914826498E-02, Number.of.Commercial.Units)
                 predict <- predict +  -0.212300301618 * x
                 x <- ifelse(is.na(Longitude), -87.8758926614, Longitude)
                 predict <- predict +  0.369573763952 * x
                 x <- ifelse(is.na(Latitude), 41.8356934568, Latitude)
                 predict <- predict +  0.106954112692 * x
                 x <- ifelse(is.na(Census.Tract), 764610.846349, Census.Tract)
                 predict <- predict +  -0.472173527897E-06 * x
                 x <- ifelse(is.na(Total.Building.Square.Feet), 7021.76296296, Total.Building.Square.Feet)
                 predict <- predict +  -0.166125747314E-03 * x
                 x <- ifelse(is.na(Full.Baths), 1.52230734565, Full.Baths)
                 predict <- predict +  0.766804781979E-01 * x
                 x <- ifelse(is.na(Age), 52.7162049697, Age)
                 predict <- predict +  -0.422053187385E-02 * x
                 x <- ifelse(is.na(Number.of.Units), 100.673318130, Number.of.Units)
                 predict <- predict +  -0.106708485345E-02 * x
                 x <- ifelse(is.na(Percent.Ownership), 0.468893879843E-01, Percent.Ownership)
                 predict <- predict +  2.11697131970 * x
                 x <- ifelse(is.na(Sale.Year), 2015.03233860, Sale.Year)
                 predict <- predict +  0.948303725330E-01 * x
                 x <- ifelse(is.na(Sale.Month), 6.85352514564, Sale.Month)
                 predict <- predict +  0.155419556074E-01 * x
                 x <- ifelse(is.na(HoodSqFt), 1856.93463026, HoodSqFt)
                 predict <- predict +  0.993936229922E-04 * x
                 x <- ifelse(is.na(PropClassSqFt), 1654.85599932, PropClassSqFt)
                 predict <- predict +  -0.830393112682E-04 * x
                 predict <- predict +  -0.182766944190 * Rooms.NA
                 predict <- predict +  0.693784174609E-01 * Longitude.NA
                 predict <- predict +  1.01179099502 * Total.Building.Square.Feet.NA
                 predict <- predict +  1.13256196890 * Percent.Ownership.NA
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
    Garage.2.Area <- as.numeric(newdata$Garage.2.Area[i])
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
    Rooms.NA <- ifelse(is.na(Rooms),1,0)
    Longitude.NA <- ifelse(is.na(Longitude),1,0)
    Total.Building.Square.Feet.NA <- ifelse(is.na(Total.Building.Square.Feet),1,0)
    Percent.Ownership.NA <- ifelse(is.na(Percent.Ownership),1,0)
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred <- c(pred,tmp[2])
}

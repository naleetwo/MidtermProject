-- MariaDB dump 10.19  Distrib 10.11.1-MariaDB, for Win64 (AMD64)
--
-- Host: localhost    Database: seoul_re
-- ------------------------------------------------------
-- Server version	10.11.1-MariaDB

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `2018year`
--

DROP TABLE IF EXISTS `2018year`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `2018year` (
  `Gu` varchar(7) NOT NULL,
  `crime` int(11) NOT NULL,
  `bus_station` int(11) NOT NULL,
  `conv_store` int(11) NOT NULL,
  `cafe` int(11) NOT NULL,
  `school` int(11) NOT NULL,
  `hospital` int(11) NOT NULL,
  `academy` int(11) NOT NULL,
  `university` float NOT NULL,
  `apt_buysell` float NOT NULL,
  `apt_rent` float NOT NULL,
  PRIMARY KEY (`Gu`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `2018year`
--

LOCK TABLES `2018year` WRITE;
/*!40000 ALTER TABLE `2018year` DISABLE KEYS */;
INSERT INTO `2018year` VALUES
('강남구',1010,534,558,1679,101,2594,2263,0,1702,654),
('강동구',241,349,220,416,73,771,638,0,858,472),
('강북구',273,460,90,211,41,450,218,0,563,336),
('강서구',424,576,209,748,104,802,707,1,728,406),
('관악구',545,485,250,479,72,681,450,1,642,369),
('광진구',363,286,137,374,52,553,430,3,943,538),
('구로구',405,478,156,407,65,556,427,3,595,360),
('금천구',273,342,133,324,39,339,222,0,516,335),
('노원구',293,537,139,433,118,717,747,6,601,297),
('도봉구',177,386,113,229,56,364,335,1,504,276),
('동대문구',243,318,154,395,58,603,363,4,693,408),
('동작구',416,460,118,416,51,570,458,3,880,502),
('마포구',735,569,219,943,54,715,644,2,989,512),
('서대문구',278,444,124,492,47,430,345,6,727,436),
('서초구',635,648,298,949,62,1201,1130,1,1461,645),
('성동구',204,439,108,344,50,432,314,2,1048,542),
('성북구',303,611,121,433,73,530,464,6,629,407),
('송파구',515,413,108,445,104,1129,1051,1,1201,577),
('양천구',193,363,147,357,79,627,1062,0,811,494),
('영등포구',592,491,263,799,52,738,467,0,845,445),
('용산구',417,349,105,391,44,304,158,1,1200,508),
('은평구',312,499,135,387,84,644,521,1,627,369),
('종로구',364,382,87,877,50,496,274,3,764,419),
('중구',357,207,198,1072,42,558,119,2,871,482),
('중랑구',238,379,144,247,57,498,273,1,541,305);
/*!40000 ALTER TABLE `2018year` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `2019year`
--

DROP TABLE IF EXISTS `2019year`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `2019year` (
  `Gu` varchar(7) NOT NULL,
  `crime` int(11) NOT NULL,
  `bus_station` int(11) NOT NULL,
  `conv_store` int(11) NOT NULL,
  `cafe` int(11) NOT NULL,
  `school` int(11) NOT NULL,
  `hospital` int(11) NOT NULL,
  `academy` int(11) NOT NULL,
  `university` float NOT NULL,
  `apt_buysell` float NOT NULL,
  `apt_rent` float NOT NULL,
  PRIMARY KEY (`Gu`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `2019year`
--

LOCK TABLES `2019year` WRITE;
/*!40000 ALTER TABLE `2019year` DISABLE KEYS */;
INSERT INTO `2019year` VALUES
('강남구',1280,548,597,1794,101,2643,2279,0,2051,676),
('강동구',280,385,254,471,73,789,672,0,1007,471),
('강북구',239,448,107,221,41,461,209,0,665,339),
('강서구',442,603,260,842,104,836,693,1,852,442),
('관악구',579,485,278,527,72,693,443,1,757,380),
('광진구',415,292,181,443,52,552,406,3,1105,546),
('구로구',363,513,184,414,67,559,406,3,707,362),
('금천구',221,362,162,339,39,356,224,0,619,346),
('노원구',272,544,153,475,118,725,740,6,703,304),
('도봉구',136,391,129,249,56,369,317,1,594,282),
('동대문구',249,319,188,408,58,607,368,4,840,423),
('동작구',367,447,141,439,51,576,465,3,1066,499),
('마포구',673,568,202,851,54,730,683,2,1221,536),
('서대문구',268,446,160,525,47,435,346,6,922,463),
('서초구',785,649,314,974,62,1226,1119,1,1755,638),
('성동구',205,442,123,383,50,444,293,2,1245,558),
('성북구',259,604,148,455,73,528,450,6,771,415),
('송파구',459,429,123,476,106,1165,1115,1,1440,576),
('양천구',199,365,174,392,79,626,1037,0,1038,483),
('영등포구',520,504,303,844,52,756,446,0,1039,478),
('용산구',362,344,118,422,44,307,155,1,1463,532),
('은평구',283,511,126,368,85,662,525,1,729,395),
('종로구',370,375,94,896,50,494,259,3,1015,538),
('중구',314,211,215,1135,42,560,118,2,1024,453),
('중랑구',212,380,186,296,57,511,276,1,660,370);
/*!40000 ALTER TABLE `2019year` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `2020year`
--

DROP TABLE IF EXISTS `2020year`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `2020year` (
  `Gu` varchar(7) NOT NULL,
  `crime` int(11) NOT NULL,
  `bus_station` int(11) NOT NULL,
  `conv_store` int(11) NOT NULL,
  `cafe` int(11) NOT NULL,
  `school` int(11) NOT NULL,
  `hospital` int(11) NOT NULL,
  `academy` int(11) NOT NULL,
  `university` float NOT NULL,
  `apt_buysell` float NOT NULL,
  `apt_rent` float NOT NULL,
  PRIMARY KEY (`Gu`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `2020year`
--

LOCK TABLES `2020year` WRITE;
/*!40000 ALTER TABLE `2020year` DISABLE KEYS */;
INSERT INTO `2020year` VALUES
('강남구',1048,545,606,1834,101,2693,2361,0,2117,781),
('강동구',289,383,268,517,74,823,724,0,1170,515),
('강북구',217,415,122,243,41,466,206,0,771,394),
('강서구',410,611,279,907,103,861,694,1,964,453),
('관악구',557,470,265,559,72,698,431,1,871,411),
('광진구',352,288,200,503,52,552,405,3,1291,604),
('구로구',344,510,207,459,68,569,394,3,769,375),
('금천구',183,357,173,375,39,354,212,0,739,412),
('노원구',236,538,165,484,118,734,718,6,844,332),
('도봉구',131,366,137,273,56,367,297,1,675,314),
('동대문구',244,311,188,424,58,586,350,4,995,474),
('동작구',305,440,147,432,51,588,471,3,1230,569),
('마포구',577,570,183,755,54,742,689,2,1344,519),
('서대문구',251,457,191,522,47,441,348,6,1038,499),
('서초구',578,638,321,1005,62,1251,1148,1,1913,760),
('성동구',171,433,126,422,50,456,289,2,1471,618),
('성북구',212,599,165,492,73,540,439,6,914,462),
('송파구',444,440,160,585,106,1194,1076,1,1601,647),
('양천구',189,320,186,415,79,638,1008,0,1143,527),
('영등포구',484,494,326,864,52,779,445,0,1180,506),
('용산구',319,327,129,444,44,310,154,1,1594,579),
('은평구',251,501,148,400,85,677,519,1,827,439),
('종로구',272,374,84,862,50,494,243,3,1034,479),
('중구',283,241,213,1149,42,572,109,2,1138,549),
('중랑구',276,382,196,329,57,531,262,1,760,391);
/*!40000 ALTER TABLE `2020year` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `2021year`
--

DROP TABLE IF EXISTS `2021year`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `2021year` (
  `Gu` varchar(7) NOT NULL,
  `crime` int(11) NOT NULL,
  `bus_station` int(11) NOT NULL,
  `conv_store` int(11) NOT NULL,
  `cafe` int(11) NOT NULL,
  `school` int(11) NOT NULL,
  `hospital` int(11) NOT NULL,
  `academy` int(11) NOT NULL,
  `university` float NOT NULL,
  `apt_buysell` float NOT NULL,
  `apt_rent` float NOT NULL,
  PRIMARY KEY (`Gu`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `2021year`
--

LOCK TABLES `2021year` WRITE;
/*!40000 ALTER TABLE `2021year` DISABLE KEYS */;
INSERT INTO `2021year` VALUES
('강남구',946,520,615,1873,101,2790,2383,0,2351,824),
('강동구',234,365,280,579,76,852,654,0,1437,516),
('강북구',222,409,132,251,41,467,210,0,939,393),
('강서구',434,569,282,984,103,887,697,1,1175,470),
('관악구',528,467,285,593,72,710,434,1,1089,460),
('광진구',325,270,220,551,52,559,426,3,1517,667),
('구로구',341,497,229,474,68,583,386,3,945,364),
('금천구',246,342,203,400,39,356,208,0,914,403),
('노원구',293,527,189,523,118,737,684,6,1116,368),
('도봉구',131,358,149,281,56,366,285,1,899,355),
('동대문구',282,303,204,427,58,596,359,4,1174,513),
('동작구',304,437,161,453,51,595,487,3,1670,630),
('마포구',524,563,221,906,54,762,692,2,1568,585),
('서대문구',292,454,201,496,47,437,342,6,1256,533),
('서초구',540,605,317,1004,62,1306,1146,1,2208,796),
('성동구',220,445,144,437,50,460,290,2,1806,658),
('성북구',222,601,180,524,73,533,451,6,1144,497),
('송파구',496,414,208,692,106,1214,1045,1,1983,672),
('양천구',202,317,198,425,79,651,979,0,1353,535),
('영등포구',480,466,330,916,53,793,440,0,1378,554),
('용산구',248,328,130,449,44,323,137,1,1903,623),
('은평구',293,497,166,408,84,688,509,1,1042,444),
('종로구',271,369,85,850,50,489,217,3,1239,568),
('중구',282,200,212,1148,42,572,91,2,1285,566),
('중랑구',273,380,223,367,57,535,257,1,969,419);
/*!40000 ALTER TABLE `2021year` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `economics`
--

DROP TABLE IF EXISTS `economics`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `economics` (
  `Quarter` varchar(15) NOT NULL,
  `unit_price` float NOT NULL,
  `price_rate` float NOT NULL,
  `GDP` float NOT NULL,
  `basic_rate` float NOT NULL,
  `unemp_rate` float NOT NULL,
  `move_rate` float NOT NULL,
  PRIMARY KEY (`Quarter`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `economics`
--

LOCK TABLES `economics` WRITE;
/*!40000 ALTER TABLE `economics` DISABLE KEYS */;
INSERT INTO `economics` VALUES
('2006/Q1',497,75.48,246614,4,5.2,0.281827),
('2006/Q2',424,75.951,248153,4.25,4.3,0.271969),
('2006/Q3',407,76.571,253913,4.5,4.3,0.239791),
('2006/Q4',462,76.323,256922,4.5,4.5,0.308306),
('2007/Q1',393,76.992,263441,4.5,4.5,0.295117),
('2007/Q2',445,77.811,269569,4.5,3.9,0.253211),
('2007/Q3',448,78.332,274330,5,3.9,0.220113),
('2007/Q4',478,78.902,282320,5,4,0.271817),
('2008/Q1',491,79.919,283234,5,4.3,0.26604),
('2008/Q2',542,81.581,290039,5,3.8,0.271422),
('2008/Q3',554,82.673,293765,5.25,3.8,0.237121),
('2008/Q4',589,82.449,287178,3,3.8,0.228137),
('2009/Q1',633,83.045,289784,2,4.7,0.2398),
('2009/Q2',638,83.837,298340,2,4.8,0.230684),
('2009/Q3',634,84.31,307751,2,4.5,0.227369),
('2009/Q4',617,84.434,309472,2,4.1,0.224894),
('2010/Q1',631,85.523,320326,2,5.7,0.228222),
('2010/Q2',578,86.07,329462,2,4.5,0.214636),
('2010/Q3',597,86.733,333669,2.25,4.4,0.190457),
('2010/Q4',633,87.165,339153,2.5,4.3,0.214437),
('2011/Q1',587,88.806,340957,3,5.1,0.230421),
('2011/Q2',575,89.497,343886,3.25,4.6,0.211894),
('2011/Q3',584,90.476,348648,3.25,4.3,0.200876),
('2011/Q4',595,90.62,355446,3.25,4.1,0.205071),
('2012/Q1',569,91.481,357741,3.25,5,0.211282),
('2012/Q2',579,91.671,358200,3.25,4.2,0.188529),
('2012/Q3',561,91.933,359966,3,4.1,0.16918),
('2012/Q4',578,92.175,364204,2.75,3.5,0.200333),
('2013/Q1',581,92.906,368436,2.75,4.5,0.199061),
('2013/Q2',564,92.785,373119,2.5,4.1,0.188629),
('2013/Q3',537,93.189,377743,2.5,3.6,0.168216),
('2013/Q4',577,93.16,381521,2.5,3.5,0.195043),
('2014/Q1',581,93.954,387114,2.5,4.5,0.20088),
('2014/Q2',567,94.277,388610,2.5,4.7,0.191046),
('2014/Q3',587,94.476,390535,2.25,4.4,0.181516),
('2014/Q4',587,94.075,396669,2,4.1,0.19787),
('2015/Q1',593,94.609,408685,1.75,4.6,0.200385),
('2015/Q2',605,94.808,411778,1.5,4.6,0.202431),
('2015/Q3',611,95.086,417096,1.5,4,0.186309),
('2015/Q4',651,94.941,420462,1.5,3.7,0.202321),
('2016/Q1',630,95.422,429164,1.5,4.7,0.196272),
('2016/Q2',690,95.605,435020,1.25,4.2,0.180203),
('2016/Q3',684,95.785,434707,1.25,4.1,0.181166),
('2016/Q4',680,96.319,441888,1.25,3.9,0.199301),
('2017/Q1',708,97.521,450590,1.25,4.9,0.200775),
('2017/Q2',754,97.442,454357,1.25,4.5,0.171468),
('2017/Q3',766,97.91,467730,1.25,4.5,0.180837),
('2017/Q4',850,97.707,463020,1.5,4.2,0.168318),
('2018/Q1',796,98.571,468070,1.5,5.1,0.192268),
('2018/Q2',755,98.896,472616,1.5,5,0.1683),
('2018/Q3',879,99.424,479431,1.5,4.9,0.157278),
('2018/Q4',824,99.453,478075,1.75,4.1,0.183311),
('2019/Q1',866,99.105,474927,1.75,5.3,0.189613),
('2019/Q2',1063,99.541,480818,1.75,4.8,0.152243),
('2019/Q3',1062,99.469,482307,1.5,4,0.155202),
('2019/Q4',1047,99.747,486446,1.25,3.5,0.173654),
('2020/Q1',921,100.06,480213,0.75,4.5,0.210129),
('2020/Q2',1080,99.55,475813,0.5,5.3,0.16936),
('2020/Q3',1162,100.19,489773,0.5,4,0.171727),
('2020/Q4',1287,100.2,494928,0.5,4.4,0.177878),
('2021/Q1',1315,101.49,504874,0.5,6.1,0.190241),
('2021/Q2',1371,102.03,514696,0.5,5.2,0.157394),
('2021/Q3',1427,102.73,521818,0.75,4,0.162507),
('2021/Q4',1443,103.75,530269,1,4,0.154705),
('2022/Q1',1382,105.35,532766,1.25,4.4,0.164387),
('2022/Q2',1452,107.54,540784,1.75,3.6,0.132973),
('2022/Q3',1269,108.76,538804,2.5,2.8,0.12787),
('2022/Q4',1249,109.2,539324,3.25,2.9,0.12318);
/*!40000 ALTER TABLE `economics` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2023-05-01  9:43:30

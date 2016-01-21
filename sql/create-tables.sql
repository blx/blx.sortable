CREATE TABLE `product` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(200) NOT NULL,
  `manufacturer` varchar(80) DEFAULT NULL,
  `family` varchar(80) DEFAULT NULL,
  `model` varchar(80) DEFAULT NULL,
  `announced_date` date DEFAULT NULL,
  PRIMARY KEY (`id`),
  FULLTEXT KEY `idx_manufacturer` (`manufacturer`)
) ENGINE=InnoDB AUTO_INCREMENT=2792 DEFAULT CHARSET=utf8;

CREATE TABLE `listing` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `title` varchar(600) NOT NULL,
  `manufacturer` varchar(80) DEFAULT NULL,
  `currency` varchar(10) DEFAULT NULL,
  `price` decimal(9,2) DEFAULT NULL,
  PRIMARY KEY (`id`),
  FULLTEXT KEY `idx_manufacturer` (`manufacturer`)
) ENGINE=InnoDB AUTO_INCREMENT=23266 DEFAULT CHARSET=utf8;
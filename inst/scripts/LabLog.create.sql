CREATE TABLE `FRAMES` (
  `frameid` int(10) unsigned NOT NULL auto_increment,
  `expid` int(10) unsigned NOT NULL default '0',
  `prefix` varchar(20) NOT NULL default 'exp',
  PRIMARY KEY  (`frameid`,`expid`),
  KEY `frameid` (`frameid`),
  KEY `prefix` (`prefix`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
CREATE TABLE `JOURNAL` (
  `expid` int(10) unsigned NOT NULL auto_increment,
  `sample` int(10) unsigned NOT NULL default '0',
  `type` varchar(20) default NULL,
  `date` datetime default NULL,
  PRIMARY KEY  (`expid`,`sample`),
  KEY `expid` (`expid`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
CREATE TABLE `SAMPLES` (
  `sample` smallint(5) unsigned NOT NULL auto_increment,
  `sid` varchar(20) default NULL,
  `composition` varchar(60) default NULL,
  PRIMARY KEY  (`sample`),
  KEY `sid` (`sid`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
 CREATE TABLE `PARAMS` (
  `frameid` int(10) unsigned NOT NULL default '0',
  `parname` varchar(20) NOT NULL default '',
  `parunit` varchar(20) default NULL,
  `value` float default NULL,
  PRIMARY KEY  (`frameid`,`parname`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

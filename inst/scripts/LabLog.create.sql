 CREATE TABLE JOURNAL (
   expid smallint(5) unsigned NOT NULL auto_increment,
   sample smallint(5) unsigned NOT NULL default '0',
   type varchar(20) default NULL,
   date datetime default NULL,
   PRIMARY KEY  (expid,sample)
 ) TYPE=MyISAM;
 CREATE TABLE PARGROUPS (
   pargroup smallint(5) unsigned NOT NULL auto_increment,
   comment varchar(20) default NULL,
   PRIMARY KEY  (pargroup)
 ) TYPE=MyISAM;
 CREATE TABLE PARS (
   parid smallint(5) unsigned NOT NULL auto_increment,
   expid smallint(5) unsigned NOT NULL default '0',
   pargroup smallint(5) unsigned NOT NULL default '0',
   parname varchar(20) default NULL,
   parunit varchar(20) default NULL,
   PRIMARY KEY  (parid,expid,pargroup)
 ) TYPE=MyISAM;
 CREATE TABLE PARVALUES (
   parid smallint(5) unsigned NOT NULL default '0',
   tuple smallint(5) unsigned NOT NULL default '0',
   value float default NULL,
   PRIMARY KEY  (parid,tuple)
 ) TYPE=MyISAM;
 CREATE TABLE SAMPLES (
   sample smallint(5) unsigned NOT NULL auto_increment,
   sid varchar(20) default NULL,
   composition varchar(60) default NULL,
   PRIMARY KEY  (sample)
 ) TYPE=MyISAM;

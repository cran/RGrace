select SAMPLES.*,
	JOURNAL.expid,
	JOURNAL.type,
	JOURNAL.date,
	group_concat(distinct concat(if(PARAMS.parname is null, "" , PARAMS.parname)," = ",if(PARAMS.value is null, "" , PARAMS.value)," ", if(PARAMS.parunit is null, "" , PARAMS.parunit)) order by PARAMS.parname separator "\n") as param,
	concat(FRAMES.prefix,FRAMES.frameid) as tablename,
	group_concat(distinct concat(information_schema.columns.column_name,"(",information_schema.columns.column_comment,")") order by information_schema.columns.ordinal_position separator "\n") as 'raw data'
	from SAMPLES 
	join JOURNAL using (sample) 
	join (FRAMES 
		join PARAMS using (frameid)
		left join information_schema.columns on (information_schema.columns.table_name=concat(FRAMES.prefix,FRAMES.frameid))) on (JOURNAL.expid=FRAMES.expid)
where (information_schema.columns.table_schema='LabLog' or FRAMES.prefix='') 
group by FRAMES.frameid 
order by SAMPLES.sample,JOURNAL.expid,FRAMES.frameid 
limit 10;
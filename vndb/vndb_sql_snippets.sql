-- I'm not 100% sure doing select X in (subquery) will work.

-- select VNs by producer given producer name
select * from VNs
where id in (select distinct vn from relations
             where producer in (select distinct id from producers
                                where name like @name));
-- select VNs by tag
select * from VNs
where id in (select distinct vn from vn_tags
             where tag = (select id from tags where name = @name));

-- select tags of VN
select * from tags
where id = (select ag from vn_tags
            where vn = (select id from VNs where name = @name))

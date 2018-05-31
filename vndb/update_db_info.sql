with vn_info as (select COUNT(*) as num_vns, MAX(id) as max_vn_id from VNs)
     release_info as (select COUNT(*) as num_releases, 
                      MAX(id) as max_release_id from releases)
     producer_info as (select COUNT(*) as num_producers, 
                       MAX(id) as max_producer_id from producers)
     character_info as (select COUNT(*) as num_characters, 
                        MAX(id) as max_character_id from characters)
     staff_info as (select COUNT(*) as num_staff, 
                    MAX(id) as max_staff_id from staff)
  insert or replace into db_stats (num_vns, max_vn_id, 
                                   num_releases, max_release_id, 
                                   num_producers, max_producer_id, 
                                   max_characters, max_character_id,
                                   num_staff, max_staff_id, row_limiter)
  values (vn_info.num_vns, vn_info.max_vn_id, 
          release_info.num_releases, release_info.max_release_id, 
          producer_info.num_producers, producer_info.max_producer_id,
          character_info.max_characters, character_info.max_character_id,
          staff_info.num_staff, staff_info.max_staff_id, 1);

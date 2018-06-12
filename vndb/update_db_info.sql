update db_info set
  num_vns = (select COUNT(*) from VNs),
  max_vn_id = (select MAX(id) from VNs),
  num_releases = (select COUNT(*) from releases),
  max_release_id = (select MAX(id) from releases),
  num_producers = (select COUNT(*) from producers),
  max_producer_id = (select MAX(id) from producers),
  num_characters = (select COUNT(*) from characters),
  max_character_id = (select MAX(id) from characters),
  num_staff = (select COUNT(*) from staff),
  max_staff_id = (select MAX(id) from staff),
  num_tags = (select COUNT(*) from tags),
  max_tag_id = (select MAX(id) from tags),
  num_traits = (select COUNT(*) from traits),
  max_trait_id = (select MAX(id) from traits)
where row_limiter = 1;

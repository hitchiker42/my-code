#ifndef __SQL_H__
#define __SQL_H__
/*
  Text of sql prepared statments.
*/
#include <string_view>
using namespace std::literals::string_view_literals;
//Select X by id.
static constexpr std::string_view sql_select_vn_by_id =
  "select * from VNs where id = @id;"sv;
static constexpr std::string_view sql_select_producer_by_id =
  "select * from producers where id = @id;"sv;
static constexpr std::string_view sql_select_release_by_id =
  "select * from releases where id = @id;"sv;
static constexpr std::string_view sql_select_character_by_id =
  "select * from characters where id = @id;"sv;
static constexpr std::string_view sql_select_staff_by_id =
  "select * from staff where id = @id;"sv;
static constexpr std::string_view sql_select_tag_by_id =
  "select * from tags where id = @id;"sv;
static constexpr std::string_view sql_select_trait_by_id =
  "select * from traits where id = @id;"sv;
//static constexpr std::string_view sql_select_vn_image_by_id =
//  "select image from vn_images where vn = @id;"sv;
//static constexpr std::string_view sql_select_character_image_by_id =
//  "select image from character_images where character = @id;"sv;
//These are used instead of the simple versions commented out above because
//they are more useful, and can be used the same was as the simple 
//versions if needed.
static constexpr std::string_view sql_select_vn_image_by_id =
  "select vn_images.image, VNs.title from 
     VNs join vn_images on VNs.id == vn_images.vn where VNs.id == @id;"sv;
static constexpr std::string_view sql_select_character_image_by_id =
  "select character_images.image, characters.name from 
     characters join character_images on 
       character.id == character_images.character
     where character.id == @id;"sv;
/*
  Select statements using derived tables
*/
static constexpr std::string_view sql_select_vn_by_tag_name = 
  R"EOF(select * from VNs
        where id in
          (select distinct vn from vn_tags
           where tag = (select id from tags where name like @name));)EOF"sv;
static constexpr std::string_view sql_select_vn_by_producer_name = 
  R"EOF(select * from VNs
        where id in
          (select distinct vn from vn_producer_relations
           where producer = (select id from producers 
                             where name like @name));)EOF"sv;
static constexpr std::string_view sql_select_character_by_trait_name = 
  R"EOF(select * from characters
        where id in
          (select distinct character from character_traits
           where trait = (select id from tags where name like @name));)EOF"sv;
// Insert statements for the base tables (the actual information)
static constexpr std::string_view sql_insert_vn =
  R"EOF(insert or replace into VNs values (
        @id, @title, @original, @released,
        @languages, @orig_lang, @platform, @aliases,
        @length, @description, @links, @image, @image_nsfw,
        @anime, @relations, @tags, @popularity, @rating,
        @votecount, @screens, @staff, '[]', '[]' ,'[]',
        -1, -1, @date, 0);)EOF"sv;
static constexpr std::string_view sql_insert_release =
  R"EOF(insert or replace into releases values (
        @id, @title, @original, @released, @type,
        @patch, @languages, @website, @notes, @minage,
        @platforms, @resolution, @voiced, @animation,
        @vn, @producers);)EOF"sv;
static constexpr std::string_view sql_insert_producer =
  R"EOF(insert or replace into producers values (
        @id, @name, @original, @type, @language,
        @links, @aliases, @description, @relations);)EOF"sv;
static constexpr std::string_view sql_insert_staff =
  R"EOF(insert or replace into staff values (
        @id, @name, @original, @gender, @language, @links,
        @description, @aliases, @main_ailas, @vns, @voiced);)EOF"sv;
static constexpr std::string_view sql_insert_character =
  R"EOF(insert or replace into characters values (
        @id, @name, @original, @gender, @aliases,
        @description, @image, @traits, @vns, @voiced);)EOF"sv;
static constexpr std::string_view sql_insert_trait =
  R"EOF(insert or replace into traits values (
        @id, @name, @full_name, @description,
        @meta, @chars, @aliases, @parents);)EOF"sv;
static constexpr std::string_view sql_insert_tag =
  R"EOF(insert or replace into tags values (
        @id, @name, @description, @meta, @vns,
        @category, @aliases, @parents);)EOF"sv;
//Insert statements for the derived tables (relations between tables)
static constexpr std::string_view sql_insert_vn_producer_relation =
  R"EOF(insert or ignore into vn_producer_relations values (
        @vn_id, @producer_id, @release_id);)EOF"sv;
static constexpr std::string_view sql_insert_vn_tag =
  R"EOF(insert or replace into vn_tags values (@vn_id, @tag_id, @score);)EOF"sv;
static constexpr std::string_view sql_insert_character_trait =
  R"EOF(insert or ignore into character_traits values (
        @character_id, @trait_id);)EOF"sv;
static constexpr std::string_view sql_insert_vn_character_actor_relation =
  R"EOF(insert or ignore into vn_character_actor_relations values (
        @vn_id, @character_id, @actor_id);)EOF"sv;
static constexpr std::string_view sql_insert_vn_staff_relation =
  R"EOF(insert or ignore into vn_staff_relations values (
        @vn_id, @staff_id);)EOF"sv;
static constexpr std::string_view sql_insert_staff_alias =
  R"EOF(insert or replace into staff_aliases values (
        @staff_id, @alias_id, @alias_name);)EOF"sv;
// //Currently updating a vnlist entry will clobber any existing vote info,
// //so updating the vnlist and votelist must be done together.
// static constexpr std::string_view sql_insert_vnlist_entry =
//   R"EOF(insert or replace into vnlist (vn, status, added, notes) 
//           values (@vn_id, @status, @added, @notes);)EOF"sv;
// //This is an update rather than an insert since we store votes as
// //a part of the vnlist table.
// static constexpr std::string_view sql_insert_votelist_entry =
//   R"EOF(update vnlist set
//          vote = @vote, vote_added = @added
//          where vn = @vn_id;)EOF"sv;
/*
  Since the vnlist and votelist are stored in the same table
  we clobber the votelist info when we replace a vnlist entry, a solution
  to this is what sqlite calls an upsert, basically it just combines an 
  insert and an update.
  This is only available in very recent versions of sqlite, so we need
  build sqlite ourselves
*/

static constexpr std::string_view sql_insert_vnlist_entry =
  R"EOF(insert into vnlist (vn, status, added, notes) 
          values (@vn_id, @status, @added, @notes)
        on conflict(vn) do update
        set status = @status, added = @added, notes = @notes;)EOF"sv;
static constexpr std::string_view sql_insert_votelist_entry =
  R"EOF(insert into vnlist (vn, vote, vote_added)
          values (@vn_id, @vote, @added)
        on conflict(vn) do update
        set vote = @vote, vote_added = @added;)EOF"sv;

static constexpr std::string_view sql_insert_wishlist_entry =
  R"EOF(insert or replace into wishlist(vn, priority, added) 
          values (@vn_id, @priority, @added);)EOF"sv;
static constexpr std::string_view sql_insert_vn_image =
  R"EOF(insert or replace into vn_images values (
        @vn_id, @image_blob);)EOF"sv;
static constexpr std::string_view sql_insert_character_image =
  R"EOF(insert or replace into character_images values (
        @character_id, @image_blob);)EOF"sv;
/*
  These queries find ids missing from a table, the basic idea is to
  create a table of integers 1 - max_id using the with recursive clause
  then do a left outer join on the table of interest on the id 
  (an outer join includes all rows even if the column to join on in null in
   one of the tables) then omit any rows where the id is non null.
*/
static constexpr std::string_view sql_find_missing_tags =
  R"EOF(with recursive
         seq(x) as (values(1) union all select x+1 from seq 
           where x < (select max_tag_id from db_info))
        select seq.x
          from seq left outer join tags on seq.x = tags.id
          where tags.id is null)EOF"sv;
static constexpr std::string_view sql_find_missing_traits =
  R"EOF(with recursive
         seq(x) as (values(1) union all select x+1 from seq 
           where x < (select max_trait_id from db_info))
        select seq.x
          from seq left outer join traits on seq.x = traits.id
          where traits.id is null)EOF"sv;
#endif /* __SQL_H__ */

/* Local Variables: */
/* mode: c++ */
/* End: */

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
  R"EOF(insert or relpace into releases values (
        @id, @title, @original, @released, @type,
        @patch, @languages, @website, @notes, @minage,
        @platforms, @resolution, @voiced, @animation,
        @vn, @producers);)EOF"sv;
static constexpr std::string_view sql_insert_producer =
  R"EOF(insert or relpace into producers values (
        @id, @name, @original, @type, @language,
        @links, @aliases, @description, @relations);)EOF"sv;
static constexpr std::string_view sql_insert_character =
  R"EOF(insert or relpace into staff values (
        @id, @name, @original, @gender, @language, @links,
        @description, @aliases, @main_ailas, @vns, @voiced);)EOF"sv;
static constexpr std::string_view sql_insert_staff =
  R"EOF(insert or relpace into characters values (
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
static constexpr std::string_view sql_insert_relation =
  R"EOF(insert or relpace into relations values (
        @vn_id, @producer_id, @release_id);)EOF"sv;
static constexpr std::string_view sql_insert_vn_tags =
  R"EOF(insert or relpace into vn_tags (@vn_id, @tag_id);)EOF"sv;
static constexpr std::string_view sql_insert_character_traits =
  R"EOF(insert or relpace into character_traits (
        @character_id, @trait_id);)EOF"sv;
static constexpr std::string_view sql_insert_vn_character_actor_relations =
  R"EOF(insert or relpace into vn_character_actor_relations (
        @vn_id, @character_id, @actor_id);)EOF"sv;
  
#endif /* __SQL_H__ */

/* Local Variables: */
/* mode: c++ */
/* End: */

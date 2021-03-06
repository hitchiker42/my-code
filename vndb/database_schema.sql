-- We don't really need foreign_keys since we rarely if ever delete anything,
-- but I'm including them where they make sense, just as a kind of failsafe.
pragma foreign_keys = true;


-- Main tables

create table if not exists VNs (
id integer primary key not null,
title text not null,
original text,
released text, --date
languages json, --json array of strings
orig_lang json, --""
platform json, --""
aliases json, -- list of aliases as a json array.
-- length is a builtin function, so add a prefix for clarity.
vn_length integer check (vn_length between 1 and 5),
description text,
links json, -- json object
image text, -- url
image_nsfw int not null check (image_nsfw between 0 and 1),
anime json, --json array of objects
-- may make into just an array of ints
relations json, --json array of objects
tags json, --json array of arrays
popularity real not null check(popularity between 0 and 100),
rating real not null check(rating between 1 and 10 or rating is 0.0),
votecount integer not null,
screens json, -- json array of objects
-- may change to array of ints
staff json, -- json array of objects
--Additional columns
releases json default '[]', -- json array of ints (mabybe make into an object?)
producers json default '[]', -- ""
characters json default '[]', -- ""
vnlist_id integer, -- references vnlist (id)
wishlist_id integer, -- references wishlist(id)
last_cached numeric default current_timestamp, -- timestamp of last access
times_accessed integer default 0
);
-- A title index won't be all that useful for searching by title, since
-- that will usually involve using 'like' or 'regexp', but it is useful
-- for sorting by title, which we often want to do
create index if not exists VN_title_idx on VNs (title);
create index if not exists VN_date_idx on VNs (released);
create index if not exists VN_rating on VNs (rating);

create table if not exists releases (
id integer primary key not null,
title text not null,
original text,
released text, -- date released, or tba, or null if unknown
type text, -- complete, partial, trial
patch integer not null, --boolean indicating if this is a patch
-- freeware integer,
-- doujin integer,
languages text, -- list of languages
website text,
notes text,
minage integer,
-- gtin text, -- JAN/UPC/EAN code
-- catalog text, --catalog number
platforms json, -- list of platforms this release is for
-- media json,  -- array of objects {type, quantity}
resolution string, --screen resolution
voiced intger, --boolean is this vn voiced
animation json, -- 2 elt array story/ero animations on a scale of 1-4
vn json not null, --array of objects {id, title original}
producers json not null --array of objects {id, developer (bool),
                        -- producer(bool), name, original, type}
);

create table if not exists  producers (
id integer primary key not null,
name text not null,
original text,
type string,
language string,
links json,
aliases json,
description text,
relations json
);
create index if not exists producer_name_idx on producers (name);

create table if not exists characters (
id integer primary key not null,
name text not null,
original text,
gender text check (gender isnull or gender in ('m','f','b')),
-- bloodt text check (bloodt isnull or bloodt in('a','b','ab','o'))
-- birthday
aliases json,
description text,
image text,
traits json not null,
vns json not null,
voiced json not null
);
create index if not exists character_name_idx on characters (name);

create table if not exists staff (
id integer primary key not null,
name text not null,
original text,
gender text,
language text not null,
links json not null,
description text,
aliases json not null,
main_alias integer not null unique,
vns json not null,
voiced json not null
);


create table if not exists tags (
id integer primary key not null,
name text unique not null,
description text not null,
meta bool not null,
vns integer not null, -- count of vns with this tag
category string not null,
aliases json not null, -- array of strings
parents json not null -- array of ints
);
create index if not exists tag_name_idx on tags(name);

create table if not exists  traits (
id integer primary key not null,
name text not null,
full_name text unique not null,
description text not null,
meta bool not null,
-- may replace with array of ints
chars integer not null,
aliases json not null, -- array of strings
parents json not null -- array of ints
);
create index if not exists traits_name_idx on traits(name);
create index if not exists traits_full_name_idx on traits(full_name);

-- Info about above tables, the max_id fields are given to make it
-- easy to update the database with new info from the server
create table if not exists db_info (
num_vns integer not null,
max_vn_id integer not null,
num_releases integer not null,       
max_release_id integer not null,       
num_producers integer not null,
max_producer_id integer not null,
num_characters integer not null,
max_character_id integer not null,
num_staff integer not null,
max_staff_id integer not null,
num_tags integer not null,
max_tag_id integer not null,
num_traits integer not null,
max_trait_id integer not null,
-- Limit table to only one row
row_limiter integer not null primary key check (row_limiter = 1)
);
-- Make sure db_info has a row, since we modify it using update statements.
insert or ignore into db_info values (0,0,0,0,0,0,0,0,0,0,0,0,0,0,1);

-- Aux tables, Don't hold core database objects, but still contain unique data.

-- Tables to hold images for characters and vns, these store
-- the raw jpeg image as a blob.
create table if not exists vn_images (
vn integer primary key not null references VNs (id),
image blob
);
create table if not exists character_images (
character integer primary key not null references characters (id),
image blob
);

-- Tables for wishlist / vnlist
create table if not exists vnlist (
vn integer primary key not null references VNs (id),
status integer check (status between 0 and 4),
added integer, -- unix timestamp
notes text, 
-- Information from votelist, votes on vns not on the vnlist aren't stored.
vote real,
vote_added integer, --unix timestamp
-- Info exclusive to the local database.
path text, -- where this vn is located
-- If the VN doesn't work for some reason (text hooking, needs a crack, 
-- locale issues, etc) this will be non null and explain the reason.
not_playable text
);
-- Indexes go here.

create table if not exists wishlist (
vn integer primary key not null references VNs(id),
priority integer check (priority between 0 and 3),
added integer, --unix timestamp
-- exclusive to local database
notes text
);

-- Derived tables
-- Store relations between tables, most are without rowid tables. 

-- Table which relates vns, producers and releases
-- release is a keyword, so add _id as a suffix. (or just quote it)
create table if not exists vn_producer_relations (
vn integer not null references VNs (id),
producer integer not null references producers (id),
release_id integer not null references releases (id),
primary key (vn, producer, release_id)
) without rowid;
--index vns and producers, no need to index releases
--we may be able to remove this index, since we added without rowid.
create index if not exists vp_relations_vn_idx
       on vn_producer_relations (vn);
create index if not exists vp_relations_producer_idx
       on vn_producer_relations (producer);

-- Relations between VNs, characters, and voice actors
create table if not exists vn_character_actor_relations (
vn integer not null references VNs (id),
character integer not null references characters (id),
actor integer not null references staff (id),
-- Technically this isn't unique, you could have someone creditied
-- twice for the same character in one vn under two different aliases.
primary key (vn, character, actor)
) without rowid;
create index if not exists vca_relations_vn_idx
       on vn_character_actor_relations (vn);
create index if not exists vca_relations_character_idx
       on vn_character_actor_relations (character);
create index if not exists vca_relations_actor_idx
       on vn_character_actor_relations (actor);

-- Relations between VNs and staff (excluding voice actors)
-- There is also an alias id and role associated with each relation
-- that you can get by parsing the vns field of the 'staff'.
create table if not exists vn_staff_relations (
vn integer not null references VNs (id),
staff integer not null references staff (id),
primary key (vn, staff)
) without rowid;
create index if not exists vs_relations_vn_idx
       on vn_staff_relations (vn);
create index if not exists vs_relations_staff_idx
       on vn_staff_relations (staff);

-- Table which maps staff aliases (both ids and names) into staff ids.
-- When looking for staff members by name use this table.
create table if not exists staff_aliases (
staff_id integer not null references staff (id),
-- Since this is unqiue we may as well make it the primary key
alias_id integer not null primary key,
alias_name text not null
);
create index if not exists staff_aliases_staff_idx on staff_aliases (staff_id);
create index if not exists staff_aliases_name_idx on staff_aliases (alias_name);
-- I'm pretty sure this won't do anything since it's the primary key.
create index if not exists staff_aliases_alias_idx on staff_aliases (alias_id);

--table which relates vns and tags
create table if not exists vn_tags (
vn integer not null references VNs (id),
tag integer not null references tags (id),
score real not null check (score between 0.0 and 3.0),
primary key (vn, tag)
) without rowid;
--index both vns and tags since lookup on both is likely.
-- I'm not sure about score.
create index if not exists vn_tags_vn_idx on vn_tags (vn);
create index if not exists vn_tags_tag_idx on vn_tags (tag);

--table which relates characters and traits
create table if not exists character_traits (
character integer not null references characters (id),
trait integer not null references traits (id),
primary key (character, trait)
) without rowid;
create index if not exists character_traits_character_idx
       on character_traits (character);
create index if not exists character_traits_trait_idx
       on character_traits (trait);

-- Local Variables:
-- sql-product: sqlite
-- End:

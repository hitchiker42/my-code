pragma foreign_keys = true;

create table if not exists VNs (
id integer primary key not null,
title text not null,
original text,
released text, --date
languages json, --json array of strings
orig_lang json, --""
platform json, --""
aliases string, -- list of aliases, with some delimiter (may change to json)
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
rating real not null check(rating between 1 and 10),
votecount integer not null,
screens json, -- json array of objects
-- may change to array of ints
staff json, -- json array of objects
--Additional columns 
releases json default '[]', -- json array of ints (mabybe make into an object?)
producers json default '[]', -- ""
characters json default '[]', -- ""
vnlist_id integer default -1, -- references vnlist (id)
wishlist_id integer default -1, -- references wishlist(id)
last_cached numeric default current_timestamp, -- timestamp of last access
times_accessed integer default 0
);
-- A title index won't be all that useful for searching by title, since
-- that will usually involve using 'like' or 'regexp', but it is useful
-- for sorting by title, which we often want to do
create index if not exists VN_title_idx on VNs (title);
create index if not exists VN_date_idx on VNs (released);
create index if not exists VN_rating on VNs (rating);

create table if not exists  producers (
id integer primary key not null,
name text not null,
original text,
type string,
language string,
links json,
aliases string,
description text,
relations json
);
create index if not exists producer_name_idx on producers (name);

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

-- Table which relates vns, producers and releases 
-- release is a keyword, so add _id as a suffix.
create table if not exists relations (
release_id integer not null references releases (id),
vn integer not null references VNs (id),
producer integer not null references producers (id),
primary key (release_id, vn, producer)
);
--index vns and producers, no need to index releases
create index if not exists relations_vn_idx on relations (vn);
create index if not exists relations_producer_idx on relations (producer);

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

--table which relates vns and tags
create table if not exists vn_tags (
vn integer not null references VNs (id),
tag integer not null references tags (id),
primary key (vn, tag)
);
--index both vns and tags since lookup on both is likely.
create index if not exists vn_tags_vn_idx on vn_tags (vn);
create index if not exists vn_tags_tag_idx on vn_tags (tag);

-- Local Variables:
-- sql-product: sqlite
-- End:

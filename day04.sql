create or replace function parse_range(text) returns int8range language sql as $$
  select int8range(split_part($1, '-', 1)::int,
                   split_part($1, '-', 2)::int + 1)
$$;

create temp table raw_pairs (a text, b text);

\copy raw_pairs from './day04.txt' with delimiter ',' csv

create table pairs as select parse_range(a) as a, parse_range(b) as b from raw_pairs;

select format('Part 1: %s', count(*)) from pairs where a @> b or b @> a;

select format('Part 2: %s', count(*)) from pairs where a && b;



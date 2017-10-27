package io.chaordic.db

case object Postgres extends Dialect{
  def metadataBool(s: String): Boolean = {
    s.toLowerCase.trim match{
      case "yes" => true
      case "no" => false
    }
  }
}
/*
serial
float4
point
int8
bit
path
polygon
timestamp
bigserial
circle
json
cidr
line
txid_snapshot
uuid
hstore
tsvector
pg_lsn
text
macaddr
float8
bytea
tsquery
bpchar
interval
int4
bool
date
xml
inet
money
varbit
_uuid
box
varchar
numeric
time
int2
lseg
jsonb
 */

/*
numeric_col numeric
text_col text
smallint_col int2
bigint_col int8
bytea_col bytea
macaddr_col macaddr
polygon_col polygon
txid_snapshot_col txid_snapshot
integer_col int4
bigserial_col bigserial
bitv_col varbit
json_col json
inet_col inet
uuid_array _uuid
characterv_col varchar
point_col point
tsquery_col tsquery
boolean_col bool
pg_lsn_col pg_lsn
character_col bpchar
line_col line
cidr_col cidr
date_col date
box_col box
smallserial_col int2
bit_col bit
uuid_col uuid
path_col path
id serial
circle_col circle
interval_col interval
lseg_col lseg
hstore_column hstore
jsonb_col jsonb
timestamp_col timestamp
time_col time
money_col money
xml_col xml
double_col float8
real_col float4
tsvector_col tsvector
 */
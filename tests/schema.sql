create database sweet_test default charset utf8;

use sweet_test;

create table foos (
    c_bigint bigint default 10,
    c_blob blob(1024),
    c_bool bool default 0,
    c_char char default 'c',
    c_date date not null,
    c_datetime datetime not null,
    c_double double precision not null,
    c_integer integer default 100 null,
    c_numeric numeric null,
    c_real real,
    c_smallint smallint, 
    c_text text(1024),
    c_time time,
    c_varbinary varbinary(255),
    c_varchar varchar(64) not null default ''
);


create table users (
    id int auto_increment primary key ,
    name varchar(32) not null default '',
    age int not null default 20
);

create table mobiles (
    id int auto_increment primary key ,
    name varchar(32) not null default '',
    user_id int not null,
    foreign key (user_id) references users(id)
);

create table fathers (
    id int auto_increment primary key ,
    name varchar(32) not null default '',
    user_id int not null,
    foreign key (user_id) references users(id)
);

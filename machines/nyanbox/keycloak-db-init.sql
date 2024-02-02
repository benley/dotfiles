set @passwd = load_file('/var/lib/mysql/.keycloak_db_passwd.txt');

create user if not exists 'keycloak'@'%' identified by @passwd;
create user if not exists 'keycloak'@'localhost' identified by @passwd;

create database if not exists keycloak
  character set utf8
  collate utf8_unicode_ci;

grant all privileges on keycloak.* to 'keycloak'@'%';

flush privileges;

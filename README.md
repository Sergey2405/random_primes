# random_primes
=====

An OTP application

Build
-----
```
    $ rebar3 compile
```
Run
-------------
```
    $ rebar3 shell
```
Description
-------------

The application consists of three the following parts:

The first part random_primes_gen is optional. It generates random integer numbers in range 2..N, See the option descrtion below.
The Numbers are generated with even distribution. See the option descrition below.
Generation rate is set as an option. See descrition below.
After a number is generated, it is enqueued or stored to a List in Redis DB.
Key of the list is set in the config also, see desscrption

The second part random_primes_filter is optional, starts and creates prime list numbers in the same range 2..N.
Then it starts fetchhing the previously stored numbers in the the Redis list.
It filters them according to the generated prime list.
All fetched prime numbers are stored to a Set in Dedis DB.
Key of the Set is set in the config also, see desscrption.

The third part is obligatory. It starts erlang Redis with corresponding parameters:
- IP of Redis host;
- port;
- database;

DB password is not enabled yet. Thereby, Redis server should be run on a remote/local host
without protected mode as follows:
```
redis-server --protected-mode no
```

All of these parameters are set in the config.

Generally speaking, the application can start generator of random values or filter for them separately or both
on differnt host in local network.
It is set in the config, see the description below.

Parameters
-------------

random_primes parameters - general parameters for the application.

prime_range - integer, 1000000 by default.
Defines range of generated value for random_primes_gen and random_primes_filter

rate_per_second - integer, 3000 by default - number of randomly generated values in a second.

generator - boolean, false by default. Defines application behavior - whether generator is enabled.

filter - boolean, false by default. Defines application behavior - whether filter is enabled.


eredis parameters - parameters to connect to Redis

host = Host IP, "127.0.0.1" by default.

port = port number, 6379 by default.

database - Redis DB, 0 by default.

number_list_key required parameter. Redis List key where generated numbers stored/enqueued
and then are fetched for checking if are prime.

prime_set_key - required parameter - Redis Set key where all checked prime number are stored.

 Testing
-------------

Unit tests

Checks how prime list is generated and couple of two random values -
composite and prime ones.
```
random_primes_filter:test().
```

Ckecks whether all the stored primary numbers are really primary.
```
checks_primes_in_db
```

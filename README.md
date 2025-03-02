# BarrelDB 

Simple persistent key-value store based on the [Raik's bitcask paper](https://riak.com/assets/bitcask-intro.pdf),
heavily inspired by [avinassh's py-caskdb](https://github.com/avinassh/py-caskdb).

## Query Language

BarrelDB (hopefully will) support a very minimal query language.

+ `OPEN folder_name` - Open database.
+ `CREATE folder_name` - Create and open database.
+ `PUT "key" "value"` - Put key, value pair in open database.
+ `GET "key"` - Get the value associated with key in open database.
+ `REMOVE "key"` - Remove key from open database.
+ `LIST` - List keys in open database 

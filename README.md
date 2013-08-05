hscp
====

A utility for assisting in the use of scp to move files.

How to use hscp.
================

Configuring
-----------

In the template, there is:
- User:           -- The username you would use for scp.
- Password:       -- The password for scp. (not functioning)
- Host:           -- The host for scp.
- Directory:      -- The directory to watch.
- CloneDirectory: -- The directory to clone to (must exist).
- PollInterval    -- The polling delay in microseconds.

Everything below this line is ignored (leave "." and ".."). 
Currently you need to have an ssh key and the password field does not work. 
A .config is included as an example configuration.


Starting
--------

Launch it with
 
    $ hscp path_to_config

The config must be setup properly for this to work, or it will error on runtime.
Currently you must stop polling with ^c, but the terminal will print the scp out
and other useful information.


TODO
----

- Make innitial push respect the ignored regex list
- Possibly add printing options with a -v or --verbose flag
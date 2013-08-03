hscp
====

A utility for assisting in the use of scp to move files.

How to use hscp.
================

Configuring
-----------

In the template, there is:
- User:           -- The username you would use for scp.
- Password:       -- The password for scp.
- Host:           -- The host for scp.
- Directory:      -- The directory to watch (must end in '/').
- CloneDirectory: -- The directory to clone to (must end in '/' and exist).
- PollInterval    -- The polling delay in microseconds.

Everything below this line is ignored (leave "." and ".."). 
Currently you need to have an ssh key and the password field does not work. 
.config is included as an example configuration.


Starting
--------

Launch it with
 
    $ hscp path_to_config

Currently you must stop polling with ^c, but the terminal will print the scp out
and other useful information. Currently, it does not push the source directory at
startup, so you need to edit all the files in the directory for them to get 
pushed.


TODO
----

- Get directories working properly
- Add regex ignores
- Fix emacs file buffers causing crashes (probably with regex ignores)
- Possibly add printing options with a -v or --verbose flag
hscp
====

A utility for assisting in the use of scp to move files.

How to use hscp.
================

Configuring
-----------

In the template, there is:
- User:<User_Name> -- The username you would use for scp.
- Password:<Password> -- The password for scp.
- Host:<Host_Name> -- The host for scp.
- Directory:<Directory> -- The directory to watch (must end in '/').
- CloneDirectory:<Clone_Dir> -- The directory to clone to (must end in '/').
- PollInterval:<Poll_Int> -- The polling delay in microseconds.

Everything below this line is ignored (leave "." and "..").
Currently you need to have an ssh key and the password field does not work.
.config is included as an example configuration.


Starting
--------

Launch it with $ hscp <path_to_config>
Currently you must stop polling with ^c, but the terminal will print the scp out
and other useful information. Currently, it does not push the source directory at
startup, so you need to edit all the files in the directory for them to get 
pushed.
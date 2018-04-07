# CH/OTP Test Task

A solution to the test task specified at [http://f.nn.lv/od/5c/8y/CH_OTP_Test_Task(1).pdf](http://f.nn.lv/od/5c/8y/CH_OTP_Test_Task(1).pdf).

## Build

After cloning the repository, go to its root and run:

```
stack build
```

## Run

### using a script

Run 

```
./run.sh
```

to start five messaging ("agent") nodes and the master node on localhost, with pre-set parameters. Use this file to configure hostnames and ports.

### manually

First, run a few messaging nodes:

```
stack exec chotp-exe -- agent --host 127.0.0.1 --port 10001
stack exec chotp-exe -- agent --host 127.0.0.1 --port 10002
stack exec chotp-exe -- agent --host 127.0.0.1 --port 10003
stack exec chotp-exe -- agent --host 127.0.0.1 --port 10004
stack exec chotp-exe -- agent --host 127.0.0.1 --port 10005
```

Then, to kick off the actual calculation, run the master node:

```
stack exec chotp-exe -- master --host 127.0.0.1 --port 10006 --send-for 3 --wait-for 3 --with-seed 0
```

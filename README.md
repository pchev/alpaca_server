# Alpaca Server

ASCOM Alpaca server, Free Pascal / Lazarus package 

This package is the base code to implement an Alpaca driver using Free Pascal compiler.

## Use

If Synapse package is not already installed in Lazarus, install first the package laz_synapse.lpk from the synapse directory. 

Compile the package file alpacaserver.lpk

In the driver project options, add a requirement to package alpacaserver

The files in the example directory show a dummy driver implementation that can be use as a starting point.

The package also include a serial communication unit cu_serial.pas that allow to communicate with a telescope 
using serial or tcp/ip protocol. You are free to use it or not.

## Compilation

The code can be compiled with FreePascal/Lazarus https://www.lazarus-ide.org/ 

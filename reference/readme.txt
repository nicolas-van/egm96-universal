Data Description for 15 minute worldwide binary geoid height file:
______________________________________________________________________

---- FILE: WW15MGH.DAC

The total size of the file is 2,076,480 bytes. This file was created
using an INTEGER*2 data type format and is an unformatted direct access
file. The data on the file is arranged in records from north to south.
There are 721 records on the file starting with record 1 at 90 N. The
last record on the file is at latitude 90 S. For each record, there
are 1,440 15 arc-minute geoid heights arranged by longitude from west to
east starting at the Prime Meridian (0 E) and ending 15 arc-minutes west
of the Prime Meridian (359.75 E). On file, the geoid heights are in units
of centimeters. While retrieving the Integer*2 values on file, divide by
100 and this will produce a geoid height in meters.


---- SOFTWARE:

Program: intptdac.f

This program, written in FORTRAN 77 will perform bilinear or spline
interpolation of geoid heights in meters from a binary direct access
file (WW15MGH.DAC) and a file of input locations (INPUT.DAT). The 
input file, in ASCII, must have location as two entries per line, with
latitude followed by longitude in decimal degees. The longitude must
be entered in a 0 to 360 degree format. The progam is set up as a 
spline interpolation (IWINDO=4, NBDR=2*IWINDO). It will work within 
latitudes from 89.7 to -89.7. If you want interpolations closer to
the pole, perform bilinear interpolation (IWINDO=0, NBDR=4) 
after editing and recompiling the program. The output file created
will be named OUTINTPT.DAT. It will have three entries per line. The
latitude and longitude will be followed by the geoid height to the 
nearest 1/100th of a meter.

Program executable: intptdac.exe

A DOS generated executable is provided that will work on a windows  
personal computer. Simply load file INPUT.DAT with lat/lon locations 
and have it in the same directory as the executable and binary 
direct access file WW15MGH.DAC. Then type intptdac.exe at the prompt
and press the enter button. If the user wants to create the executable 
on a PC from the software, in the open statement, use convert='big_endian'
to allow reading of the binary file.

--- POC:

Questions concerning the binary direct acess file and interpolation
software may be directed to:


Geospatial Sciences Division
NGA/PRGB
3838 Vogel Rd
Arnold MO,  63010

e-mail: wgs84@nga.mil
office phone: (314)263-4422

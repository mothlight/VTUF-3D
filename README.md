# README #

This README would normally document whatever steps are necessary to get your application up and running.

### What is this repository for? ###

* Quick summary
This is the repository for the VTUF-3D urban micro-climate model. It was created as part of the PhD thesis for Kerry Nice. https://www.researchgate.net/publication/307522405_Improved_micro-climate_modelling_assessment_of_the_influence_of_water_sensitive_urban_design_on_human_thermal_comfort
====


* Version
* 1.0

### How do I get set up? ###

* Summary of set up
Has been tested on Ubuntu using 
gfortran                4:4.8.2-1ubuntu6 amd64            GNU Fortran 95 compiler

Clone repository
The code has some circular depenencies. Run 'make' three times to create the executable.
$ cd /home/thud/projects/VTUF-3D/repository/VTUF-3D/
$ make
$ make
$ make

Contact Kerry Nice for set up and configuration of the model.

Example run is contained in PrestonBase8.zip. Unzip to a directory, modify some files, and run:

$ mkdir /tmp/Preston
$ cd /tmp/Preston/
$ unzip /tmp/PrestonBase8.zip 
$ cd PrestonBase8/
$ cp /home/thud/projects/VTUF-3D/repository/VTUF-3D/TUF3Dradiation .  
$ rm confile.dat
$ cp 1/1/confile.dat .
$ ./TUF3Dradiation

### Contribution guidelines ###

* Writing tests
* Code review
* Other guidelines

### Who do I talk to? ###

* Repo owner or admin
Kerry Nice
* Other community or team contact

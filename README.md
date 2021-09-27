# PMP-Template

## Setup 

- Install [Java JDK 8.0](https://www.oracle.com/java/technologies/javase/javase-jdk8-downloads.html)
- Install [SBT](https://www.scala-sbt.org/download.html) 
- Open a command line in the folder and run the command: `sbt`. This can take a long time, run "exit" to close sbt.
- `sbt "runMain Test"`

[Follow the quick start](https://www.cra.com/sites/default/files/pdf/Figaro_Quick_Start_Guide.pdf)


## How to run Figaro programs
To run Figaro programs that you create:

1. Open a command line prompt
2. Navigate to your local FigaroWork directory

### Example test run
![Example test run](./test_run.png)

### Example run from package
In order to run programs saved in subfolders you have to define the package name at the top of the folder: `package <name_of_package>`
To run programs in packages run: `runMain <name_of_package>.<name_of_class>`

For example: `runMain Lab5.Ex1`

![Example run from package](./example_package_run.png)

Note: don't forget the quotes around the runMain command!


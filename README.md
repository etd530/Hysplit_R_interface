# HYSPLIT R Interface for large-scale trajectory computing

#### Table of contents
[Installation](#installation)  
[How to run the program](#running)

This code contains an R program that calls [HYSPLIT](https://www.arl.noaa.gov/hysplit/) trajectory model to obtain wind trajectories, allowing to automate the 
calculation of large numbers of trajectories, and produces different types of plots summarizing the results of the different runs.

<a name="installation"/>

## Installation
Although the program itself does not require installation *per se*, you will need to have a local installation of [HYSPLIT](https://www.ready.noaa.gov/HYSPLIT.php) 
in order to use it. In addition, there are a number of R depenencies that will be required and, to ensure compatibility it is recommended to use R 4.1.2.

Therefore, it is recommended to first **install R 4.1.2 and HYSPLIT**. Then, **clone this repository** using `git` and open the R Project. The R `renv` package will
automatically install itself. After it has finished, all other dependencies can be installed by **running `renv::restore()` in the R console**. Lastly, you will need 
to open the code and change the first variable (line 728) so that it species the path to your local installation of HYSPLIT.

<a name="running"/>

## How to run the program
The main arguments required to run a set of trajectories are `--from`, which specifies the starting date(s); `--to`, which specifies the ending date(s) and `--by_hour`, 
`--by_day`, `--by_month` and `--by_year`, which specifies the intervals of time separating the first and last date(s) of the set. The latter four arguments take integers, 
e.g. `--by_hour 1` indicates to run one trajectory per hour between the starting and ending date(s) and times(s). `--from` and `--to` take dates and hours in the format 
`YYYY-MM-DD-hh-mm`, i.e. the year, month, day, hour and minute (in 24-hour format). In addition, the argument `--duration` indicates how long each trajectory calculation 
should last, and whether it should go forwards or backwards in time, e.g. `--duartion -24` indicates that each trajectory is computed up to 24 hours before its starting time.

A series of examples are provided below. Example 1 goes over the different types of output obtained form the program while the others focus on different combinations of days, 
months, etc. that can be done.

### Example 1: Running a sequence of trajectories separated by a single unit of time
One possible use of this program is to run a set of trajectory calculations from one point in time to another, separated regularly by a single "unit of time". For example, 
one may wish to run trajectories at October 22nd 2013 at 06:00h, October 22nd 2013 at 07:00h, etc., until October 25th 2013 at 06:00h, 24 hours backwards, starting at 
altitudes of 500, 1000 and 2000m AGL from coordinates (5.745974, -53.934047):

```
./Hysplit_wind_analysis_dev.R --from 2013-10-22-06-00 --to 2013-10-25-06-00 --lat 5.745974 --lon -53.934047 --altitude 500,1000,2000 --duration -24 --byhour 1
```

First of all, the program will download any necessary meteorological files from the NOAA's servers. For large runs, this can take some time, so please be patient. 
Luckily once they are downloaded, running any trajectories involving these files will be much quicker (unless you delete them, which is not recommended).

Afterwards the program will compute all trajectories and then produce the first plot, which are rasterized trajectory plots containing the trajectory density at each raster
cell. These are provided for each starting height separately:

<img src="https://user-images.githubusercontent.com/85890746/187038873-953ff429-9e78-42f5-825e-c6d54cabcd7a.png" width="250">
<img src="https://user-images.githubusercontent.com/85890746/187039171-40acd992-a090-4a3c-a330-0f959a3404c7.png" width="250">
<img src="https://user-images.githubusercontent.com/85890746/187039272-35ac67a0-77c7-4176-936b-491a2b73b5e1.png" width="250">

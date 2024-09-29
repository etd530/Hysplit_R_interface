# R interface to HYSPLIT for scalable trajectory computing

#### Table of contents
[Installation](#installation)  
[How to run the program](#running)  
[Troubleshooting](#troubleshooting)  
[Citation](#citation)  

This code contains an R program that calls [HYSPLIT](https://www.arl.noaa.gov/hysplit/) trajectory model to obtain wind trajectories, allowing to automate the 
calculation of large numbers of trajectories, and produces different types of plots summarizing the results of the different runs.

<a name="installation"/>

## Installation
Although the program itself does not require installation *per se*, you will need to have a local installation of HYSPLIT (available [here](https://www.ready.noaa.gov/HYSPLIT.php)) 
in order to use it. In addition, there are a number of R depenencies that will be required and, to ensure compatibility **it is recommended to use R 4.1.2**.

Therefore, it is recommended to first **install R 4.1.2 and HYSPLIT**. We recommend [following these instructions](https://docs.posit.co/resources/install-r/) if you need to have multiple R versions in a single machine. Then, **clone this repository** using `git` and open the R Project. The R `renv` package will automatically install itself. After it has finished, all other dependencies can be installed by **running `renv::restore()` in the R console**. You also need to open the R script and change the following:
- Change the shebang (the first line in the script) so that it points to you local installation of R 4.1.2. If you followed the recommended instructions [here]((https://docs.posit.co/resources/install-r/)), that should be `/opt/R/4.1.2/bin/Rscript`.
- Change the first variable (`hy_path`) so that it specifies the path to your local installation of HYSPLIT.
- Change the paths in `.libPaths()` and `Sys.setenv()` to point to the R library created by `renv`. This will be within the `renv/library/` folder located inside this repository.

<a name="running"/>

## How to run the program
The main arguments required to run a set of trajectories are `--from`, which specifies the starting date(s); `--to`, which specifies the ending date(s) and `--byhour`, 
`--byday`, `--bymonth` and `--byyear`, which specifies the intervals of time separating the first and last date(s) of the set. The latter four arguments take integers, 
e.g. `--by_hour 1` indicates to run one trajectory per hour between the starting and ending date(s) and times(s). `--from` and `--to` take dates and hours in the format 
`YYYY-MM-DD-hh-mm`, i.e. the year, month, day, hour and minute (in 24-hour format). In addition, the argument `--duration` indicates how long each trajectory calculation 
should last, and whether it should go forwards or backwards in time, e.g. `--duartion -24` indicates that each trajectory is computed up to 24 hours before its starting time.

A series of examples are provided below. Example 1 goes over the different types of output obtained form the program while the others focus on different combinations of days, months, etc. that can be done.

*Note: it is also possible to run a single trajectory by simply specifying the same dates and times in --from and --to.*    


### Example 1: Running a sequence of trajectories separated by a single unit of time
One possible use of this program is to run a set of trajectory calculations from one point in time to another, separated regularly by a single "unit of time". For example, 
one may wish to run trajectories at October 22nd 2013 at 06:00h, October 22nd 2013 at 07:00h, etc., until October 25th 2013 at 06:00h, 24 hours backwards, starting at 
altitudes of 500, 1000 and 2000m AGL from coordinates (5.745974, -53.934047). In such case, the arguments `--from` and `--to` simply indicate the starting and ending times, and `--byhour 1` indicates that we want one trajectory per hour between those two time points:

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


Secondly, a map with all trajectories plotted is also provided:

<img src="https://user-images.githubusercontent.com/85890746/187039560-e0bc7bcc-7aec-413a-be70-3310803f81e3.png" width="400">  


The next output are circular histogram, indicating the proportion of trajectories going in each direction:

<img src="https://user-images.githubusercontent.com/85890746/187039811-fa26baaf-0122-4e5e-8663-427afe428146.png" width="400">  

Lastly, an "altitudinal profile" plot is provided, which indicates the mean height (± standard error) of each group of trajectories at the different hours of the duration
of the run:

<img src="https://user-images.githubusercontent.com/85890746/187040016-9964941a-4680-4bb7-aa86-b4655459deb8.png" width="400">  

### Example 2: Running trajectories across multiple units of time
Instead of being interested in a single, "continuous" set of trajectories, one may be interested in a set of trajectories repeated across a larger interval. For example,
one may want to compute trajectories for all hours between 10:00 and 18:00 from days 1st to 10th of April, May and June, for years 2000 to 2022. In this case, we have a smaller sequence (from 10:00 to 18:00 by one hour) that then repeats in a larger one (from 1st to 10th day of the month, by one day), then another one (from April to June, month by month) and finally another one (from 2000 to 2002, year by year). To do this, the arguments `--from` and `--to` need to indicate, respectively, the smaller and larger element of each of the sequences, and then we indicate the step size with the arguments `byhour`, `byday`, `bymonth` and `byyear`:

```
./Hysplit_wind_analysis_dev.R --from 2000-04-01-10-00 --to 2002-06-10-18-00 --lat 5.745974 --lon -53.934047 --altitude 500,1000,2000 --duration -24 --byyear 1 --bymonth 1 --byday 1 --byhour 1
```

<img src="https://user-images.githubusercontent.com/85890746/187041780-a2c2ca5c-2340-4ae5-bb93-45c993bdb026.png" width="400">  

<a name="troubleshooting"/>

## Troubleshooting  
### Meteorological files not downloading properly  
When a run will require meteorological files that need to be downloaded, there is a chance that if the Internet connection is not working properly the file download will halt, and the program will exit with an error.  

If this happens, one needs to go to the HYSPLIT installation folder, enter the `working` folder and remove the incomplete meterological file. These files are identified by the `.gbl` extension and are named starting with RP and then indicating the year and the month. If the file is not removed, HYSPLIT will think it already has it and try to compute the trajectories, but then it will give an error, as some data will be missing.

If downloading the files is giving problems, you may also try to download them (in Linux) by running the following **from the `working/` directory of your hysplit installation**:

```
for ((year=1948; year<2023; year++)); do for month in 01 02 03 04 05 06 07 08 09 10 11 12; do wget ftp://ftp.arl.noaa.gov/pub/archives/reanalysis/RP${year}${month}.gbl; done; done
```
This will download the entire Reanalysis database from 1948 to 2022, but you may adjust this to download only the files you need. Note that **the full database until 2022 takes about 102GB of disk space**.

<a name="citation"/>

## Citation  
If you use this program please cite at least one of these articles:  
- Suchan, T., Bataille, C.P., Reich, M.S. et al. A trans-oceanic flight of over 4,200 km by painted lady butterflies. _Nature Communications_ 15: 5205 (2024). https://doi.org/10.1038/s41467-024-49079-2  
- Menchetti, M., Schifani, E., Alicata, A., Cardador, L., Sbrega, E., Toro-Delgado, E., Vila, R. The invaisve ant _Solenopsis invicta_ is established in Europe. _Current Biology_ 33(17): PR896-R897 (2023). https://doi.org/10.1016/j.cub.2023.07.036  
- Jamonneau, T., Toro-Delgado, E., Vila, R. Primer registre de _Vanessa virginiensis_ (Drury, 1773) a Catalunya. _Butlletí de la Societat Catalana de Lepidopterologia_ 14: 21-25.  

# TX_TradeStrAdj.tiga

Source code and obtained results for “TX trading strategies adjusted by using technical indicators and genetic algorithms”.


## File introduction

There are three files: data, Experiment, lib in the TX_TradeStrAdj.tiga file:

1. data: there are data(not real data) for testing experiment.

2. lib: there are some functions which we programmed, you can use following command to include those function into your program.
> source(“lib/\_program.R\_“) 

3. Experiment: there are code which we do experiment.

## Environment setting

1.	All experiments were done in R language, so first you should download and install R language on your computer from http://cran.csie.ntu.edu.tw/.  

2.	Download and install RStudio IDE (recommend) from https://www.rstudio.com/.  

3.	Can start doing experiments.  

4.	When you run program, there are some additional R packages(ex: chron, MTS, knitr, GA, genalg, .. etc) you need to install. Use following command to install package:
> install.packages(“ \_packageYouNeed\_ ”) 

## Experiments

Before starting the simulated trading experiments, you need to do data preprocessing as following steps: 
</br>

**Step1.** Run getEveryDayInfo.R (in Experiment/data_preprocess_1day file/).

**Step2.** Run follow programs :  

                 getEvery1MinInfo_2012.R (in Experiment/data_preprocess_1min/).  
                 
                 getEvery1MinInfo_2013.R (in Experiment/data_preprocess_1min/).  
                 
                 getEvery1MinInfo_2014.R (in Experiment/data_preprocess_1min/).  
                 
                 getEvery1MinInfo_2015.R (in Experiment/data_preprocess_1min/).  
                 
                 getEvery1MinInfo_2016.R (in Experiment/data_preprocess_1min/).  
  
               
**Step3.** Run getEveryDayInfo.R and workPreprocess.R (in Experiment/data_preprocess_15min/).

**Step4.** Run finalPreprocessing.R (in Experiment/strategyTrading/).  
  
</br>After you do above steps, all data preprocessing works are done. Then you can do three simulated trading experiments. </br> 
  
**Run simulated trading by Strategy 1 :** using 4EMA and KD technical indicators to construct day trading strategies.
> Run strategyTrading_4EMA_KD.R

**Run simulated trading by Strategy 2 :** using 4EMA , 4MV and KD technical indicators to construct day trading strategies.
> Run strategyTrading_4EMA_4MV_KD.R

**Run simulated trading by Strategy 3 :** using GA for choosing different MV and 4EMA and KD to construct day trading strategies.
> Run strategyTrading_GAforMV_4EMAKD.R

## Raw result

In the Raw_result file, there are six information (buyDatetime, sellDatetime, predictUpDown, buyPrice, sellPrice, profit) of every futures transaction from year 2013 to 2016.  

1. buyDatetime: buy futures datetime.  

2. sellDatetime: sell futures datetime.  

3. predictUpDown: the futures index will rise or fall, 1 -> rise and -1 -> fall.  

4. buyPrice: the buying datetime futures index.  

5. sellPrice: the selling datetime futures index.  

6. profit: the sellPrice minus buyPrice.  

**[Note]**

After you run simulated trading experiments, your R global environment have two important results: **testing_resultDetail** and **testing_resultSummary**. 

**testing_resultDetail** store above six information pf every futures transaction from year 2013 to 2016.

 **testing_resultSummary** store each day statistical results from year 2013 to 2016.




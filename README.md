# Long-Short
Cointegration analysis and exploitation for long-short strategy

# Summary
<BR>
<em> DataExplorer.R </em>

This code will read tickers from Instruments.csv file and download price data between April 2019 and March 2021 from Yahoo Finance API. From the final database of prices, it will check for cointegration (Johansen Test) for all possible pairs of assets.

The output is a table with all cointegrated pairs found.

<BR>
<em> CointegrationExplorer.R </em>

This file is used to analyze cointegration and backtest a statistical arbitrage strategy. As example, I have used the pair CGRA3-AGRO3.

Firstly, the time series is partitioned into train (to check for cointegration) and test (for backtesting). Testing period is 1 year.

Best lag order is selected and Johansen Test is performed to confirm cointegration.

<span style="text-decoration: underline;"> Backtesting and strategy summary: </span>

Strategy to be tested is based on calculated zScore of the relative price (asset A divided by Asset B). Entry will happen at extreme values of Z (negative = long, positive = short). A stop-loss is placed above/below entry based on a value of sigma. 

Note: please check code for values given to the parameters above.

<BR>
<em> Final Analysis </em>

The cointegration code outputs charts, risk metrics and 2 Excel files for validation. 
In test data maximum volume reached $30,393.08, implying on a cost of $303.93 (2% of short volume). Total net PL is $4,834.95.

<BR>
Make sure to install all packages before!


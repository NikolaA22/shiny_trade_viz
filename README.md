# shiny_trade_viz
Repository for a small shiny app that can visualize backtesting data

A work in progress with requrements changing as time goes. Goal is to have a shiny app which when fed a 
backtest result containing OHLC data together with decision column and amount bought of a stock (cryptocurrency in this case)
will produce interactive visualizations. 


Requirements:

Result .csv file with the following columns:

* time_period_start (datetime)
* price_open (numeric)
* price_close (numeric)
* buy_decision (boolean)
* sell_decision (boolean)

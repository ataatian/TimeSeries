 ///////////////////////////////////////////
 //////////////////// Problem 1,2
  clear
  set obs 1000
  gen r=.
  gen a= rnormal()
  replace r= 0.3 + 0.2*r[_n-1]-0.4*r[_n-2]+0.3*r[_n-3]+a-0.5*a[_n-1] if _n>3
  gen t=_n
  tsset t
  twoway tsline r
//  save synth_data.dta, replace
  arima r , ar(1/3) ma(1) level(99)
 eststo ARMA: arima r , ar(1/3) ma(1)
 esttab using /Users/administrator/Desktop/results/2.tex,   se(3) ar2 label compress title(Regression table\label{tab1}) mtitles star(* 0.10 ** 0.05 *** 0.01) replace //addnotes(this is great)
eststo clear

/////////////////////////////////////////////
//////////////// Problems 3,4,5,6


cd /Users/administrator/Desktop/myQualSpring2018/TimeSeries
import delimited using AAPL08.04.21-25.csv,  clear
ren v1 Ticker
ren v2 Time
ren v3 P1
ren v4 P2
ren v5 P3
ren v6 P4
ren v7 volume
ren v8 num_Trades
ren v9 Price_ave
drop v10 v11 v12 v13
//save mydata.dta, replace
gen day=.
replace day=5 if _n>=1559 & _n<=1945
keep if day==5
use mydata.dta, clear
ren P2 Price_high
ren P3 Price_low
ren P4 Price_close
ren P1 Price_open
//save mydata2.dta, replace
//1,945 obs
sort day Time
by day: gen er= Price_close-Price_open[_n+1]
by day: gen R=log(Price_close/Price_close[_n-1])
drop er
drop if R==.
sum R, detail
//save mydata3.dta
//1,940 obs
//////////////////////
import delimited using AAPL.test.csv,  clear
gen er= Price_close-Price_open[_n+1]
gen R=log(Price_close/Price_close[_n-1])
//save myTest.dta, replace
use myTest.dta, clear
gen day=6
/////////////////////
use mydata3.dta, clear
sort day Time
gen t=_n
tsset t
//save mydata3.dta, replace
/////// unit root test
dfuller R, lags(3) trend regress
dfuller R

/////// ARIMA fit
  ac R, ylabels(-.4(.2).6)
  pac R, ylabels(-.4(.2).6)
  arima R, arima(5,0,19) noconstant condition
  arima R, arima(5,0,19) condition

  arima R, ar(2,4,5) ma(2,4,5,6,12,15,19) noconstant condition
  
  arima R, ar(1/5) ma(1/5) noconstant condition

 estat ic
 estat summarize
 /////// Model adequacy
 drop resid
 predict resid, residuals
 
  wntestb resid, table
  wntestb resid
  wntestq resid
///////////////////////
 
  generate x1 =  rnormal()

  generate x2 = rnormal() + cos(2*_pi*(_n-1)/10)
  wntestb x1, table
  wntestb x2
  wntestq x1
  
  arima R, ar(1) ma(1)
  
  pac R

  arima R, ar(5) ma(5)
  estat ic
  ////////////// ARCH effect
  estat archlm,  lags(1 2 3)
  
  drop residSQ
  gen residSQ= resid*resid

  wntestb residSQ, table
  wntestb residSQ
  wntestq residSQ
  pac residSQ, ylabels(-.4(.2).6) 
   arch resid, arch(1)
   estat ic
 //  arch R, arch(1/5)
///////// All-together estimation
  arch R, ar(2,4,5) ma(2,4,5,6,12,15,19) arch(1/5)
  arch R, ar(1/5) ma(1/19) arch(1/5) noconstant condition
  
  //// Final ARCH ARIMA model:
  //// for simpler modeling and due to the significance of the coefs:
  eststo ARMA505_ARCH5: arch R, ar(1/5) ma(1/5) arch(1/5) noconstant condition
  eststo ARMA505_GARCH11: arch R, ar(1/5) ma(1/5) arch(1) garch(1)
  eststo ARMA505_EGARCH11: arch R, ar(1/5) ma(1/5) arch(1) egarch(1)
  
  use all_Samples.dta, clear
  arch R if day!=6, ar(1/5) ma(1/5) arch(1/5) noconstant condition
  arch R if day!=6, ar(1/5) ma(1/5) arch(1) garch(1)
  arch R if day!=6, ar(1/5) ma(1/5) arch(1) egarch(1)
  
  predict R_pred, y 
  gen er= R-R_pred
  gen er_sqr= er*er 
  
  predict R_pred_dy, y dynamic(1941) 
  gen er2= R-R_pred_dy
  gen er_sqr2= er2*er2 
  
  mean(er_sqr) if day==6
  mean(er_sqr2) if day==6
  //////////////////////
   predict xb, xb
   predict y, y

   predict xbs, xb structural
  
  ////////////////////////
  append using mydata3.dta
  sort day Time
  drop in 1941
//  save all_Samples.dta, replace
  drop t
  gen t=_n
  tsset t
  
  
  
  use all_Samples.dta, clear
  arima R if day!=6, ar(1/5) ma(1/5) noconstant condition
 // arima R , ar(1/5) ma(1/5) noconstant condition
 //////////////////// Prediction
  predict R_pred, y
  predict R_pred if day!=6, y
  drop R_pred
  twoway tsline R_pred R_pred_dy
  /// Dynamic prediction
  predict R_pred_dy, y dynamic(19) 
  //predict R_pred_dy, mse dynamic(1941) 
  predict resid , residuals
  drop resid
  drop R_pred_dy

  gen er= R-R_pred_dy
  gen er_sqr= er*er
  predict W, mse
  predict W if day!=6, mse
  drop er
  drop er_sqr
  drop W
    predict Q, stdp

  mean(er_sqr)
//////
  predict R_pred, y 
  gen er= R-R_pred
  gen er_sqr= er*er 
  mean(er_sqr) if day==6
  
  predict R_pred_dy, y dynamic(1941) 
  gen er2= R-R_pred_dy
  gen er_sqr2= er2*er2  
  mean(er_sqr2) if day==6
  twoway tsline R_pred R_pred_dy R if day==6
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


  
  
  

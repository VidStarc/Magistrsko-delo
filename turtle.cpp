#include <Rcpp.h>
using namespace Rcpp ;

// int fun1( int a1){
//   int b1 = a1;
//   b1 = b1*b1;
//   return(b1);
// }
// 
// // [[Rcpp::export]]
// IntegerVector fun(IntegerVector fun_data){ 
//   IntegerVector out = sapply( fun_data, fun1 ) ;
//   return(out);
// }

// [[Rcpp::export]]
double cmax(double x, double y, double z) {
  if(x >= y) {
    if(z >= x) return z;
    return x;
  } else {
    if(z >= y) return z;
    return y;
  }
}

// [[Rcpp::export]]
NumericMatrix run_analysis(
                     LogicalVector long_entry, LogicalVector long_exit,   // long entry and exit boolean vectors
                     LogicalVector short_entry, LogicalVector short_exit,  // short entry and exit boolean vectors
                     NumericVector price, 
                     NumericVector N,
                     double account_size,      // nominal account size
                     double risk_percent,      // usually 0.01 of account size
                     double stoploss_factor,   // usually 2, meaning stop loss at 2*N
                     double addition_factor,    // usually 0.5, meaning new unit added to position and 0.5*N change
                     int longs_and_shorts,      // 0 - both, -1 - shorts only, 1 - longs only.
                     int max_units,             // usually 4
                     int steps,                 // trading steps (e.g. 360 ticks)
                     int allow_over_limit,      // 1 - can go over account size limit, 0 - can't go
                     int sync_stoploss,         // 1 - use the same stoploss for all units, 0 - lift stoplosses by addition_factor separeately
                     int skip,                   // number of entries to skip calculation for at the beginning
                     int start                  // if < 0 then moving trading window analysis is done, otherwise just one simulation is carried out from start with steps.
  ) {
  int n = long_entry.size();
  NumericVector stoploss(max_units);
  NumericVector investment_quote(max_units);
  NumericVector unit_price(max_units);
  NumericVector profit(max_units);
  int result_columns = start < 0 ? 5 : (1 + 1 + 2*3 + 2*max_units);
  // start < 0: columns: long_profit, short_profit, long_position_count, short_position_count, avg_capital_utilization
  // else     : mark, capital_utilization, long_position_cnt, long_units, long_profit, long_stoploss_1, ..., stoploss_max, short_position_cnt, short_units, short_profit, short_stoploss_1, ..., short_stoploss_max 
  NumericMatrix result(n, result_columns); 
  int current_unit = -1;
  int is_open = 0;  // 1 - open, 0 - not open
  int position_type = 0; // 1 - long, 2 - short
  int capital_utilization = 0;
  int pos = 0; // current position
  int position_N = 0;   //N used for current position
  double unit_size_quote = 0; // unit size in quote currency (USD in BTCUSD, BTC is called 'base')
  double long_profit = 0;
  double short_profit = 0;
  double running_capital_utilization = 0;
  int long_position_count = 0;
  int short_position_count = 0;
  int is_end = 0;

  int start_index = start < 0 ? skip : start;
  int end_index = start < 0 ? n - 1 : start; 
  
  // fill result with zeros
  int fullsize = result.nrow() * result.ncol();
  for (int i = 0; i < fullsize; i++) {
    result[i] = 0;
  }  
  for(int period = start_index; period <= end_index; period++) {
    if(period + steps >= n) break;
    capital_utilization = 0;
    running_capital_utilization = 0;
    long_profit = 0;
    short_profit = 0;
    long_position_count = 0;
    short_position_count = 0;
    position_N = 0;
    current_unit = -1;  // counts index of current unit in position. First unit has index 0
    is_open = 0;
    is_end = 0;
    for(int k = 0; k < steps; k++) {
      pos = period + k;
      if(k == steps - 1) {
        is_end = 1;
      }
      // opening a position
      if(start >= 0) {
        result(pos, 0) = 1; // active
      }
      if(!is_open && !is_end && ((long_entry[pos] && longs_and_shorts >= 0) || (short_entry[pos] && longs_and_shorts <= 0))) {
        if(start >= 0) {
          result(pos, 0) = 2; // opening
        }
        current_unit = 0;   // first unit
        position_N = N[pos];
        unit_size_quote = account_size*risk_percent/position_N*price[pos];
        unit_price[current_unit] = price[pos];
        if(unit_size_quote > account_size && !allow_over_limit) {
            unit_size_quote = account_size; 
        }
        if(long_entry[pos]) {
            position_type = 1;
            long_position_count++;
        } else { // it must be short entry
            position_type = -1;
            short_position_count++;
        }
        investment_quote[current_unit] = unit_size_quote;
        capital_utilization = unit_size_quote;
        stoploss[current_unit] = price[pos] - position_type*stoploss_factor*position_N;
        profit[current_unit] = 0;
        is_open = 1;
      }
      // adding units
      if(is_open && !is_end && current_unit < max_units - 1 && (allow_over_limit || capital_utilization < account_size)) {
          if(position_type*(price[pos] - unit_price[current_unit]) >= addition_factor*position_N) {
              if(start >= 0) {
                result(pos, 0) = 3; // adding
              }
              current_unit++;
              unit_price[current_unit] = price[pos];
              if(allow_over_limit || capital_utilization + unit_size_quote <= account_size) {
                  capital_utilization += unit_size_quote;
                  investment_quote[current_unit] = unit_size_quote;
              } else {
                  // TODO: when shorting, we can lose more than whole account!!!
                  // Some margin should be introduced, e.g. stoplossfactor*position_N + eps
                  capital_utilization = account_size; 
                  investment_quote[current_unit] = account_size - capital_utilization;
              }
              stoploss[current_unit] = price[pos] - position_type*stoploss_factor*position_N;
              for(int u = 0; u < current_unit; u++) {  // fix stoplosses for earlier units
                if(sync_stoploss) {
                    stoploss[u] = stoploss[current_unit];
                } else {
                    stoploss[u] = stoploss[u] + position_type*addition_factor*position_N;  
                }
              }
          }
      }
      // closing units due to stoploss
      if(is_open && (price[pos] - stoploss[current_unit])*position_type <= 0) {
          int u = 0;
          for(u = current_unit; u >= 0; u--) {
            if((price[pos] - stoploss[current_unit])*position_type <= 0) {
                capital_utilization -= investment_quote[u];
                profit[current_unit] = (price[pos]/unit_price[u] - 1)*investment_quote[u]*position_type;
                if(position_type > 0) {
                  long_profit += profit[current_unit];
                } else {
                  short_profit += profit[current_unit];
                }
                current_unit--;  // close current unit
            }
          }
          if(current_unit < 0) {  // all units closed
            is_open = 0;
            capital_utilization = 0;
            if(start >= 0) {
              result(pos, 0) = 4; // stoploss closing
            }
          }
      }
      // closing position due to exit signal or end
      if(is_open && ((long_exit[pos] && position_type > 0) || (short_exit[pos] && position_type < 0) || is_end)) {
        while(current_unit >= 0) {
            profit[current_unit] = (price[pos]/unit_price[current_unit] - 1)*investment_quote[current_unit]*position_type;
            if(position_type > 0) {
              long_profit += profit[current_unit];
            } else {
              short_profit += profit[current_unit];
            }
            current_unit--;  // close current unit
        }
        capital_utilization = 0;
        is_open = 0;
        if(start >= 0) {
          result(pos, 0) = 5; // exit closing
        }
      }
      running_capital_utilization += capital_utilization;
      // filling result for single run
      if(start >= 0) {
        // result(pos, 0) = 1;
        result(pos, 1) = capital_utilization;
        int offset = 2;
        result(pos, offset + 0) = long_position_count;
        result(pos, offset + 1) = position_type > 0 ? (current_unit + 1) : 0;
        result(pos, offset + 2) = long_profit;
        if(position_type > 0) {
          for(int u = 0; u <= current_unit; u++) {
            result(pos, offset + 3 + u) = stoploss[u];
          }
        }
        offset = (2 + 3 + max_units);
        result(pos, offset + 0) = short_position_count;
        result(pos, offset + 1) = position_type < 0 ? (current_unit + 1) : 0;
        result(pos, offset + 2) = short_profit;
        if(position_type < 0) {
          for(int u = 0; u <= current_unit; u++) {
            result(pos, offset + 3 + u) = stoploss[u];
          }
        }
      }
    } // inner for
    // filling result for multiple run
    if(start < 0) {
      result(period, 0) = long_profit;
      result(period, 1) = short_profit;
      result(period, 2) = long_position_count;
      result(period, 3) = short_position_count;
      result(period, 4) = running_capital_utilization/steps;
      
      // result[n*0 + period] = long_profit;
      // result[n*1 + period] = short_profit;
      // result[n*2 + period] = long_position_count;
      // result[n*3 + period] = short_position_count;
      // result[n*4 + period] = running_capital_utilization/steps;
    } 
  } // outer or
  return result;
}
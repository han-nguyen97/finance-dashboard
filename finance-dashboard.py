# -*- coding: utf-8 -*-
"""
Created on Thu Nov 11 15:17:38 2021

@author: nnguyen5
"""
# -*- coding: utf-8 -*-
###############################################################################
# FINANCIAL DASHBOARD
###############################################################################

#==============================================================================
# Initiating
#==============================================================================

import streamlit as st
import pandas as pd
from datetime import datetime, timedelta
import yahoo_fin.stock_info as si
import numpy as np
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots


#Page layout
## Page expands to fulll width
st.set_page_config(layout = 'wide')
    
#==============================================================================
# Tab 1
#==============================================================================

def tab1():    
    #Add dashboard title and description
    st.title("Financial dashboard")
    st.write("Data source: Yahoo Finance")
    st.header("Tab 1 - Summary")
        
    # Add table to show stock data
    @st.cache
    def GetQuoteTable(ticker):
        return si.get_quote_table(ticker, dict_result = False)
            
    if ticker != '-':
            st.write('Summary of {}'.format(ticker))
            summary = pd.DataFrame(GetQuoteTable(ticker))
            summary = summary.set_index('attribute')
            summary = summary.astype(str)
            st.dataframe(summary, height=1000)
    
    # Get stock data        
    def GetStockData(ticker, start_date, end_date):
            return pd.concat([si.get_data(tick, start_date, end_date) for tick in ticker])
    
    # Add a line plot and a bar plot
    if ticker != '-':
            st.write('Adjusted close price of {}'.format(ticker))
            stock_price = pd.DataFrame(GetStockData([ticker], start_date, end_date))
            stock_price = stock_price.reset_index()
            stock_price = stock_price.rename({'index' : 'date'}, axis = 1)
            
            for tick in [ticker]:
                stock_df = stock_price[stock_price['ticker'] == tick]
                #Create a plot of adjust closing price and volume
                fig = make_subplots(specs=[[{"secondary_y": True}]])
                fig.add_trace(go.Scatter(x = stock_df['date'], y = stock_df['adjclose'], fill='tonexty', name = 'Adjust close price')) 
                fig.add_trace(go.Bar(x=stock_df ['date'], y = stock_df['volume'], yaxis = 'y2', name = 'Volume'), secondary_y = True)
                
                #Create date_buttons to show the stock price for different duration of time
                date_buttons = [
                    {'count':1, 'step':"month", 'stepmode' : "backward", 'label':"1M"},
                    {'count':3, 'step':"month", 'stepmode' : "backward", 'label':"3M"},
                    {'count':6, 'step':"month", 'stepmode' : "backward", 'label':"6M"},
                    {'count':1, 'step':"year", 'stepmode' : "todate", 'label':"YTD"},
                    {'count':1, 'step':"year", 'stepmode' : "backward", 'label':"1Y"},
                    {'count':3, 'step':"year", 'stepmode' : "backward", 'label':"3Y"},
                    {'count':5, 'step':"year", 'stepmode' : "backward", 'label':"5Y"},
                    {'step':"all", 'label':"Max"}]
                
                fig.update_layout({'xaxis':{'rangeselector':{'buttons':date_buttons}}})
                fig.update_layout(width=1000, height=500)
                              
            st.plotly_chart(fig)

#==============================================================================
# Tab 2
#==============================================================================
def tab2():
    #Add dashboard title and description
    st.title("Financial dashboard")
    st.write("Data source: Yahoo Finance")
    st.header("Tab 2 - Chart")
        
    # Get stock data
    @st.cache        
    def GetStockData(ticker, start_date, end_date):
        return pd.concat([si.get_data(tick, start_date, end_date) for tick in ticker])
    
    # Add a line plot and a bar plot
    if ticker != '-':
        st.subheader("Time Series Data")
        stock_price = pd.DataFrame(GetStockData([ticker], start_date, end_date))
        stock_price = stock_price.reset_index()
        stock_price = stock_price.rename({'index' : 'date'}, axis = 1)
        for tick in [ticker]:
            stock_df = stock_price[stock_price['ticker'] == tick]
            
            #Create sub plot
            fig = make_subplots(specs=[[{"secondary_y": True}]])
            candle = go.Candlestick(x = stock_df['date'], open = stock_df['open'], high = stock_df['high'], 
                                         low = stock_df['low'], close  = stock_df['close'], name = 'Stock price')
            line = go.Scatter(mode = 'lines', x = stock_df['date'], y = stock_df['close'], name = 'Close price', 
                              marker_color= 'rgba(51, 170, 51, .7)')
            bar = go.Bar(x=stock_df ['date'], y = stock_df['volume'], name = 'Volume')
            
            #Create a simple moving average (MA) for the stock price
            MA = go.Scatter(mode = 'lines', x = stock_df['date'], 
                            y = stock_df['close'].rolling(window=50).mean(),
                            name = 'MA', marker_color='rgba(152, 0, 0, .8)')
            
            #Add traces to the figure
            fig.add_trace(bar, secondary_y = True)  
            fig.add_trace(candle)
            fig.update_layout(xaxis_rangeslider_visible=False)
            fig.add_trace(line)
            fig.add_trace(MA)
            
            #Create date_buttons to show the stock price for different duration of time
            date_buttons = [
                {'count':1, 'step':"month", 'stepmode' : "backward", 'label':"1M"},
                {'count':3, 'step':"month", 'stepmode' : "backward", 'label':"3M"},
                {'count':6, 'step':"month", 'stepmode' : "backward", 'label':"6M"},
                {'count':1, 'step':"year", 'stepmode' : "todate", 'label':"YTD"},
                {'count':1, 'step':"year", 'stepmode' : "backward", 'label':"1Y"},
                {'count':3, 'step':"year", 'stepmode' : "backward", 'label':"3Y"},
                {'count':5, 'step':"year", 'stepmode' : "backward", 'label':"5Y"},
                {'step':"all", 'label':"Max"}]
            
            #Create a button to change between candles and lines plot
            buttons = [{'label':'Candles', 'method':'update', 
                                 'args':[{'visible' : [True, True, False, True]}]},
                                {'label':'Lines','method':'update', 
                                 'args':[{'visible':[True, False, True, True]}]}]
            
            fig.update_layout({'xaxis':{'rangeselector':{'buttons':date_buttons}}})
            fig.update_layout({'updatemenus':[{'type':'dropdown', 'x':0.1, 'y':1, 
                                               'showactive':True, 'active':0, 'buttons':buttons}]})
            fig.update_layout(width=1000, height=500)
        st.plotly_chart(fig)
            
#==============================================================================
# Tab 3
#==============================================================================
def tab3():
    #Add dashboard title and description
    st.title("Financial dashboard")
    st.write("Data source: Yahoo Finance")
    st.header("Tab 3 - Statistics")
            
    # Add table to show statistics data
    @st.cache 
    def GetStats(ticker):
        return si.get_stats(ticker)
    
    def GetStatsValuation(ticker):
        return si.get_stats_valuation(ticker)  
            
    if ticker != '-':
            st.write('Statistics information of {}'.format(ticker))
            statistics = pd.DataFrame(GetStats(ticker))
            valuation = pd.DataFrame(GetStatsValuation(ticker))
                
            cols = st.columns(2)
            cols[0].caption('Valuation Measures')
            cols[0].dataframe(valuation)
            cols[0].caption('Financial Highlights')
            cols[0].caption('Fiscal Year')
            cols[0].dataframe(statistics[29:30])
            cols[0].caption('Profitability')
            cols[0].dataframe(statistics[31:32])
            cols[0].caption('Management Effectiveness')
            cols[0].dataframe(statistics[33:34])
            cols[0].caption('Income Statement')
            cols[0].dataframe(statistics[35:42])
            cols[0].caption('Balance Sheet')
            cols[0].dataframe(statistics[43:48])
            cols[0].caption('Cash Flow Statement')
            cols[0].dataframe(statistics[49:50])
            cols[1].caption('Trading Information')
            cols[1].caption('Stock Price History')
            cols[1].dataframe(statistics[:6])
            cols[1].caption('Share Statistics')
            cols[1].dataframe(statistics[7:18], height = 600)
            cols[1].caption('Dividends & Splits')
            cols[1].dataframe(statistics[19:28])
            
#==============================================================================
# Tab 4
#==============================================================================
def tab4():
    #Add dashboard title and description
    st.title("Financial dashboard")
    st.write("Data source: Yahoo Finance")
    st.header("Tab 4 - Financials")
    
    #Create two containers laid out 2 columns side by side
    col1, col2 = st.columns(2)
    with col1:
        if st.button('Income Statement'):
             income = si.get_income_statement(ticker)
             st.dataframe(income) 
        if st.button('Balance Sheet') :
             bs = si.get_balance_sheet(ticker)
             st.dataframe(bs)
        if st.button('Cash Flow'):
             cashflow = si.get_cash_flow(ticker)  
             st.dataframe(cashflow)
    with col2:     
        if st.button('Annual'):
            st.write('Income Statement')
            income = si.get_income_statement(ticker)
            st.dataframe(income) 
            st.write('Balance Sheet')
            bs = si.get_balance_sheet(ticker)
            st.dataframe(bs)
            st.write('Cash Flow')
            cashflow = si.get_cash_flow(ticker)  
            st.dataframe(cashflow)
        
        if st.button('Quarterly'):
            st.write('Income Statement')
            income = si.get_income_statement(ticker, yearly = False)
            st.dataframe(income) 
            st.write('Balance Sheet')
            bs = si.get_balance_sheet(ticker, yearly = False)
            st.dataframe(bs)
            st.write('Cash Flow')
            cashflow = si.get_cash_flow(ticker, yearly = False)
            st.dataframe(cashflow)
#==============================================================================
# Tab 5
#==============================================================================
def tab5():
    #Add dashboard title and description
    st.title("Financial dashboard")
    st.write("Data source: Yahoo Finance")
    st.header("Tab 5 - Analysis")
    
    # Add table to show analysis data
    def GetAnalystsInfo(ticker):
            return si.get_analysts_info(ticker)
        
    if ticker != '-':
            st.write('Analysis of {}'.format(ticker))
            analysis = GetAnalystsInfo(ticker)
            for item in analysis.keys():
                analysis[item] = st.dataframe(analysis[item])
            
#==============================================================================
# Tab 6
#==============================================================================
def tab6():
    #Add dashboard title and description
    st.title("Financial dashboard")
    st.write("Data source: Yahoo Finance")
    st.header("Tab 6 - Monte Carlo simulation")

    # Get stock data 
    @st.cache        
    def GetStockData(ticker, start_date, end_date):
        return pd.concat([si.get_data(tick, start_date, end_date) for tick in ticker])
    
    # Setup the Monte Carlo simulation
    np.random.seed(123)
    simulations = st.selectbox('Number of simulations', (200, 500, 1000))
    time_horizone = st.selectbox('Time', (30, 60, 90))
        
    # Run the simulation
    if ticker != '-':
            stock_price = pd.DataFrame(GetStockData([ticker], start_date, end_date))
            
    for tick in [ticker]:
        stock_df = stock_price[stock_price['ticker'] == tick]
        close_price = stock_df['close']
        daily_return = close_price.pct_change()
        daily_volatility = np.std(daily_return)
        
        simulation_df = pd.DataFrame()
    
    for i in range(simulations):
        
        # The list to store the next stock price
        next_price = []
        
        # Create the next stock price
        last_price = close_price.iloc[-1]
        
        for j in range(time_horizone):
            # Generate the random percentage change around the mean (0) and std (daily_volatility)
            future_return = np.random.normal(0, daily_volatility)
    
            # Generate the random future price
            future_price = last_price * (1 + future_return)
    
            # Save the price and go next
            next_price.append(future_price)
            last_price = future_price
        
        # Store the result of the simulation
        simulation_df[i] = next_price
    
    # Plot the simulation stock price in the future
    st.write('Monte Carlo simulation for {}'.format(ticker))  
    fig = px.line(simulation_df, x = None, y = simulation_df.index)
    fig.add_hline(y=close_price.iloc[-1])
    fig.update_layout(showlegend = False)
    fig.update_layout(width=1000, height=500)
    
    st.plotly_chart(fig)
    
    #Get the ending price
    ending_price = simulation_df.iloc[-1:, :].values[0, ]
    
    # Price at 95% confidence interval
    future_price_95ci = np.percentile(ending_price, 5)

    # Value at Risk
    VaR = close_price[-1] - future_price_95ci
    st.write('VaR at 95% confidence interval is: ' + str(np.round(VaR, 2)) + ' USD')
    
#==============================================================================
# Tab 7
#==============================================================================
def tab7():
    st.title("Financial dashboard")
    st.write("Data source: Yahoo Finance")
    st.header("Tab 7 - Profile")
    
    # Get stock data 
    @st.cache        
    def GetStockData(ticker, start_date, end_date):
        return pd.concat([si.get_data(tick, start_date, end_date) for tick in ticker])
    
    if ticker != '-':
            stock_price = pd.DataFrame(GetStockData([ticker], start_date, end_date))
            
    for tick in [ticker]:
        stock_df = stock_price[stock_price['ticker'] == tick]
        stock_df = stock_df.reset_index()
        stock_df = stock_df.rename({'index' : 'date'}, axis = 1)
        date = stock_df['date']
        close_price = stock_df['close']
        daily_return = close_price.pct_change()
        st.metric(label = ticker, value = np.round(close_price.iloc[-1], 2), 
                  delta = np.round(daily_return.iloc[-1], 2))
        st.write('As of '+ str(date.iloc[-1]))
    
    @st.cache 
    def GetCompanyInfo(ticker):
        return si.get_company_info(ticker)
    def GetComapanyOfficers(ticker):
        return si.get_company_officers(ticker)
    
    #Get the company information
    if ticker != '-':
            info = pd.DataFrame(GetCompanyInfo(ticker))
            info = info.astype(str)
            col1, col2 = st.columns(2)
            with col1:
                info.iloc[-2,0]
                info.iloc[6,0]
                info.iloc[-6,0]
                info.iloc[-5,0]
                info.iloc[7,0]
            with col2:
                st.write('Sector(s): ' + str(info.iloc[1, 0]))
                st.write('Industry: ' + str(info.iloc[-1,0]))
                st.write('Full Time Employess: ' + str(info.iloc[2,0]))
    
    st.write('Key Executives')
    
    #Get the company officiers information
    officiers =  GetComapanyOfficers(ticker)
    st.dataframe(officiers.iloc[:,:4])
#==============================================================================
# Main body
#==============================================================================

def run():
    
    # Add the ticker selection on the sidebar
    # Get the list of stock tickers from S&P500 
    if st.sidebar.button('Update'):
        si.tickers_sp500()
    
    ticker_list = ['-'] + si.tickers_sp500()
    
    
    # Add selection box
    global ticker
    ticker = st.sidebar.selectbox("Select a ticker", ticker_list)
    
    # Add select begin-end date
    global start_date, end_date
    col1, col2 = st.sidebar.columns(2)
    start_date = col1.date_input("Start date", datetime.today().date() - timedelta(days=30))
    end_date = col2.date_input("End date", datetime.today().date())
    
    # Add a radio box
    select_tab = st.sidebar.radio("Select tab", ['Summary', 'Chart', 'Statistics', 
                                                 'Financials', 'Analysis', 
                                                 'Monte Carlo simulation', 'Profile'])
    
    # Show the selected tab
    if select_tab == 'Summary':
        # Run tab 1
        tab1()
    elif select_tab == 'Chart':
        # Run tab 2
        tab2()
    elif select_tab == 'Statistics':
        # Run tab 3
        tab3()
    elif select_tab == 'Financials':
        # Run tab 4
        tab4()
    elif select_tab == 'Analysis':
        # Run tab 5
        tab5()
    elif select_tab == 'Monte Carlo simulation':
        # Run tab 6
        tab6()
    elif select_tab == 'Profile':
        # Run tab 7
        tab7()
    
if __name__ == "__main__":
    run()
    
###############################################################################
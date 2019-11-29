# -*- coding: utf-8 -*-
"""
Created on Thu Oct 24 14:52:17 2019

@author: SHARDUL
"""

import numpy as np
from flask import Flask, request, jsonify, render_template,session, redirect
import pickle
import matplotlib.pyplot as plt
import pandas as pd
from datetime import datetime as dt
import seaborn as sns; sns.set()
import statsmodels.api as sm
import warnings
import itertools

app = Flask(__name__)
model = pickle.load(open('Model/trained.pickle', 'rb'))

@app.route('/')
def home():
    return render_template('index3.html')
    
@app.route('/eda', methods=['GET', 'POST'])
def eda():
    return render_template('eda.html')
@app.route('/predict',methods=['POST'])
def predict():
    '''
    For rendering results on HTML GUI
    '''
    int_features = [x for x in request.form.values()]
    #int_features[0]
    #final_features = [np.array(int_features)]
    #prediction = model.predict(final_features)
    
    plt.style.use('fivethirtyeight')
    plt.rcParams['axes.labelsize'] = 14
    plt.rcParams['xtick.labelsize'] = 12
    plt.rcParams['ytick.labelsize'] = 12
    plt.rcParams['text.color'] = 'k'
    df=pd.read_csv('air_visit_data.csv',sep=";")
    
    ss=str(int_features[0])
    ss=str('air_ba937bf13d40fb24')
    own=df.loc[df['air_store_id'] == ss]
    df=own
    df['visit_date'] = pd.to_datetime(df['visit_date'])
    
#    dfinal.dtypes
    df['visit_date'].min(), df['visit_date'].max()
    
#    df.isnull().sum()
    
    df = df.groupby('visit_date')['visitors'].sum().reset_index()
    
    df = df.set_index('visit_date')
#    df.index
    y = df['visitors'].resample('MS').mean()
    y.fillna(0, inplace=True)
#    y['2016':]
    
    p = d = q = range(0, 2)
    pdq = list(itertools.product(p, d, q))
    seasonal_pdq = [(x[0], x[1], x[2], 12) for x in list(itertools.product(p, d, q))]
    pak_param=0
    seaonal=0
    maic=9999999
    
    for param in pdq:
        for param_seasonal in seasonal_pdq:
            try:
                mod = sm.tsa.statespace.SARIMAX(y,
                                                order=param,
                                                seasonal_order=param_seasonal,
                                                enforce_stationarity=False,
                                                enforce_invertibility=False)
                results = mod.fit()
                if results.aic<maic:
                    maic=results.aic
                    pak_param=param
                    seaonal=param_seasonal
                    
    #            print('ARIMA{}x{}12 - AIC:{}'.format(param, param_seasonal, results.aic))
            except:
                continue
    
    mod = sm.tsa.statespace.SARIMAX(y,
                                    order=pak_param,
                                    seasonal_order=seaonal,
                                    enforce_stationarity=False,
                                    enforce_invertibility=False)
    results = mod.fit()
    pred = results.get_prediction(start=pd.to_datetime('2019-01-01'), dynamic=False)
    pred_ci = pred.conf_int()
    #own.boxplot(own.reserve_visitors)
    y_forecasted = pred.predicted_mean
    y_truth = y['2016-01-01':]
    mse = ((y_forecasted - y_truth) ** 2).mean()
#    print('The Mean Squared Error of our forecasts is {}'.format(round(mse, 2)))
#    print('The Root Mean Squared Error of our forecasts is {}'.format(round(np.sqrt(mse), 2)))
    pred_uc = results.get_forecast(steps=30)
    pred_ci = pred_uc.conf_int()
#    ax = y.plot(label='Observed Past Records', figsize=(10, 5))
#    pred_uc.predicted_mean.plot(ax=ax, label='Expected Number Of Visitors')
#    ax.fill_between(pred_ci.index,
#                    pred_ci.iloc[:, 0],
#                   pred_ci.iloc[:, 1], color='k', alpha=.25)
#    ax.set_xlabel('Date')
#    ax.set_ylabel('Number Of Visitors In A Month')

#    print(pred_uc)
#    print(pred_uc.conf_int())
#    print("Predicted Vaues")
#    print(pred_uc.predicted_mean)
#    output = round(prediction[0], 2)
#    type(pred_uc.predicted_mean)
    final=pred_uc.predicted_mean.to_frame()
    final.reset_index(inplace=True)
    
    final=final.rename(columns={int(0): "Visitors_Count"})
    final["Visitors_Count"]=final["Visitors_Count"].abs()
#    final.round("a")
    final["Visitors_Count"]=final["Visitors_Count"].round()
    #for i in range(0,len(pred_uc.predicted_mean)):
    #    pred_uc.predicted_mean[i]=int(round(pred_uc.predicted_mean[i]))
    output=str(final['Visitors_Count'])    
    return render_template('index3.html',tables=[final.to_html(classes='data')],titles=final.columns.values)
#    return render_template('index3.html', prediction_text=output)
if __name__ == "__main__":
    app.run(debug=True)
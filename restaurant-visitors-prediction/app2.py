# -*- coding: utf-8 -*-
"""
Created on Thu Oct 31 14:11:23 2019

@author: DELL
"""

# -*- coding: utf-8 -*-
"""
Created on Thu Oct 24 14:52:17 2019

@author: SHARDUL
"""

import numpy as np
from flask import Flask, request, jsonify, render_template
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
    return render_template('index2.html')

from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
from matplotlib.figure import Figure

@app.route('/plot.png')
def plot_png():
    fig = create_figure()
    output = io.BytesIO()
    FigureCanvas(fig).print_png(output)
    return Response(output.getvalue(), mimetype='image/png')

def create_figure():
    return ax


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
    df=pd.read_csv('hpg_reserve.csv',sep=";")
    ss="hpg_c2458e9aaee8d652"
    ss=str(int_features[0])
    own=df.loc[df['hpg_store_id'] == ss]
    df=own
    df['visit_datetime'] = pd.to_datetime(df['visit_datetime'])
    
#    df.dtypes
    df['visit_datetime'].min(), df['visit_datetime'].max()
    
#    df.isnull().sum()
    
    df = df.groupby('visit_datetime')['reserve_visitors'].sum().reset_index()
    
    df = df.set_index('visit_datetime')
#    df.index
    y = df['reserve_visitors'].resample('MS').mean()
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
    pred = results.get_prediction(start=pd.to_datetime('2016-01-01'), dynamic=False)
    pred_ci = pred.conf_int()
    y_forecasted = pred.predicted_mean
    y_truth = y['2016-01-01':]
    mse = ((y_forecasted - y_truth) ** 2).mean()
#    print('The Mean Squared Error of our forecasts is {}'.format(round(mse, 2)))
#    print('The Root Mean Squared Error of our forecasts is {}'.format(round(np.sqrt(mse), 2)))
    pred_uc = results.get_forecast(steps=20)
    pred_ci = pred_uc.conf_int()
    ax = y.plot(label='Observed Past Records', figsize=(10, 5))
    pred_uc.predicted_mean.plot(ax=ax, label='Expected Number Of Visitors')
    ax.fill_between(pred_ci.index,
                    pred_ci.iloc[:, 0],
                    pred_ci.iloc[:, 1], color='k', alpha=.25)
    ax.set_xlabel('Date')
    ax.set_ylabel('Number Of Visitors In A Month')
    plt.legend()
    graph=plt.show()
    plot_png()
#    print(pred_uc)
#    print(pred_uc.conf_int())
#    print("Predicted Vaues")
#    print(pred_uc.predicted_mean)
#    output = round(prediction[0], 2)
    
    for i in range(0,len(pred_uc.predicted_mean)):
        pred_uc.predicted_mean[i]=int(round(pred_uc.predicted_mean[i]))
    
    return render_template('index.html', prediction_text='Employee Salary should be $ {}'.format(pred_uc.predicted_mean))
    return render_template('index.html', prediction_text='Employee Salary should be $ {}'.format(pred_uc.predicted_mean))

if __name__ == "__main__":
    app.run(debug=True)
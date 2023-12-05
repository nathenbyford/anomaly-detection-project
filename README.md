# Anomaly detection in time series project

Final project as part of STA 5360 advanced data driven methods at Baylor University. Final presentation can be found [here](www.natebyford.com/anomaly-detection-project)

The goal of this project to gain familiarity with data driven methods of anomaly detection and learn how to implement them better. In this project I use 4 different anomaly detection methods on the twitter tags section of the Numenta Anomaly Benchmark ([NAB](https://github.com/numenta/NAB)).

The methods of anomaly detection compared:

* Linear regression leverage points
* Seasonal decomposition of Time series using Loess (STL)
* Recursive Neural Network
* Isolation Forest

## Data

The data is a time series of twitter tags from 10 large companies with pre-determined outliers seen in the figure below. Each time series has about between 2 and 5 anomalies, so there is a clear imbalance problem in the dataset meaning that there are far more "normal" values in the data compared to the number of anomalous values.

![Figure of data](/final_report/data_plot.png)

## Methods


function(VARS, element, context) {
    let ds = VARS.DS.data();
    //console.log("DEBUG:typeof ds:",typeof ds);
    //console.log("DEBUG:ds:",ds);
    //console.log("DEBUG:ds[0]:",ds[0]);
    
    let arrayMean = function (arr) {
        return arr.reduce((a, b) => a + b, 0) / arr.length;
    };
    
    // Functions to find MeanSquareError
    let MSE = function (fact, predict) {
        let valueArray = fact.map(
            function (v, i) {
                return Math.pow((v - predict[i]), 2);
            });
        return (valueArray.reduce((a, x) => (a + x), 0)) / valueArray.length;
    };
    
    // Functions to find RootMeanSquareError
    let RMSE = function (fact, predict) {
        let valueArray = fact.map(
            function (v, index) {
                return Math.pow((v - predict[index]), 2);
            });
        return Math.sqrt(valueArray.reduce((acc, value) => (acc + value), 0)) / valueArray.length;
    };
    
    // Functions to find MeanAbsError
    let MAE = function (fact, predict) {
        let valueArray = fact.map(
            function (v, index) {
                return Math.abs(v - predict[index]);
            });
        return ( valueArray.reduce((acc, value) => (acc + value)) / valueArray.length );
    };

    // Functions to find MeanAbsPersError
    let MAPE = function (fact, predict) {
        let valueArray = fact.map(
            function (v, index) {
                return Math.abs(v - predict[index])/v;
            });
        return ( valueArray.reduce((acc, value) => (acc + value)) / valueArray.length * 100);
    };      
    
    // Functions to find 100 - MAPE
    let accuracy = function (fact, predict) {
        let valueArray = fact.map(
            function (v, index) {
                return Math.abs(v - predict[index])/v;
            });
        return 100 - ( valueArray.reduce((acc, value) => (acc + value)) / valueArray.length * 100 );
    };

    // Functions to find std
    let standDev = function (arr) {
        let valueMean = arr.reduce((acc, b) => acc + b, 0) / arr.length;
        return Math.sqrt(arr.reduce((acc, x) => (acc + Math.pow(x - valueMean, 2))) / arr.length);
    };
    
    // Functions to find minV
    let minValue = function (arr) {
        return Math.min.apply(null, arr);
    };
    
    let maxValue = function (arr) {
        return Math.max.apply(null, arr);
    };
    


    // Prepare array of parameter with numbers
    let fact = ds.map((value) => {
        //return value.p_load_fact_smooth;
        return value.FACT_P_L;
    });
    let predict = ds.map((value) => {
        //return value.predict_p_load;
        return value.PRED_P_L;
    });

    let date = ds.map((value) => {
        return value.date;
    });

    let value = {};
    
    value.predictMinValue = Math.round(minValue(predict));
    value.predictMaxValue = Math.round(maxValue(predict));
    
    value.mse = Math.round(MSE(fact, predict));
    value.rmse = Math.round(RMSE(fact, predict));
    value.mae = Math.round(MAE(fact, predict));
    value.mape = Math.round(MAPE(fact, predict));       // ошибка в процентах    
    value.accuracy = Math.round(accuracy(fact, predict)); //точность в %

    value.factMean = Math.round(arrayMean(fact));
    value.predictMean = Math.round(arrayMean(predict));
    // value.factStandDev = standDev(p_fact, value.factMean);
    value.predictStandDev = Math.round(standDev(predict));
    value.date = date[0];
    
    console.log('DEBUG:calculationP:date ' + date);
    console.log('DEBUG:calculationP:fact ' + fact);
    console.log('DEBUG:calculationP:predict ' + predict);
    console.log('DEBUG:calculationP:value.maep ' + value.maep);
    
  return [value];
}
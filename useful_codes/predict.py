model_type_ls = ["lda", "qda", "xgboost", "logcv", "rf"
                 , "extra", "nb", "adaboost", "svm", "knn"]
results_sel = {}

for model_type in model_type_ls:
    
    recall_ls = []
    precision_ls = []
    accuracy_ls = []
    
    for m in list(datasets.keys()):
    
        X_train_scaled, X_test_scaled, y_train, y_test = preprocess(path=datasets[m]["path"], 
                                                                    exc_cols=datasets[m]["exc_cols"], 
                                                                    rows=datasets[m]["rows"],
                                                                    target=datasets[m]["target"])

    
        model_name = model_type+"_"+m+".p"
        
        try:
            model_fit = pickle.load(open('models/%s'%model_name, mode='rb'))
            y_pred = model_fit.predict(X_test_scaled)
            cr = pd.DataFrame(classification_report(y_true=y_test, 
                                                    y_pred=y_pred, 
                                                    output_dict=True))

            precision_ls.append(cr.iloc[:,0]["precision"])
            recall_ls.append(cr.iloc[:,1]["recall"])
            accuracy_ls.append(cr["accuracy"][0])
        except:
            precision_ls.append(np.nan)
            recall_ls.append(np.nan)
            accuracy_ls.append(np.nan)
            
        
        print(model_name)
        print('\nClassification Report:')
        display(cr)
    
    results_sel[model_type+"_precision_0"] = precision_ls
    results_sel[model_type+"_recall_1"] = recall_ls
    results_sel[model_type+"_accuracy"] = accuracy_ls
    
results_df = pd.DataFrame(results_sel, index=datasets.keys())
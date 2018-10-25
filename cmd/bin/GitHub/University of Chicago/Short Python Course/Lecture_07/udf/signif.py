def signif(fit_result_table):
    coef_table = pd.DataFrame(fit_result_table[1].data,  columns =  ['variable','coef','std err','z','P>|z|','[0.025','0.975]']).\
                                                                    set_index('variable')
    # first row is columns names, drop it
    coef_table = coef_table.drop([''], axis = 0)
    coef_table.iloc[:,3] = coef_table.iloc[:,3].astype(np.float32)

        # define a significance column similar to R
    coef_table[' '] = ['ooo' if i<0.001 else 'oo' if i>0.001 and i<=.01\
                  else 'o'  if i>.01 and i <=.05\
                  else '.'  if i>.05 and i <=.1 else ' ' for i in coef_table.iloc[:,3]]
    return(coef_table)  
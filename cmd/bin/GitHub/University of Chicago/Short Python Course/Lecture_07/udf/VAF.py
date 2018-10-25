def VAF(pdf, kmeans_obj):
    
    '''
    pdf: pandas dataframe
       
    keamsn_obje: the fitted object of a KMeans method from sklearn
    
    returns an nparray:
    [within cluster SS], between-clusters SS, total SS, variance accounted for.
    '''
    wss = []
    totss = []
    n = kmeans_obj.n_clusters
    labels = kmeans_obj.labels_
    centers = kmeans_obj.cluster_centers_
    grd_mean = np.nanmean(np.array(pdf), axis = 0)
    pdf = pdf.copy()
    pdf['l'] = labels
    
    for i in range(n):
        subset = pdf[pdf['l'] == i].copy()
        n_subset = subset.shape[0]
        subset = np.array(subset.iloc[:,:-1])
        subset_ =np.transpose([(j - centers[i])**2 for j in subset])
        wss_ = np.sum(np.nansum(subset_, axis = 0))
        wss.append(wss_)
        
        subset_ =np.transpose([(j - grd_mean)**2 for j in subset])
        totss_ = np.sum(np.nansum(subset_, axis = 1))
        totss.append(totss_)
        
    
    bss = np.sum(totss) - np.sum(wss)
    
    VAF = np.sum(bss)/ np.sum(totss)
    
    return(np.array([wss, bss, np.sum(totss), VAF]))
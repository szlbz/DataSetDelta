lazarus自带的bufDataSet和MemDataSet缺少类似TClientDataSet的Delta功能，这个单元从TDataSet扩展了Delta，适用于所有TDataSet。  
使用方法：  
1、在unit的uses添加DataSetDelta  
2、xxxDataSet.ActivateMonitoring(true)//启动Delta功能  
3、xxxDataSet.GetActionSQL('test');//根据Delta生成SQL  
注意：  
使用GetActionSQL后会清空Delta的记录。  
  
BufDataset使用Delta的方法：  
BufDataset1.ActivateMonitoring(true)//启动Delta功能  
BufDataset1.GetActionSQL('test');//根据Delta生成SQL  

====================================================  
The bufDataSet and MemDataSet that come with Lazarus lack the Delta functionality similar to TClientDataSet. This unit extends Delta from TDataSet, making it applicable to all TDataSet instances.  
  
How to use:  
  
Add DataSetDelta to the uses clause of your unit.  
xxxDataSet.ActivateMonitoring(true); // Enable the Delta functionality  
xxxDataSet.GetActionSQL('test'); // Generate SQL based on the Delta  
Note:  
Using GetActionSQL will clear the Delta records.  
  
How to use Delta with BufDataset:  
  
BufDataset1.ActivateMonitoring(true); // Enable the Delta functionality  
BufDataset1.GetActionSQL('test'); // Generate SQL based on the Delta  

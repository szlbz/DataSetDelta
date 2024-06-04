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

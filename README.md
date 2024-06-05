lazarus自带的bufDataSet和MemDataSet缺少类似TClientDataSet的Delta功能，这个单元从TDataSet扩展了Delta，适用于所有TDataSet。  
使用方法：  
1、在unit的uses添加DataSetDelta  
2、ActivateMonitoring(true)//true--启动Delta功能  false--停止Delta  
3、GetActionSQL(const ATableName: String; const AKeyFields: String = '');//根据Delta生成SQL  
注意：  
使用GetActionSQL后会清空Delta的记录。  
  
BufDataset使用Delta的方法：  
  dcm1:TDataSetChangesMonitor;  
  dcm2:TDataSetChangesMonitor;  

  dcm1:=TDataSetChangesMonitor.Create(self);  
  dcm2:=TDataSetChangesMonitor.Create(self);  
  dcm1.DataSet:=BufDataset1; //监控BufDataset1的数据变化  
  dcm2.DataSet:=BufDataset2; //监控BufDataset2的数据变化  
  dcm1.ActivateMonitoring(true);  
  dcm2.ActivateMonitoring;  

  sql:=dcm1.GetActionSQL('test');//读取Delta生成的SQL
  sql:=dcm2.GetActionSQL('demo');

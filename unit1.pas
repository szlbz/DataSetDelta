unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, DB, Forms, Controls, Graphics, Dialogs,
  LCLType, DBGrids, StdCtrls, TypInfo, DataSetDelta;

type

  { TForm1 }

  TForm1 = class(TForm)
    BufDataset1: TBufDataset;
    BufDataset2: TBufDataset;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    DataSetChangesMonitor1: TDataSetChangesMonitor;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
  public
    dcm1:TDataSetChangesMonitor;
    dcm2:TDataSetChangesMonitor;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button2Click(Sender: TObject);
var sql:string;
begin
  if BufDataset1.State in [dsEdit, dsInsert] then
    BufDataset1.Post;
  sql:=DataSetChangesMonitor1.GetActionSQL('test');
  if sql<>'' then
    memo1.Lines.Add(sql);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if DataSetChangesMonitor1.Active then
  begin
  DataSetChangesMonitor1.Active:=false;
  Button1.caption:='开启监控';
  end
  else
  begin
    DataSetChangesMonitor1.Active:=true;
    Button1.caption:='停止监控';
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var sql:string;
begin
  if BufDataset2.State in [dsEdit, dsInsert] then
    BufDataset2.Post;
  sql:=dcm2.GetActionSQL('demo');
  if sql<>'' then
    memo1.Lines.Add(sql);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(dcm1) then freeandnil(dcm1);
  if Assigned(dcm2) then freeandnil(dcm2);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if BufDataset1.State in [dsEdit, dsInsert] then
    BufDataset1.Post;
  if BufDataset2.State in [dsEdit, dsInsert] then
    BufDataset2.Post;
  if (dcm1.Changed) or (dcm2.Changed) then
  begin
    if application.MessageBox('数据有变化，是否退出？', '注意', MB_YESNO) = IDNO then
      CanClose:=false;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BufDataset1.FieldDefs.Clear;
  BufDataset1.FieldDefs.Add('test1', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftString')), 30);
  BufDataset1.FieldDefs.Add('test2', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftinteger')));
  BufDataset1.CreateDataset;

  BufDataset2.FieldDefs.Clear;
  BufDataset2.FieldDefs.Add('test11', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftString')), 30);
  BufDataset2.FieldDefs.Add('test12', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftinteger')));
  BufDataset2.FieldDefs.Add('test13', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftinteger')));
  BufDataset2.CreateDataset;

  memo1.Lines.Clear;
  BufDataset1.Open;
  BufDataset2.Open;

  dcm1:=TDataSetChangesMonitor.Create(self);
  dcm2:=TDataSetChangesMonitor.Create(self);
  //dcm1.DataSet:=BufDataset1; //监控BufDataset1的数据变化
  dcm2.DataSet:=BufDataset2; //监控BufDataset2的数据变化
  DataSetChangesMonitor1.Active:=true;
  //DataSetChangesMonitor1.ActivateMonitoring(true);
  dcm2.ActivateMonitoring;
end;

end.


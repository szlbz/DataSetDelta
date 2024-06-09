unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, DB, SQLDB, SQLite3Conn, Forms, Controls,
  Graphics, Dialogs, LCLType, DBGrids, StdCtrls, TypInfo, DataSetDelta,
  ZConnection, ZDataset,LConvEncoding;

type

  { TForm1 }

  TForm1 = class(TForm)
    BufDataset2: TBufDataset;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    QFDataSetMonitor1: TQFDataSetMonitor;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    Memo1: TMemo;
    SQLite3Connection1: TSQLite3Connection;
    SQLTransaction1: TSQLTransaction;
    ZConnection1: TZConnection;
    ZQuery1: TZQuery;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
  public
    dcm1:TQFDataSetMonitor;
    dcm2:TQFDataSetMonitor;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button2Click(Sender: TObject);
var sql:string;
begin
  if ZQuery1.State in [dsEdit, dsInsert] then
    ZQuery1.Post;
  sql:=QFDataSetMonitor1.GetActionSQL('hardware');
  if sql<>'' then
    memo1.Lines.Add(sql);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if QFDataSetMonitor1.Active then
  begin
    QFDataSetMonitor1.Active:=false;
    Button1.caption:='开启监控';
  end
  else
  begin
    QFDataSetMonitor1.Active:=true;
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
  if ZQuery1.State in [dsEdit, dsInsert] then
    ZQuery1.Post;
  if BufDataset2.State in [dsEdit, dsInsert] then
    BufDataset2.Post;
  if (QFDataSetMonitor1.Changed) or (dcm2.Changed) then
  begin
    if application.MessageBox('数据有变化，是否退出？', '注意', MB_YESNO) = IDNO then
      CanClose:=false;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  BufDataset2.FieldDefs.Clear;
  BufDataset2.FieldDefs.Add('vstr1', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftString')), 30);
  BufDataset2.FieldDefs.Add('vint1', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftinteger')));
  BufDataset2.FieldDefs.Add('vint2', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftinteger')));
  BufDataset2.CreateDataset;

  memo1.Lines.Clear;
  BufDataset2.Open;

  dcm2:=TQFDataSetMonitor.Create(self);
  dcm2.DataSet:=BufDataset2; //监控BufDataset2的数据变化
  dcm2.Active:=true;//.ActivateMonitoring;

  {$ifdef linux}
  SQLiteLibraryName:=ExtractFilePath(Application.ExeName)+'libsqlite3.so';
  {$else}
  SQLiteLibraryName:=utf8tocp936(ExtractFilePath(Application.ExeName)+'sqlite3.dll');
  {$endif}
  ZConnection1.Disconnect;
  ZConnection1.Protocol:='sqlite';
  ZConnection1.LibraryLocation:=utf8tocp936(ExtractFilePath(Application.ExeName)+{$ifdef linux}'libsqlite3.so'{$else}'sqlite3.dll'{$endif});
  ZConnection1.Properties.Clear;
  ZConnection1.Properties.Add('encrypted=yes');
  ZConnection1.Properties.Add('cipher=sqlcipher');
  ZConnection1.Properties.Add('sqlcipher=legacy');
  ZConnection1.Properties.Add('legacy=1');
  ZConnection1.Properties.Add('controls_cp=CP_UTF8');
  ZConnection1.Properties.Add('AutoEncodeStrings=True');
  ZConnection1.Database:='demo.db3';
  ZConnection1.Password:='123asd';
  ZConnection1.Connect;
  QFDataSetMonitor1.Active:=true;
  ZQuery1.Close;
  ZQuery1.SQL.Text:='select * from hardware';
  ZQuery1.Open;

end;

end.


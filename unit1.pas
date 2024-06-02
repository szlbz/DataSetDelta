unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, DB, Forms, Controls, Graphics, Dialogs,
  DBGrids, StdCtrls,  Memds,TypInfo,Variants,DataSetDelta;

type

  { TForm1 }

  TForm1 = class(TForm)
    BufDataset1: TBufDataset;
    BufDataset1test1: TStringField;
    BufDataset1test2: TLongintField;
    Button1: TButton;
    Button2: TButton;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  //frLocalizationController1.ShowLocalizationEditor;
  //frxReport1.DesignReport();
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  s:string;
begin
  if BufDataset1.ChangesDataSet<>nil then
  begin
    memo1.Lines.Add(BufDataset1.ChangesDataSet.RecordCount.ToString);

    if BufDataset1.ChangesDataSet.FieldByName('NewValues').DataType=ftString then
      memo1.Lines.Add('NewValues='+BufDataset1.ChangesDataSet.FieldByName('NewValues').AsString);
    if BufDataset1.ChangesDataSet.FieldByName('NewValues').DataType=ftInteger then
      memo1.Lines.Add('NewValues='+BufDataset1.ChangesDataSet.FieldByName('NewValues').AsInteger.ToString);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BufDataset1.ActivateMonitoring(true);
  if BufDataset1.ChangesDataSet<>nil then
     DataSource2.DataSet:=BufDataset1.ChangesDataSet;
end;

end.


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
    DBGrid1: TDBGrid;
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
var sql:string;
begin
  sql:=BufDataset1.GetActionSQL('test');
  if sql<>'' then
    memo1.Lines.Add(sql);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  memo1.Lines.Clear;
  BufDataset1.ActivateMonitoring(true);
end;

end.


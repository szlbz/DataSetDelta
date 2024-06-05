unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, DB, Forms, Controls, Graphics, Dialogs,
  DBGrids, StdCtrls,  Memds,TypInfo,Variants,DataSetDelta,lazutf8;

type

  { TForm1 }

  TForm1 = class(TForm)
    BufDataset1: TBufDataset;
    Button1: TButton;
    Button2: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
var
  s: string;
  function StringContainsChinese(const str: Widestring): boolean;
  var
    i: integer;
  begin

    for i := 1 to Length(str) do
    begin
       // 检查是否在基本汉字范围内
      if (Ord(str[i]) >= $4E00) and (Ord(str[i]) <= $9FFF) then
      begin
        Result := true;
        Exit; // 找到一个就返回 true
      end;
      // 可以添加更多的检查，例如扩展的 Unicode 区域
      // ...
    end;
    Result := false; // 没有找到中文字符
  end;
begin
  s:='12中文测试'+LineEnding;
  if StringcontainsChinese(s) then
    showmessage('字符串包含中文')
  else
    showmessage('字符串不包含中文');
end;

procedure TForm1.Button2Click(Sender: TObject);
var sql:string;
begin
  sql:=BufDataset1.GetActionSQL('test');
  if sql<>'' then
    memo1.Lines.Add(sql);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  BufDataset1.ActivateMonitoring(false);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BufDataset1.FieldDefs.Add('test1', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftString')), 30);
  BufDataset1.FieldDefs.Add('test2', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftinteger')));
  BufDataset1.CreateDataset;

  memo1.Lines.Clear;
  BufDataset1.Open;
  BufDataset1.ActivateMonitoring(true);
end;

end.


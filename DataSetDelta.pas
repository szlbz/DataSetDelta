{*******************************************************}
{                                                       }
{ 为lazarus TDataSet增加类似TClientDataSet的Delta功能   }
{                    适用于所有TDataSet                 }
{               只需在unit的uses添加DataSetDelta        }
{ BufDataset1.ActivateMonitoring(true)//启动Delta功能   }
{ BufDataset1.GetActionSQL('test');//根据Delta生成SQL   }
{                                                       }
{               Copyright(c) 2024-2024                  }
{              秋风(QQ315795176)原创出品                }
{                                                       }
{                 All rights reserved                   }
{                     保留所有权利                      }
{                                                       }
{*******************************************************}

unit DataSetDelta;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, DB, TypInfo,Variants;

type

  TDataStateValue = (dsvOriginal, dsvDeleted, dsvInserted, dsvUpdated);
  //TDataState = set of TDataStateValue;

  TDataSetChangesHelper =class Helper for TDataset
  public
    procedure CreateTable;
    function GetActionSQL(const ATableName : String; const AKeyFields: String = ''): String;
    function GetChangedCount:int64;
    procedure BeforeInserts(DataSet: TDataSet);
    procedure BeforeEdits(DataSet: TDataSet);
    procedure BeforeDeletes(DataSet:TDataSet);
    procedure AfterPosts(DataSet: TDataSet);
    procedure ActivateMonitoring(Value:Boolean);
    property ChangedCount:int64 read GetChangedCount;
  end;

implementation

var
  FDataState:string;//TDataStateValue;
  Foldvalue:array of Variant;
  FBeforeEdit: TDataSetNotifyEvent;
  FBeforeDelete: TDataSetNotifyEvent;
  FBeforeInsert: TDataSetNotifyEvent;
  FAfterPost: TDataSetNotifyEvent;
  FNewDataSet:TBufDataSet;
  FOldDataSet:TBufDataSet;

function TDataSetChangesHelper.GetChangedCount:int64;
begin
  Result:=FOldDataSet.RecordCount;
end;

procedure TDataSetChangesHelper.CreateTable;
var
  i:integer;
  LFieldName, LFieldType, LFieldValue: string;
  LFieldSize : Integer;
begin
  FNewDataSet:=TBufDataSet.Create(nil);
  for I := 0 to Self.FieldCount - 1 do
  begin
    LFieldName := Self.Fields[I].FieldName;
    LFieldType := GetEnumName(TypeInfo(TFieldType), Integer(Self.Fields[I].DataType));
    LFieldSize := Self.Fields[I].DataSize;
    if (LFieldType = 'ftString') then
      FNewDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)), LFieldSize)
    else
      FNewDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)));
  end;
  FNewDataSet.FieldDefs.Add('DataState', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftString')), 30);
  FNewDataSet.CreateDataset;

  FOldDataSet:=TBufDataSet.Create(nil);
  for I := 0 to Self.FieldCount - 1 do
  begin
    LFieldName := Self.Fields[I].FieldName;
    LFieldType := GetEnumName(TypeInfo(TFieldType), Integer(Self.Fields[I].DataType));
    LFieldSize := Self.Fields[I].DataSize;
    if (LFieldType = 'ftString')  then
      FOldDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)), LFieldSize)
    else
      FOldDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)));
  end;
  FOldDataSet.FieldDefs.Add('DataState', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftString')), 30);
  FOldDataSet.CreateDataset;
end;

procedure TDataSetChangesHelper.BeforeInserts(DataSet: TDataSet);
var
  i:integer;
begin
  if Foldvalue<>nil then
  begin
    FDataState:='Inserted';
    for i:=0 to DataSet.Fields.Count-1 do
      Foldvalue[i]:=null;
  end;
end;

procedure TDataSetChangesHelper.BeforeEdits(DataSet: TDataSet);
var
  i:integer;
begin
  if Foldvalue<>nil then
  begin
    FDataState:='Updated';
    for i:=0 to DataSet.Fields.Count-1 do
      Foldvalue[i]:=DataSet.Fields[i].NewValue;
  end;
end;

procedure TDataSetChangesHelper.BeforeDeletes(DataSet: TDataSet);
var
  i:integer;
begin
  FDataState:='Deleted';
  if Foldvalue<>nil then
  begin
    FNewDataSet.Append;
    FOldDataSet.Append;
    for i:=0 to DataSet.Fields.Count-1 do
    begin
      FNewDataSet.Fields[i].Value := DataSet.Fields[i].NewValue;
      FOldDataSet.Fields[i].Value := null;
    end;
    FOldDataSet.FieldByName('DataState').AsString:=FDataState;
    FOldDataSet.Post;
    FNewDataSet.Post;
  end;
end;

procedure TDataSetChangesHelper.AfterPosts(DataSet: TDataSet);
var
  i:integer;
  s:string;
begin
  if Foldvalue<>nil then
  begin
    FNewDataSet.Append;
    FOldDataSet.Append;
    for i:=0 to DataSet.Fields.Count-1 do
    begin
      FNewDataSet.Fields[i].Value := DataSet.Fields[i].NewValue;
      FOldDataSet.Fields[i].Value := Foldvalue[i];
      FOldDataSet.FieldByName('DataState').AsString:=FDataState;
    end;
    FNewDataSet.Post;
    FOldDataSet.Post;
  end;
end;

procedure TDataSetChangesHelper.ActivateMonitoring(Value:Boolean);
var
  FieldDef: TFieldDefs;
begin
  if Value then
  begin
    if not (csDesigning in ComponentState) then
    begin
      setlength(Foldvalue,self.Fields.Count);
      FBeforeEdit:=self.BeforeEdit;
      FBeforeDelete:=self.BeforeDelete;
      FBeforeInsert:=self.BeforeInsert;
      FAfterPost:=self.AfterPost;
      self.BeforeEdit:=@BeforeEdits;
      self.BeforeDelete:=@BeforeDeletes;
      self.BeforeInsert:=@BeforeInserts;
      self.AfterPost:=@AfterPosts;
      CreateTable;
    end;
  end
  else
  begin
    self.BeforeEdit:=FBeforeEdit;
    self.BeforeDelete:=FBeforeDelete;
    self.BeforeInsert:=FBeforeInsert;
    self.AfterPost:=FAfterPost;
    FBeforeEdit:=nil;
    FBeforeDelete:=nil;
    FBeforeInsert:=nil;
    FAfterPost:=nil;
    Foldvalue:=nil;
    FNewDataSet.Free;
    FOldDataSet.Free;
  end;
end;

function TDataSetChangesHelper.GetActionSQL(const ATableName
  : String; const AKeyFields: String = ''): String;
var
  nFldOrder: integer;
  cFldName, s1, s2: String;

  function SQLValue(const ARow: TBufDataSet; AOrder: Integer): String;
  var
    cName, cValue: String;
    eType: TFieldType;
  begin
    cName := ARow.Fields[AOrder].FieldName;
    eType := ARow.Fields[AOrder].DataType;
    cValue := ARow.Fields[AOrder].Value;
    if eType in [ftString, ftDate, ftTime, ftDateTime,
      ftFixedChar, ftWideString] then
    begin
      Result := QuotedStr(cValue)
    end
    else
    if eType in [ftBoolean] then
    begin
      if SameText(cValue, 'True') then
          Result := '1'
      else
          Result := '0';
    end
    else
        Result := cValue;
  end;

  function MakeWhere(const ARow: TBufDataSet): String;
  var
    cKeyFields: String;
    i: Integer;
  begin
    cKeyFields := AKeyFields + ',';
    Result := '';
    for i := 0 to ARow.FieldCount - 1 do
    begin
      cFldName := ARow.Fields[i].FieldName;
      if cFldName<>'DataState' then
      begin
        if (cKeyFields = ',') or (Pos(cFldName + ',', cKeyFields) > 0) then
        begin
          if Result <> '' then
              Result := Result + ' AND ';
          if ARow.Fields[i].IsNull then
              Result := Result + cFldName + ' IS NULL'
          else
              Result := Result + cFldName + ' = ' + SQLValue(ARow, i);
        end;
      end;
    end;
  end;
begin
  Result := '';
  if (FNewDataSet.RecordCount>0) then
  begin
    FNewDataSet.First;
    FOldDataSet.First;
    while not FOldDataSet.EOF do
    begin
      if FOldDataSet.FieldByName('DataState').AsString.ToUpper='INSERTED' then
      begin
        s1 := '';
        s2 := '';
        for nFldOrder := 0 to FNewDataSet.FieldCount - 1 do
        begin
          cFldName := FNewDataSet.Fields[nFldOrder].FieldName;
          if cFldName<>'DataState' then
          begin
            if not FNewDataSet.Fields[nFldOrder].IsNull then
            begin
              if s1 <> '' then
                  s1 := s1 + ',';
              if s2 <> '' then
                  s2 := s2 + ',';
              s1 := s1 + cFldName;
              s2 := s2 + SQLValue(FNewDataSet, nFldOrder);
            end;
          end;
        end;
        Result :=Result+ 'INSERT INTO ' + ATableName + ' (' + s1 + ')' +
          ' VALUES (' + s2 + ')'+#13+#10;
      end;
      if FOldDataSet.FieldByName('DataState').AsString.ToUpper='Updated'.ToUpper then
      begin
        s2 := '';
        for nFldOrder := 0 to FNewDataSet.FieldCount - 1 do
        begin
          cFldName := FNewDataSet.Fields[nFldOrder].FieldName;
          if cFldName<>'DataState' then
          begin
            if FOldDataSet.FieldByName(cFldName).AsVariant <> FNewDataSet.FieldByName(cFldName).AsVariant then
            begin
              if s2 <> '' then
                  s2 := s2 + ', ';
              if FNewDataSet.FieldByName(cFldName).IsNull then
                  s2 := s2 + cFldName + ' = NULL'
              else
                  s2 := s2 + cFldName + ' = ' + SQLValue(FNewDataSet, nFldOrder);
            end;
          end;
        end;
        Result :=Result+ 'UPDATE ' + ATableName + ' SET ' + s2 +
          ' WHERE ' + MakeWhere(FOldDataSet)+#13+#10;
      end;
      if FOldDataSet.FieldByName('DataState').AsString.ToUpper='Deleted'.ToUpper then
      begin
        Result :=Result+ 'DELETE FROM ' + ATableName + ' WHERE ' + MakeWhere(FNewDataSet)+#13+#10;
      end;
      FOldDataSet.Next;
      FNewDataSet.Next;
    end;

    FNewDataSet.Clear;
    FOldDataSet.Clear;
    FNewDataSet.Free;
    FOldDataSet.Free;
    CreateTable;
    Foldvalue:=nil;
    setlength(Foldvalue,self.Fields.Count);
 end;
end;

end.


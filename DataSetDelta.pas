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
  Classes, SysUtils, BufDataset, DB, TypInfo, Variants;

type

  TDataStateValue = (dsvOriginal, dsvDeleted, dsvInserted, dsvUpdated);

  TDataSetChangesHelper =class Helper for TDataset
  private
    procedure CreateTable;
    procedure BeforeInserts(DataSet: TDataSet);
    procedure BeforeEdits(DataSet: TDataSet);
    procedure BeforeDeletes(DataSet:TDataSet);
    procedure AfterPosts(DataSet: TDataSet);
    function GetChangedCount:int64;
  public
    function GetActionSQL(const ATableName : String; const AKeyFields: String = ''): String;
    procedure ActivateMonitoring(Value:Boolean);
    property ChangedCount:int64 read GetChangedCount;
  end;

implementation

var
  FDataState:string;
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
  LFieldName, LFieldType: string;
  LFieldSize : Integer;
begin
  if Assigned(FNewDataSet) then
    freeandnil(FNewDataSet);
  if Assigned(FOldDataSet) then
    freeandnil(FOldDataSet);

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
  FNewDataSet.FieldDefs.Add('DataSetName', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftString')), 30);
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
  FOldDataSet.FieldDefs.Add('DataSetName', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftString')), 30);
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
    Foldvalue[DataSet.Fields.Count]:=self.Name;
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
    Foldvalue[DataSet.Fields.Count]:=self.Name;
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
    FNewDataSet.FieldByName('DataSetName').AsString:=self.Name;
    FOldDataSet.FieldByName('DataSetName').AsString:=self.Name;
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
      FNewDataSet.FieldByName('DataSetName').AsString:=self.Name;
      FOldDataSet.FieldByName('DataSetName').AsString:=self.Name;
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
      setlength(Foldvalue,self.Fields.Count+1);
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
    if Assigned(FNewDataSet) then
      freeandnil(FNewDataSet);
    if Assigned(FOldDataSet) then
      freeandnil(FOldDataSet);
  end;
end;

function TDataSetChangesHelper.GetActionSQL(const ATableName
  : String; const AKeyFields: String = ''): String;
var
  nFldOrder: integer;
  cFldName, s1, s2: String;

  function SQLValue(const ADataSet: TBufDataSet; AOrder: Integer): String;
  var
    cName, cValue: String;
    eType: TFieldType;
  begin
    cName := ADataSet.Fields[AOrder].FieldName;
    eType := ADataSet.Fields[AOrder].DataType;
    cValue := ADataSet.Fields[AOrder].Value;
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

  function MakeWhere(const ADataSet: TBufDataSet): String;
  var
    cKeyFields: String;
    i: Integer;
  begin
    cKeyFields := AKeyFields + ',';
    Result := '';
    for i := 0 to ADataSet.FieldCount - 1 do
    begin
      cFldName := ADataSet.Fields[i].FieldName;
      if (cFldName<>'DataState') and (cFldName<>'DataSetName') then
      begin
        if (cKeyFields = ',') or (Pos(cFldName + ',', cKeyFields) > 0) then
        begin
          if Result <> '' then
              Result := Result + ' AND ';
          if ADataSet.Fields[i].IsNull then
              Result := Result + cFldName + ' IS NULL'
          else
              Result := Result + cFldName + ' = ' + SQLValue(ADataSet, i);
        end;
      end;
    end;
  end;
begin
  Result := '';
  if Assigned(FNewDataSet) then
  begin
    if (FNewDataSet.RecordCount>0) then
    begin
      FNewDataSet.First;
      FOldDataSet.First;
      while not FOldDataSet.EOF do
      begin
        if UpperCase(FOldDataSet.FieldByName('DataSetName').AsString)=UpperCase(self.name) then
        begin
          //INSERTED
          if FOldDataSet.FieldByName('DataState').AsString.ToUpper='INSERTED' then
          begin
            s1 := '';
            s2 := '';
            for nFldOrder := 0 to FNewDataSet.FieldCount - 1 do
            begin
              cFldName := FNewDataSet.Fields[nFldOrder].FieldName;
              if (cFldName<>'DataState') and (cFldName<>'DataSetName') then
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
              ' VALUES (' + s2 + ')'+LineEnding;
          end;
          //Updated
          if FOldDataSet.FieldByName('DataState').AsString.ToUpper='Updated'.ToUpper then
          begin
            s2 := '';
            for nFldOrder := 0 to FNewDataSet.FieldCount - 1 do
            begin
              cFldName := FNewDataSet.Fields[nFldOrder].FieldName;
              if (cFldName<>'DataState') and (cFldName<>'DataSetName') then
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
              ' WHERE ' + MakeWhere(FOldDataSet)+LineEnding;
          end;
          //Deleted
          if FOldDataSet.FieldByName('DataState').AsString.ToUpper='Deleted'.ToUpper then
          begin
            Result :=Result+ 'DELETE FROM ' + ATableName + ' WHERE ' + MakeWhere(FNewDataSet)+LineEnding;
          end;

        end;
        FOldDataSet.Next;
        FNewDataSet.Next;
      end;
      FOldDataSet.First;
      while not FOldDataSet.EOF do
      begin
        if UpperCase(FOldDataSet.FieldByName('DataSetName').AsString)=UpperCase(self.name) then FOldDataSet.Delete;
        FOldDataSet.Next;
      end;
      FNewDataSet.First;
      while not FNewDataSet.EOF do
      begin
        if UpperCase(FNewDataSet.FieldByName('DataSetName').AsString)=UpperCase(self.name) then FNewDataSet.Delete;
        FNewDataSet.Next;
      end;
      //freeandnil(FNewDataSet);
      //freeandnil(FOldDataSet);
      //CreateTable;
      Foldvalue:=nil;
      setlength(Foldvalue,self.Fields.Count+1);
    end;
  end;
end;

initialization

finalization
  FBeforeEdit:=nil;
  FBeforeDelete:=nil;
  FBeforeInsert:=nil;
  FAfterPost:=nil;
  Foldvalue:=nil;
  if Assigned(FNewDataSet) then
    freeandnil(FNewDataSet);
  if Assigned(FOldDataSet) then
    freeandnil(FOldDataSet);

end.


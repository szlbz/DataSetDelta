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
    function GetChangesDataSet : TBufDataSet;
    function GetDelta : TStream;
    function GetActionSQL(const ATableName : String; const AKeyFields: String = ''): String;
    function GetChangedCount:int64;
    procedure BeforeInserts(DataSet: TDataSet);
    procedure BeforeEdits(DataSet: TDataSet);
    procedure BeforeDeletes(DataSet:TDataSet);
    procedure AfterPosts(DataSet: TDataSet);
    procedure ActivateMonitoring(Value:Boolean);
    property Delta : TStream read GetDelta;
    property ChangesDataSet:TBufDataSet read GetChangesDataSet;
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
  FChangesDataSet:TBufDataSet;

function TDataSetChangesHelper.GetChangesDataSet : TBufDataSet;
begin
  Result:=FChangesDataSet;
end;

function TDataSetChangesHelper.GetChangedCount:int64;
begin
  Result:=FChangesDataSet.RecordCount;
end;

procedure TDataSetChangesHelper.BeforeInserts(DataSet: TDataSet);
var
  i:integer;
begin
  if Foldvalue<>nil then
  begin
    FDataState:='Inserted';//dsvInserted;
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
    FDataState:='Updated';//dsvUpdated;
    for i:=0 to DataSet.Fields.Count-1 do
    Foldvalue[i]:=DataSet.Fields[i].NewValue;
  end;
end;

procedure TDataSetChangesHelper.BeforeDeletes(DataSet: TDataSet);
var
  i:integer;
begin
  FDataState:='Deleted';//dsvDeleted;
  if Foldvalue<>nil then
  begin
    for i:=0 to DataSet.Fields.Count-1 do
    begin
      FChangesDataSet.Append;
      FChangesDataSet.FieldByName('FieldName').AsString:=dataset.Fields[i].FieldName;
      FChangesDataSet.FieldByName('FieldType').AsInteger:=ord(dataset.Fields[i].DataType);
      FChangesDataSet.FieldByName('NewValues').AsVariant:=dataset.Fields[i].CurValue;
      FChangesDataSet.FieldByName('OldValues').AsVariant:=null;
      FChangesDataSet.FieldByName('DataState').AsString:=FDataState;//ord(FDataState);
      FChangesDataSet.Post;
    end;
  end;
end;

procedure TDataSetChangesHelper.AfterPosts(DataSet: TDataSet);
var
  i:integer;
  s:string;
begin
  if Foldvalue<>nil then
  begin
    for i:=0 to DataSet.Fields.Count-1 do
    begin
      if dataset.Fields[i].newValue<>Foldvalue[i] then
      begin
        FChangesDataSet.Append;
        FChangesDataSet.FieldByName('FieldName').AsString:=dataset.Fields[i].FieldName;
        FChangesDataSet.FieldByName('FieldType').AsInteger:=ord(dataset.Fields[i].DataType);
        //FChangesDataSet.FieldByName('FieldSize').AsInteger:=dataset.Fields[i].fi;
        FChangesDataSet.FieldByName('NewValues').AsVariant:=dataset.Fields[i].CurValue;
        FChangesDataSet.FieldByName('OldValues').AsVariant:=Foldvalue[i];
        FChangesDataSet.FieldByName('DataState').AsString:=FDataState;//ord(FDataState);
        FChangesDataSet.Post;
      end;
    end;
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

      FChangesDataSet:=TBufDataSet.Create(nil);
      // 设置字段定义
      FieldDef := TFieldDefs.Create(nil);
      try
        FieldDef.Add('FieldName', ftString, 50);
        FieldDef.Add('FieldType', ftInteger);
        FieldDef.Add('FieldSize', ftInteger);
        FieldDef.Add('NewValues', ftVariant);
        FieldDef.Add('OldValues', ftVariant);
        FieldDef.Add('DataState', ftString,10);
        FChangesDataSet.FieldDefs := FieldDef;

        // 创建数据集
        FChangesDataSet.CreateDataSet;
      finally
        FieldDef.Free;
      end;

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
    FChangesDataSet.Free;
  end;
end;

function TDataSetChangesHelper.GetDelta: TStream;
var
  I, J, K : Integer;
  LFieldName, LFieldType, LFieldValue: string;
  LFieldSize : Integer;
  mDataSet:TBufDataSet;
begin
  Result := TMemoryStream.Create;
  mDataSet:= TBufDataSet.Create(nil);
  try
    //定义字段
    for I := 0 to Self.FieldCount - 1 do
    begin
      LFieldName := Self.Fields[I].FieldName;
      LFieldType := GetEnumName(TypeInfo(TFieldType), Integer(Self.Fields[I].DataType));
      LFieldSize := Self.Fields[I].DataSize;
      if (LFieldType = 'ftString') or (LFieldType = 'ftBCD') then
        mDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)))
      else
        mDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)), LFieldSize);
    end;
    mDataSet.CreateDataset;
    mDataSet.Open;

    //复制数据
    Self.First;
    for I := 0 to Self.RecordCount - 1 do
    begin
      for J := 0 to Self.FieldCount - 1 do
      begin
        if Self.Fields[J].NewValue <> Self.Fields[J].CurValue or
           Self.Fields[J].NewValue <> Self.Fields[J].OldValue or
           Self.Fields[J].CurValue <> Self.Fields[J].OldValue then
        begin
          mDataSet.Append;
          for K := 0 to Self.FieldCount - 1 do
          begin
            mDataSet.FieldValues[Self.Fields[K].FieldName].NewValue := Self.Fields[K].NewValue;
            mDataSet.FieldValues[Self.Fields[K].FieldName].CurValue := Self.Fields[K].CurValue;
            mDataSet.FieldValues[Self.Fields[K].FieldName].OldValue := Self.Fields[K].OldValue;
          end;
          Break;
        end;
      end;
      Self.Next;
    end;
    mDataSet.SaveToStream(Result);
  finally
    FreeAndNil(mDataSet);
  end;
end;

function TDataSetChangesHelper.GetActionSQL(const ATableName
  : String; const AKeyFields: String = ''): String;
var
  nFldOrder: integer;
  cFldName, s1, s2: String;
  nrow, orow: TBufDataSet;

  function SQLValue(const ARow: TBufDataSet; AOrder: Integer): String;
  var
    cName, cValue: String;
    eType: TFieldType;
  begin
    cName := ARow.Fields[AOrder].FieldName;
    eType := ARow.Fields[AOrder].DataType;
    cValue := ARow.Fields[AOrder].Value;
    if eType in [ftString, ftDate, ftTime, ftDateTime,
      ftFixedChar, ftWideString, ftOraTimeStamp] then
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
begin
  Result := '';
{
  case self.Action of
    Insert:
      begin
        s1 := '';
        s2 := '';
        nrow := self.NewRow;
        for nFldOrder := 0 to nrow.FieldCount - 1 do
        begin
          cFldName := nrow.Fields[nFldOrder].FieldName;
          if not nrow.Fields[i].IsNull then
          begin
            if s1 <> '' then
                s1 := s1 + ',';
            if s2 <> '' then
                s2 := s2 + ',';
            s1 := s1 + cFldName;
            s2 := s2 + SQLValue(nrow, nFldOrder);
          end;
        end;
        Result := 'INSERT INTO ' + ATableName + ' (' + s1 + ')' +
          ' VALUES (' + s2 + ')';
      end;
    Updated:
      begin
        s2 := '';
        nrow := self.NewRow;
        orow := self.OldRow;
        for nFldOrder := 0 to nrow.FieldCount - 1 do
        begin
          cFldName := nrow.Fields[nFldOrder].FieldName;
          if orow.asCode[cFldName] <> nrow.asCode[cFldName] then
          begin
            if s2 <> '' then
                s2 := s2 + ', ';
            if nrow.isNull[cFldName] then
                s2 := s2 + cFldName + ' = NULL'
            else
                s2 := s2 + cFldName + ' = ' + SQLValue(nrow, nFldOrder);
          end;
        end;
        Result := 'UPDATE ' + ATableName + ' SET ' + s2 +
          ' WHERE ' + MakeWhere(orow);
      end;
    Delete:
      begin
        orow := self.OldRow;
        Result := 'DELETE FROM ' + ATableName + ' WHERE ' + MakeWhere(orow);
      end;
  end;
}
end;

end.


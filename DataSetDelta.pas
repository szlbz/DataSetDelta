{*******************************************************}
{                                                       }
{ 为lazarus TDataSet增加类似TClientDataSet的Delta功能   }
{       根据TDataSet的变化直接生成对应的SQL             }
{                 适用于所有TDataSet                    }
{                                                       }
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
  TDataStateValues=set of TDataStateValue;

  TQFDataSetMonitor =class(TComponent)
  private
    FDataState:TDataStateValue;
    Foldvalue:array of Variant;
    FBeforeEdit: TDataSetNotifyEvent;
    FBeforeDelete: TDataSetNotifyEvent;
    FBeforeInsert: TDataSetNotifyEvent;
    FAfterPost: TDataSetNotifyEvent;
    FAfterOpens: TDataSetNotifyEvent;
    FNewDataSet:TBufDataSet;
    FOldDataSet:TBufDataSet;
    FDataSet:TDataSet;
    FActive:Boolean;
    procedure CreateMonitorDataSet;
    procedure SetDataSet(AValue: TDataSet);
    procedure SetActive(AValue: Boolean);
    procedure BeforeInserts(DataSet: TDataSet);
    procedure BeforeEdits(DataSet: TDataSet);
    procedure BeforeDeletes(DataSet:TDataSet);
    procedure AfterPosts(DataSet: TDataSet);
    procedure AfterOpens(DataSet: TDataSet);
    function GetChanged:Boolean;
    procedure ActivateMonitoring(AValue : Boolean = True);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetActionSQL(const ATableName : String; const AKeyFields: String = ''): String;
    property Changed:Boolean read GetChanged;
    property Active:Boolean read FActive write SetActive;
 published
   property DataSet:TDataSet read FDataSet write SetDataSet;
 end;


procedure Register;

implementation

{$R *.res}

procedure Register;
begin
  RegisterComponents('Data Access', [TQFDataSetMonitor]);
end;

constructor TQFDataSetMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := False;
end;

destructor TQFDataSetMonitor.Destroy;
begin
  inherited Destroy;

  FBeforeEdit:=nil;
  FBeforeDelete:=nil;
  FBeforeInsert:=nil;
  FAfterPost:=nil;
  Foldvalue:=nil;
  if Assigned(FNewDataSet) then
    FreeAndNil(FNewDataSet);
  if Assigned(FOldDataSet) then
    FreeAndNil(FOldDataSet);
end;

function TQFDataSetMonitor.GetChanged:Boolean;
begin
  if Assigned(FOldDataSet) then
    Result:=FOldDataSet.RecordCount>0
  else
    Result:=false;
end;

procedure TQFDataSetMonitor.SetActive(AValue: Boolean);
begin
  if (AValue <> FActive) then
    FActive:=AValue;
  ActivateMonitoring(FActive);
end;

procedure TQFDataSetMonitor.SetDataSet(AValue: TDataSet);
begin
  if (AValue <> FDataSet) then
  begin
    FDataSet:=AValue;
  end;
end;

procedure TQFDataSetMonitor.CreateMonitorDataSet;
var
  i:integer;
  LFieldName, LFieldType: string;
  LFieldSize : Integer;
begin
  if FDataSet.Fields.Count>0 then
  begin
  if Foldvalue<>nil then Foldvalue:=nil;
  try
    setlength(Foldvalue,FDataSet.Fields.Count);
  except
    exit;
  end;

  if Assigned(FNewDataSet) then
    FreeAndNil(FNewDataSet);
  if Assigned(FOldDataSet) then
    FreeAndNil(FOldDataSet);
  try
    FNewDataSet:=TBufDataSet.Create(nil);
  finally
    for I := 0 to FDataSet.FieldCount - 1 do
    begin
      LFieldName := FDataSet.Fields[I].FieldName;
      LFieldType := GetEnumName(TypeInfo(TFieldType), Integer(FDataSet.Fields[I].DataType));
      LFieldSize := FDataSet.Fields[I].DataSize;
      if (LFieldType = 'ftString') then
        FNewDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)), LFieldSize)
      else
        FNewDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)));
    end;
    FNewDataSet.FieldDefs.Add('DataState', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftinteger')));
    FNewDataSet.CreateDataset;
  end;
  try
    FOldDataSet:=TBufDataSet.Create(nil);
  finally
    for I := 0 to FDataSet.FieldCount - 1 do
    begin
      LFieldName := FDataSet.Fields[I].FieldName;
      LFieldType := GetEnumName(TypeInfo(TFieldType), Integer(FDataSet.Fields[I].DataType));
      LFieldSize := FDataSet.Fields[I].DataSize;
      if (LFieldType = 'ftString')  then
        FOldDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)), LFieldSize)
      else
        FOldDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)));
    end;
    FOldDataSet.FieldDefs.Add('DataState', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftinteger')));
    FOldDataSet.CreateDataset;
  end;
  end;

end;

procedure TQFDataSetMonitor.BeforeInserts(DataSet: TDataSet);
var
  i:integer;
begin
  if Foldvalue<>nil then
  begin
    FDataState:=dsvInserted;
    for i:=0 to DataSet.Fields.Count-1 do
      Foldvalue[i]:=null;
  end;
end;

procedure TQFDataSetMonitor.BeforeEdits(DataSet: TDataSet);
var
  i:integer;
begin
  if Foldvalue<>nil then
  begin
    FDataState:=dsvUpdated;
    for i:=0 to DataSet.Fields.Count-1 do
      Foldvalue[i]:=DataSet.Fields[i].NewValue;
  end;
end;

procedure TQFDataSetMonitor.BeforeDeletes(DataSet: TDataSet);
var
  i:integer;
begin
  FDataState:=dsvDeleted;
  if Foldvalue<>nil then
  begin
    if Assigned(FNewDataSet) and Assigned(FOldDataSet) then
    begin
      FNewDataSet.Append;
      FOldDataSet.Append;
      for i:=0 to DataSet.Fields.Count-1 do
      begin
        FNewDataSet.Fields[i].Value := DataSet.Fields[i].NewValue;
        FOldDataSet.Fields[i].Value := null;
      end;
      FOldDataSet.FieldByName('DataState').Asinteger:=ord(FDataState);
      FOldDataSet.Post;
      FNewDataSet.Post;
    end;
  end;
end;

procedure TQFDataSetMonitor.AfterPosts(DataSet: TDataSet);
var
  i:integer;
  s:string;
begin
  if Foldvalue<>nil then
  begin
    if Assigned(FNewDataSet) and Assigned(FOldDataSet) then
    begin
      FNewDataSet.Append;
      FOldDataSet.Append;
      for i:=0 to DataSet.Fields.Count-1 do
      begin
        FNewDataSet.Fields[i].Value := DataSet.Fields[i].NewValue;
        FOldDataSet.Fields[i].Value := Foldvalue[i];
        FOldDataSet.FieldByName('DataState').Asinteger:=ord(FDataState);
      end;
      FNewDataSet.Post;
      FOldDataSet.Post;
    end;
  end;
end;

procedure TQFDataSetMonitor.AfterOpens(DataSet: TDataSet);
begin
  CreateMonitorDataSet;
end;

procedure TQFDataSetMonitor.ActivateMonitoring(AValue : Boolean = True);
begin
  if AValue then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if Assigned(FDataSet) then
      begin
        FBeforeEdit:=FDataSet.BeforeEdit;
        FBeforeDelete:=FDataSet.BeforeDelete;
        FBeforeInsert:=FDataSet.BeforeInsert;
        FAfterPost:=FDataSet.AfterPost;
        FDataSet.BeforeEdit:=@BeforeEdits;
        FDataSet.BeforeDelete:=@BeforeDeletes;
        FDataSet.BeforeInsert:=@BeforeInserts;
        FDataSet.AfterPost:=@AfterPosts;
        FDataSet.AfterOpen:=@AfterOpens;
        CreateMonitorDataSet;
      end;
    end;
  end
  else
  begin
    FBeforeEdit:=nil;
    FBeforeDelete:=nil;
    FBeforeInsert:=nil;
    FAfterPost:=nil;
    Foldvalue:=nil;
    if Assigned(FNewDataSet) then
      FreeAndNil(FNewDataSet);
    if Assigned(FOldDataSet) then
      FreeAndNil(FOldDataSet);
  end;
end;

function TQFDataSetMonitor.GetActionSQL(const ATableName
  : String; const AKeyFields: String = ''): String;
var
  nFldOrder: integer;
  cFldName, s1, s2: String;

  function SQLValue(const ADataSet: TBufDataSet; AFieldIndex: Integer): String;
  var
    cValue: String;
    eType: TFieldType;
  begin
    eType := ADataSet.Fields[AFieldIndex].DataType;
    cValue := ADataSet.Fields[AFieldIndex].Value;
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
      if (cFldName<>'DataState') then
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
  if Assigned(FNewDataSet) and (Assigned(FOldDataSet)) then
  begin
    if (FNewDataSet.RecordCount>0) then
    begin
      FNewDataSet.First;
      FOldDataSet.First;
      while not FOldDataSet.EOF do
      begin
        //INSERTED
        if FOldDataSet.FieldByName('DataState').Asinteger =ord(dsvINSERTED) then
        begin
          s1 := '';
          s2 := '';
          for nFldOrder := 0 to FNewDataSet.FieldCount - 1 do
          begin
            cFldName := FNewDataSet.Fields[nFldOrder].FieldName;
            if (cFldName<>'DataState')  then
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
        if FOldDataSet.FieldByName('DataState').Asinteger=ord(dsvUpdated) then
        begin
          s2 := '';
          for nFldOrder := 0 to FNewDataSet.FieldCount - 1 do
          begin
            cFldName := FNewDataSet.Fields[nFldOrder].FieldName;
            if (cFldName<>'DataState') then
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
        if FOldDataSet.FieldByName('DataState').Asinteger=ord(dsvDeleted) then
        begin
          Result :=Result+ 'DELETE FROM ' + ATableName + ' WHERE ' + MakeWhere(FNewDataSet)+LineEnding;
        end;
        FOldDataSet.Next;
        FNewDataSet.Next;
      end;
      CreateMonitorDataSet;//生成后清空 FOldDataSet 、FNewDataSet，并重新生成FOldDataSet 、FNewDataSet
    end;
  end;
end;

initialization

finalization

end.


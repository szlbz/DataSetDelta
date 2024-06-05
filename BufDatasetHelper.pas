unit BufDatasetHelper;

interface

uses Classes,Sysutils,db,bufdataset,bufdataset_parser;

type

  TBufDataSetHelper =class Helper for TCustomBufDataset
  public
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean;
    procedure SetFieldData(Field: TField; Buffer: Pointer);
  end;

implementation

function TBufDataSetHelper.GetFieldData(Field: TField; Buffer: Pointer): Boolean;

var
  CurrBuff : TRecordBuffer;
  vData    : variant;
begin
  Result := False;
  if State = dsOldValue then
  begin
    if FSavedState = dsInsert then
      CurrBuff := nil // old values = null
    else if GetActiveRecordUpdateBuffer then
      CurrBuff := FUpdateBuffer[FCurrentUpdateBuffer].OldValuesBuffer
    else
      // There is no UpdateBuffer for ActiveRecord, so there are no explicit old values available
      // then we can assume, that old values = current values
      CurrBuff := CurrentIndexBuf.CurrentBuffer;
  end
  else
    CurrBuff := GetCurrentBuffer;

  if not assigned(CurrBuff) then Exit; //Null value

  If Field.FieldNo > 0 then // If =-1, then calculated/lookup field or =0 unbound field
    begin
    if GetFieldIsNull(pbyte(CurrBuff),Field.FieldNo-1) then
      Exit;
    if assigned(Buffer) then
      begin
      inc(CurrBuff,FFieldBufPositions[Field.FieldNo-1]);
      if Field.IsBlob then // we need GetFieldSize for BLOB but Field.DataSize for others - #36747
        Move(CurrBuff^, Buffer^, GetFieldSize(FieldDefs[Field.FieldNo-1]))
      else
      if Field.DataType =ftVariant then
      begin
        vData:=PVariant(CurrBuff)^;
        PVariant(Buffer)^ := vData;
      end
      else
        Move(CurrBuff^, Buffer^, Field.DataSize);
      end;
    Result := True;
    end
  else
    begin
    Inc(CurrBuff, GetRecordSize + Field.Offset);
    Result := Boolean(CurrBuff^);
    if Result and assigned(Buffer) then
      begin
      inc(CurrBuff);
      Move(CurrBuff^, Buffer^, Field.DataSize);
      end;
    end;
end;

procedure TBufDataSetHelper.SetFieldData(Field: TField; Buffer: Pointer);

var CurrBuff : pointer;
    NullMask : pbyte;
    vData    : variant;
begin
  if not (State in dsWriteModes) then
    DatabaseErrorFmt(SNotEditing, [Name], Self);
  CurrBuff := GetCurrentBuffer;
  If Field.FieldNo > 0 then // If =-1, then calculated/lookup field or =0 unbound field
    begin
    if Field.ReadOnly and not (State in [dsSetKey, dsFilter, dsRefreshFields]) then
      DatabaseErrorFmt(SReadOnlyField, [Field.DisplayName]);	
    if State in [dsEdit, dsInsert, dsNewValue] then
      Field.Validate(Buffer);	
    NullMask := CurrBuff;

    inc(CurrBuff,FFieldBufPositions[Field.FieldNo-1]);
    if assigned(buffer) then
    begin
      if Field.IsBlob then // we need GetFieldSize for BLOB but Field.DataSize for others - #36747
        Move(Buffer^, CurrBuff^, GetFieldSize(FieldDefs[Field.FieldNo-1]))
      else
      if Field.DataType =ftVariant then
      begin
        vData:=PVariant(Buffer)^;
        PVariant(CurrBuff)^ := vData;
      end
      else
         Move(Buffer^, CurrBuff^, Field.DataSize);
      unSetFieldIsNull(NullMask,Field.FieldNo-1);
    end
    else
      SetFieldIsNull(NullMask,Field.FieldNo-1);
    end
  else
    begin
    Inc(CurrBuff, GetRecordSize + Field.Offset);
    Boolean(CurrBuff^) := Buffer <> nil;
    inc(CurrBuff);
    if assigned(Buffer) then
    begin
      if Field.DataType =ftVariant then
      begin
        vData:=PVariant(Buffer)^;
        PVariant(CurrBuff)^ := vData;
      end
      else
        Move(Buffer^, CurrBuff^, Field.DataSize);
    end;
    end;
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, PtrInt(Field));
end;

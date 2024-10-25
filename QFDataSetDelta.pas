{*******************************************************}
{                                                       }
{ 为lazarus TDataSet增加类似TClientDataSet的Delta功能   }
{       根据TDataSet的变化直接生成对应的SQL             }
{                 适用于所有TDataSet                    }
{                                                       }
{                                                       }
{               Copyright(c) 2024-2024                  }
{              秋风(QQ315795176)原创出品                }
{              高勇(QQ120180714)修改完善                }
{                                                       }
{                 All rights reserved                   }
{                     保留所有权利                      }
{                                                       }
{ 感谢高勇高老板增强GetActionSQL功能                    }
{ 感谢ccc(QQ1650680975)为delphi unidac 增加类似         }
{ TClientDataSet的Delta功能                             }
{ 可按此方法修改内存表为friedac                         }
{ https://gitee.com/cityboat888/DataSetDelta.git        }
{*******************************************************}

unit QFDataSetDelta;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ELSE}
{$ENDIF}

//{$mode objfpc}{$H+}
//{$I zLib.inc} //CrossSocket样式，推荐使用
//在文件前面加上述的编译开关，基本与delphi一致的习惯。

interface

uses
  Classes, SysUtils, DB, TypInfo, Variants,base64,
  fpjson,jsonparser,jsonConf,jsonScanner,//fpc自己的json工具
  {$IFDEF FPC}
    BufDataset
  {$ELSE}
    VirtualTable
  {$ENDIF}
  ;

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
    FAfterOpen: TDataSetNotifyEvent;

    FNewDataSet:{$IFDEF FPC} TBufDataSet{$ELSE}TVirtualTable{$ENDIF};
    FOldDataSet:{$IFDEF FPC} TBufDataSet{$ELSE}TVirtualTable{$ENDIF};
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
    function GetActionSQL(const ATableName : String; const AKeyFields: String = ''): String; overload;
    function GetActionSQL(const DataBaseType:string;aTablename: string; AKeyFields: string; aAutoIncFieldname: string = ''; aNotEditFields: string = ''; aReturnStrType: integer = 1): String; overload;
    property Changed:Boolean read GetChanged;
 published
   property Active:Boolean read FActive write SetActive;
   property DataSet:TDataSet read FDataSet write SetDataSet;
 end;

{$IFDEF FPC}

{$ELSE}
 const
   LineEnding = #13#10;
{$ENDIF}


procedure Register;

implementation

//{$R *.res}

function BitStrToOStr(const BitStr: String): String;
var
  vD: Byte;
  i: Integer;
  vHextStr: String;
  vP: PChar;
  vLen: Integer;
begin
  vLen := Length(BitStr);
  if vLen mod 3 > 0 then
  begin
    SetLength(vHextStr, vLen div 3 + 1);
    vLen := vLen div 3 + 1;
  end
  else
  begin
    SetLength(vHextStr, vLen div 3);
    vLen := vLen div 3;
  end;
  // 初始化
  vD := 0;
  vP := PChar(BitStr) + Length(BitStr) - 1;
  i := 0; // 开始计数
  while vP^ <> #0 do
  begin
    if vP^ = '1' then
    begin
      case i of
        0:
          vD := vD + 1;
        1:
          vD := vD + 2;
        2:
          vD := vD + 4;
      end;
    end;
    Dec(vP);
    Inc(i);
    if i = 3 then
    begin
      case vD of
        0 .. 9:
          vHextStr[vLen] := Chr(vD + $30);
      end;
      Dec(vLen);
      i := 0;
      vD := 0;
    end;
  end;
  if i > 0 then
  begin
    case vD of
      0 .. 9:
        vHextStr[vLen] := Chr(vD + $30);
    end;
  end;
  Result := vHextStr;
end;

function HexToBitStr(HexStr: string): string;
const
  cBitStrings: array [0 .. 15] of string =
    (
    '0000', '0001', '0010', '0011',
    '0100', '0101', '0110', '0111',
    '1000', '1001', '1010', '1011',
    '1100', '1101', '1110', '1111'
    );
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(HexStr) do
    Result := Result + cBitStrings[StrToIntDef('$' + HexStr[i], 0)];
  while Pos('0', Result) = 1 do
    Delete(Result, 1, 1);
end; { HexToBit }

function HextoIntStr(HexStr: String): string;
begin
  Result := IntToStr(StrToInt('$' + (HexStr)));
end;

function HexToOStr(HexStr: string): string;
begin
  Result := BitStrToOStr(HexToBitStr(HexStr));
end;

procedure BinToHex(ABuffer: Pointer; ABufSize: Integer;
  AText: PChar);
const
  XD: array[0..15] of char = ('0', '1', '2', '3', '4', '5', '6', '7',
                              '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  I: Integer;
  PBuffer: PByte;
  PText: PChar;
begin
  PBuffer := ABuffer;
  PText := AText;
  for I := 0 to ABufSize - 1 do
  begin
    PText[0] := XD[(PBuffer[I] shr 4) and $0f];
    PText[1] := XD[PBuffer[I] and $0f];
    Inc(PText, 2);
  end;
end;
function BinToHex(ABuffer: Pointer; ABufSize: Integer): string;
begin
  SetLength(Result, ABufSize * 2);
  BinToHex(ABuffer, ABufSize, PChar(Result));
end;
function BytesToHex(const ABytes: TBytes; AOffset,
  ACount: Integer): string;
begin
  Result := BinToHex(@ABytes[AOffset], ACount);
end;
function BytesToHex(const ABytes: TBytes): string;
begin
  Result := BytesToHex(ABytes, 0, Length(ABytes));  IntToStr(1);
end;

function HexstringToOStringForProgresql(str: string): string;
var
  s, t,tempstr: string;
  i, j: integer;
  p: pchar;
begin
  s := '';
  i := 1;
  while i < length(str) do begin
    t := str[i] + str[i + 1];
    tempstr:=HexToOStr(t);
    case length(tempstr) of
      0:tempstr:='000';
      1:tempstr:='00'+tempstr;
      2:tempstr:='0'+tempstr;
    end;

    s :=s +'\\' +tempstr ;
    i := i + 2;
  end;
  result := s;
end;

function StreamToBase64(const Outstream:TStream):String;
var
  Encoder   : TBase64EncodingStream;
  sm: TStringStream;
  Buffer: Pointer;
  BufferSize, i,Count: LongInt;
begin
  sm:=TStringStream.Create('');
  Outstream.Position:=0;
  Encoder:=TBase64EncodingStream.create(sm);
  Encoder.CopyFrom(Outstream,Outstream.Size);
  Result:=sm.DataString;
  Encoder.Free;
  sm.Free;
end;

procedure Register;
begin
  RegisterComponents('QF QFDataSetDelta', [TQFDataSetMonitor]);
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
    if FActive then
    begin
      FBeforeEdit:=FDataSet.BeforeEdit;
      FBeforeDelete:=FDataSet.BeforeDelete;
      FBeforeInsert:=FDataSet.BeforeInsert;
      FAfterPost:=FDataSet.AfterPost;
      FAfterOpen:=FDataSet.AfterOpen;
      FDataSet.BeforeEdit:={$IFDEF FPC}@{$endif}BeforeEdits;
      FDataSet.BeforeDelete:={$IFDEF FPC}@{$endif}BeforeDeletes;
      FDataSet.BeforeInsert:={$IFDEF FPC}@{$endif}BeforeInserts;
      FDataSet.AfterPost:={$IFDEF FPC}@{$endif}AfterPosts;
      FDataSet.AfterOpen:={$IFDEF FPC}@{$endif}AfterOpens;
      //FDataSet.BeforeEdit:=BeforeEdits;
      //FDataSet.BeforeDelete:=BeforeDeletes;
      //FDataSet.BeforeInsert:=BeforeInserts;
      //FDataSet.AfterPost:=AfterPosts;
      //FDataSet.AfterOpen:=AfterOpens;
    end
    else
    begin
      FBeforeEdit:=nil;
      FBeforeDelete:=nil;
      FBeforeInsert:=nil;
      FAfterPost:=nil;
      FAfterOpen:=nil;
    end;
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
      {$IFDEF FPC}
      FNewDataSet:=TBufDataSet.Create(nil);
      {$ELSE}
      FNewDataSet:=TVirtualTable.Create(nil);
      {$ENDIF}
    finally
      for I := 0 to FDataSet.FieldCount - 1 do
      begin
        LFieldName := FDataSet.Fields[I].FieldName;
        LFieldType := GetEnumName(TypeInfo(TFieldType), Integer(FDataSet.Fields[I].DataType));
        LFieldSize := FDataSet.Fields[I].DataSize;
        //try
        //  FNewDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)), LFieldSize)
        //except
        //  FNewDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)));
        //end;
        if (LFieldType = 'ftString') or (LFieldType = 'ftWideString') or (LFieldType = 'ftMemo') or (LFieldType = 'ftFmtMemo') then
          FNewDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)), LFieldSize)
        else
          FNewDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)));
      end;
      FNewDataSet.FieldDefs.Add('DataState', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftinteger')));
      {$IFDEF FPC}
      FNewDataSet.CreateDataset;
      {$ELSE}
      FNewDataSet.active:=true;
      {$ENDIF}
    end;
    try
      {$IFDEF FPC}
      FOldDataSet:=TBufDataSet.Create(nil);
      {$ELSE}
      FOldDataSet:=TVirtualTable.Create(nil);
      {$ENDIF}
    finally
      for I := 0 to FDataSet.FieldCount - 1 do
      begin
        LFieldName := FDataSet.Fields[I].FieldName;
        LFieldType := GetEnumName(TypeInfo(TFieldType), Integer(FDataSet.Fields[I].DataType));
        LFieldSize := FDataSet.Fields[I].DataSize;
        //try
        //  FOldDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)), LFieldSize)
        //except
        //  FOldDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)));
        //end;
        if (LFieldType = 'ftString') or (LFieldType = 'ftWideString') or (LFieldType = 'ftMemo') or (LFieldType = 'ftFmtMemo')   then
          FOldDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)), LFieldSize)
        else
          FOldDataSet.FieldDefs.Add(LFieldName, TFieldType(GetEnumValue(TypeInfo(TFieldType), LFieldType)));
      end;
      FOldDataSet.FieldDefs.Add('DataState', TFieldType(GetEnumValue(TypeInfo(TFieldType), 'ftinteger')));
      {$IFDEF FPC}
      FOldDataSet.CreateDataset;
      {$ELSE}
      FOldDataSet.active:=true;
      {$ENDIF}
    end;
  end;

end;

function TQFDataSetMonitor.GetActionSQL(const DataBaseType:string;aTablename: string; AKeyFields: string; aAutoIncFieldname: string = ''; aNotEditFields: string = ''; aReturnStrType: integer = 1): String;
var
  nFldOrder: integer;
  cFldName, s1, s2,AllSQLStr,SQLStr,LineEnding,YinHao1,YinHao2:String;
  JA: TJSONArray;
  ItemKey, ItemNoField: Tstringlist;

  function GetASQLValue(aDataType: TFieldType; AVariant: Variant;DataBaseType:string='mysql'): string;
  var
    sTmp,aVarType: string; aBytesStream: TBytesStream; aExtended:Extended;
  begin
    //TFieldType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
    //ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate,  ftTime, ftDateTime,
    //ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    //ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
    //ftWideString, ftLargeint, ftADT, ftArray, ftReference,
    //ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface,
    //ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, ftFixedWideChar, ftWideMemo);
    // 判断字段的数据类型

    try
      if (AVariant)=Null then
      begin
       result:='Null';
       exit;
      end;
      case aDataType of
        ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency, ftAutoInc, ftBCD, ftLargeint:
          begin
           sTmp := (AVariant);
           if sTmp='' then
            sTmp := '0';
           if trystrtofloat(sTmp,aExtended)=false then   //2021-06-12 防止注入的处理 QuotedStr()
           begin
             raise Exception.CreateFmt('%s %s', ['您输入了非法数字:', sTmp]);
             exit;
           end;
          end;
        ftDate:
          begin
            sTmp := (AVariant);
            // 日期如果没有填写默认为Null
            if sTmp = '' then
              sTmp := 'Null'
            else
              begin
                sTmp:=sTmp.Replace('/','-',[rfReplaceAll]);//2022-07-14 ZCH  TMemTableEh的内存表的数据日期时间字段没有改？我先自已处理成横线格式。
                sTmp := QuotedStr(FormatDateTime('yyyy-mm-dd', StrToDate(sTmp)));    //2021-06-12 防止注入的处理 QuotedStr()
                //sTmp := '''' + FormatDateTime('yyyy-mm-dd', StrToDate(sTmp)) + '''';
              end;
          end;
        ftDateTime:
          begin
            sTmp := (AVariant);
            // 日期如果没有填写默认为Null
            if sTmp = '' then
              sTmp := 'Null'
            else
              begin
                sTmp:=sTmp.Replace('/','-',[rfReplaceAll]);//2022-07-14 ZCH  TMemTableEh的内存表的数据日期时间字段没有改？我先自已处理成横线格式。
                sTmp := QuotedStr(FormatDateTime('yyyy-mm-dd hh:nn:ss', StrToDateTime(sTmp)));
                //sTmp := '''' + FormatDateTime('yyyy-mm-dd hh:nn:ss', StrToDateTime(sTmp)) + '''';
              end;
          end;
        ftBoolean:
          begin
            try
              if AVariant = True then
                sTmp := '1'
              else
                sTmp := '0';
            except
              sTmp := '0';
            end;
          end;
        ftWideString: //2021-06-20  sqlserver的nvarchar,nchar等n开头的字段，保存的是unicode字符串，sql提交时，字符串值前面要加N，如: N'字符串内容'
          begin
            if (DataBaseType.ToLower='sqlserver') then
             sTmp := 'N'+QuotedStr((AVariant))
            else
             sTmp := QuotedStr((AVariant));
          end;
        ftBlob, ftGraphic, ftTypedBinary:
          begin
              {
              三、string 转为Tbytes

              1、bytes:= bytesof(str) 已转为ansi编码
              2、bytes:= widebytesof(str) UNICODE 编码

              四、ansistring 转为Tbytes

              1、bytes:= bytesof(str) ansi编码
              2、bytes:= widebytesof(string(str)) UNICODE 编码
              }

            try  //如果AVariant为''字符串
               try
        {$IFDEF NEXTGEN}
                sTmp := BytesToHex(bytesof(string(AVariant))); //mysql用法  //AVariant为stirng
        {$ELSE}
                sTmp := BytesToHex(bytesof(ansistring(AVariant)));//mysql用法  //在windows下，AVariant为ansistirng
        {$ENDIF}
               except  //如果为tbytes，如MemTableEh
                sTmp := BytesToHex(AVariant);//ehlib保存的2进制格式不一样。
               end;

              if DataBaseType.ToLower='mysql' then
               sTmp := '0x'+sTmp//mysql用法
              else if (DataBaseType.ToLower='sqlserver') or (DataBaseType.ToLower='sql server') then
               sTmp := '0x'+sTmp//sql server用法
              else if DataBaseType.ToLower='oracle' then
               sTmp := 'rawtohex('''+sTmp+''')'//Oracle用法  2021-04-20没有测试，因为没有这个数据库
              else if DataBaseType.ToLower='sqlite' then
               sTmp := 'X'''+sTmp+''''//sqlite用法  2021-04-20没有测试，因为没有这个数据库
              else if DataBaseType.ToLower='access' then
               sTmp := '0x'+sTmp//access用法
              else if DataBaseType.ToLower='postgresql' then
               sTmp := 'E'''+ HexstringToOStringForProgresql(sTmp)+''''//postgresql用法
              else
               sTmp := '0x'+sTmp;

              if (sTmp='X''''') or(sTmp='0x') or (sTmp='rawtohex('''')') then
               sTmp :='Null';

            except
               sTmp :='Null';
            end;

          end
      else
        sTmp := QuotedStr((AVariant)); // 将数据库表里的内容单引号替换为2个单引号，这是sql下的转意做法
        //sTmp := '''' + VarToStr(AVariant).Replace('''', '''''') + '''';
      end;
    finally
      //
    end;

    result := sTmp;
  end;

  function MakeWhere(YinHao1,YinHao2:string;const ADataSet: {$IFDEF FPC} TBufDataSet{$ELSE}TVirtualTable{$ENDIF}): String;
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
              Result := Result + YinHao1+cFldName+YinHao2 + ' IS NULL'
          else
              Result := Result + YinHao1+cFldName+YinHao2 + ' = ' + GetASQLValue(ADataSet.Fields[i].DataType, ADataSet.Fields[i].Value,DataBaseType);
        end;
      end;
    end;
  end;
begin
  Result := '';
  LineEnding := ';';
  if DataBaseType.ToLower='mysql' then  //将表名和字段名包起来，这样保留字就可以作表名和字段名了
  begin
   YinHao1 := '`';//mysql用法
   YinHao2 := '`';
  end
  else if (DataBaseType.ToLower='sqlserver') or (DataBaseType.ToLower='sql server') then
  begin
   YinHao1 := '[';//sql server用法
   YinHao2 := ']';
  end
  else if DataBaseType.ToLower='oracle' then
  begin
   YinHao1 := '"';//Oracle用法 2023-09-18修正为双引号
   YinHao2 := '"';
  end
  else if (DataBaseType.ToLower='sqlite') or (DataBaseType.ToLower='access')  or (DataBaseType.ToLower='postgresql')   or (DataBaseType.ToLower='firebird') then
  begin
   YinHao1 := '"';//sqlite用法
   YinHao2 := '"';
  end
  else
  begin
  YinHao1 := '';
  YinHao2 := '';
  end;

  if YinHao1<>'' then     //2021-06-21修改 sqlserver如果引用别的库等：Delete from [DRDB].[LinkDB].[dbo].[Server_Item_TT] Where  [ITEMid] = 2435;
  begin
   aTablename :=aTablename.Replace(YinHao1,'').Replace(YinHao2,'').Replace('.',YinHao2+'.'+YinHao1) ;//postgresql用法:"dwry"."dwry"  模式和表名之间需要双引号
  end;

  JA:=nil;
  if aReturnStrType = 2 then
   JA := TJSONArray.Create;
  ItemKey := Tstringlist.Create;
  ItemNoField := Tstringlist.Create;

  AKeyFields:=AKeyFields.Replace(';',',').Replace('；',',').Replace('，',','); //分隔符号兼容中英文分号与逗号2021-11-12
  aNotEditFields:=aNotEditFields.Replace(';',',').Replace('；',',').Replace('，',','); //分隔符号兼容中英文分号与逗号2021-11-12
  ItemKey.Delimiter := ',';
  ItemKey.CaseSensitive:=false;  //===2021-06-20===防止将字段修改大小写，sqlserver字段大小写敏感
  ItemKey.DelimitedText := AKeyFields;

  ItemNoField.Delimiter := ',';
  ItemNoField.CaseSensitive:=false;
  ItemNoField.DelimitedText := aNotEditFields.ToLower;

  try
    try
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
                if (ItemNoField.IndexOf(cFldName) > -1) or (FNewDataSet.Fields[nFldOrder].DataType = ftAutoInc) or (cFldName.ToLower = aAutoIncFieldname.ToLower) or (FNewDataSet.Fields[nFldOrder].FieldKind <> fkData) then
                  // 排除不需要插入的字段 和没有输入值的字段 插入时自增长字段也不能赋值,2022-01-15增加：如果不是数据字段也不能赋值，如计算字段
                  Continue;
                if (cFldName<>'DataState')  then
                begin
                  if not FNewDataSet.Fields[nFldOrder].IsNull then
                  begin
                    if s1 <> '' then
                        s1 := s1 + ',';
                    if s2 <> '' then
                        s2 := s2 + ',';
                    s1 := s1 + YinHao1+cFldName+YinHao2;
                    s2 := s2 + GetASQLValue(FNewDataSet.Fields[nFldOrder].DataType, FNewDataSet.Fields[nFldOrder].Value,DataBaseType);
                  end;
                end;
              end;
              if s2<>'' then
               SQLStr :='INSERT INTO ' +YinHao1+ ATableName+YinHao2 + ' (' + s1 + ')' +
                ' VALUES (' + s2 + ')'+LineEnding;

              if aReturnStrType = 1 then
                AllSQLStr := AllSQLStr + SQLStr;
              if aReturnStrType = 2 then
                JA.Add(SQLStr);

            end;
            //Updated
            if FOldDataSet.FieldByName('DataState').Asinteger=ord(dsvUpdated) then
            begin
              s2 := '';
              for nFldOrder := 0 to FNewDataSet.FieldCount - 1 do
              begin
                cFldName := FNewDataSet.Fields[nFldOrder].FieldName;
                if (ItemNoField.IndexOf(cFldName) > -1) or (FNewDataSet.Fields[nFldOrder].DataType = ftAutoInc) or (cFldName.ToLower = aAutoIncFieldname.ToLower) or (FNewDataSet.Fields[nFldOrder].FieldKind <> fkData) then
                  // 排除不需要插入的字段 和没有输入值的字段 插入时自增长字段也不能赋值,2022-01-15增加：如果不是数据字段也不能赋值，如计算字段
                  Continue;
                if (cFldName<>'DataState') then
                begin
                  if FOldDataSet.FieldByName(cFldName).AsVariant <> FNewDataSet.FieldByName(cFldName).AsVariant then
                  begin
                    if s2 <> '' then
                        s2 := s2 + ', ';
                    if FNewDataSet.FieldByName(cFldName).IsNull then
                        s2 := s2 + YinHao1+cFldName+YinHao2 + ' = NULL'
                    else
                        s2 := s2 + YinHao1+cFldName+YinHao2 + ' = ' + GetASQLValue(FNewDataSet.Fields[nFldOrder].DataType, FNewDataSet.Fields[nFldOrder].Value,DataBaseType);
                  end;
                end;
              end;
              if s2<>'' then
               SQLStr :='UPDATE ' + YinHao1+ATableName+YinHao2 + ' SET ' + s2 +
                ' WHERE ' + MakeWhere(YinHao1,YinHao2,FOldDataSet)+LineEnding;

              if aReturnStrType = 1 then
                AllSQLStr := AllSQLStr + SQLStr;
              if aReturnStrType = 2 then
                JA.Add(SQLStr);

            end;
            //Deleted
            if FOldDataSet.FieldByName('DataState').Asinteger=ord(dsvDeleted) then
            begin
              SQLStr :='DELETE FROM ' + YinHao1+ATableName+YinHao2 + ' WHERE ' + MakeWhere(YinHao1,YinHao2,FNewDataSet)+LineEnding;
              if aReturnStrType = 1 then
                AllSQLStr := AllSQLStr + SQLStr;
              if aReturnStrType = 2 then
                JA.Add(SQLStr);
            end;
            FOldDataSet.Next;
            FNewDataSet.Next;
          end;
          CreateMonitorDataSet;//生成后清空 FOldDataSet 、FNewDataSet，并重新生成FOldDataSet 、FNewDataSet
        end;
      end;
    except
      result := '';
      exit;
    end;

    if aReturnStrType = 1 then
      result := AllSQLStr;
    if aReturnStrType = 2 then
    begin
      result := JA.ToString;
    end;
  finally
    if (aReturnStrType = 2) and (JA <> nil) then
    begin
      FreeAndNil(JA);
    end;
    FreeAndNil(ItemKey);
    FreeAndNil(ItemNoField);
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
        FAfterOpen:=FDataSet.AfterOpen;
        FDataSet.BeforeEdit:={$IFDEF FPC}@{$endif}BeforeEdits;
        FDataSet.BeforeDelete:={$IFDEF FPC}@{$endif}BeforeDeletes;
        FDataSet.BeforeInsert:={$IFDEF FPC}@{$endif}BeforeInserts;
        FDataSet.AfterPost:={$IFDEF FPC}@{$endif}AfterPosts;
        FDataSet.AfterOpen:={$IFDEF FPC}@{$endif}AfterOpens;
        //FDataSet.BeforeEdit:=BeforeEdits;
        //FDataSet.BeforeDelete:=BeforeDeletes;
        //FDataSet.BeforeInsert:=BeforeInserts;
        //FDataSet.AfterPost:=AfterPosts;
        //FDataSet.AfterOpen:=AfterOpens;
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
    FAfterOpen:=nil;
    Foldvalue:=nil;
    if Assigned(FNewDataSet) then
      FreeAndNil(FNewDataSet);
    if Assigned(FOldDataSet) then
      FreeAndNil(FOldDataSet);
  end;
end;

function TQFDataSetMonitor.GetActionSQL(const ATableName
  : String; const AKeyFields: String = ''): String; overload;
var
  nFldOrder: integer;
  cFldName, s1, s2: String;

  function SQLValue(const ADataSet:  {$IFDEF FPC} TBufDataSet{$ELSE}TVirtualTable{$ENDIF}; AFieldIndex: Integer): String;
  var
    cValue,cName: String;
    eType: TFieldType;
    sm:TStream;
    BlobField: TBlobField;
    F2:TField;
  begin
    eType := ADataSet.Fields[AFieldIndex].DataType;
    cName:= ADataSet.FieldDefs[AFieldIndex].Name;
    cValue := ADataSet.Fields[AFieldIndex].Value;
    if eType in [ftBlob] then   //将blob转为base64
    begin
      sm:=TStream.Create;
      F2:=ADataSet.FieldByName(cName);
      TBlobField(F2).SaveToStream(sm);
      Result :='^BLOB^.^'+ATableName+'^BLOB^#^'+StreamToBase64(sm)+'^BLOB^@^'+cName+'^BLOB^_^';
      sm.Free;
    end
    else
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

  function MakeWhere(const ADataSet: {$IFDEF FPC} TBufDataSet{$ELSE}TVirtualTable{$ENDIF}): String;
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


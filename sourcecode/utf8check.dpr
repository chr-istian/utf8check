{ -----------------------------------------------------------------------------}
{ UTF8check by Christian Welt                                                  }
{                                                                              }
{ 2022-03-16 Version 1.0                                                       }
{                                                                              }
{ If you wonder why this is a single file application:                         }
{ When I try to split the program into Delphi units, it is detected as         }
{ threatening by various virus scanners. I decided not to use a code-signing   }
{ certificate for this project. So the software can remain unsigned, but can   }
{ still be used as it is.                                                      }
{ -----------------------------------------------------------------------------}

program utf8check;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  WinApi.Windows;

{ ------------------------------- UTILS.TYPES ---------------------------------}

type

  TByteArray = packed array of Byte;

{ ------------------------------- UTILS.TIME ----------------------------------}

// TickDif
// calculates a time difference between the actual system ticks and a time in
// the past in system ticks passed as parameter.
function TickDif(
  tick: Cardinal // time in the past as system ticks value
): Cardinal;     // returns the difference in system ticks
var
  c: Cardinal;   // actual system ticks
begin
  c := GetTickCount;
  // since this is an unsigned value we need to calculate the difference right...
  if ( c >= tick ) then
    result := c - tick
  else
    result := High(Cardinal) - tick + c;
end;

// TicksToStr
// generates a readable string representing a time difference
function TicksToStr(
  ticks: Cardinal // time difference as system ticks value
): String;        // returns a readable string representing the time difference
begin
  // miliseconds
  result := IntToStr(ticks mod 1000) + 'ms';
  ticks := ticks div 1000;
  // seconds
  if ( ticks > 0 ) then
  begin
    result := IntToStr(ticks mod 60) + 's ' + result;
    ticks := ticks div 60;
  end;
  // minutes
  if ( ticks > 0 ) then
  begin
    result := IntToStr(ticks mod 60) + 'm ' + result;
    ticks := ticks div 60;
  end;
  // hours
  if ( ticks > 0 ) then
    result := IntToStr(ticks) + 's ' + result;
end;

{ ------------------------------ ENCODING.BOM ---------------------------------}

type

  TBOMArray = array[0..3] of Byte;

  TBOMType = record
    BOM: TBOMArray;
    Len: Cardinal;
    Name: String;
  end;

  TBOM = (bomUTF8, bomUTF16_BE, bomUTF16_LE, bomUTF32_BE, bomUTF32_LE, bomUTF7,
          bomUTF1, bomUTFEBCDIC, bomSCSU, bomBOCU1, bomGB18030, bomUnknown);

const

  cBOMTypes : array[Low(TBOM)..High(TBOM)] of TBOMType =
    ( ( BOM: ($EF, $BB, $BF, $00); Len: 3; Name: 'UTF-8' ),
      ( BOM: ($FE, $FF, $00, $00); Len: 2; Name: 'UTF-16 (BE)' ),
      ( BOM: ($FF, $FE, $00, $00); Len: 2; Name: 'UTF-16 (LE)' ),
      ( BOM: ($00, $00, $FE, $FF); Len: 4; Name: 'UTF-32 (BE)' ),
      ( BOM: ($FF, $FE, $00, $00); Len: 4; Name: 'UTF-32 (LE)' ),
      ( BOM: ($2B, $2F, $76, $00); Len: 3; Name: 'UTF-7' ),
      ( BOM: ($F7, $64, $4C, $00); Len: 3; Name: 'UTF-1' ),
      ( BOM: ($DD, $73, $66, $73); Len: 4; Name: 'UTF-EBCDIC' ),
      ( BOM: ($0E, $FE, $FF, $00); Len: 3; Name: 'SCSU' ),
      ( BOM: ($FB, $EE, $28, $00); Len: 3; Name: 'BOCU-1' ),
      ( BOM: ($84, $31, $95, $33); Len: 3; Name: 'GB 18030' ),
      ( BOM: ($00, $00, $00, $00); Len: 0; Name: '(Unknown)' ) );

// ReadBOM
// returns the BOM type as defined at the beginning of the utf-8 data
function ReadBOM(
  var bytes: TByteArray // utf-8 data, passed by reference for performance sake
): TBOM;                // returns BOM type as found at the beginning of the data
var
  bom: TBOM;            // BOM type for iteration
  index: Cardinal;      // iteration counter
  len: Cardinal;        // len of the BOM
  bomArray: TBOMArray;  // BOM itself
begin
  // iterating over the BOMs until we find the right one
  result := bomUnknown;
  bom := Low(TBOM);
  while ( bom < bomUnknown ) and ( result = bomUnknown ) do
  begin
    // make loop invariants local variables for performance sake
    len := cBOMTypes[bom].Len;
    bomArray := cBOMTypes[bom].BOM;
    if ( Cardinal(Length(bytes)) >= len ) then
    begin
      // loop over the BOM bytes to compare them
      index := 0;
      while ( index < len ) and ( bytes[index] = bomArray[index] ) do
        INC(index);
      if ( index = len ) then
        // all BOM bytes are equal, we have a result
        result := bom;
    end;
    Inc(bom);
  end;
end;

{ ----------------------------- ENCODING.UTF-8 --------------------------------}

resourcestring

  MESSAGE_HEADER = 'UTF8check V1.0';
  MESSAGE_FOOTER = 'Checked %d file(s) in %s.';
  MESSAGE_HELP   = #13#10 +
    'utf8check.exe [Options] <pathname>' + #13#10#13#10 +
    ' Parameters' + #13#10 +
    ' pathname: file- or directoryname' + #13#10#13#10 +
    ' Options' + #13#10 +
    ' /brief /b        - only log errors to stdout' + #13#10 +
    ' /ignore-bom /ib  - ignore missing BOM' + #13#10 +
    ' /e:<extensions>  - semicolon separated list of file extensions' + #13#10 +
    '                    e.g. /e:pas;cpp;java;txt' + #13#10+
    ' /s               - recurse subdirectories';

  ERROR_MESSAGE_ERROR                        = '  ERROR: ';
  ERROR_MESSAGE_FILE_ERROR                   = 'File error - ';
  ERROR_MESSAGE_ERROR_LINE                   = '%s in line %d (Byte %d)';
  ERROR_INVALID_BOM                          = 'Wrong BOM %s';

  ERROR_MESSAGE_BOM_MISSING                  = 'BOM missing';
  ERROR_MESSAGE_INVALID_VALUE                = 'Invalid value';
  ERROR_MESSAGE_RESERVED_VALUE               = 'Reserved value';
  ERROR_MESSAGE_UNEXPECTED_END_OF_SEQUENCE   = 'Unexpected end of sequence';
  ERROR_MESSAGE_UNEXPECTED_END_OF_DATA       = 'Unexpected end of data';
  ERROR_MESSAGE_SEQUENCE_EXEEDS_MAXIMUM      = 'Sequence exceeds maximum';
  ERROR_MESSAGE_UNEXPECTED_CONTINUATION_BYTE = 'Unexpected continuation byte';

const

  ERROR_OK                           = 0;
  ERROR_INVALID_VALUE                = 1;
  ERROR_RESERVED_VALUE               = 2;
  ERROR_UNEXPECTED_END_OF_SEQUENCE   = 3;
  ERROR_UNEXPECTED_END_OF_DATA       = 4;
  ERROR_SEQUENCE_EXEEDS_MAXIMUM      = 5;
  ERROR_UNEXPECTED_CONTINUATION_BYTE = 6;

  cErrorMessage : array[1..6] of String = (
    ERROR_MESSAGE_INVALID_VALUE,
    ERROR_MESSAGE_RESERVED_VALUE,
    ERROR_MESSAGE_UNEXPECTED_END_OF_SEQUENCE,
    ERROR_MESSAGE_UNEXPECTED_END_OF_DATA,
    ERROR_MESSAGE_SEQUENCE_EXEEDS_MAXIMUM,
    ERROR_MESSAGE_UNEXPECTED_CONTINUATION_BYTE);

// CheckChar
// checks a single position in the utf-8 data for validity
function CheckChar(
  var bytes: TByteArray;  // utf-8 data, passed by reference for performance sake
  var index: Cardinal;    // index referring to the actual position in utf-8 data
  out errorCode: Cardinal // error code, result value
): Boolean;               // returns TRUE, if the checked char is a valid value in the utf-8 data
var
  b: Byte;                // byte value at position to check
  i: Cardinal;            // iteration counter
  len: Cardinal;          // length of utf-8 byte sequence
  mask: Byte;             // mask for boolean operations
  value: Cardinal;        // value of utf-8 byte sequence
begin
  errorCode := ERROR_OK;
  // make byte a local variable for performance sake
  b := bytes[index];
  if ( ( b and $80 ) = 0 ) then
  begin
    // the byte has the most significant bit not set and therefore it is a
    // "normal" octet
    Inc(index); // skipping the valid byte
    result := TRUE;
  end
  else
  begin
    // if the byte has the most significant bit set it can either be a starting
    // byte or a continuation byte
    result := FALSE;
    if ( ( b and $C0 ) = $C0 ) then
    begin
      // we found a starting byte
      // counting the following bits to determine the length of the sequence
      len := 2;
      mask := $E0;
      while ( ( ( b and mask ) = mask ) and ( mask <= $F0 ) ) do
      begin
        Inc(len);
        mask := ( mask SHR 1 ) or $80;
      end;
      // by definition the sequence MUST not be longer than four bytes
      if ( len <= 4 ) then
      begin
        // the sequence MUST end before the end of data
        if ( Cardinal(Length(bytes)) > index + len - 1 ) then
        begin
          // calcualting the value of the sequence
          value := b and ( not mask );
          i := 1;
          while ( i <= len ) and ( ( bytes[index + i] and $C0 ) = $80 ) do
          begin
            value := ( value SHL 6 ) or ( bytes[index + i] and $3F );
            Inc(i);
          end;
          // checking the value for validity
          if ( i = len ) then
          begin
            case len of
              2: begin
                   // by definition one MUST NOT encode a value into a longer
                   // sequence than mathematically needed
                   // values lower than $80 could be encoded in a single byte
                   if ( value >= $80 ) then
                     result := TRUE
                   else
                     errorCode := ERROR_INVALID_VALUE;
                 end;
              3: begin
                   // by definition one MUST NOT encode a value into a longer
                   // sequence than mathematically needed
                   // values lower than $800 could be encoded in two bytes
                   if ( value >= $800 ) then
                     // by definition the values $D800 - $DFFF are reserved for
                     // utf-16 low and high surrogates and are invalid in utf-8 encoding
                     if ( ( value < $D800 ) or ( value > $DFFF ) ) then
                       result := TRUE
                     else
                       errorCode := ERROR_RESERVED_VALUE
                   else
                     errorCode := ERROR_INVALID_VALUE;
                 end;
              4: begin
                   // by definition one MUST NOT encode a value into a longer
                   // sequence than mathematically needed
                   // values lower than $10000 could be encoded in three bytes
                   // by definition values $110000 and greater are not used and
                   // therefore are invalid
                   // ! the is an exception to that rule for flag symbols, that we
                   // do intentionally disregard !
                   if ( value >= $10000 ) and ( value < $110000) then
                     result := TRUE
                   else
                     errorCode := ERROR_INVALID_VALUE;
                 end;
            end;
            if ( result ) then
              Inc(index, len); // skipping the valid sequence of bytes
          end
          else
            // there were not as many contuation bytes as the starting byte
            // signaled therefore the sequence ended prematurely
            errorCode := ERROR_UNEXPECTED_END_OF_SEQUENCE;
        end
        else
          // a sequence was defined to extend beyond the end of the file
          errorCode := ERROR_UNEXPECTED_END_OF_DATA;
      end
      else
        // the sequence found is longer than the allowed four bytes
        errorCode := ERROR_SEQUENCE_EXEEDS_MAXIMUM;
    end
    else
      // we found a continuation byte without a prepending starting byte
      errorCode := ERROR_UNEXPECTED_CONTINUATION_BYTE;
  end;
end;

// CheckData
// checks utf-8 data for validity
function CheckData(
  var filename: String;
  var bytes: TByteArray;     // utf-8 data, passed by reference for performance sake
  ignoreMissingBOM: Boolean; // should a missing BOM be treated as an error or not?
  brief: Boolean             // should all files be listed or only erroneous files?
): Boolean;                  // returns TRUE if the utf-8 data is valid

  var filenameFlag: Boolean;
  procedure LogError(s: String);
  begin
    if ( brief ) and ( not filenameFlag ) then
    begin
      WriteLn(filename);
      filenameFlag := TRUE;
    end;
    WriteLn(s);
  end;

var
  bom: TBOM;                // found BOM type
  errorCode: Cardinal;      // errorCode as returned from CheckChar procedure
  index: Cardinal;          // index referring to the actual position in utf-8 data
  success: Boolean;         // success as returned from CheckChar procedure
  line: Cardinal;           // actual line number in the utf-8 data, measured for error messages
begin
  index := 0;
  filenameFlag := FALSE;
  // reading the BOM
  bom := ReadBOM(bytes);
  case bom of
    bomUTF8    : result := TRUE; // utf-8 BOM, all correct
    bomUnknown : begin
                   // unknown BOM rather indicates a missing BOM
                   result := ignoreMissingBOM;
                   if ( not result ) then
                     LogError(ERROR_MESSAGE_ERROR + ERROR_MESSAGE_BOM_MISSING);
                 end;
    else         begin
                   // known BOM, but not an utf-8 BOM, that is an error
                   result := FALSE;
                   LogError(ERROR_MESSAGE_ERROR + Format(ERROR_INVALID_BOM, [cBOMTypes[bom].Name] ))
                 end;
  end;
  if ( result ) then
  begin
    // skippn' da BOM (yeah man!)
    Inc(index, cBOMTypes[bom].Len);
    // iterating over all the data
    line := 1;
    repeat
      success := TRUE;
      // iterating until the next error
      while ( index < Cardinal(Length(bytes)) ) and ( success ) do
      begin
        if ( bytes[index] = $A ) then Inc(line);
        success := CheckChar(bytes, index, errorCode);
      end;
      if ( not success ) then
      begin
        // dropping error message to log
        result := FALSE;
        LogError(ERROR_MESSAGE_ERROR + Format(ERROR_MESSAGE_ERROR_LINE, [cErrorMessage[errorCode], line, index]));
        // skipping the invalid byte
        Inc(index);
      end;
    until ( index >= Cardinal(Length(bytes)) ) or ( errorCode = ERROR_UNEXPECTED_END_OF_DATA );
  end;
end;

// CheckFile
// checks a file for valid utf-8 data
function CheckFile(
  filename: String;          // filename
  ignoreMissingBOM: Boolean; // should a missing BOM be treated as an error or not?
  brief: Boolean             // should all files be listed or only erroneous files?
): Boolean;                  // returns TRUE, if the file contains valid utf-8 data
var
  bytes: TByteArray;         // utf-8 data as read from the file
  bytesRead: Cardinal;       // bytes actually read from ReadFile function
  handle: THandle;           // file handle
  size: Int64;               // file size
begin
  result := FALSE;
  // open file
  handle := CreateFile(PWideChar(filename), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if ( handle <> INVALID_HANDLE_VALUE ) then
  begin
    // read all file data
    GetFileSizeEx(handle, size);
    SetLength(bytes, size);
    result :=  ( ReadFile(handle, bytes[0], size, bytesRead, nil) and ( size = bytesRead ) );
    // close file
    CloseHandle(handle);
    if ( result ) then
    begin
      // check file data
      result := CheckData(filename, bytes, ignoreMissingBOM, brief);
      if ( result ) and ( not brief ) then
        // dropping file name to log
        WriteLn(filename);
    end
    else
      WriteLn(ERROR_MESSAGE_ERROR + ERROR_MESSAGE_FILE_ERROR + SysErrorMessage(GetLastError));
  end
  else
    WriteLn(ERROR_MESSAGE_ERROR + ERROR_MESSAGE_FILE_ERROR + SysErrorMessage(GetLastError));
end;

// CheckFile
// checks files within a directory for valid utf-8 data
function CheckDirectory(
  path: String;              // path to directory to check (MUST be without trailing backslash)
  extensions: String;        // file extensions to be considered, empty means all extensions
  recurse: Boolean;          // recursively check all subdirectories
  ignoreMissingBOM: Boolean; // should a missing BOM be treated as an error or not?
  brief: Boolean;            // should all files be listed or only erroneous files?
  out files: Cardinal        // number of files checked, measured for log output
): Boolean;                  // returns TRUE if all files referenced contain valid utf-8 data
var
  handle: THandle;           // find file handle
  findData: TWin32FindDataW; // find file data
begin
  handle := FindFirstFile(PWideChar(path + '\*'), findData);
  result := ( handle <> INVALID_HANDLE_VALUE );
  if ( result ) then
  begin
    repeat
      if ( findData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0 ) then
      begin
        if ( recurse ) and ( String(findData.cFileName) <> '.' ) and ( String(findData.cFileName) <> '..' ) then
          if ( not CheckDirectory(path + '\' + findData.cFileName, extensions, recurse, ignoreMissingBOM, brief, files) ) then
            result := FALSE;
      end
      else
      begin
        if ( extensions = '' ) or ( Pos(Copy(ExtractFileExt(findData.cFileName), 2, High(Integer)), extensions) <> 0 ) then
        begin
          Inc(files);
          if ( not CheckFile(path + '\' + findData.cFileName, ignoreMissingBOM, brief) ) then
            result := FALSE;
        end;
      end;
    until not FindNextFile(handle, findData);
    FindClose(handle);
  end
  else
    WriteLn(ERROR_MESSAGE_ERROR + ERROR_MESSAGE_FILE_ERROR + SysErrorMessage(GetLastError));
end;

// CheckPathname
// checks all files referenced for valid utf-8 data
function CheckPathname(
  pathname: String;          // file or directory reference
  extensions: String;        // file extensions to be considered, empty means all extensions
  recurse: Boolean;          // recursively check all subdirectories
  ignoreMissingBOM: Boolean; // should a missing BOM be treated as an error or not?
  brief: Boolean;            // should all files be listed or only erroneous files?
  out files: Cardinal        // number of files checked, measured for log output
): Boolean;                  // returns TRUE if all files referenced contain valid utf-8 data
var
  handle: THandle;           // find file handle
  findData: TWin32FindDataW; // find file data
begin
  pathname := ExcludeTrailingBackslash(pathname);
  // if pathname is an relative path we make it absolute
  if ( Pos('\', pathname) = 0 ) then
    pathname := ExtractFilePath(ParamStr(0)) + pathname;
  // checking if a single file or directory is meant
  handle := FindFirstFile(PWideChar(pathname), findData);
  result := ( handle <> INVALID_HANDLE_VALUE );
  FindClose(handle);
  if ( result ) then
  begin
    if ( findData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0 ) then
    begin
      // directory meant
      result := CheckDirectory(pathname, extensions, recurse, ignoreMissingBOM, brief, files);
    end
    else
    begin
      // single file meant
      if ( extensions = '' ) or ( Pos(Copy(ExtractFileExt(pathname), 2, High(Integer)), extensions) <> 0 ) then
      begin
        files := 1;
        result := CheckFile(pathname, ignoreMissingBOM, brief);
      end;
    end;
  end
  else
    WriteLn(ERROR_MESSAGE_ERROR + ERROR_MESSAGE_FILE_ERROR + SysErrorMessage(GetLastError));
end;

{ ------------------------ SIMPLE PARAMETER CHECKING --------------------------}

// HasParam
// checks if passed string is part of the parameter list
function HasParam(
  p: String // parameter string
): Boolean; // returns TRUE if parameter is found
var
  index: Integer; // iteration index
begin
  result := FALSE;
  index := 1;
  while ( not result ) and ( index <= ParamCount) do
  begin
    result := ( UpperCase(Copy(ParamStr(index), 1, Length(p))) = UpperCase(p) );
    Inc(index);
  end;
end;

// GetParamValue
// checks if passed string is part of the parameter list and returns trailing
// chars as param value
function GetParamValue(
  p: String // parameter string
): String;  // returns parameter value
var
  index: Integer; // iteration index
  found: Boolean; // found flag
begin
  found := FALSE;
  index := 1;
  while ( not found ) and ( index < ParamCount) do
  begin
    found := ( UpperCase(Copy(ParamStr(index), 1, Length(p))) = UpperCase(p) );
    if ( found ) then
      result := Copy(ParamStr(index), Length(p) + 1, High(Integer));
    Inc(index);
  end;
end;

{ ---------------------------------- MAIN -------------------------------------}

var
  time: Cardinal; // start time in system ticks, measured for log output
  files: Cardinal; // number of files checked, measured for log output
begin
  WriteLn(MESSAGE_HEADER);
  if ( HasParam('/?') or HasParam('-?')) then
  begin
    WriteLn(MESSAGE_HELP);
  end
  else
  begin
    files := 0;
    time := GetTickCount;
    if ( not CheckPathname(
               ParamStr(ParamCount),
               GetParamValue('/E:'),
               HasParam('/S'),
               ( HasParam('/IGNORE-BOM') or HasParam('/IB') ),
               ( HasParam('/BRIEF') or HasParam('/B') ),
               files) ) then
      ExitCode := 1;
    Write(Format(MESSAGE_FOOTER, [files, TicksToStr(TickDif(time))]));
    if ( ExitCode = 0 ) then
      WriteLn(' No errors.')
    else
      WriteLn;
  end;
end.

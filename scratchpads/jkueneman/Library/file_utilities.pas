unit file_utilities;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF DARWIN}
  CFBase, CFBundle, CFURL, CFString,
  {$ENDIF}
  Classes, SysUtils, Forms;

const
  PATH_UNIX_APPLICATION_PATH = 'Desktop\';

function GetApplicationPath: string;

implementation

function GetApplicationPath: string;
{$IFDEF DARWIN}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$ENDIF}
begin
  // Under OSX we get the path of the executable
  {$IFDEF DARWIN}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);
  Result := pathStr;
  Result := ExtractFilePath(Result);
  {$ENDIF}
    // Under Windows we get the path of the executable
  {$IFDEF Windows}
  Result := ExtractFilePath(Application.ExeName);
  {$ENDIF}
  {$IFDEF Linux}
  Result := PATH_UNIX_APPLICATION_PATH;    // Linux is typically hardcoded to a path
  {$ENDIF}
end;

end.

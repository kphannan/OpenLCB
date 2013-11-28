// *****************************************************************************
//
// This file is the template for the application to define how the Configuration
// Memory Protocol accesses Configuration Memory
//
// In order for these to be called you must define the node/vnode implements the
// Configuration Address Space in several places.
// In the template_node and template_vnode files
// 1) Protocol Identification Protocol (PIP) must define Configuration Memory Protocol exists (USER_PIV_SUPPORTED_PROTOCOLS)
// 2) USER_CONFIGMEM_LOWEST_SPACE must be low enough to cover the MSI_CONFIG value
//
// *****************************************************************************

unit template_configuration;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes,
  SysUtils,
  {$ENDIF}
  opstackdefines;

function AppCallback_ReadConfiguration(ConfigAddress: DWord; ReadCount: Word; DatagramData: PByte): Word;
function AppCallback_WriteConfiguration(ConfigAddress: DWord; ReadCount: Word; DatagramData: PByte): Word;

implementation

{$IFDEF FPC}
const
  MAX_MAP = $FFFF;
var
  ConfigurationMap: array[0..MAX_MAP - 1] of Byte;
  i: Integer;
{$ENDIF}

  // *****************************************************************************
  //  procedure AppCallback_ReadConfiguration
  //     Parameters: : ConfigAddress:  The offset into the flat address space to
  //                                   start reading from
  //                   ReadCount    :  The number of bytes to read
  //                   DatagramData :  Pointer to the start of the Datagram Array
  //                                   were to write the data to
  //     Returns:    The number of bytes actually read;
  //     Description: Override to read from the configuration memory (could be
  //                  SPI EEPROM, internal EEPROM, files, etc)
  // *****************************************************************************
function AppCallback_ReadConfiguration(ConfigAddress: DWord; ReadCount: Word; DatagramData: PByte): Word;
var
  i: Integer;
begin
  for i := 0 to ReadCount - 1 do
  begin
    {$IFDEF FPC}
    if ConfigAddress < MAX_MAP then
      DatagramData^ := ConfigurationMap[ConfigAddress+i];
    Inc(DatagramData);
    {$ENDIF}
  end;
  Result := ReadCount;
end;

// *****************************************************************************
//  procedure AppCallback_WriteConfiguration
//     Parameters: : ConfigAddress:  The offset into the flat address space to
//                                   start writing from
//                   ReadCount    :  The number of bytes to write
//                   DatagramData :  Pointer to the start of the Datagram Array
//                                   were to write the data to
//     Returns:    The number of bytes actually written;
//     Description: Override to write from the configuration memory (could be
//                  SPI EEPROM, internal EEPROM, files, etc)
// *****************************************************************************
function AppCallback_WriteConfiguration(ConfigAddress: DWord; ReadCount: Word; DatagramData: PByte): Word;
var
  i: Integer;
  Temp: Byte;
begin
  for i := 0 to ReadCount - 1 do
  begin
    {$IFDEF FPC}
    if ConfigAddress < MAX_MAP then
      ConfigurationMap[ConfigAddress+i] := DatagramData^;
    Inc(DatagramData);
    {$ENDIF}
  end;
  Result := ReadCount;
end;

{$IFDEF FPC}
initialization
  for i := 0 to MAX_MAP - 1 do
    ConfigurationMap[i] := 0;
{$ENDIF}

end.


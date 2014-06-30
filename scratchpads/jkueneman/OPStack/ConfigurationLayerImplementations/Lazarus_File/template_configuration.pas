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
  FileUtil,
  {$ENDIF}
  opstacktypes;

{$IFDEF FPC}
procedure SetConfigurationFile(FilePath: WideString);
procedure ZeroConfiguration;
{$ENDIF}

procedure TemplateConfiguration_Initialize;
function AppCallback_ReadConfiguration(ConfigAddress: DWord; ReadCount: Word; DatagramData: PByte): Word;
function AppCallback_WriteConfiguration(ConfigAddress: DWord; WriteCount: Word; DatagramData: PByte): Word;

function AppCallback_ReadAcdiUser(ConfigAddress: DWord; ReadCount: Word; ConfigData: PByte): Word;
function AppCallback_WriteAcdiUser(ConfigAddress: DWord; WriteCount: Word; ConfigData: PByte): Word;

implementation

{$IFDEF FPC}
uses
  template_vnode, template_node;

const
  MAX_ADDRESS_SPACE = USER_VNODE_CONFIGURATION_MEMORY_SIZE * (USER_MAX_NODE_COUNT-1) + USER_CONFIGURATION_MEMORY_SIZE;
var
  i: Integer;
  AddressSpace: TMemoryStream;
  ConfigurationFile: WideString;

procedure SetConfigurationFile(FilePath: WideString);
begin
  ConfigurationFile := FilePath;
  if FileExists(UTF8ToSys(ConfigurationFile)) then
    AddressSpace.LoadFromFile(ConfigurationFile);
end;

procedure ZeroConfiguration;
begin
  for i := 0 to MAX_ADDRESS_SPACE - 1 do
    AddressSpace.WriteByte(0);
end;

{$ENDIF}

  // *****************************************************************************
  //  procedure TemplateConfiguration_Initialize
  //     Parameters: : 
  //     Returns:    
  //     Description: 
  // *****************************************************************************
procedure TemplateConfiguration_Initialize;
begin
end;

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
  iCount: Integer;
begin
  Result := 0;
  {$IFDEF FPC}
  if ConfigAddress + ReadCount < AddressSpace.Size then
  begin
    AddressSpace.Position := ConfigAddress;
    for iCount := 0 to ReadCount - 1 do
    begin
      DatagramData^ := AddressSpace.ReadByte;
      Inc(DatagramData);
      Inc(Result);
    end;
  end;
  {$ENDIF}
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
function AppCallback_WriteConfiguration(ConfigAddress: DWord; WriteCount: Word; DatagramData: PByte): Word;
var
  iCount: Integer;
begin
  Result := 0;
  {$IFDEF FPC}
  if ConfigAddress + WriteCount < AddressSpace.Size then
  begin
    AddressSpace.Position := ConfigAddress;
    for iCount := 0 to WriteCount - 1 do
    begin
      AddressSpace.WriteByte( DatagramData^);
      Inc(DatagramData);
      Inc(Result);
    end;
  end;
  if (Result > 0) and (DirectoryExists( ExtractFilePath( UTF8ToSys(ConfigurationFile)))) then
    AddressSpace.SaveToFile(UTF8ToSys(ConfigurationFile));
  {$ENDIF}
end;

// *****************************************************************************
//  procedure AppCallback_ReadAcdiUser
//     Parameters: : ConfigAddress:  The offset into the flat address space to
//                                   start reading from
//                   ReadCount    :  The number of bytes to read
//                   DatagramData :  Pointer to the start of the Datagram Array
//                                   were to write the data to
//     Returns:    The number of bytes actually read;
//     Description: Override to read from the configuration memory (could be
//                  SPI EEPROM, internal EEPROM, files, etc)
// *****************************************************************************
function AppCallback_ReadAcdiUser(ConfigAddress: DWord; ReadCount: Word; ConfigData: PByte): Word;
begin
  // Here we use the same configuration memory for the AcdiUser strings as normal configuration memory
  // that data starts after the user strings
  // The user can completly override this if desired, the library has filled in
  // The first 128 Bytes of the Configuration Memory are for the User Strings in this example
  // (Version ID + 63 + 64) as defined in the Template_Node and Template_VNode files
   Result := AppCallback_ReadConfiguration(ConfigAddress, ReadCount, ConfigData)
end;

// *****************************************************************************
//  procedure AppCallback_WriteAcdiUser
//     Parameters: : ConfigAddress:  Is the _beginning_ of the block of configruation memory
//                                   for the node or vNode (offset 0)
//                   ReadCount    :  The number of bytes to read
//                   DatagramData :  Pointer to the start of the Datagram Array
//                                   were to write the data to
//     Returns:    The number of bytes actually written;
//     Description: Override to read from the configuration memory (could be
//                  SPI EEPROM, internal EEPROM, files, etc)
// *****************************************************************************
function AppCallback_WriteAcdiUser(ConfigAddress: DWord; WriteCount: Word; ConfigData: PByte): Word;
begin
  // Here we use the same configuration memory for the AcdiUser strings as normal configuration memory
  // that data starts after the user strings
  // The user can completly override this if desired, the library has filled in
  // The first 128 Bytes of the Configuration Memory are for the User Strings in this example
  // (Version ID + 63 + 64)  as defined in the Template_Node and Template_VNode files
   Result := AppCallback_WriteConfiguration(ConfigAddress, WriteCount, ConfigData)
end;

{$IFDEF FPC}
initialization
  ConfigurationFile := '';
  AddressSpace := TMemoryStream.Create;
  AddressSpace.Size := MAX_ADDRESS_SPACE;
  AddressSpace.Position := 0;
  ZeroConfiguration;

finalization
  FreeAndNil(AddressSpace)
{$ENDIF}


end.


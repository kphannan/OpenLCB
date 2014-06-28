// *****************************************************************************
//
// This file is the template for the application to define the Configuration
// Memory Protocol memory spaces options. Most applications don't have to
// modify this file unless doing something special with the memory spaces
//
// *****************************************************************************

unit template_configmem;

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  {$IFDEF FPC}
  Classes, SysUtils,
  {$ENDIF}
  template_node,
  template_vnode,
  nmranetdefines,
  opstackdefines;


function AppCallback_AddressSpacePresent(Node: PNMRAnetNode; AddressSpace: Byte): Boolean;
function AppCallback_AddressSpaceReadOnly(Node: PNMRAnetNode; AddressSpace: Byte): Boolean;
function AppCallback_AddressSpaceSize(Node: PNMRAnetNode; AddressSpace: Byte): DWord;

implementation

// *****************************************************************************
//  procedure AppCallback_AddressSpacePresent
//     Parameters: : Node:         Node that the Config Memory protocol is being called on
//                   AddressSpace: The address space the Config Memory protocol is
//                                 is accessing
//     Returns:    True if the passed Config Memory Address Space is Present
//     Description: Defines if the address space options claim the memory space
//                  is present or not
// *****************************************************************************
function AppCallback_AddressSpacePresent(Node: PNMRAnetNode; AddressSpace: Byte): Boolean;
begin
  Result := True;     // All are contiguous are present
end;

// *****************************************************************************
//  procedure AppCallback_AddressSpaceReadOnly
//     Parameters: : Node:         Node that the Config Memory protocol is being called on
//                   AddressSpace: The address space the Config Memory protocol is
//                                 is accessing
//     Returns:    True if the passed Config Memory Address Space is read only (writes will fail)
//     Description: Defines if the address space options claim the memory space
//                  is readonly or not
// *****************************************************************************
function AppCallback_AddressSpaceReadOnly(Node: PNMRAnetNode; AddressSpace: Byte): Boolean;
begin
  Result := False;
  case AddressSpace of
    MSI_CDI,
    MSI_ACDI_MFG,
    MSI_ALL        : Result := True;
  end;
end;

// *****************************************************************************
//  procedure AppCallback_AddressSpaceSize
//     Parameters: : Node:         Node that the Config Memory protocol is being called on
//                   AddressSpace: The address space the Config Memory protocol is
//                                 is accessing
//     Returns:    The size of the Address Space.  This can be $FFFFFFFF if need be
//     Description: Defines if the address space options claim the memory space
//                  size is.  Some other feature (like defined number of nulls)
//                  may be used to actually cause the caller to stop accessing the
//                  memory
// *****************************************************************************
function AppCallback_AddressSpaceSize(Node: PNMRAnetNode; AddressSpace: Byte): DWord;
begin
  Result := $FFFFFFFF;
end;

end.

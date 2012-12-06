unit unitLinuxFTDI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


{
procedure TForm1.Button1Click(Sender: TObject);
var i : Integer;
    ok : boolean;
    DeviceCount : DWord;
    aDevice_String : Array [1..50] of Char;
    SerialNo,descryptor : String;

begin
  Memo1.Clear;
  FT_GetNumDevices(DeviceCount,Nil,FT_LIST_NUMBER_ONLY);                        // Get Number of Devices
  If DeviceCount > 0 then
  begin
    For I := 0 to DeviceCount-1 do
    Begin
      ok := FT_ListDevices(i,@aDevice_String,(FT_OPEN_BY_SERIAL_NUMBER or FT_LIST_BY_INDEX)) = FT_OK;
      SerialNo := aDevice_String;
      if ok then begin
      ok := FT_ListDevices(i,@aDevice_String,(FT_OPEN_BY_DESCRIPTION or FT_LIST_BY_INDEX)) = FT_OK;
      descryptor := aDevice_String;
      end;
         if ok then
         begin
         Memo1.Lines.Add(IntToStr(I));
         Memo1.Lines.Add(SerialNo);
         Memo1.Lines.Add(descryptor);
         end;
    End;
  end;
 // OpenBtn.Enabled := (Listview1.SelCount > 0);
end;

procedure TForm1.Button2Click(Sender: TObject);
Var Index : Integer;
begin
  index := 0;
  index := FT_Open(strtoint(Edit1.Text),FT_Handle) ;

  showmessage(inttostr(index));

  If index = FT_OK then                                      // Try to open the Device
  Begin

    FT_ResetDevice(FT_Handle);                                                  // Reset
    FT_SetTimeouts(FT_Handle,16,50);                                            // Define TX Timeout (RX is not important for this app)
    FT_SetBaudRate(FT_Handle,250000);                                           // Set the DMX Baudrate
    FT_SetDataCharacteristics(FT_Handle,FT_DATA_8,FT_STOP_2,FT_PARITY_NONE);    // Set DMX512 Framing Characteristics
    FT_SetFlowControl(FT_Handle,FT_FLOW_NONE,FT_XON_Value,FT_XOFF_Value);       // DMX512 needs no Handshaking

    Timer1.Enabled:= true ;
  end


end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Timer1.Enabled := False;                                                      // Stop the DMX Refresh
  FT_Close(FT_Handle);                                                          // Close the USB Device
  FT_Handle := 0;                                                               // Reset the Handle
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  for x := 1 to 250 do begin
  if (x mod 2) <> 0 then DMX_Buf[x] := 255 else DMX_Buf[x] := 0 ;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  for x := 1 to 500 do
     if DMX_Buf[x] < 128 then DMX_Buf[x] := 254  else DMX_Buf[x] := 0 ;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  for x := 1 to 500 do DMX_Buf[x] := 0 ;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var ok : boolean;
    Count : DWord;
begin
 //  y := y + 1 ;
 //  if y = 20 then begin
 //     y := 0 ;
 //     if x >= 40 then x := 8 ;
 //     x := x + 1  ;
 //     if DMX_Buf[x] = 0 then DMX_Buf[x] := 254 else DMX_Buf[x] := 0 ;
 //     end;


  Timer1.Enabled := False;                                                      // Stop DMX Refresh Timer
  ok := FT_SetBreakOn(FT_Handle)=FT_ok;                                         // Set the Reset condition
  if ok then ok := FT_SetBreakOff(FT_Handle) = FT_OK;                           // Remove the Reset condition
  if ok then ok := FT_Write(FT_Handle,@DMX_Buf,DMXChannelsMax+1,Count)=FT_ok;   // Send the Start-Byte and the 512 DMX512 channels
  if ok then Timer1.Enabled := True                                             // Restart the DMX Refresh Timer
        else Button3Click(Self);                                               // If we get an error, we stop the transmission
end;
}

       {
Type FT_Result = Integer;
     FT_ProgramData = Record
       VID : Word;
       PID : Word;
       pManufacturer : PChar;
       pManufacturerId : PChar;
       pDescription : PChar;
       pSerialNumber : PChar;
       MaxPower: Word;                    // 0 < MaxPower <= 500
       PnP: Word;                         // 0 = disabled, 1 = enabled
       SelfPowered : Word;                // 0 = bus powered, 1 = self powered
       RemoteWakeup : Word;               // 0 = not capable, 1 = capable
       // Rev4 extensions:
       Rev4 : Boolean;                    // true if Rev4 chip, false otherwise
       IsoIn : Boolean;                   // true if in endpoint is isochronous
       IsoOut : Boolean;                  // true if out endpoint is isochronous
       PullDownEnable : Boolean;          // true if pull down enabled
       SerNumEnable : Boolean;            // true if serial number to be used
       USBVersionEnable : Boolean;        // true if chip uses USBVersion
       USBVersion : Word;                 // BCD (0x0200 => USB2)
     END;


Var
    // Port Handle Returned by the Open Function
    FT_HANDLE : DWord = 0;
    FT_XON_Value : Byte = $11;
    FT_XOFF_Value : Byte = $13;
    FT_EVENT_Value : Byte = $0;
    FT_ERROR_Value : Byte = $0;

Const
     // FT_Result Values
    FT_OK = 0;
    FT_INVALID_HANDLE = 1;
    FT_DEVICE_NOT_FOUND = 2;
    FT_DEVICE_NOT_OPENED = 3;
    FT_IO_ERROR = 4;
    FT_INSUFFICIENT_RESOURCES = 5;
    FT_INVALID_PARAMETER = 6;
    FT_SUCCESS = FT_OK;
    // FT_Open_Ex Flags
    FT_OPEN_BY_SERIAL_NUMBER = 1;
    FT_OPEN_BY_DESCRIPTION = 2;
    // FT_List_Devices Flags
    FT_LIST_NUMBER_ONLY = $80000000;
    FT_LIST_BY_INDEX = $40000000;
    FT_LIST_ALL = $20000000;
    // Data Bits Selection
    FT_DATA_7 = 7;
    FT_DATA_8 = 8;
    // Stop Bits Selection
    FT_STOP_1 = 0;
    FT_STOP_2 = 2;
    // Parity Selection
    FT_PARITY_NONE = 0;
    FT_PARITY_ODD = 1;
    FT_PARITY_EVEN = 2;
    FT_PARITY_MARK = 3;
    FT_PARITY_SPACE = 4;
    // Flow Control Selection
    FT_FLOW_NONE = $0000;
    FT_FLOW_RTS_CTS = $0100;
    FT_FLOW_DTR_DSR = $0200;
    FT_FLOW_XON_XOFF = $0400;
    // Purge Commands
    FT_PURGE_RX = 1;
    FT_PURGE_TX = 2;
    // DLL Name
    FT_DLL_Name = 'libftd2xx';

    function FT_ListDevices(pvArg1:Dword;pvArg2:Pointer;dwFlags:Dword) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_ListDevices';
    function FT_GetNumDevices(VAR dwCount:DWord;pvArg2:Pointer;dwFlags:Dword) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_ListDevices';
    function FT_Open(PVDevice:Integer; VAR ftHandle:DWord ) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_Open';
    function FT_OpenEx(pvArg1:Pointer;dwFlags:Dword;VAR ftHandle:DWord) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_OpenEx';
    function FT_Close(ftHandle:Dword) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_Close';
    function FT_Read(ftHandle:Dword; FTInBuf : Pointer; BufferSize : LongInt; VAR dwCount:DWord) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_Read';
    function FT_Write(ftHandle:Dword; FTOutBuf : Pointer; BufferSize : LongInt; VAR dwCount:DWord) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_Write';
    function FT_ResetDevice(ftHandle:Dword) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_ResetDevice';
    function FT_SetBaudRate(ftHandle:Dword;dwBaudRate:DWord) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_SetBaudRate';
    function FT_SetDivisor(ftHandle:Dword;usDivisor:Word) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_SetDivisor';
    function FT_SetDataCharacteristics(ftHandle:Dword;ucWordLength,ucStopBits,ucParity:Byte) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_SetDataCharacteristics';
    function FT_SetFlowControl(ftHandle:Dword;usFlowControl:Word;ucXonChar,ucXoffChar:Byte) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_SetFlowControl';
    function FT_SetDtr(ftHandle:Dword) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_SetDtr';
    function FT_ClrDtr(ftHandle:Dword) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_ClrDtr';
    function FT_SetRts(ftHandle:Dword) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_SetRts';
    function FT_ClrRts(ftHandle:Dword) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_ClrRts';
    function FT_GetModemStatus(ftHandle:Dword;VAR dwModemStatus:DWord) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_GetModemStatus';
    function FT_SetChars(ftHandle:Dword;ucEventChar,ucEventCharEnabled,ucErrorChar,ucErrorCharEnabled : Byte) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_SetChars';
    function FT_Purge(ftHandle:Dword;dwMask:Dword) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_Purge';
    function FT_SetTimeouts(ftHandle:Dword;dwReadTimeout,dwWriteTimeout:Dword) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_SetTimeouts';
    function FT_GetQueueStatus(ftHandle:Dword;VAR dwRXBytes:DWord) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_GetQueueStatus';
    function FT_SetBreakOn(ftHandle:Dword) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_SetBreakOn';
    function FT_SetBreakOff(ftHandle:Dword) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_SetBreakOff';
    function FT_GetStatus(ftHandle:Dword;VAR dwRXCount,dwTXCount, dwEventState: DWord) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_GetStatus';
    function FT_SetEventNotification(ftHandle:Dword;dwEventMask:DWord;pEventHandle:Pointer) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_SetEventNotification';
    function FT_EE_Program(ftHandle:Dword;VAR EEPROMData : FT_ProgramData) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_EE_Program';
    function FT_EE_Read(ftHandle:Dword;VAR EEPROMData : FT_ProgramData) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_EE_Read';
    function FT_EE_UARead (ftHandle:DWord; pEEPROMData : Pointer; dwDataLen:DWord;VAR dwBytesRead:DWord) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_EE_UARead';
    function FT_EE_UAWrite(ftHandle:DWord; pEEPROMData : Pointer; dwDataLen:DWord) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_EE_UAWrite';
    function FT_GetLatencyTimer(ftHandle:Dword;VAR ucTimer:Byte) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_GetLatencyTimer';
    function FT_SetLatencyTimer(ftHandle:Dword;ucTimer:Byte) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_SetLatencyTimer';
    function FT_GetBitMode(ftHandle:Dword;Var ucPins:Byte) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_GetBitMode';
    function FT_SetBitMode(ftHandle:Dword;ucMask,ucEnable:Byte) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_SetBitMode';
    function FT_SetUSBParameters(ftHandle:Dword;dwInSize,dwOutSize:DWord) : FT_Result ; cdecl ; External FT_DLL_Name name 'FT_SetUSBParameters';
    function FT_W32_CreateFile(pDeviceName:PChar;dwAccess,dwShareMode,lpSecAttributes,dwCreate,dwAttrsAndFlags,htemplate:DWord) : DWord ; cdecl ; External FT_DLL_Name name 'FT_W32_CreateFile';
          }

implementation

end.


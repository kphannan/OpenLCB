unit opstackcore_learn;

// TODO:  Implement Learning

{$IFDEF FPC}
interface
{$ENDIF}

{$I Options.inc}

uses
  template_node,
  template_vnode,
  opstacknode,
  opstackbuffers,
  template_event_callbacks,
  nmranetutilities,
  nmranetdefines,
  opstackdefines,
  opstacktypes;

procedure Learn(AMessage: POPStackMessage; DestNode: PNMRAnetNode);

implementation

procedure Learn(AMessage: POPStackMessage; DestNode: PNMRAnetNode);
begin

end;

end.


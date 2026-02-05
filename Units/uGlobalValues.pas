unit uGlobalValues;

interface

uses
  System.Classes;

var
  // Global marker category data built from loaded XML.
  MarkerCategory: TStringlist;
  // Cached MapID lookup table loaded from MapID.101.
  MapIDCache: String;

// Reserved for potential per-thread workers (not currently used).
threadvar
  DropAndDragThread:  TThread;
  MapListThread:      TThread;
  SearchZipThread:    TThread;
  OKTWExportThread:   TThread;
  KXJsonExportThread: TThread;
  LVSearchThread:     TThread;

implementation

end.

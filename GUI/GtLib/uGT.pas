unit uGT;

interface

type
  card = cardinal; // 32 bit unsigned
  int = integer; // 32 bit signed
  card16 = word;
  // bool = boolean;   // 1 byte boolean collides with windows.pas
  aBits = 0 .. 31;
  aBitSet = set of aBits;
  aBuf = array [0 .. 8000000] of byte;
  apBytes = ^aBuf;
  aShortStr = string[255];

  {
    Shortint	-128..127   signed 8-bit
    Smallint   	-32768..32767   signed 16-bit
    Longint   	-2147483648..2147483647   signed 32-bit
    Int64   	-2^63..2^63-1   signed 64-bit
    Byte   		0..255   unsigned 8-bit
    Word   		0..65535   unsigned 16-bit
    Longword   	0..4294967295   unsigned 32-bit
    UInt64   	0..2^64–1   unsigned 64-bit
  }

  // aLogProc = procedure ( ErrorNo : integer; const s : string ) of object;   // move to generic logger

const
  FF = -1; // $FFFFFFFF;

implementation

end.

unit ASCII;

interface

const
	nul = #0;
	soh = #1;
	stx = #2;
	etx = #3;
	eot = #4;
	enq = #5;
	ack = #6;
	bel = #7;
	bs = #8;
	tab = #9;
	lf = #10;
	vt = #11;
	ff = #12;
	cr = #13;
	so = #14;
	si = #15;
	nak = #21; // $15
	esc = #27; // $1B
	rs = #30; // $1E repeat send

	eol = cr + lf;

	uqm = '¿'; // upside-down question mark $BF
	bullet = '•'; // 149 $95  Arial .... Courier New

	{ CR = #13;
	  LF = #10;
	  TAB = #9;
	  EOL = CR + LF; }
	BOM = char($FEFF); // unicode standard byte order mark

	// procedure MakePrintable( var ch : char );   use uMisc version

implementation

procedure MakePrintable(var ch: char);
begin
	if ch < ' ' then
		ch := ' '
	else if ch > '~' then
		ch := ' ';
end;

end.

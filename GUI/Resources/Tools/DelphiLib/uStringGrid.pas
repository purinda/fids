unit uStringGrid;

{	draws a grid, of no particular size or shape, onto a panel }
interface

uses  Controls, StdCtrls, ExtCtrls, Forms, Classes, uGT;

type
    aCols = array of string;
    aJustify = ( juRight, juCenter, juLeft );  // defaults to right

	cStringGrid = class( TPaintBox )
		public
            constructor  CreateSG( pan : TPanel );
            destructor   Destroy;   override;
            procedure    Free;
            procedure    Paint; override;  //( sender : TObject )
            procedure    DefaultFont;
            procedure    Clear;
            procedure    SetText( col : int; row : int; txt : string );
			function     GetText( c, r : int ) : string;
            procedure    PaintText( col : int; row : int; txt : string );   // set + invalidate form
            procedure    SaveToFile( name : string );
		private
            mNRows, mNCols : int;
            mRowText : array of aCols;
            mJustify : array of aJustify;  // one per column
            // oPaint : TPaintBox;
    		mPanel   : TPanel;                // parent control
            procedure SetJustify( col : int; j : aJustify );
            function  GetJustify( col : int ) : aJustify;
		public
			property  JustifyCol [ col : int ] : aJustify
            	read  GetJustify
            	write SetJustify;
    	end;


implementation

uses  ASCII, Graphics, Dialogs;

const
	bordX = 6;
    bordY = 6;
    colGap = 10;
    titleGap = 4; // gap after title row
    lineGap = 4;


constructor  cStringGrid.CreateSG( pan : TPanel );

    begin
    inherited  Create( nil );
    mPanel := pan;
    pan.ShowCaption := false;
    pan.Align := alNone;  // need to resize it at will
    //oPaint := TPaintBox.Create( nil );
    Parent := pan;
    Align := alClient;     // fit to parent panel
    Canvas.Brush.Color := pan.Color;
    OnClick := pan.OnClick;
    end;


destructor   cStringGrid.Destroy;

	begin
	Clear;
    //oPaint.Free;
    inherited
	end;


procedure    cStringGrid.Free;

	begin
	if self <> nil then  Destroy;
	end;


procedure    cStringGrid.Clear;

    var
        r, c : int;
        cols : aCols;
    begin
    for r := Low( mRowText ) to High( mRowText ) do  begin
        cols := mRowText[ r ];
        for c := Low( cols ) to High( cols ) do  cols[ c ] := '';
        end;
    mNRows := 0;
    mNCols := 0;
    end;


procedure    cStringGrid.PaintText( col, row : int; txt : string );

    begin      // paint as you go data
    SetText( col, row, txt );
    Paint;  // ( self );   // invalidate
    end;


function  cStringGrid.GetJustify( col : int ) : aJustify;

	begin
    if col > High( mJustify ) then  result := juRight
    else  result := mJustify[ col ];
    end;


procedure cStringGrid.SetJustify( col : int; j : aJustify );

	begin
    if col > High( mJustify ) then  SetLength( mJustify, col + 8 );
    mJustify[ col ] := j;
    end;


procedure    cStringGrid.SetText( col : int; row : int; txt : string );

    begin       // paint later
    if ( row >= 0 ) and ( col >= 0 )then  begin
        if row > High( mRowText ) then  SetLength( mRowText, row + 8 );
        if row >= mNRows then  mNRows := row + 1;   // new width

        if col > High( mRowText[ row ] ) then SetLength( mRowText[ row ], col + 8 );
        mRowText[ row, col ] := txt;
        if col >= mNCols then  mNCols := col + 1;
		end;
    end;


procedure    cStringGrid.SaveToFile( name : string );

  var
    f : TextFile;
    c, r : int;
        cols : aCols;
  begin
  AssignFile( f, name );
  Rewrite( f );
  for r := 0 to mNRows - 1 do  begin
    for c := 0 to mNCols - 1 do  begin
        cols := mRowText[ r ];
    	if c <= High( cols ) then  Write( f, cols[ c ] + tab )
        else                       Write( f, tab );
      end;
    Writeln( f );
    end;
  CloseFile( f );
  end;


procedure    cStringGrid.DefaultFont;

    begin
    Canvas.Font.Name := 'MS Sans Serif';
    Canvas.Font.Color := clBlack;
    Canvas.Font.Size := -13;
    Canvas.Font.Style := [];
    Canvas.Brush.Color := clBtnFace;
    end;


function   cStringGrid.GetText( c, r : int ) : string;

	var
    	cols : aCols;
	begin
    result := '';
    if r <= High( mRowText ) then  begin
        cols := mRowText[ r ];
        if c <= High( cols ) then  result := cols[ c ];
    	end;
    end;


procedure    cStringGrid.Paint;  // ( sender : TObject );

    var
        r, c, w, mw, totWidth, x, y, dy, h : int;
        colWidth : array of int;
        s : string;
    begin
    if mNCols > High( mJustify ) then  SetLength( mJustify, mNCols + 1 );   	// calc display extent
    dy := Canvas.TextHeight( 'Ay' ) + lineGap;
    totWidth := 0;
    SetLength( colWidth, mNCols );
    for c := 0 to mNCols - 1 do  begin
        mw := 0;
        for r := 0 to mNRows - 1 do  begin
            s := GetText( c, r );
            if s <> '' then  begin
                w := Canvas.TextWidth( s );
                if w > mw then  mw := w;  		// find widest text in column
                end;
            end;
        colWidth[ c ] := mw + colGap;
        Inc( totWidth, mw + colGap );
        end;
    w := totWidth + bordX * 2;
    h := titleGap + dy * mNRows + bordY * 2;

    if ( mPanel.Width <> w ) or ( mPanel.Height <> h ) then  begin				// set panel to match extent
        mPanel.Height := h;
        mPanel.Width := w;
    	end     // and force re-paint  ie calls paint again, next time going through else below

    else  begin
    	Canvas.FillRect( mPanel.ClientRect );   // clearing frame      	paint grid image
        x := bordX;
        for c := 0 to mNCols - 1 do  begin
    		y := bordY;
            for r := 0 to mNRows - 1 do  begin
                s := GetText( c, r );
                if s <> '' then  begin
                    w := Canvas.TextWidth( s );
                    case mJustify[ c ] of
                        juRight  : Canvas.TextOut( x + colWidth[ c ] - w, y, s );
                        juLeft   : Canvas.TextOut( x, y, s );
                        juCenter : Canvas.TextOut( x + ( colWidth[ c ] - w ) div 2, y, s );
                        end;
                    end;
                if r = 0 then y := y + titleGap;
                y := y + dy;
                end; // for row

            x := x + colWidth[ c ] + colGap;
            end; // for col
	    end;
    end;


end.

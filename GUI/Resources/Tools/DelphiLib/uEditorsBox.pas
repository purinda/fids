unit uEditorsBox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
  uGT, uDataDictionary, uDbTree, uMirrorDB;

type
	cEditorsBox = class
    	constructor	Create( form : TForm; container : TPaintBox );
        private
            miLink		: ifDB;  		// DD links
            miEdits		: iEdits;		// editors call back
            mOptions	: asListOption;
            mForm		: TForm;

            mContainer	: TPaintBox;
			mUpdate		: Boolean;
        	mX, mY		: int;

			function	GetFormValue( f : cField; out valid : boolean ) : string;
			procedure	AddLabel( item : TControl; l : string; HGap : integer = 0 );
			procedure	FormPaint(Sender: TObject);
			procedure	PickerClick(Sender: TObject);
			procedure	OnChange(Sender: TObject);
			procedure	KeyPressed(Sender: TObject; var Key: Char);
            function	GetLeft : int;
            function	GetTop : int;
			function	FindField( tag : int ) : cField;

		public
			oFields		: cFieldList;
            property	Options : asListOption  read mOptions  write mOptions;
			procedure	NewData( const xml : string );  // aDeltaNotify - called whenever DB changes
    		procedure	InitFrame( intfDB : ifDB; intfEdit : iEdits );
            procedure	FrameReady;
            function	ErrorMesg( er : aDDErrorCode ) : boolean;
            property	Left : int  read GetLeft;
            property	Top	: int  read GetTop;
		end;

implementation   // ________________________________________________________________

uses
	StdCtrls, uUtils;

const
	xGap = 20;   // form element spacing
    yGap = 6;


constructor	cEditorsBox.Create( form : TForm; container : TPaintBox );

	begin
    mForm := form;
    mForm.OnPaint := FormPaint;
    mContainer := container;
    end;


procedure	cEditorsBox.InitFrame( intfDB : ifDB; intfEdit : iEdits );

	begin
    miLink.DB := intfDB.DB;
    miLink.ID := intfDB.ID;
    miEdits := intfEdit;
    miEdits.NewData := NewData;
    miEdits.FormValue := GetFormValue;
    if oFields = nil then  oFields := cFieldList.Create( miLink, miEdits )
    else  oFields.Clear;
    end;


function	cEditorsBox.GetLeft : int;

	begin
    result := mContainer.Left;
    end;


function	cEditorsBox.GetTop : int;

	begin
    result := mContainer.Top;
    end;


function	cEditorsBox.GetFormValue( f : cField; out valid : boolean ) : string;

	var
        eb : TEdit;
        cb : TComboBox;
        lab : TLabel;
	begin
    result := '';     valid := true;
    if f <> nil then  begin
        if f.Control is TEdit then  begin
            eb := TEdit( f.Control );
            result := eb.Text;
            end
        else if f.Control is TComboBox then  begin
            cb := TComboBox( f.Control );
            result := cb.Text;
            end
        else if f.Control is TLabel then  begin
            lab := TLabel( f.Control );
            result := lab.Caption;
            end
        else  valid := false;
        result := Trim( result );
    	end;
    end;


function	cEditorsBox.FindField( tag : int ) : cField;

	var
        f : cField;
	begin
    result := nil;
    for f in oFields do  begin
        if f.Tag = tag then  begin
            result := f;
            break;
        	end;
    	end;
    end;


procedure	cEditorsBox.PickerClick(Sender: TObject);

	begin
    if Assigned( miEdits.PickerClick ) then  begin
        miEdits.PickerClick( FindField( ( Sender as TControl ).Tag ) );
    	end;
    end;


procedure	cEditorsBox.OnChange(Sender: TObject);

	var
        f : cField;
        cb : TComboBox;
        pt, new : apNode;
        path : TStringList;
	begin
    if sender is TComboBox then  begin   // change selection path change
        cb := TComboBox( sender );
        f := oFields.FindField( cb.Tag );
        if f <> nil then  begin
            if foSelector in f.Options then  begin
                pt := f.GetNode;// RelativeOffset( oFields.Selected, f.mStepBack, f.oIndexPath );
                new := FindName( Back( pt, 1 ), cb.Text );
                if ( new <> nil ) and ( new <> pt ) then  begin
                    path := oFields.UpHillPath( f, oFields.IndexField );
                    if ( path <> nil ) and Assigned( miEdits.SetSelected ) then  begin
                        miEdits.SetSelected( FollowPath( path, new ) );
                        // mForm.Invalidate;
                    	end;
                    end;
            	end;
        	end;
    	end
    else if sender is TEdit then  begin
    	f := FindField( ( sender as TControl ).Tag );
        if f <> nil then f.IsTyping := true;
    	end;
    end;


procedure	cEditorsBox.KeyPressed(Sender: TObject; var Key: Char);

    begin
    if Key = #13 then  begin    // catch enter key
        if sender is TControl then  begin   // change selection path change
            if Assigned( miEdits.EnterPressed ) then  begin
            	miEdits.EnterPressed( oFields.FindField( TControl( Sender ).Tag ) );
                Key := #0;
            	end;
            end;
        end;
    end;


procedure	cEditorsBox.FrameReady;

	var                        // builds controls needed to display and edit data
    	em, w : int;
        f : cField;
        con : TControl;
        cbx : TComboBox;
        eb : TEdit;
        but : TButton;
	begin
    oFields.Ready;

    em := mForm.Canvas.TextWidth( 'M' );  // width of an em assuming controls using default form font
    mUpdate := true;

    mX := 120 + Left;  mY := 20 + Top;  w := 0;
    for f in oFields do  begin                    // put any required field items
        if foSelector in f.Options then  begin   // Selector fields are displayed drop downs
            cbx := TComboBox.Create( nil );
            con := cbx;
            cbx.OnChange := OnChange;           // change allows selection/path changes
            //if ( foAbstract in f.Options ) and ( foEditable in f.Options ) then  begin
                //cbx.Style := csDropDown;
                //end;
            end
        else if not ( foEditable in f.Options ) then  begin
            con := TLabel.Create( nil );
            end
        else   begin
        	eb := TEdit.Create( nil );
            con := eb;
            eb.OnChange := OnChange;
            eb.OnKeyPress := KeyPressed;
        	end;

        con.Parent := mForm;
        f.Control := con;
        con.Tag := f.Tag;
        con.Left := mX;
        con.Top := mY;
        if con.Width < f.Width * em then  con.Width := f.Width * em;
        if con.Width > w then  w := con.Width;

        if foPathPicker in f.Options then  begin  // add a picker button
            but := TButton.Create( nil );
            but.Parent := mForm;
            f.Button := but;
            but.Left := mX + 10 + con.Width;
            but.Top := mY + 2;
            but.Width := 17;
            but.Height := 17;
            but.Caption := '...';
            but.OnClick := PickerClick;
            but.Tag := f.Tag;
            end;
        mY := mY + con.Height + yGap;
        end;
    mContainer.Height := mY - Top;
    mContainer.Width := mX + w - Left;
    mUpdate := false;
    end;


function	cEditorsBox.ErrorMesg( er : aDDErrorCode ) : boolean;

	begin
    result := er <> ecNone;
    if result then  begin
        ShowMessage( 'ERROR ' + EnumToStr( Ord( er ), TypeInfo( aDDErrorCode ) ) );
    	end;
    end;


procedure	cEditorsBox.AddLabel( item : TControl; l : string; HGap : integer = 0 );

	var
        w : int;
	begin
    w := mForm.Canvas.TextWidth( l );
    mForm.Canvas.TextOut( item.Left - xGap - w, item.Top + HGap, l );
    end;


procedure	cEditorsBox.FormPaint(Sender: TObject);

	var
        f : cField;
        dd : TComboBox;
        eb : TEdit;
        lb : TLabel;
	begin
    if not mUpdate then  begin
        for f in oFields do  begin
            if f.Control is TComboBox then  begin
                dd := TComboBox( f.Control );
                AddLabel( dd, f.Title, 3 );
                end
            else if f.Control is TEdit then  begin
                eb := TEdit( f.Control );
                AddLabel( eb, f.Title, 3 );
                end
            else if f.Control is TLabel then  begin
                lb := TLabel( f.Control );
                AddLabel( lb, f.Title );
                end;
            end;
        end;
    end;


procedure	cEditorsBox.NewData( const xml : string );  // aDeltaNotify - called whenever DB changes

	var
        f : cField;
        dd : TComboBox;
        eb : TEdit;
        lb : TLabel;
        me, key, sub : apNode;
        x, curIndex : int;
        list : TStringList;
	begin                            // try not to disturb user edits
    list := TStringList.Create;
    if not mUpdate then  begin
        for f in oFields do  begin
        	if not f.IsTyping then  begin
                if f.Control is TComboBox then  begin
                    dd := TComboBox( f.Control );
                    if foAbstract in f.Options then  begin
                        list.Clear;
                        me := f.GetNode;
                        key := Back( me, 1 );
						if key <> nil then  begin
                            x := -1;  curIndex := -1;
                            while EachSubNode( key, x, sub ) do  begin
                                list.Add( NodeName( sub ) );
                                if sub = me then  curIndex := x;
                                end;
                            if not ListsEqual( list, dd.Items ) or ( curIndex <> dd.ItemIndex ) then  begin  // list changed or index changed
                                dd.Items := list;
                                dd.ItemIndex := curIndex;
                                end;
                        	end
                        else dd.Text := f.GetPresentation;
                        end;
                    end
                else if f.Control is TEdit then  begin
                    eb := TEdit( f.Control );
                    if eb.Text <> f.GetPresentation then  eb.Text := f.GetPresentation;
                    end
                else if f.Control is TLabel then  begin
                    lb := TLabel( f.Control );
                    if lb.Caption <> f.GetPresentation then  lb.Caption := f.GetPresentation;
                    end;
            	end
            else
            	f.IsTyping := false;
            end;
        end;
    list.Free;
    end;


{   USAGE
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ExtCtrls,
  uDbTree, uEditorsBox, uMirrorDb, uDataDictionary;

type
	TfEditForm = class(TForm)
    	pbEditors: TPaintBox;
    	procedure FormCreate(Sender: TObject);
	private
		miDB : iDBFields;
        oEditors : cEditorsBox;
        mSelectedRule  : apNode;
        procedure	SetSelected( sel : apNode );
	public
		procedure	InitForm( intf : iDBFields );
        property	SelectedRule : apNode read mSelectedRule  write SetSelected;
  end;

var
  fEditForm: TfEditForm;


implementation
$R *.dfm
uses	uFidsTags;



procedure TfEditForm.btKeepEditsClick(Sender: TObject);

    begin
    oEditors.ErrorMesg( oEditors.oFields.UpdateFields );
    end;


procedure TfEditForm.btAddThisRuleClick(Sender: TObject);

	begin
    oEditors.ErrorMesg( oEditors.oFields.NewBranch );
	end;


procedure TfEditForm.btAddThisShareClick(Sender: TObject);

    begin
    oEditors.ErrorMesg( oEditors.oFields.NewSub( 'FlightNo' ) );
    end;


procedure TfEditForm.btDeleteRuleClick(Sender: TObject);

    begin
    oEditors.oFields.DeleteBranch;
    end;


procedure	TfEditForm.InitForm( intf : iDBFields );    // called after db ready

	begin
    miDB := intf;   // form local copy of db interface stuff
    oEditors.InitFrame( miDB );
    with oEditors.oFields do  begin  // saves typing
        InitFields( tagTimetable, pathDataShape + tagTimeTable, 'FlightNo' );
        Selected := mSelectedRule;
        AddField( 'RuleName',[ foSelector, foEditable ] );
        AddField( 'Path' );
        AddField( 'Time', [ foEditable ] );
        AddField( 'Days' );
        AddField( 'FlightNo',[ foSelector ] );
        AddField( 'Ports', [ foEditable ] );
        AddField( 'CheckIns', [ foEditable ] );
        //RowField := 'FlightNo';   // field to enumerate grid over
        end;
    oEditors.FrameReady;
    end;


procedure	TfEditForm.SetSelected( sel : apNode );    // tell form which record to look at

	begin
    oEditors.oFields.Selected := sel;   // tell data dictionary stuff what to work with
    mSelectedRule := sel;
    Invalidate;  // force form redraw
    Show;        // does nothing if showing already
    end;


procedure TfEditForm.FormCreate(Sender: TObject);      // attach cEditorsBox to your TPaintBox region

    begin
    oEditors := cEditorsBox.Create( self, pbEditors );
    end;
end.
}
end.

unit ufRuleEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, uTTRules, uGT;

type
  TfRuleEdit = class(TForm)
    ebRuleName: TEdit;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    ebTime: TEdit;
    Label3: TLabel;
    ebStart: TEdit;
    Label4: TLabel;
    ebEnd: TEdit;
    Label5: TLabel;
    ebExcept: TEdit;
    Label6: TLabel;
    gbDays: TGroupBox;
    RadioButton1: TCheckBox;
    RadioButton2: TCheckBox;
    RadioButton3: TCheckBox;
    RadioButton4: TCheckBox;
    RadioButton5: TCheckBox;
    RadioButton6: TCheckBox;
    RadioButton7: TCheckBox;
    ebPorts: TEdit;
    Label7: TLabel;
    ebGates: TEdit;
    Label8: TLabel;
    ebBays: TEdit;
    Label9: TLabel;
    ebCheckIns: TEdit;
    Label10: TLabel;
    btOK: TButton;
    gbDaysExcept: TGroupBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    btNew: TButton;
    btDelete: TButton;
    lbFlightNumber: TLabel;
    Button1: TButton;
    lbFlightNum: TLabel;
    procedure FormShow(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btNewClick(Sender: TObject);
    procedure ebRuleNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    	oTTRule : cTTRule;
		function	CheckError : boolean;
		procedure	Clear;
		procedure	SetCheckBoxes( group : TGroupBox; vals : int );
		function	GetCheckBoxes( group : TGroupBox ) : int;
		procedure	CollectFieldValues;
        procedure	SetTTRule( rule : cTTRule );
  public
		property	TTRule : cTTRule read oTTRule  write SetTTRule;   // how form knows which rule to edit
  		end;

var
  fRuleEdit: TfRuleEdit;

implementation
{$R *.dfm}

uses
	uFlight, uUtils, uFidsTags;


procedure	TfRuleEdit.SetCheckBoxes( group : TGroupBox; vals : int );

	var                  // relies on check box tags set to 2^n values
        x : int;
        c : TControl;
        cb : TCheckBox;
	begin                // sets the 7 day check values from a single value
    for x := 0 to group.ControlCount - 1 do  begin
        c := group.Controls[ x ];
        if c is TCheckBox then  begin
            cb := TCheckBox( c );
            cb.Checked := vals and cb.Tag <> 0;
            end;
	    end;
    end;


function	TfRuleEdit.GetCheckBoxes( group : TGroupBox ) : int;

	var                  // relies on check box tags set to 2^n values
        x : int;
        c : TControl;
        cb : TCheckBox;
	begin              // read the 7 day check values into a single value
    result := 0;
    for x := 0 to group.ControlCount - 1 do  begin
        c := group.Controls[ x ];
        if c is TCheckBox then  begin
            cb := TCheckBox( c );
            if cb.Checked then  result := result or cb.Tag;
            end;
	    end;
    end;


function	TfRuleEdit.CheckError : boolean;

	begin             // produces crude error messages from error number - true is NO errors
    result := true;
	if oTTRule.Error <> teNone then  begin
    	ShowMessage( 'ERROR ' + EnumToStr( Ord( oTTRule.Error ), TypeInfo( aTTValidationError ) ) );
        result := false;
    	end;
    end;


procedure	TfRuleEdit.CollectFieldValues;

	begin            // feed all fields into cTTRules for validation and global update of changes
                     // should be automated with data dictionary ?
    oTTRule.Error := teNone;
    oTTRule.Presentation[ tfPath ] := ComboBox1.Items[ ComboBox1.ItemIndex ];
    oTTRule.Presentation[ tfRuleName ] := ebRuleName.Text;
    oTTRule.Presentation[ tfTime ] := ebTime.Text;
    oTTRule.Presentation[ tfDateStart ] := ebStart.Text;
    oTTRule.Presentation[ tfDateEnd ] := ebEnd.Text;
    oTTRule.Presentation[ tfDateException ] := ebExcept.Text;
    //mTTRule.oTemplate.Presentation[ ffFlight ] := ebFlightNumber.Text;     derrived from rule name
    oTTRule.oTemplate.Presentation[ ffPorts ] := ebPorts.Text;
    oTTRule.oTemplate.Presentation[ ffGates ] := ebGates.Text;
    oTTRule.oTemplate.Presentation[ ffBays ] := ebBays.Text;
    oTTRule.oTemplate.Presentation[ ffCheckIns ] := ebCheckIns.Text;

    oTTRule.Presentation[ tfDays ] := IntToHex( GetCheckBoxes( gbDays ), 2 );
    oTTRule.Presentation[ tfDaysExcept ] := IntToHex( GetCheckBoxes( gbDaysExcept ), 2 );
    end;


procedure TfRuleEdit.ebRuleNameChange(Sender: TObject);

    begin        // new flight name is derrived from rule name ?
    lbFlightNum.Caption := StripTail( ebRuleName.Text );
    end;


procedure	TfRuleEdit.btDeleteClick(Sender: TObject);

	begin          // delete a rule
    oTTRule.Delete;
    if CheckError() then  ModalResult := mrOK;
	end;


procedure	TfRuleEdit.btNewClick(Sender: TObject);

    begin           // ceate a new rule in timetable
    oTTRule.DbNode := nil;
    oTTRule.oTemplate.DBNode := nil;
    CollectFieldValues;
    if CheckError() then  begin
        oTTRule.New;
        if CheckError() then  ModalResult := mrOK;
    	end;
    end;


procedure	TfRuleEdit.btOKClick(Sender: TObject);

    begin                   // ok finished edits so update any changed fields
    CollectFieldValues;
    if CheckError() then  ModalResult := mrOK;
    end;


procedure	TfRuleEdit.Clear;

	begin
    ebRuleName.Text := '';
    ebTime.Text := '';
    ebStart.Text := '';
    ebEnd.Text := '';
    ebExcept.Text := '';
    lbFlightNum.Caption := '';
    ebPorts.Text := '';
    ebGates.Text := '';
    ebBays.Text := '';
    ebCheckIns.Text := '';
    SetCheckBoxes( gbDays, 0 );
    SetCheckBoxes( gbDaysExcept, 0 );
    end;


procedure TfRuleEdit.FormCreate(Sender: TObject);

	var
    	kind : aFlightKind;
    begin                         // init flight kind drop down
    for kind := Succ( Low( aFlightKind )) to High( aFlightKind ) do  begin
        ComboBox1.Items.Add( EnumToStr( Ord( kind ), TypeInfo( aFlightKind ) ) );  // Arrivals etc
    	end;
    end;


procedure	TfRuleEdit.FormShow(Sender: TObject);

	var
    	kind, s : string;
        x, days : int;
	begin         // initialize various edit controls
    if oTTRule <> nil then  begin
        kind := oTTRule.Presentation[ tfPath ];   // set kind selector
        for x := 0 to ComboBox1.Items.Count - 1 do  begin
        	if kind = ComboBox1.Items[ x ] then  begin
                ComboBox1.ItemIndex := x;
                break;
            	end;
        	end;
        ebRuleName.Text := oTTRule.Presentation[ tfRuleName ];
        ebTime.Text := oTTRule.Presentation[ tfTime ];
        ebStart.Text := oTTRule.Presentation[ tfDateStart ];
        ebEnd.Text := oTTRule.Presentation[ tfDateEnd ];
        ebExcept.Text := oTTRule.Presentation[ tfDateException ];
        lbFlightNum.Caption := StripTail( oTTRule.Presentation[ tfRuleName ] );
        ebPorts.Text := oTTRule.oTemplate.Presentation[ ffPorts ];
        ebGates.Text := oTTRule.oTemplate.Presentation[ ffGates ];
        ebBays.Text := oTTRule.oTemplate.Presentation[ ffBays ];
        ebCheckIns.Text := oTTRule.oTemplate.Presentation[ ffCheckIns ];

        s := oTTRule.Presentation[ tfDays ];
        x := 1;  days := GetHex( s, x );
        SetCheckBoxes( gbDays, days );

        s := oTTRule.Presentation[ tfDaysExcept ];
        x := 1;  days := GetHex( s, x );
        SetCheckBoxes( gbDaysExcept, days );
    	end
    else  Clear;
	end;


procedure	TfRuleEdit.SetTTRule( rule : cTTRule );

	begin             // tells form which rule to adjust
    if oTTRule = nil then
    	oTTRule := cTTRule.Create( rule.DB, rule.ReqID );

    oTTRule.DbNode := rule.DbNode;  // need own object since vst calls GetText a lot
    oTTRule.oTemplate.DBNode := rule.oTemplate.DBNode;
    end;


end.

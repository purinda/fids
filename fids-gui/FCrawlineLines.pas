unit FCrawlineLines;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, VirtualTrees, ComCtrls, ToolWin, ExtCtrls, uController, uCommon,
    uXmlParser, StdCtrls, uUtils, uPoller, uPacket, uFlight,
    uGlobalDefs,
    uGT, uMessageHub, udbTree, uMirrorDB, uFidsTags,
    uFIDSXml,  Math, uTTRules;

type
    // UDC Datastructure for UDC and Crawling line management
    PUDC = ^TUDC;

    TUDC = record
        IP: String;
        Location: String;
        Group: String;
        Status: String;
        Text: String;
        Blinking: String;
        Crawling: String;
        MsgType: String;
        Background: String;
        Foreground: String;
        GroupHeader: Boolean;
        DBPath: String;
    end;
    TUDCs = array of TUDC;


    TFCrawlineLinesAllocator = class(TForm)
        VST: TVirtualStringTree;
        ControlBar1: TControlBar;
        ToolBar1: TToolBar;
        ToolButton2: TToolButton;
        ToolButton3: TToolButton;
        ToolButton4: TToolButton;
        ToolButton5: TToolButton;
        ToolButton6: TToolButton;
        procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
        procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
          var NodeDataSize: Integer);
        procedure VSTPaintText(Sender: TBaseVirtualTree;
          const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          TextType: TVSTTextType);
        procedure UpdateVST();
    procedure FormShow(Sender: TObject);
    procedure VSTDblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ToolButton6Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);

    private
        { Private declarations }
    public
        FXml: TFIDSxml;
        { Public declarations }
    end;

var
    FCrawlineLinesAllocator: TFCrawlineLinesAllocator;
    fcWindow: CFlightController;
    FlightFields: array of aFlightField;

implementation

uses CrawlingEdit, uConnection;

{$R *.dfm}

procedure TFCrawlineLinesAllocator.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
    Hide;
    CanClose := false;
end;

procedure TFCrawlineLinesAllocator.FormShow(Sender: TObject);
begin
    UpdateVST();

    // Header width
    VST.Header.Columns.Items[0].Width := 120;
    VST.Header.Columns.Items[1].Width := 60;
    VST.Header.Columns.Items[2].Width := 160;
    VST.Header.Columns.Items[3].Width := 70;
    VST.Header.Columns.Items[4].Width := 70;
    VST.Header.Columns.Items[5].Width := 40;
    VST.Header.Columns.Items[6].Width := 40;

end;

procedure TFCrawlineLinesAllocator.ToolButton5Click(Sender: TObject);
var
    CurrentNodeData: PUDC;
    Req : String;
begin
    if (VST.SelectedCount <= 0) then
        Exit;

    if (VST.GetFirstSelected().ChildCount > 0) then
        Exit;


    //Get the current node details
    CurrentNodeData := VST.GetNodeData( vst.GetFirstSelected() );

    if not('Inactive' = CurrentNodeData.Status) then
    begin
        req := FormatEditRequest(CurrentNodeData.DBPath + 'Status|', 'Inactive', CurrentNodeData.Status, 'Feed');
        DB.BroadcastRequest(req );
    end;

    // Redraw everything
    UpdateVST;

end;

procedure TFCrawlineLinesAllocator.ToolButton6Click(Sender: TObject);
var
    CurrentNodeData: PUDC;
    Req : String;
begin
    if (VST.SelectedCount <= 0) then
        Exit;

    //Get the current node details
    CurrentNodeData := VST.GetNodeData( vst.GetFirstSelected() );

    if not('Active' = CurrentNodeData.Status) then
    begin
        req := FormatEditRequest(CurrentNodeData.DBPath + 'Status|', 'Active', CurrentNodeData.Status, 'Feed');
        DB.BroadcastRequest(req);
    end;

    // Redraw everything
    UpdateVST;


end;

procedure TFCrawlineLinesAllocator.UpdateVST();
var
    Items: TUDCs;
    I, J, K: Integer;
    frame,page, udc: apNode;
    TableData, PDataRecord, tmp: PUDC;
    ChildNode, ParentNode, tmpLoopNode: PVirtualNode;
    AddAsChild :Boolean;
begin

    vst.BeginUpdate;
    vst.Clear;

    FXml := FXml.Instance;
    SetLength(Items, 20);

    K:=0;
    // Set number of UDCs to store data
    for i := 0 to DB.GetNode(fidsDisplayConfigPath).SubNodes.Count - 1 do
    begin

        frame := FXml.GetDataTree.GetNode(fidsDisplayConfigPath).SubNodes.Items[i];
        //ShowMessage(fidsDisplayConfigPath+'|'+frame.NodeName +'|Devices');
        for j := 0 to DB.GetNode(fidsDisplayConfigPath+'|'+frame.NodeName +'|Devices').SubNodes.Count - 1 do
        begin
            page := FXml.GetDataTree.GetNode(fidsDisplayConfigPath+'|'+frame.NodeName+'|Devices').SubNodes.Items[j];
            udc := page.SubNodes.Items[0];

            // Assign values to UDC structures
            Items[k].IP := apNode(udc.SubNodes.Items[0]).Content;
            Items[k].Location := apNode(udc.SubNodes.Items[1]).Content;
            Items[k].Text := apNode(udc.SubNodes.Items[2]).Content;
            Items[k].Group := apNode(udc.SubNodes.Items[3]).Content;
            Items[k].Status := apNode(udc.SubNodes.Items[4]).Content;
            Items[k].Blinking := apNode(udc.SubNodes.Items[5]).Content;
            Items[k].Crawling := apNode(udc.SubNodes.Items[6]).Content;
            Items[k].MsgType := apNode(udc.SubNodes.Items[7]).Content;
            Items[k].Background := apNode(udc.SubNodes.Items[8]).Content;
            Items[k].Foreground := apNode(udc.SubNodes.Items[9]).Content;

            Items[k].DBPath := ResolvePathStr( apNode(udc) );
            inc(k);
        end;
    end;


    // Populate the VST
    for i := 0 to k-1 do
    begin
        try
            // Add first group and first item
            if (i = 0) then
            begin
                Items[i].GroupHeader := true;
                ParentNode := VST.AddChild(nil);
                TableData := VST.GetNodeData(ParentNode);
                TableData^ := Items[I];

                Items[i].GroupHeader := false;
                ChildNode := VST.AddChild(ParentNode);
                TableData := VST.GetNodeData(ChildNode);
                TableData^ := Items[I];
            end
            else
            begin

                AddAsChild := false;
                ///////////////////////////////////////////////
                tmpLoopNode := VST.GetFirst();
                while assigned(tmpLoopNode) do
                begin
                    tmp := VST.GetNodeData(tmpLoopNode);
                    tmpLoopNode := VST.GetNext(tmpLoopNode);

                    if (tmp.Group = Items[i].Group) then
                    begin
                        AddAsChild := true;
                        tmpLoopNode := VST.GetPrevious(tmpLoopNode);
                        break;
                    end
                end;

                if (AddAsChild) then
                begin
                    Items[i].GroupHeader := false;
                    ChildNode := VST.AddChild(tmpLoopNode);
                    TableData := VST.GetNodeData(ChildNode);
                    TableData^ := Items[I];
                end
                else
                begin
                    Items[i].GroupHeader := true;
                    ParentNode := VST.AddChild(nil);
                    TableData := VST.GetNodeData(ParentNode);
                    TableData^ := Items[I];

                    Items[i].GroupHeader := false;
                    ChildNode := VST.AddChild(ParentNode);
                    TableData := VST.GetNodeData(ChildNode);
                    TableData^ := Items[I];
                end;
                ///////////////////////////////////////////////

            end;

            VST.Expanded[ParentNode] := True;
        except
            // DO NOTHING; EXPECTED ISSUE
            // beep;
        end;

    end;

    vst.EndUpdate;
end;


procedure TFCrawlineLinesAllocator.VSTDblClick(Sender: TObject);
var
    CurrentNodeData: PUDC;
    Req : String;
begin
    if (vst.SelectedCount <= 0) then
        Exit;

    //Get the current node details
    CurrentNodeData := VST.GetNodeData( vst.GetFirstSelected() );

    FCrawlingLineEdit.txtIP.Text := CurrentNodeData.IP;
    FCrawlingLineEdit.txtMsg.Text := CurrentNodeData.Text;
    FCrawlingLineEdit.cmbLocation.Text := CurrentNodeData.Location;
    FCrawlingLineEdit.cmbGroup.Text := CurrentNodeData.Group;
    FCrawlingLineEdit.cmbStatus.Text := CurrentNodeData.Status;
    FCrawlingLineEdit.cmbBlinking.Text := CurrentNodeData.Blinking;
    FCrawlingLineEdit.cmbCrawling.Text := CurrentNodeData.Crawling;
    FCrawlingLineEdit.cmbMsgType.Text := CurrentNodeData.MsgType;
    FCrawlingLineEdit.cmbBg.Text := CurrentNodeData.Background;
    FCrawlingLineEdit.cmbFg.Text := CurrentNodeData.Foreground;

    // ShowMessage(CurrentNodeData.DBPath);
    // Set current crawling line details
    if (FCrawlingLineEdit.ShowModal = mrOk) then
    begin
        if not(FCrawlingLineEdit.txtIP.Text = CurrentNodeData.IP) then
        begin
            req := FormatEditRequest(CurrentNodeData.DBPath + 'IPs|', FCrawlingLineEdit.txtIP.Text, CurrentNodeData.IP, 'Feed');
            DB.BroadcastRequest(req);
        end;

        if not(FCrawlingLineEdit.cmbLocation.Text = CurrentNodeData.Location) then
        begin
            req := FormatEditRequest(CurrentNodeData.DBPath + 'Location|', FCrawlingLineEdit.cmbLocation.Text, CurrentNodeData.Location, 'Feed');
            DB.BroadcastRequest(req);
        end;

        if not(FCrawlingLineEdit.txtMsg.Text = CurrentNodeData.Text) then
        begin
            req := FormatEditRequest(CurrentNodeData.DBPath + 'Message|', FCrawlingLineEdit.txtMsg.Text, CurrentNodeData.Text, 'Feed');
            DB.BroadcastRequest(req);
        end;

        if not(FCrawlingLineEdit.cmbGroup.Text = CurrentNodeData.Group) then
        begin
            req := FormatEditRequest(CurrentNodeData.DBPath + 'Group|', FCrawlingLineEdit.cmbGroup.Text, CurrentNodeData.Group, 'Feed');
            DB.BroadcastRequest(req);
        end;


        if not(FCrawlingLineEdit.cmbStatus.Text = CurrentNodeData.Status) then
        begin
            req := FormatEditRequest(CurrentNodeData.DBPath + 'Status|', FCrawlingLineEdit.cmbStatus.Text, CurrentNodeData.Status, 'Feed');
            DB.BroadcastRequest(req);
        end;


        if not(FCrawlingLineEdit.cmbBlinking.Text = CurrentNodeData.Blinking) then
        begin
            req := FormatEditRequest(CurrentNodeData.DBPath + 'Blinking|', FCrawlingLineEdit.cmbBlinking.Text, CurrentNodeData.Blinking, 'Feed');
            DB.BroadcastRequest(req);
        end;

        if not(FCrawlingLineEdit.cmbCrawling.Text = CurrentNodeData.Crawling) then
        begin
            req := FormatEditRequest(CurrentNodeData.DBPath + 'Crawling|', FCrawlingLineEdit.cmbCrawling.Text, CurrentNodeData.Crawling, 'Feed');
            DB.BroadcastRequest(req);
        end;

        if not(FCrawlingLineEdit.cmbFg.Text = CurrentNodeData.Foreground) then
        begin
            req := FormatEditRequest(CurrentNodeData.DBPath + 'Foreground|', FCrawlingLineEdit.cmbFg.Text, CurrentNodeData.Foreground, 'Feed');
            DB.BroadcastRequest(req);
        end;

        if not(FCrawlingLineEdit.cmbBg.Text = CurrentNodeData.Background) then
        begin
            req := FormatEditRequest(CurrentNodeData.DBPath + 'Background|', FCrawlingLineEdit.cmbBg.Text, CurrentNodeData.Background, 'Feed');
            DB.BroadcastRequest(req);
        end;

        // Redraw everything
        UpdateVST;
    end;

end;

procedure TFCrawlineLinesAllocator.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
    NodeDataSize := SizeOf(TUDC);
end;

procedure TFCrawlineLinesAllocator.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
    Data: PUDC;
begin
    Data := VST.GetNodeData(Node);

    if (data.GroupHeader=true) then
    begin
        case Column of
            0:
                CellText := Data^.Group;
            1:
                CellText := '';
            2:
                CellText := '';
            3:
                CellText := '';
            4:
                CellText := '';
            5:
                CellText := '';
            6:
                CellText := '';
            7:
                CellText := '';
        end;
    end
    else
    begin
        case Column of
            0:
                CellText := Data^.IP;
            1:
                CellText := Data^.Status;
            2:
                CellText := Data^.Text;
            3:
                CellText := Data^.Blinking;
            4:
                CellText := Data^.Crawling;
            5:
                CellText := Data^.MsgType;
            6:
                CellText := Data^.Background;
            7:
                CellText := Data^.Foreground;
        end;
    end;


end;

procedure TFCrawlineLinesAllocator.VSTPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
Var
    Data: PUDC;
begin
    Data := VST.GetNodeData(Node);

    try
        if LowerCase(VST.Header.Columns.Items[Column].Text) = 'status' then
        begin

            if LowerCase(Data^.Status) = 'inactive' then
            begin
                TargetCanvas.Font.Color := clRed;
                TargetCanvas.Font.Style := [fsBold];
            end;

            if LowerCase(Data^.Status) = 'active' then
            begin
                TargetCanvas.Font.Color := clBlue;
                TargetCanvas.Font.Style := [fsBold];
            end;

            if LowerCase(Data^.Status) = 'mixed' then
            begin
                TargetCanvas.Font.Color := rgb(244, 164, 96);
                TargetCanvas.Font.Style := [fsBold];
            end;

        end;
    except

    end;

end;

end.

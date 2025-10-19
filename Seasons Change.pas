{
  Generate Seasons.
}
unit Seasons;

// ----------------------------------------------------
//Create variables that will need to be used accross multiple functions/procedures.
// ----------------------------------------------------

var
    bSaveLandHeights, bCreateLandscapeHeights, bCreateLandscapeSnowMeshes, bPlaceLandscapeSnow, bCreateTestPlugin, bUserAlterLandRulesChanged,
    bLoadPreviousLandHeights, bSaveUserRules: boolean;
    uiScale: integer;
    sIgnoredWorldspaces: string;

    SeasonsMainFile: IwbFile;
    statGroup, scolGroup: IwbGroupRecord;
    flatSnowStatic: IwbElement;

    slPluginFiles, slLandscapeCells: TStringList;
    tlLandRecords, tlBasesThatAlterLand, tlStats, tlWinterDecals: TList;
    joWinningCells, joSeasons, joLandscapeHeights, joLandscapeHeightsAltered, joLandFiles, joAlterLandRules, joUserAlterLandRules, joLoadOrderFormIDFileID: TJsonObject;

    lvAlterLandRules: TListView;
    btnAlterLandRuleOk, btnAlterLandRuleCancel: TButton;

const
    sSeasonsFileName = 'Seasons.esm';
    SCALE_FACTOR_TERRAIN = 8;
    bsSCALE_FACTOR_TERRAIN = 3;
    // epsilon to use for CompareValue calls
    // between positions and rotations, positions have more significant decimals (6), so this is set
    // to compliment that
    EPSILON = 0.000001;

// ----------------------------------------------------
// Main functions and procedures go immediately below.
// ----------------------------------------------------

procedure CreateObjects;
{
    Creates objects.
}
begin

    joSeasons := TJsonObject.Create;
    joLandscapeHeights := TJsonObject.Create;
    joLandscapeHeightsAltered := TJsonObject.Create;
    joLandFiles := TJsonObject.Create;
    joAlterLandRules := TJsonObject.Create;
    joUserAlterLandRules := TJsonObject.Create;
    joWinningCells := TJsonObject.Create;
    joLoadOrderFormIDFileID := TJsonObject.Create;

    tlLandRecords := TList.Create;
    tlBasesThatAlterLand := TList.Create;
    tlStats := TList.Create;
    tlWinterDecals := TList.Create;

    slPluginFiles := TStringList.Create;
    slLandscapeCells := TStringList.Create;
end;

function Finalize: integer;
{
    This function is called at the end.
}
begin

    joSeasons.Free;
    joAlterLandRules.Free;
    joLandFiles.Free;
    joLandscapeHeights.Free;
    joLandscapeHeightsAltered.Free;
    joWinningCells.Free;
    joLoadOrderFormIDFileID.Free;

    tlLandRecords.Free;
    tlBasesThatAlterLand.Free;
    tlStats.Free;
    tlWinterDecals.Free;

    slPluginFiles.Free;
    slLandscapeCells.Free;

    if bSaveUserRules and bUserAlterLandRulesChanged then begin
        AddMessage('Saving ' + IntToStr(joUserAlterLandRules.Count) + ' object snow alteration user rule(s) to ' + wbDataPath + 'Seasons\AlterLandUserRules.json');
        joUserAlterLandRules.SaveToFile(wbDataPath + 'Seasons\AlterLandUserRules.json', False, TEncoding.UTF8, True);
    end;
    joUserAlterLandRules.Free;
    Result := 0;
end;

function Initialize: integer;
{
  This function is called at the beginning.
}
begin
    Result := 0;
    //Globals
    bSaveLandHeights := False;

    //Gui settings
    bCreateLandscapeHeights := False;
    bCreateLandscapeSnowMeshes := False;
    bPlaceLandscapeSnow := False;
    bCreateTestPlugin := False;
    bLoadPreviousLandHeights := False;

    //Rules
    bSaveUserRules := False;
    bUserAlterLandRulesChanged := False;

    //Get scaling
    uiScale := Screen.PixelsPerInch * 100 / 96;
    AddMessage('UI scale: ' + IntToStr(uiScale));

    CreateObjects;
    FetchRules;

    if not MainMenuForm then begin
        Finalize;
        Result := 1;
        Exit;
    end;
    if bCreateTestPlugin then CreatePlugin;

    // if bLoadPreviousLandHeights then begin
    //     if FileExists(wbScriptsPath + 'Seasons\LandHeights.json') then
    //         joLandscapeHeights.LoadFromFile(wbScriptsPath + 'Seasons\LandHeights.json');
    // end
    // else if FileExists(wbScriptsPath + 'Seasons\LandHeightsPreAlteration.json') then
    //     joLandscapeHeights.LoadFromFile(wbScriptsPath + 'Seasons\LandHeightsPreAlteration.json');

    EnsureDirectoryExists(wbScriptsPath + 'Seasons\output\Meshes\LandscapeSnow');
    EnsureDirectoryExists(wbScriptsPath + 'Seasons\output\Meshes\LOD\LandscapeSnow');
    CollectRecords;
    if not bLoadPreviousLandHeights then begin
        // AlterLandHeightsForTheseBases;
        ApplyAlterations;
        FixLandscapeSeams;
    end;
    // ProcessLandRecords;
    // ProcessStats;
    // CreateWinterDecals;
end;

// ----------------------------------------------------
// UI functions and procedures go below.
// ----------------------------------------------------

function MainMenuForm: Boolean;
{
  Main menu form.
}
var
    btnStart, btnCancel, btnRuleEditor: TButton;
    frm: TForm;
    gbOptions: TGroupBox;
    fImage: TImage;
    pnl: TPanel;
    picSeasons: TPicture;
    chkCreateTestPlugin, chkCreateLandscapeHeights, chkCreateLandscapeSnowMeshes, chkPlaceLandscapeSnow, chkLoadLastLandHeights: TCheckBox;
begin
    Result := False;
    frm := TForm.Create(nil);
    try
        frm.Caption := 'Seasons Change';
        frm.Width := 600;
        frm.Height := 480;
        frm.Position := poMainFormCenter;
        frm.BorderStyle := bsDialog;
        frm.KeyPreview := True;
        frm.OnKeyDown := FormKeyDown;

        picSeasons := TPicture.Create;
        picSeasons.LoadFromFile(wbScriptsPath + 'Seasons\Seasons Change.jpg');

        fImage := TImage.Create(frm);
        fImage.Picture := picSeasons;
        fImage.Parent := frm;
        fImage.Width := 549;
        fImage.Height := 300;
        fImage.Left := (frm.Width - fImage.Width)/2;
        fImage.Top := 12;
        fImage.Stretch := True;

        gbOptions := TGroupBox.Create(frm);
        gbOptions.Parent := frm;
        gbOptions.Left := 10;
        gbOptions.Top := fImage.Top + fImage.Height + 24;
        gbOptions.Width := frm.Width - 30;
        gbOptions.Caption := 'Seasons Change';
        gbOptions.Height := 94;

        chkCreateTestPlugin := TCheckBox.Create(gbOptions);
        chkCreateTestPlugin.Parent := gbOptions;
        chkCreateTestPlugin.Left := 16;
        chkCreateTestPlugin.Top := 16;
        chkCreateTestPlugin.Width := 120;
        chkCreateTestPlugin.Caption := 'Create Test Plugin';
        chkCreateTestPlugin.Hint := 'Creates a test plugin.';
        chkCreateTestPlugin.ShowHint := True;

        chkCreateLandscapeHeights := TCheckBox.Create(gbOptions);
        chkCreateLandscapeHeights.Parent := gbOptions;
        chkCreateLandscapeHeights.Left := chkCreateTestPlugin.Left + chkCreateTestPlugin.Width + 16;
        chkCreateLandscapeHeights.Top := chkCreateTestPlugin.Top;
        chkCreateLandscapeHeights.Width := 170;
        chkCreateLandscapeHeights.Caption := 'Force Recreate Land Heights';
        chkCreateLandscapeHeights.Hint := 'Forces all land height values to be recalculated.';
        chkCreateLandscapeHeights.ShowHint := True;

        chkLoadLastLandHeights := TCheckBox.Create(gbOptions);
        chkLoadLastLandHeights.Parent := gbOptions;
        chkLoadLastLandHeights.Left := chkCreateLandscapeHeights.Left + chkCreateLandscapeHeights.Width + 16;
        chkLoadLastLandHeights.Top := chkCreateLandscapeHeights.Top;
        chkLoadLastLandHeights.Width := 170;
        chkLoadLastLandHeights.Caption := 'Load Last Land Heights';
        chkLoadLastLandHeights.Hint := 'Skips generating alterations to land heights, using last Land Heights json.';
        chkLoadLastLandHeights.ShowHint := True;

        chkCreateLandscapeSnowMeshes := TCheckBox.Create(gbOptions);
        chkCreateLandscapeSnowMeshes.Parent := gbOptions;
        chkCreateLandscapeSnowMeshes.Left := 16;
        chkCreateLandscapeSnowMeshes.Top := chkLoadLastLandHeights.Top + 24;
        chkCreateLandscapeSnowMeshes.Width := 170;
        chkCreateLandscapeSnowMeshes.Caption := 'Force Recreate Snow Models';
        chkCreateLandscapeSnowMeshes.Hint := 'Forces recreation of all snow models.';
        chkCreateLandscapeSnowMeshes.ShowHint := True;

        chkPlaceLandscapeSnow := TCheckBox.Create(gbOptions);
        chkPlaceLandscapeSnow.Parent := gbOptions;
        chkPlaceLandscapeSnow.Left := chkCreateLandscapeSnowMeshes.Left + chkCreateLandscapeSnowMeshes.Width + 16;
        chkPlaceLandscapeSnow.Top := chkCreateLandscapeSnowMeshes.Top;
        chkPlaceLandscapeSnow.Width := 100;
        chkPlaceLandscapeSnow.Caption := 'Place Snow';
        chkPlaceLandscapeSnow.Hint := 'Place snow.';
        chkPlaceLandscapeSnow.ShowHint := True;

        btnRuleEditor := TButton.Create(frm);
        btnRuleEditor.Parent := frm;
        btnRuleEditor.Caption := 'Rules';
        btnRuleEditor.OnClick := AlterLandRuleEditor;
        btnRuleEditor.Width := 100;
        btnRuleEditor.Left := 16;
        btnRuleEditor.Top := gbOptions.Top + gbOptions.Height + 24;

        btnStart := TButton.Create(frm);
        btnStart.Parent := frm;
        btnStart.Caption := 'Start';
        btnStart.ModalResult := mrOk;
        btnStart.Top := gbOptions.Top + gbOptions.Height + 24;

        btnCancel := TButton.Create(frm);
        btnCancel.Parent := frm;
        btnCancel.Caption := 'Cancel';
        btnCancel.ModalResult := mrCancel;
        btnCancel.Top := btnStart.Top;

        btnStart.Left := gbOptions.Width - btnStart.Width - btnCancel.Width - 16;
        btnCancel.Left := btnStart.Left + btnStart.Width + 8;

        pnl := TPanel.Create(frm);
        pnl.Parent := frm;
        pnl.Left := gbOptions.Left;
        pnl.Top := btnStart.Top - 12;
        pnl.Width := gbOptions.Width;
        pnl.Height := 2;

        frm.ActiveControl := btnStart;
        frm.ScaleBy(uiScale, 100);
        frm.Font.Size := 8;
        frm.Height := btnStart.Top + btnStart.Height + btnStart.Height + 30;

        chkCreateTestPlugin.Checked := bCreateTestPlugin;
        chkCreateLandscapeHeights.Checked := bCreateLandscapeHeights;
        chkCreateLandscapeSnowMeshes.Checked := bCreateLandscapeSnowMeshes;
        chkPlaceLandscapeSnow.Checked := bPlaceLandscapeSnow;
        chkLoadLastLandHeights.Checked := bLoadPreviousLandHeights;

        if frm.ShowModal <> mrOk then Exit;

        bCreateTestPlugin := chkCreateTestPlugin.Checked;
        bCreateLandscapeHeights := chkCreateLandscapeHeights.Checked;
        bCreateLandscapeSnowMeshes := chkCreateLandscapeSnowMeshes.Checked;
        bPlaceLandscapeSnow := chkPlaceLandscapeSnow.Checked;
        bLoadPreviousLandHeights := chkLoadLastLandHeights.Checked;
        Result := True;
    finally
        frm.Free;
    end;
end;

procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{
    Cancel if Escape key is pressed.
}
begin
    if Key = VK_ESCAPE then TForm(Sender).ModalResult := mrCancel;
end;

procedure frmOptionsFormClose(Sender: TObject; var Action: TCloseAction);
{
    Close form handler.
}
begin
    if TForm(Sender).ModalResult <> mrOk then Exit
    else bSaveUserRules := True;
end;

function AlterLandRuleEditor: Boolean;
var
    i: integer;
    mnRules: TPopupMenu;
    MenuItem: TMenuItem;
    lbl: TLabel;
    frm: TForm;
begin
    Result := False;
    frm := TForm.Create(nil);
    try
        frm.Caption := 'Rule Editor';
        frm.Width := 750;
        frm.Height := 600;
        frm.Position := poMainFormCenter;
        frm.BorderStyle := bsSizeable;
        frm.KeyPreview := True;
        frm.OnClose := frmOptionsFormClose;
        frm.OnKeyDown := FormKeyDown;
        frm.OnResize := frmAlterLandResize;

        lvAlterLandRules := TListView.Create(frm);
        lvAlterLandRules.Parent := frm;
        lvAlterLandRules.Name := 'ListView';

        lvAlterLandRules.Top := 24;
        lvAlterLandRules.Width := frm.Width - 36;
        lvAlterLandRules.Left := (frm.Width - lvAlterLandRules.Width)/2;
        lvAlterLandRules.Height := frm.Height - 110;
        lvAlterLandRules.ReadOnly := True;
        lvAlterLandRules.ViewStyle := vsReport;
        lvAlterLandRules.RowSelect := True;
        lvAlterLandRules.DoubleBuffered := True;
        lvAlterLandRules.Columns.Add.Caption := 'EditorID';
        lvAlterLandRules.Columns[0].Width := 350;
        lvAlterLandRules.Columns.Add.Caption := 'ID';
        lvAlterLandRules.Columns[1].Width := 275;
        lvAlterLandRules.Columns.Add.Caption := 'Alteration';
        lvAlterLandRules.Columns[2].Width := 80;
        lvAlterLandRules.OwnerData := True;
        lvAlterLandRules.OnData := lvAlterLandRulesData;
        lvAlterLandRules.OnDblClick := lvAlterLandRulesDblClick;
        lvAlterLandRules.Items.Count := joAlterLandRules.Count;
        CreateLabel(frm, 16, lvAlterLandRules.Top - 20, 'Object Landscape Snow Alterations');

        mnRules := TPopupMenu.Create(frm);
        lvAlterLandRules.PopupMenu := mnRules;
        MenuItem := TMenuItem.Create(mnRules);
        MenuItem.Caption := 'Add';
        MenuItem.OnClick := AlterLandRulesMenuAddClick;
        mnRules.Items.Add(MenuItem);
        MenuItem := TMenuItem.Create(mnRules);
        MenuItem.Caption := 'Delete';
        MenuItem.OnClick := AlterLandRulesMenuDeleteClick;
        mnRules.Items.Add(MenuItem);
        MenuItem := TMenuItem.Create(mnRules);
        MenuItem.Caption := 'Edit';
        MenuItem.OnClick := AlterLandRulesMenuEditClick;
        mnRules.Items.Add(MenuItem);

        btnAlterLandRuleOk := TButton.Create(frm);
        btnAlterLandRuleOk.Parent := frm;
        btnAlterLandRuleOk.Name := 'OK';
        btnAlterLandRuleOk.Caption := 'OK';
        btnAlterLandRuleOk.ModalResult := mrOk;
        btnAlterLandRuleOk.Top := lvAlterLandRules.Height + lvAlterLandRules.Top + 8;

        btnAlterLandRuleCancel := TButton.Create(frm);
        btnAlterLandRuleCancel.Parent := frm;
        btnAlterLandRuleCancel.Name := 'Cancel';
        btnAlterLandRuleCancel.Caption := 'Cancel';
        btnAlterLandRuleCancel.ModalResult := mrCancel;
        btnAlterLandRuleCancel.Top := btnAlterLandRuleOk.Top;

        btnAlterLandRuleOk.Left := (frm.Width - btnAlterLandRuleOk.Width - btnAlterLandRuleCancel.Width - 8)/2;
        btnAlterLandRuleCancel.Left := btnAlterLandRuleOk.Left + btnAlterLandRuleOk.Width + 8;

        frm.ScaleBy(uiScale, 100);
        frm.Font.Size := 8;

        if frm.ShowModal <> mrOk then Exit;

        Result := True;
    finally
        frm.Free;
    end;
end;

function EditAlterLandRuleForm(var key, edid: string; var alteration: integer; keyReadOnly: boolean): boolean;
var
    frmRule: TForm;
    pnl: TPanel;
    btnOk, btnCancel, btnReferences: TButton;
    edAlteration: TEdit;
    cbEditorID, cbKey: TComboBox;
begin
    Result := False;
    frmRule := TForm.Create(nil);
    try
        frmRule.Caption := 'Object Landscape Snow Alteration Rule';
        frmRule.Width := 600;
        frmRule.Height := 180;
        frmRule.Position := poMainFormCenter;
        frmRule.BorderStyle := bsDialog;
        frmRule.KeyPreview := True;
        frmRule.OnKeyDown := FormKeyDown;
        frmRule.OnClose := frmAlterLandRuleFormClose;

        cbEditorID := TComboBox.Create(frmRule);
        cbEditorID.Parent := frmRule;
        cbEditorID.Name := 'cbEditorID';
        cbEditorID.Left := 120;
        cbEditorID.Top := 12;
        cbEditorID.Width := frmRule.Width - 150;
        cbEditorID.Style := csDropDown;
        CreateLabel(frmRule, 16, cbEditorID.Top + 4, 'Editor ID');

        cbKey := TComboBox.Create(frmRule);
        cbKey.Parent := frmRule;
        cbKey.Name := 'cbKey';
        cbKey.Left := 120;
        cbKey.Top := cbEditorID.Top + 28;
        cbKey.Width := frmRule.Width - 150;

        cbKey.OnExit := KeyChange;
        cbKey.Style := csDropDown;
        CreateLabel(frmRule, 16, cbKey.Top + 4, 'ID');

        edAlteration := TEdit.Create(frmRule);
        edAlteration.Parent := frmRule;
        edAlteration.Name := 'edAlteration';
        edAlteration.Left := 120;
        edAlteration.Top := cbKey.Top + 28;
        edAlteration.Width := frmRule.Width - 150;
        edAlteration.OnKeyPress := alterationValidation;
        CreateLabel(frmRule, 16, edAlteration.Top + 4, 'Alteration');

        btnOk := TButton.Create(frmRule);
        btnOk.Parent := frmRule;
        btnOk.Name := 'OK';
        btnOk.Caption := 'OK';
        btnOk.ModalResult := mrOk;
        btnOk.Top := edAlteration.Top + (2 * edAlteration.Height);

        btnCancel := TButton.Create(frmRule);
        btnCancel.Parent := frmRule;
        btnCancel.Name := 'Cancel';
        btnCancel.Caption := 'Cancel';
        btnCancel.ModalResult := mrCancel;
        btnCancel.Top := btnOk.Top;

        btnReferences := TButton.Create(frmRule);
        btnReferences.Parent := frmRule;
        btnReferences.Name := 'btnReferences';
        btnReferences.Caption := 'References';
        btnReferences.OnClick := ReferenceRules;
        btnReferences.Top := btnOk.Top;
        btnReferences.Left := 16;

        btnOk.Left := frmRule.Width - btnOk.Width - btnCancel.Width - 32;
        btnCancel.Left := btnOk.Left + btnOk.Width + 8;

        pnl := TPanel.Create(frmRule);
        pnl.Parent := frmRule;
        pnl.Left := 10;
        pnl.Top := btnOk.Top - 12;
        pnl.Width := frmRule.Width - 32;
        pnl.Height := 2;

        frmRule.Height := btnOk.Top + (3 * btnOk.Height);
        frmRule.ScaleBy(uiScale, 100);
        frmRule.Font.Size := 8;

        cbEditorID.Items.Add(edid);
        cbEditorID.ItemIndex := 0;
        cbEditorID.Enabled := False;

        cbKey.Items.Add(key);
        cbKey.ItemIndex := 0;
        cbKey.Enabled := keyReadOnly;

        if SameText(cbKey.Text, '') then btnReferences.Enabled := False;

        edAlteration.Text := IntToStr(alteration);

        /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        if frmRule.ShowModal <> mrOk then Exit;
        /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        key := cbKey.Text;
        edid := cbEditorID.Text;
        alteration := StrToInt(edAlteration.Text);
        Result := True;
    finally
        frmRule.Free;
    end;
end;

procedure lvAlterLandRulesData(Sender: TObject; Item: TListItem);
{
    Populate lvRules
}
var
    i: integer;
    key: string;
begin
    key := joAlterLandRules.Names[Item.Index];
    Item.Caption := joAlterLandRules.O[key].S['editorid'];
    Item.SubItems.Add(key);
    Item.SubItems.Add(joAlterLandRules.O[key].S['alteration']);
end;

procedure alterationValidation(Sender: TObject; var Key: Char);
var
    validChars: string;

    edAlteration: TEdit;
begin
    if Key = #8 then Exit;
    validChars := '0123456789';
    if Pos(Key, validChars) > 0 then Exit;
    if Key <> '-' then Key := #0;
    edAlteration := TEdit(Sender);
    if (edAlteration.SelStart <> 0) or (Pos('-', edAlteration.Text) > 0) then
        Key := #0;
end;

procedure KeyChange(Sender: TObject);
var
    bSuccess: boolean;
    key, edid, fileName, fileFormId, recordId, baseRecordId: string;
    formid: cardinal;

    f: IwbFile;
    r, base: IwbElement;

    cbKey, cbEditorID: TComboBox;
    edAlteration: TEdit;
    frm: TForm;
    btnReferences: TButton;
begin
    bSuccess := False;
    cbKey := TComboBox(Sender);
    frm := GetParentForm(cbKey);
    cbEditorID := TComboBox(frm.FindComponent('cbEditorID'));
    edAlteration := TEdit(frm.FindComponent('edAlteration'));
    key := cbKey.Text;
    if ContainsText(key, ':') then begin
        try
            edid := EditorID(GetRecordFromFormIdFileId(key));
            bSuccess := True;
        except
            bSuccess := False;
        end;
    end else begin
        if IsValidHex(key) then begin
            formid := StrToInt('$' + key);
            f := GetFileFromLoadOrderFormID(formid);
            if Assigned(f) then begin
                fileName := GetFileName(f);
                fileFormId := IntToHex(LoadOrderFormIDtoFileFormID(f, key), 8);
                recordId := fileFormId + ':' + fileName;
                r := GetRecordFromFormIdFileId(recordId);
                if Assigned(r) then begin
                    if Pos(Signature(r),'REFR,PHZD') <> 0 then begin
                        base := LinksTo(ElementByPath(r, 'NAME'));
                        baseRecordId := RecordFormIdFileId(base);
                        cbKey.Text := baseRecordId;
                        edid := EditorID(base);
                        bSuccess := True;
                        if joAlterLandRules.Contains(baseRecordId) then begin
                            edAlteration.Text := joAlterLandRules.O[baseRecordId].S['alteration'];
                            ShowMessage('Found an existing base object rule for this reference.' + #13#10 + 'If you need to alter this reference differently from the base, use the References button to make a rule for it attached to this base.');
                        end else ShowMessage('Changed the formid to reference the base object.' + #13#10 + 'If you only want to modify this reference, set the alteration for this base object to 0,' + #13#10 + 'and then add your alteration via the References button.');
                    end else begin
                        cbKey.Text := recordId;
                        edid := EditorID(r);
                        bSuccess := True;
                    end;
                end;
            end;
        end;
    end;

    if bSuccess then begin
        cbEditorID.Text := edid;
        btnReferences := TButton(frm.FindComponent('btnReferences'));
        btnReferences.Enabled := true;
    end;
end;

procedure lvAlterLandRulesDblClick(Sender: TObject);
{
    Double click to edit rule
}
begin
    AlterLandRulesMenuEditClick(nil);
end;

procedure AlterLandRulesMenuAddClick(Sender: TObject);
{
    Add rule
}
var
    idx, alteration: integer;
    key, edid: string;
begin
    key := '';
    edid := '';
    alteration := 16;

    if not EditAlterLandRuleForm(key, edid, alteration, True) then Exit;

    joAlterLandRules.O[key].S['editorid'] := edid;
    joAlterLandRules.O[key].S['alteration'] := alteration;

    joUserAlterLandRules.O[key].S['editorid'] := edid;
    joUserAlterLandRules.O[key].S['alteration'] := alteration;
    bUserAlterLandRulesChanged := True;

    lvAlterLandRules.Items.Count := joAlterLandRules.Count;
    lvAlterLandRules.Refresh;
end;

procedure AlterLandRulesMenuEditClick(Sender: TObject);
{
    Edit rule
}
var
    idx, alteration: integer;
    key, edid: string;
begin
    if not Assigned(lvAlterLandRules.Selected) then Exit;
    idx := lvAlterLandRules.Selected.Index;

    key := joAlterLandRules.Names[idx];
    edid := joAlterLandRules.O[key].S['editorid'];
    alteration := joAlterLandRules.O[key].S['alteration'];

    if not EditAlterLandRuleForm(key, edid, alteration, False) then Exit;

    joAlterLandRules.O[key].S['editorid'] := edid;
    joAlterLandRules.O[key].S['alteration'] := alteration;

    joUserAlterLandRules.O[key].S['editorid'] := edid;
    joUserAlterLandRules.O[key].S['alteration'] := alteration;
    bUserAlterLandRulesChanged := True;

    lvAlterLandRules.Items.Count := joAlterLandRules.Count;
    lvAlterLandRules.Refresh;
end;

procedure AlterLandRulesMenuDeleteClick(Sender: TObject);
{
    Delete rule
}
var
    idx, uidx: integer;
    key: string;
begin
    if not Assigned(lvAlterLandRules.Selected) then Exit;
    idx := lvAlterLandRules.Selected.Index;
    key := joAlterLandRules.Names[idx];
    uidx := joUserAlterLandRules.IndexOf(key);
    if uidx > -1 then begin
        joAlterLandRules.Delete(idx);
        joUserAlterLandRules.Delete(uidx);
        bUserAlterLandRulesChanged := True;
        lvAlterLandRules.Items.Count := joAlterLandRules.Count;
        lvAlterLandRules.Refresh;
    end else MessageDlg('This rule cannot be deleted. You may set the alteration to 0' + #13#10 + 'if your really want it to not be used.', mtInformation, [mbOk], 0);
end;

procedure frmAlterLandRuleFormClose(Sender: TObject; var Action: TCloseAction);
{
    Close rule edit menu handler.
}
begin
    if TForm(Sender).ModalResult <> mrOk then Exit;
    if TComboBox(TForm(Sender).FindComponent('cbKey')).Text = '' then begin
        MessageDlg('ID must not be empty.', mtInformation, [mbOk], 0);
        Action := caNone;
    end;
    if TEdit(TForm(Sender).FindComponent('edAlteration')).Text = '' then begin
        MessageDlg('Alteration must not be empty.', mtInformation, [mbOk], 0);
        Action := caNone;
    end;
end;

procedure frmAlterLandResize(Sender: TObject);
{
    Handle resizing of elements in the Alter Land rule menu.
}
var
    frm: TForm;
    btnOK, btnCancel: TButton;
    lv: TListView;
begin
    try
        frm := TForm(Sender);
        btnOK := TButton(frm.FindComponent('OK'));
        btnCancel := TButton(frm.FindComponent('Cancel'));
        lv := TListView(frm.FindComponent('ListView'));

        lv.Width := frm.Width - 36;
        lv.Left := (frm.Width - lv.Width)/2;
        lv.Height := frm.Height - btnOK.Height - btnOK.Height - btnOK.Height - btnOK.Height;

        btnOK.Top := lv.Height + lv.Top + 8;
        btnCancel.Top := btnOK.Top;
        btnOK.Left := (frm.Width - btnOK.Width - btnCancel.Width - 8)/2;
        btnCancel.Left := btnOK.Left + btnOK.Width + 8;
    except
        frm := TForm(Sender);
    end;
end;

function ReferenceRules(Sender: TObject): Boolean;
var
    i: integer;
    key: string;

    mnRules: TPopupMenu;
    MenuItem: TMenuItem;
    lbl: TLabel;
    frm, frmEditRule: TForm;
    cbBase: TComboBox;
    btnOK, btnCancel: TButton;
    lvReferenceRules: TListView;
begin
    frmEditRule := GetParentForm(TButton(Sender));
    key := TComboBox(frmEditRule.FindComponent('cbKey')).Text;
    frm := TForm.Create(nil);
    try
        frm.Caption := 'Reference Rules';
        frm.Width := 600;
        frm.Height := 600;
        frm.Position := poMainFormCenter;
        frm.BorderStyle := bsSizeable;
        frm.KeyPreview := True;
        frm.OnClose := frmOptionsFormClose;
        frm.OnKeyDown := FormKeyDown;
        frm.OnResize := frmReferenceRulesResize;

        cbBase := TComboBox.Create(frm);
        cbBase.Parent := frm;
        cbBase.Name := 'cbBase';
        cbBase.Width := 250;
        cbBase.Left := frm.Width/2 - cbBase.Width/2;
        cbBase.Enabled := False;

        lvReferenceRules := TListView.Create(frm);
        lvReferenceRules.Parent := frm;
        lvReferenceRules.Name := 'ListView';

        lvReferenceRules.Top := 24;
        lvReferenceRules.Width := frm.Width - 36;
        lvReferenceRules.Left := (frm.Width - lvReferenceRules.Width)/2;
        lvReferenceRules.Height := frm.Height - 110;
        lvReferenceRules.ReadOnly := True;
        lvReferenceRules.ViewStyle := vsReport;
        lvReferenceRules.RowSelect := True;
        lvReferenceRules.DoubleBuffered := True;
        lvReferenceRules.Columns.Add.Caption := 'Reference';
        lvReferenceRules.Columns[0].Width := 350;
        lvReferenceRules.Columns.Add.Caption := 'Alteration';
        lvReferenceRules.Columns[1].Width := 275;
        lvReferenceRules.OwnerData := True;
        lvReferenceRules.OnData := lvReferenceRulesData;
        lvReferenceRules.Items.Count := joAlterLandRules.O[key].O['references'].Count;

        mnRules := TPopupMenu.Create(frm);
        lvReferenceRules.PopupMenu := mnRules;
        MenuItem := TMenuItem.Create(mnRules);
        MenuItem.Name := 'MenuItem';
        MenuItem.Caption := 'Add';
        MenuItem.OnClick := ReferenceRulesMenuAddClick;
        mnRules.Items.Add(MenuItem);
        MenuItem := TMenuItem.Create(mnRules);
        MenuItem.Caption := 'Delete';
        MenuItem.OnClick := ReferenceRulesMenuDeleteClick;
        mnRules.Items.Add(MenuItem);
        MenuItem := TMenuItem.Create(mnRules);
        MenuItem.Caption := 'Edit';
        MenuItem.OnClick := ReferenceRulesMenuEditClick;
        mnRules.Items.Add(MenuItem);

        btnOK := TButton.Create(frm);
        btnOK.Parent := frm;
        btnOK.Name := 'OK';
        btnOK.Caption := 'OK';
        btnOK.ModalResult := mrOk;
        btnOK.Top := lvReferenceRules.Height + lvReferenceRules.Top + 8;

        btnCancel := TButton.Create(frm);
        btnCancel.Parent := frm;
        btnCancel.Name := 'Cancel';
        btnCancel.Caption := 'Cancel';
        btnCancel.ModalResult := mrCancel;
        btnCancel.Top := btnOK.Top;

        btnOK.Left := (frm.Width - btnOK.Width - btnCancel.Width - 8)/2;
        btnCancel.Left := btnOK.Left + btnOK.Width + 8;

        frm.ScaleBy(uiScale, 100);
        frm.Font.Size := 8;

        cbBase.Text := key;

        if frm.ShowModal <> mrOk then begin
            Result := False;
            Exit;
        end
        else Result := True;

    finally
        frm.Free;
    end;
end;

function EditReferenceRuleForm(var key, refKey: string; var alteration: integer; keyReadOnly: boolean): boolean;
var
    frmRule: TForm;
    pnl: TPanel;
    btnOk, btnCancel, btnReferences: TButton;
    edAlteration: TEdit;
    cbBase, cbRefKey: TComboBox;
begin
    Result := False;
    frmRule := TForm.Create(nil);
    try
        frmRule.Caption := 'Reference Alteration Rule';
        frmRule.Width := 600;
        frmRule.Height := 180;
        frmRule.Position := poMainFormCenter;
        frmRule.BorderStyle := bsDialog;
        frmRule.KeyPreview := True;
        frmRule.OnKeyDown := FormKeyDown;
        frmRule.OnClose := frmEditReferenceRuleFormClose;

        cbBase := TComboBox.Create(frmRule);
        cbBase.Parent := frmRule;
        cbBase.Name := 'cbBase';
        cbBase.Left := 120;
        cbBase.Top := 12;
        cbBase.Width := frmRule.Width - 150;
        cbBase.Style := csDropDown;
        CreateLabel(frmRule, 16, cbBase.Top + 4, 'Base ID');

        cbRefKey := TComboBox.Create(frmRule);
        cbRefKey.Parent := frmRule;
        cbRefKey.Name := 'cbRefKey';
        cbRefKey.Left := 120;
        cbRefKey.Top := cbBase.Top + 28;
        cbRefKey.Width := frmRule.Width - 150;

        cbRefKey.OnExit := RefKeyChange;
        cbRefKey.Style := csDropDown;
        CreateLabel(frmRule, 16, cbRefKey.Top + 4, 'ID');

        edAlteration := TEdit.Create(frmRule);
        edAlteration.Parent := frmRule;
        edAlteration.Name := 'edAlteration';
        edAlteration.Left := 120;
        edAlteration.Top := cbRefKey.Top + 28;
        edAlteration.Width := frmRule.Width - 150;
        edAlteration.OnKeyPress := alterationValidation;
        CreateLabel(frmRule, 16, edAlteration.Top + 4, 'Alteration');

        btnOk := TButton.Create(frmRule);
        btnOk.Parent := frmRule;
        btnOk.Name := 'OK';
        btnOk.Caption := 'OK';
        btnOk.ModalResult := mrOk;
        btnOk.Top := edAlteration.Top + (2 * edAlteration.Height);

        btnCancel := TButton.Create(frmRule);
        btnCancel.Parent := frmRule;
        btnCancel.Name := 'Cancel';
        btnCancel.Caption := 'Cancel';
        btnCancel.ModalResult := mrCancel;
        btnCancel.Top := btnOk.Top;

        btnOk.Left := frmRule.Width - btnOk.Width - btnCancel.Width - 32;
        btnCancel.Left := btnOk.Left + btnOk.Width + 8;

        pnl := TPanel.Create(frmRule);
        pnl.Parent := frmRule;
        pnl.Left := 10;
        pnl.Top := btnOk.Top - 12;
        pnl.Width := frmRule.Width - 32;
        pnl.Height := 2;

        frmRule.Height := btnOk.Top + (3 * btnOk.Height);
        frmRule.ScaleBy(uiScale, 100);
        frmRule.Font.Size := 8;

        cbBase.Items.Add(key);
        cbBase.ItemIndex := 0;
        cbBase.Enabled := False;

        cbRefKey.Items.Add(refKey);
        cbRefKey.ItemIndex := 0;
        cbRefKey.Enabled := keyReadOnly;

        edAlteration.Text := IntToStr(alteration);

        btnOk.Enabled := not keyReadOnly;

        /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        if frmRule.ShowModal <> mrOk then Exit;
        /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        key := cbBase.Text;
        refKey := cbRefKey.Text;
        alteration := StrToInt(edAlteration.Text);
        Result := True;
    finally
        frmRule.Free;
    end;
end;

procedure RefKeyChange(Sender: TObject);
var
    bReferenceFound: boolean;
    baseRecordId, fileFormId, fileName, refKey: string;
    formid: cardinal;

    r, base: IwbElement;
    f: IwbFile;

    cbBase, cbRefKey: TComboBox;
    edAlteration: TEdit;
    frm: TForm;
    btnOk: TButton;
begin
    bReferenceFound := False;
    cbRefKey := TComboBox(Sender);
    frm := GetParentForm(cbRefKey);
    cbBase := TComboBox(frm.FindComponent('cbBase'));
    edAlteration := TEdit(frm.FindComponent('edAlteration'));
    btnOk := TButton(frm.FindComponent('OK'));

    refKey := cbRefKey.Text;
    if refKey = '' then Exit;
    if ContainsText(refKey, ':') then begin
        r := GetRecordFromFormIdFileId(refKey);
        if (Assigned(r) and (Pos(Signature(r),'REFR,PHZD') <> 0))
            then bReferenceFound := True;
    end
    else if IsValidHex(refKey) then begin
        formid := StrToInt('$' + refKey);
        f := GetFileFromLoadOrderFormID(formid);
        if Assigned(f) then begin
            fileName := GetFileName(f);
            fileFormId := IntToHex(LoadOrderFormIDtoFileFormID(f, refKey), 8);
            refKey := fileFormId + ':' + fileName;
            r := GetRecordFromFormIdFileId(refKey);
            if (Assigned(r) and (Pos(Signature(r),'REFR,PHZD') <> 0))
                then bReferenceFound := True;
        end;
    end;

    if bReferenceFound then begin
        base := LinksTo(ElementByPath(r, 'NAME'));
        baseRecordId := RecordFormIdFileId(base);
        if joAlterLandRules.Contains(baseRecordId) then begin
            cbRefKey.Text := refKey;
            cbBase.Text := baseRecordId;
            btnOk.Enabled := True;
            if joAlterLandRules.O[baseRecordId].O['references'].Contains(refKey) then ShowMessage('This reference rule already exists.');
        end else begin
            ShowMessage(refKey + ' is the reference of a base object ' + baseRecordId + ' that does not have a base object rule.'
            + #13#10 + 'You must add the base object rule first.');
            cbRefKey.Text := #0;
            btnOk.Enabled := False;
        end;
    end else begin
        ShowMessage(refKey + ' is not a valid reference.');
        btnOk.Enabled := False;
    end;
end;

procedure frmEditReferenceRuleFormClose(Sender: TObject; var Action: TCloseAction);
{
    Close rule edit menu handler.
}
begin
    if TForm(Sender).ModalResult <> mrOk then Exit;
    if TComboBox(TForm(Sender).FindComponent('cbRefKey')).Text = '' then begin
        MessageDlg('Reference ID must not be empty.', mtInformation, [mbOk], 0);
        Action := caNone;
    end;
    if TEdit(TForm(Sender).FindComponent('edAlteration')).Text = '' then begin
        MessageDlg('Alteration must not be empty.', mtInformation, [mbOk], 0);
        Action := caNone;
    end;
end;

procedure lvReferenceRulesData(Sender: TObject; Item: TListItem);
{
    Populate lvReferenceRules
}
var
    i: integer;
    key, refKey: string;

    frm: TForm;
    cbBase: TComboBox;
begin
    frm := GetParentForm(TListView(Sender));
    cbBase := TComboBox(frm.FindComponent('cbBase'));
    key := cbBase.Text;

    refKey := joAlterLandRules.O[key].O['references'].Names[Item.Index];
    Item.Caption := refKey;
    Item.SubItems.Add(joAlterLandRules.O[key].O['references'].O[refKey].S['alteration']);
end;

procedure ReferenceRulesMenuEditClick(Sender: TObject);
{
    Edit rule
}
var
    idx, alteration: integer;
    key, refKey: string;

    lvReferenceRules: TListView;
    frm: TForm;
    cbBase: TComboBox;
    MenuItem: TMenuItem;
begin
    MenuItem := TMenuItem(Sender);
    frm := TForm(MenuItem.Owner.Owner);
    lvReferenceRules := TListView(frm.FindComponent('ListView'));
    cbBase := TComboBox(frm.FindComponent('cbBase'));
    key := cbBase.Text;

    if not Assigned(lvReferenceRules.Selected) then Exit;
    idx := lvReferenceRules.Selected.Index;

    refKey := joAlterLandRules.O[key].O['references'].Names[idx];
    alteration := joAlterLandRules.O[key].O['references'].O[refKey].S['alteration'];

    if not EditReferenceRuleForm(key, refKey, alteration, False) then Exit;

    joAlterLandRules.O[key].O['references'].O[refKey].S['alteration'] := alteration;
    joUserAlterLandRules.O[key].S['editorid'] := joAlterLandRules.O[key].S['editorid'];
    joUserAlterLandRules.O[key].S['alteration'] := joAlterLandRules.O[key].S['alteration'];
    joUserAlterLandRules.O[key].O['references'].O[refKey].S['alteration'] := alteration;
    bUserAlterLandRulesChanged := True;

    lvReferenceRules.Items.Count := joAlterLandRules.O[key].O['references'].Count;
    lvReferenceRules.Refresh;
end;

procedure ReferenceRulesMenuAddClick(Sender: TObject);
{
    Add rule
}
var
    alteration: integer;
    key, refKey: string;

    lvReferenceRules: TListView;
    frm: TForm;
    cbBase: TComboBox;
    MenuItem: TMenuItem;
begin
    MenuItem := TMenuItem(Sender);
    frm := TForm(MenuItem.Owner.Owner);
    lvReferenceRules := TListView(frm.FindComponent('ListView'));
    cbBase := TComboBox(frm.FindComponent('cbBase'));
    key := cbBase.Text;

    refKey := '';
    alteration := 24;

    if not EditReferenceRuleForm(key, refKey, alteration, True) then Exit;

    cbBase.Text := key;

    joAlterLandRules.O[key].O['references'].O[refKey].S['alteration'] := alteration;
    joUserAlterLandRules.O[key].S['editorid'] := joAlterLandRules.O[key].S['editorid'];
    joUserAlterLandRules.O[key].S['alteration'] := joAlterLandRules.O[key].S['alteration'];
    joUserAlterLandRules.O[key].O['references'].O[refKey].S['alteration'] := alteration;
    bUserAlterLandRulesChanged := True;

    lvReferenceRules.Items.Count := joAlterLandRules.O[key].O['references'].Count;
    lvReferenceRules.Refresh;
end;

procedure ReferenceRulesMenuDeleteClick(Sender: TObject);
{
    Delete rule
}
var
    idx, uidx: integer;
    key, refKey: string;

    lvReferenceRules: TListView;
    frm: TForm;
    cbBase: TComboBox;
    MenuItem: TMenuItem;
begin
    MenuItem := TMenuItem(Sender);
    frm := TForm(MenuItem.Owner.Owner);
    lvReferenceRules := TListView(frm.FindComponent('ListView'));
    cbBase := TComboBox(frm.FindComponent('cbBase'));
    key := cbBase.Text;
    if not Assigned(lvReferenceRules.Selected) then Exit;
    idx := lvReferenceRules.Selected.Index;
    refKey := joAlterLandRules.O[key].O['references'].Names[idx];
    uidx := joUserAlterLandRules.O[key].O['references'].IndexOf(refKey);
    if uidx > -1 then begin
        joAlterLandRules.O[key].O['references'].Delete(idx);
        joUserAlterLandRules.O[key].O['references'].Delete(uidx);
        bUserAlterLandRulesChanged := True;
        lvReferenceRules.Items.Count := joAlterLandRules.O[key].O['references'].Count;
        lvReferenceRules.Refresh;
    end else MessageDlg('This rule cannot be deleted. You may set the alteration to 0' + #13#10 + 'if your really want it to not be used.', mtInformation, [mbOk], 0);
end;

procedure frmReferenceRulesResize(Sender: TObject);
{
    Handle resizing of elements in the Reference Rule menu.
}
var
    frm: TForm;
    btnOK, btnCancel: TButton;
    lv: TListView;
    cbBase: TComboBox;
begin
    try
        frm := TForm(Sender);
        btnOK := TButton(frm.FindComponent('OK'));
        btnCancel := TButton(frm.FindComponent('Cancel'));
        lv := TListView(frm.FindComponent('ListView'));
        cbBase := TComboBox(frm.FindComponent('cbBase'));

        lv.Width := frm.Width - 36;
        lv.Left := (frm.Width - lv.Width)/2;
        lv.Height := frm.Height - btnOK.Height - btnOK.Height - btnOK.Height - btnOK.Height;

        cbBase.Left := frm.Width/2 - cbBase.Width/2;


        btnOK.Top := lv.Height + lv.Top + 8;
        btnCancel.Top := btnOK.Top;
        btnOK.Left := (frm.Width - btnOK.Width - btnCancel.Width - 8)/2;
        btnCancel.Left := btnOK.Left + btnOK.Width + 8;
    except
        frm := TForm(Sender);
    end;
end;


// ----------------------------------------------------
// Actual Work
// ----------------------------------------------------

procedure CreatePlugin;
begin
    SeasonsMainFile := AddNewFile;
    AddMasterIfMissing(SeasonsMainFile, GetFileName(FileByIndex(0)));
    statGroup := Add(SeasonsMainFile, 'STAT', True);
    scolGroup := Add(SeasonsMainFile, 'SCOL', True);
    slPluginFiles.Add(GetFileName(SeasonsMainFile));
end;

procedure CollectRecords;
{
    Collect records;
}
var
    bLandHasChanged: boolean;
    i, j, count, blockidx, subblockidx, cellidx, cellX, cellY: integer;
    recordid, fileName, wrldEdid: string;

    tlLand: TList;
    slHideQuads: TStringList;

    f: IwbFile;
    g, wrldgroup: IwbGroupRecord;
    r, rWrld, wWrld, block, subblock, rCell, wCell, land, landFlags: IwbElement;
begin
    slHideQuads := TStringList.Create;
    count := 0;
    try
        for i := 0 to Pred(FileCount) do begin
            f := FileByIndex(i);
            fileName := GetFileName(f);

            //Collect LAND
            g := GroupBySignature(f, 'WRLD');
            for j := 0 to Pred(ElementCount(g)) do begin
                rWrld := ElementByIndex(g, j);
                recordid := RecordFormIdFileId(rWrld);
                if Pos(recordid, sIgnoredWorldspaces) <> 0 then continue;
                wrldEdid := GetElementEditValues(rWrld, 'EDID');
                wWrld := WinningOverride(rWrld);
                if GetElementNativeValues(wWrld, 'DATA\No Landscape') = 1 then continue;

                wrldgroup := ChildGroup(rWrld);
                for blockidx := 0 to Pred(ElementCount(wrldgroup)) do begin
                    block := ElementByIndex(wrldgroup, blockidx);
                    for subblockidx := 0 to Pred(ElementCount(block)) do begin
                        subblock := ElementByIndex(block, subblockidx);
                        for cellidx := 0 to Pred(ElementCount(subblock)) do begin
                            rCell := ElementByIndex(subblock, cellidx);
                            if (Signature(rCell) <> 'CELL') or GetIsPersistent(rCell) then continue;
                            cellX := GetElementNativeValues(rCell, 'XCLC\X');
                            cellY := GetElementNativeValues(rCell, 'XCLC\Y');
                            joWinningCells.O[wrldEdid].O[cellX].S[cellY] := RecordFormIdFileId(rCell);
                            land := GetLandscapeForCell(rCell);
                            if not Assigned(land) then continue;
                            if not IsWinningOverride(land) then continue;
                            if not ElementExists(land, 'VHGT') then continue;
                            wCell := WinningOverride(rCell);
                            landFlags := ElementByPath(wCell, 'XCLC\Land Flags');
                            if GetElementEditValues(landFlags, 'Hide - Quad 1') <> '0'
                                then slHideQuads.Add(ShortName(rCell) + #9 + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'has hidden land quad 1.');
                            if GetElementEditValues(landFlags, 'Hide - Quad 2') <> '0'
                                then slHideQuads.Add(ShortName(rCell) + #9 + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'has hidden land quad 2.');
                            if GetElementEditValues(landFlags, 'Hide - Quad 3') <> '0'
                                then slHideQuads.Add(ShortName(rCell) + #9 + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'has hidden land quad 3.');
                            if GetElementEditValues(landFlags, 'Hide - Quad 4') <> '0'
                                then slHideQuads.Add(ShortName(rCell) + #9 + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'has hidden land quad 4.');

                            AddMessage(IntToStr(count) + #9 + ShortName(land) + #9 + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY));
                            bLandHasChanged := CreateLandscapeHeights(land, wCell, wWrld, wrldEdid);
                            if bLandHasChanged then begin
                                slLandscapeCells.Add(wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY));
                                Inc(count);
                            end;
                            if bCreateLandscapeSnowMeshes or bPlaceLandscapeSnow or bLandHasChanged then begin
                                tlLandRecords.Add(land);
                            end;
                            if (bCreateLandscapeSnowMeshes and not bLandHasChanged) then
                                slLandscapeCells.Add(wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY));
                            //if count > 10 then break;
                        end;
                        //if count > 10 then break;
                    end;
                    //if count > 10 then break;
                end;
                //if count > 10 then break;
            end;

            g := GroupBySignature(f, 'STAT');
            for j := 0 to Pred(ElementCount(g)) do begin
                r := ElementByIndex(g, j);
                if not IsWinningOverride(r) then continue;
                if ReferencedByCount(r) = 0 then continue;
                recordid := RecordFormIdFileId(r);
                tlStats.Add(r);
                if not joAlterLandRules.Contains(recordid) then continue;
                tlBasesThatAlterLand.Add(r);
            end;

            g := GroupBySignature(f, 'SCOL');
            for j := 0 to Pred(ElementCount(g)) do begin
                r := ElementByIndex(g, j);
                if not IsWinningOverride(r) then continue;
                if ReferencedByCount(r) = 0 then continue;
                recordid := RecordFormIdFileId(r);
                if not joAlterLandRules.Contains(recordid) then continue;
                tlBasesThatAlterLand.Add(r);
            end;

            g := GroupBySignature(f, 'FURN');
            for j := 0 to Pred(ElementCount(g)) do begin
                r := ElementByIndex(g, j);
                if not IsWinningOverride(r) then continue;
                if ReferencedByCount(r) = 0 then continue;
                recordid := RecordFormIdFileId(r);
                if not joAlterLandRules.Contains(recordid) then continue;
                tlBasesThatAlterLand.Add(r);
            end;

            g := GroupBySignature(f, 'ACTI');
            for j := 0 to Pred(ElementCount(g)) do begin
                r := ElementByIndex(g, j);
                if not IsWinningOverride(r) then continue;
                if ReferencedByCount(r) = 0 then continue;
                recordid := RecordFormIdFileId(r);
                if not joAlterLandRules.Contains(recordid) then continue;
                tlBasesThatAlterLand.Add(r);
            end;

            g := GroupBySignature(f, 'MSTT');
            for j := 0 to Pred(ElementCount(g)) do begin
                r := ElementByIndex(g, j);
                if not IsWinningOverride(r) then continue;
                if ReferencedByCount(r) = 0 then continue;
                recordid := RecordFormIdFileId(r);
                if not joAlterLandRules.Contains(recordid) then continue;
                tlBasesThatAlterLand.Add(r);
            end;

            g := GroupBySignature(f, 'LIGH');
            for j := 0 to Pred(ElementCount(g)) do begin
                r := ElementByIndex(g, j);
                if not IsWinningOverride(r) then continue;
                if ReferencedByCount(r) = 0 then continue;
                recordid := RecordFormIdFileId(r);
                if not joAlterLandRules.Contains(recordid) then continue;
                tlBasesThatAlterLand.Add(r);
            end;

            g := GroupBySignature(f, 'HAZD');
            for j := 0 to Pred(ElementCount(g)) do begin
                r := ElementByIndex(g, j);
                if not IsWinningOverride(r) then continue;
                if ReferencedByCount(r) = 0 then continue;
                recordid := RecordFormIdFileId(r);
                if not joAlterLandRules.Contains(recordid) then continue;
                tlBasesThatAlterLand.Add(r);
            end;

        end;
    finally
        ListStringsInStringList(slHideQuads);
        slHideQuads.Free;
    end;
    AddMessage('New LAND Records: ' + IntToStr(count));
    // if bCreateLandscapeHeights then begin
    //     joLandscapeHeights.SaveToFile(wbScriptsPath + 'Seasons\LandHeightsPreAlteration.json', False, TEncoding.UTF8, True);
    //     joLandscapeHeightsAltered.SaveToFile(wbScriptsPath + 'Seasons\LandHeightsAlteredPreAlteration.json', False, TEncoding.UTF8, True);
    // end;
end;

procedure ProcessStats;
{
    Process STAT records.
}
var
    i: integer;
    model, winterDecal: string;

    r, rModl: IwbElement;
begin
    for i := 0 to Pred(tlStats.Count) do begin
        r := ObjectToElement(tlStats[i]);
        rModl := ElementByPath(r, 'Model\MODL');
        if not Assigned(rModl) then continue;
        model := wbNormalizeResourceName(GetEditValue(rModl), resMesh);
        winterDecal := StringReplace(model, 'meshes', 'meshes\WinterDecals', [rfIgnoreCase]);
        if ResourceExists(winterDecal) then tlWinterDecals.Add(r);
    end;
end;

procedure CreateWinterDecals;
{
    Creates winter decals.
}
var
    i: integer;
    model, winterDecal, statEdid: string;

    base, rWinterDecal: IwbElement;
begin
    if not Assigned(SeasonsMainFile) then Exit;
    for i := 0 to Pred(tlWinterDecals.Count) do begin
        base := ObjectToElement(tlWinterDecals[i]);
        model := wbNormalizeResourceName(GetElementEditValues(base, 'Model\MODL'), resMesh);
        winterDecal := StringReplace(model, 'meshes', 'meshes\WinterDecals', [rfIgnoreCase]);
        statEdid := GetElementEditValues(base, 'EDID');
        rWinterDecal := CreateNewStat(winterDecal, '', '', '', 'winterDecal_' + statEdid);
        AddMessage('Processing Winter Decals: ' + #9 + statEdid);
        ProcessWinterDecalREFRs(base, base, rWinterDecal, False);
    end;
end;

procedure ProcessWinterDecalREFRs(base, fromBase, rWinterDecal: IwbElement; bSCOL: boolean);
{
    Places winter decals;
}
var
    i, j: integer;
    scale, posX, posY, posZ, rotX, rotY, rotZ, pmPosX, pmPosY, pmPosZ, pmRotX, pmRotY, pmRotZ, pmScale,
    posXHere, posYHere, posZHere, raw_x, raw_y, raw_z, rawr_x, rawr_y, rawr_z: double;
    wrldEdid, onam, baseRecordId: string;

    position, positionHere: TwbVector;

    r, rCell, rWrld, scolParts, scolPart, placements, placement: IwbElement;
begin
    for i := Pred(ReferencedByCount(base)) downto 0 do begin
        r := ReferencedByIndex(base, i);
        if Signature(r) = 'SCOL' then begin
            ProcessWinterDecalREFRs(r, base, rWinterDecal, True);
            continue;
        end;
        if Signature(r) <> 'REFR' then continue;
        if not IsWinningOverride(r) then continue;
        if GetIsDeleted(r) then continue;
        if ElementExists(r, 'XESP') then continue; //skip enable parented objects for now. TODO make a way to have "double XESP"
        rCell := WinningOverride(LinksTo(ElementByIndex(r, 0)));
        if Signature(rCell) <> 'CELL' then continue;
        if GetElementEditValues(rCell, 'DATA - Flags\Is Interior Cell') = 1 then continue;
        rWrld := WinningOverride(LinksTo(ElementByIndex(rCell, 0)));
        if Signature(rWrld) <> 'WRLD' then continue;
        if Pos(RecordFormIdFileId(rWrld), sIgnoredWorldspaces) <> 0 then continue;
        wrldEdid := GetElementEditValues(rWrld, 'EDID');

        if ElementExists(r, 'XSCL') then scale := GetElementNativeValues(r, 'XSCL') else scale := 1;
        posX := GetElementNativeValues(r, 'DATA\Position\X');
        posY := GetElementNativeValues(r, 'DATA\Position\Y');
        posZ := GetElementNativeValues(r, 'DATA\Position\Z');
        rotX := GetElementNativeValues(r, 'DATA\Rotation\X');
        rotY := GetElementNativeValues(r, 'DATA\Rotation\Y');
        rotZ := GetElementNativeValues(r, 'DATA\Rotation\Z');

        if bSCOL then begin
            baseRecordId := RecordFormIdFileId(fromBase);
            scolParts := ElementByPath(base, 'Parts');
            for j := 0 to Pred(ElementCount(scolParts)) do begin
                scolPart := ElementByIndex(scolParts, j);
                onam := RecordFormIdFileId(LinksTo(ElementByPath(scolPart, 'ONAM')));
                if SameText(onam, baseRecordId) then begin
                    break;
                end;
            end;
            placements := ElementByPath(scolPart, 'DATA');
            for j := 0 to Pred(ElementCount(placements)) do begin
                placement := ElementByIndex(placements, j);

                pmPosX := GetElementNativeValues(placement, 'Position\X') * scale;
                pmPosY := GetElementNativeValues(placement, 'Position\Y') * scale;
                pmPosZ := GetElementNativeValues(placement, 'Position\Z') * scale;
                pmRotX := GetElementNativeValues(placement, 'Rotation\X');
                pmRotY := GetElementNativeValues(placement, 'Rotation\Y');
                pmRotZ := GetElementNativeValues(placement, 'Rotation\Z');
                pmScale := GetElementNativeValues(placement, 'Scale') * scale;

                if ((rotX = 0) and (rotY = 0) and (rotZ = 0)) then begin
                    raw_x := pmPosX;
                    raw_y := pmPosY;
                    raw_z := pmPosZ;
                end else begin
                    rotate_position(
                        pmPosX, pmPosY, pmPosZ,         // initial position
                        rotX, rotY, rotZ,              // rotation to apply - x y z
                        raw_x, raw_y, raw_z           // (output) raw final position
                    );
                end;

                if ((pmRotX = 0) and (pmRotY = 0) and (pmRotZ = 0)) then begin
                    rawr_x := rotX;
                    rawr_Y := rotY;
                    rawr_Z := rotZ;
                end else begin
                    rotate_rotation(
                        rotX, rotY, rotZ,                  // initial rotation
                        pmRotX, pmRotY, pmRotZ,           // rotation to apply - x y z
                        rawr_x, rawr_y, rawr_z           // (output) raw rotation
                    );
                end;

                posXHere := posX + raw_x;
                posYHere := posY + raw_y;
                posZHere := posZ + raw_z;
                PlaceWinterDecal(r, rWinterDecal, rWrld, wrldEdid, posXHere, posYHere, posZHere, rawr_x, rawr_y, rawr_z, pmScale);
            end;
        end;
        PlaceWinterDecal(r, rWinterDecal, rWrld, wrldEdid, posX, posY, posZ, rotX, rotY, rotZ, scale);
    end;
end;

procedure PlaceWinterDecal(r, rWinterDecal, rWrld: IwbElement; wrldEdid: string; posX, posY, posZ, rotX, rotY, rotZ, scale: double);
var
    winterDecalFormid, cellRecordId: string;

    position: TwbVector;
    c: TwbGridCell;

    rCell, nCell, winterDecalRef, base, eScale: IwbElement;
begin
    AddMessage(#9 + ShortName(r) + #9 + IntToStr(Round(posX)) + ', ' + IntToStr(Round(posY)) + ', ' + IntToStr(Round(posZ)));
    position.x := posX;
    position.y := posY;
    position.z := posZ;
    c := wbPositionToGridCell(position);
    cellRecordId := joWinningCells.O[wrldEdid].O[c.X].S[c.Y];
    if cellRecordId = '' then Exit;

    rCell := WinningOverride(GetRecordFromFormIdFileId(cellRecordId));
    if not Assigned(rCell) then Exit;

    AddRequiredElementMasters(rWrld, SeasonsMainFile, False, True);
    AddRequiredElementMasters(rCell, SeasonsMainFile, False, True);
  	SortMasters(SeasonsMainFile);
    wbCopyElementToFile(rWrld, SeasonsMainFile, False, True);
    nCell := wbCopyElementToFile(rCell, SeasonsMainFile, False, True);
    winterDecalRef := Add(nCell, 'REFR', True);
    winterDecalFormid := IntToHex(GetLoadOrderFormID(rWinterDecal), 8);

    SetElementEditValues(winterDecalRef, 'DATA\Position\X', posX);
    SetElementEditValues(winterDecalRef, 'DATA\Position\Y', posY);
    SetElementEditValues(winterDecalRef, 'DATA\Position\Z', posZ);
    SetElementEditValues(winterDecalRef, 'DATA\Rotation\X', rotX);
    SetElementEditValues(winterDecalRef, 'DATA\Rotation\Y', rotY);
    SetElementEditValues(winterDecalRef, 'DATA\Rotation\Z', rotZ);

    if scale <> 1 then begin
        eScale := Add(winterDecalRef, 'XSCL', True);
        SetNativeValue(eScale, scale);
    end;

    base := ElementByPath(winterDecalRef, 'NAME');
    SetEditValue(base, winterDecalFormid);
    AddLinkedReference(winterDecalRef, 'WorkshopStackedItemParentKEYWORD [KYWD:001C5EDD]', Name(r));
end;

procedure AlterLandHeightsForTheseBases;
{
    Alters land heights for certain base objects placed near landscape.
}
var
    i, alteration, x1, y1, z1, x2, y2, z2: integer;
    baseRecordId: string;

    base: IwbElement;
begin
    for i:= 0 to Pred(tlBasesThatAlterLand.Count) do begin
        base := ObjectToElement(tlBasesThatAlterLand[i]);
        GetBounds(x1, y1, z1, x2, y2, z2, base);
        baseRecordId := RecordFormIdFileId(base);
        alteration := joAlterLandRules.O[baseRecordId].S['alteration'];
        AddMessage('Processing base ' + #9 + Name(base) + #9 + IntToStr(alteration));
        ProcessBasesThatAlterLand(base, base, alteration, x1, y1, z1, x2, y2, z2, False, baseRecordId);
    end;
    // if bCreateLandscapeHeights then begin
    //     joLandscapeHeightsAltered.SaveToFile(wbScriptsPath + 'Seasons\LandHeightsAltered.json', False, TEncoding.UTF8, True);
    // end;
end;

function ProcessBasesThatAlterLand(base, fromBase: IwbElement; alteration, x1, y1, z1, x2, y2, z2: integer; bSCOL: boolean; baseRecordId: string): boolean;
{
    Process a base object to see if it alters land heights.
}
var
    i: integer;
    alterationRefr, wrldEdid: string;

    r, rCell, rWrld: IwbElement;
begin
    Result := False;
    for i := 0 to Pred(ReferencedByCount(base)) do begin
        //Check each reference to see if it is in an exterior cell with landscape, and is close enough to the landscape to alter it.
        //If so, we need to alter the land heights near the reference.
        r := ReferencedByIndex(base, i);
        if Signature(r) = 'SCOL' then begin
            ProcessBasesThatAlterLand(r, base, alteration, x1, y1, z1, x2, y2, z2, True, baseRecordId);
            // if SCOL, then the next iteration, r becomes the base, and base becomes the fromBase
            continue;
        end;
        if (Pos(Signature(r), 'REFR,PHZD') = 0) then continue;
        if not IsWinningOverride(r) then continue;
        if GetIsDeleted(r) then continue;
        if GetIsCleanDeleted(r) then continue;
        //if ElementExists(r, 'XESP') then continue; //skip enable parented references, since the object is not always present.
        rCell := WinningOverride(LinksTo(ElementByIndex(r, 0)));
        if (Signature(rCell) <> 'CELL') then continue;
        if (GetElementEditValues(rCell, 'DATA - Flags\Is Interior Cell') = 1) then continue;
        rWrld := WinningOverride(LinksTo(ElementByIndex(rCell, 0)));
        if (Pos(RecordFormIdFileId(rWrld), sIgnoredWorldspaces) > 0) then continue;

        wrldEdid := GetElementEditValues(rWrld, 'EDID');

        // If we got this far, this REFR is in an exterior cell with landscape, and we will need to alter the land heights IF it is close enough to the landscape.
        alterationRefr := joAlterLandRules.O[baseRecordId].O['references'].O[RecordFormIdFileId(r)].S['alteration'];
        if alterationRefr = '' then alterationRefr := IntToStr(alteration);
        AddMessage(#9 + 'Processing reference ' + #9 + ShortName(r) + #9 + wrldEdid + #9 + alterationRefr);
        AlterLandHeightsForThisRefr(r, base, fromBase, wrldEdid, StrToInt(alterationRefr), x1, y1, z1, x2, y2, z2, rWrld, bSCOL);
    end;
end;

procedure GetBounds(var x1, y1, z1, x2, y2, z2: integer; base: IwbElement);
var
    radius: integer;
begin
    // if SameText(Signature(base), 'HAZD') then begin
    //     radius := Round(GetElementNativeValues(base, 'DNAM\Radius') * 32 div 3);
    //     x1 := -1 * radius;
    //     y1 := -1 * radius;
    //     z1 := -64;
    //     x2 := radius;
    //     y2 := radius;
    //     z2 := 64;
    //     Exit;
    // end;
    x1 := GetElementNativeValues(base, 'OBND\X1');
    y1 := GetElementNativeValues(base, 'OBND\Y1');
    z1 := GetElementNativeValues(base, 'OBND\Z1');
    x2 := GetElementNativeValues(base, 'OBND\X2');
    y2 := GetElementNativeValues(base, 'OBND\Y2');
    z2 := GetElementNativeValues(base, 'OBND\Z2');
end;

procedure AddObjectBounds(var x1, y1, z1, x2, y2, z2: integer; base: IwbElement);
var
    obnd: IwbElement;
begin
    obnd := ElementByPath(base, 'OBND');
    if not Assigned(obnd) then obnd := Add(base, 'OBND', True);
    SetElementNativeValues(obnd, 'X1', x1);
    SetElementNativeValues(obnd, 'Y1', y1);
    SetElementNativeValues(obnd, 'Z1', z1);
    SetElementNativeValues(obnd, 'X2', x2);
    SetElementNativeValues(obnd, 'Y2', y2);
    SetElementNativeValues(obnd, 'Z2', z2);
end;

procedure AlterLandHeightsForThisRefr(r, base, fromBase: IwbElement; wrldEdid: string; alterationRefr, x1, y1, z1, x2, y2, z2: integer; rWrld: IwbElement; bSCOL: boolean);
{
    Alters land heights for a specific reference.
}
var
    i, x1n, y1n, z1n, x2n, y2n, z2n: integer;
    scale, posX, posY, posZ, rotX, rotY, rotZ, pmScale, pmPosX, pmPosY, pmPosZ, pmRotX, pmRotY, pmRotZ,
    posXHere, posYHere, posZHere, raw_x, raw_y, raw_z, rawr_x, rawr_y, rawr_z: double;
    onam, baseRecordId: string;

    realBase, scolParts, scolPart, placements, placement: IwbElement;
begin
    // If this was from an scol, r is the placed SCOL reference, base is the SCOL, and fromBase is the underlying STAT
    //Get scale and position of the reference.
    if ElementExists(r, 'XSCL') then scale := GetElementNativeValues(r, 'XSCL') else scale := 1;
    posX := GetElementNativeValues(r, 'DATA\Position\X');
    posY := GetElementNativeValues(r, 'DATA\Position\Y');
    posZ := GetElementNativeValues(r, 'DATA\Position\Z');
    rotX := GetElementNativeValues(r, 'DATA\Rotation\X');
    rotY := GetElementNativeValues(r, 'DATA\Rotation\Y');
    rotZ := GetElementNativeValues(r, 'DATA\Rotation\Z');

    //If the object is an SCOL, we need to adjust the position to be the SCOL placement position.
    if bSCOL then begin
        baseRecordId := RecordFormIdFileId(fromBase);
        scolParts := ElementByPath(base, 'Parts');
        for i := 0 to Pred(ElementCount(scolParts)) do begin
            scolPart := ElementByIndex(scolParts, i);
            onam := RecordFormIdFileId(LinksTo(ElementByPath(scolPart, 'ONAM')));
            if SameText(onam, baseRecordId) then begin
                break;
            end;
        end;
        placements := ElementByPath(scolPart, 'DATA');
        for i := 0 to Pred(ElementCount(placements)) do begin
            placement := ElementByIndex(placements, i);

            pmPosX := GetElementNativeValues(placement, 'Position\X') * scale;
            pmPosY := GetElementNativeValues(placement, 'Position\Y') * scale;
            pmPosZ := GetElementNativeValues(placement, 'Position\Z') * scale;
            pmRotX := GetElementNativeValues(placement, 'Rotation\X');
            pmRotY := GetElementNativeValues(placement, 'Rotation\Y');
            pmRotZ := GetElementNativeValues(placement, 'Rotation\Z');
            pmScale := GetElementNativeValues(placement, 'Scale') * scale;

            if ((rotX = 0) and (rotY = 0) and (rotZ = 0)) then begin
                raw_x := pmPosX;
                raw_y := pmPosY;
                raw_z := pmPosZ;
            end else begin
                rotate_position(
                    pmPosX, pmPosY, pmPosZ,         // initial position
                    rotX, rotY, rotZ,              // rotation to apply - x y z
                    raw_x, raw_y, raw_z           // (output) raw final position
                );
            end;

            if ((pmRotX = 0) and (pmRotY = 0) and (pmRotZ = 0)) then begin
                rawr_x := rotX;
                rawr_Y := rotY;
                rawr_Z := rotZ;
            end else begin
                rotate_rotation(
                    rotX, rotY, rotZ,                  // initial rotation
                    pmRotX, pmRotY, pmRotZ,           // rotation to apply - x y z
                    rawr_x, rawr_y, rawr_z           // (output) raw rotation
                );
            end;

            posXHere := posX + raw_x;
            posYHere := posY + raw_y;
            posZHere := posZ + raw_z;
            x1n := Floor(x1 * pmScale);
            y1n := Floor(y1 * pmScale);
            z1n := Floor(z1 * pmScale);
            x2n := Ceil(x2 * pmScale);
            y2n := Ceil(y2 * pmScale);
            z2n := Ceil(z2 * pmScale);
            AlterLandHeightsForThisPlacement(alterationRefr, x1n, y1n, z1n, x2n, y2n, z2n, posXHere, posYHere, posZHere, rawr_x, rawr_y, rawr_z, wrldEdid, realBase, rWrld);
        end;
    end
    else begin
        x1n := Floor(x1 * scale);
        y1n := Floor(y1 * scale);
        z1n := Floor(z1 * scale);
        x2n := Ceil(x2 * scale);
        y2n := Ceil(y2 * scale);
        z2n := Ceil(z2 * scale);
        AlterLandHeightsForThisPlacement(alterationRefr, x1n, y1n, z1n, x2n, y2n, z2n, posX, posY, posZ, rotX, rotY, rotZ, wrldEdid, realBase, rWrld);
    end;
end;

procedure AlterLandHeightsForThisPlacement(alterationRefr, x1n, y1n, z1n, x2n, y2n, z2n: integer; posX, posY, posZ, rotX, rotY, rotZ: double; wrldEdid: string; realBase, rWrld: IwbElement);
{
    Alters land heights for a specific placement of a base object.
}
var
    i, j, k, row, row_closest, column, column_closest, cellXHere, cellYHere, vz, oldZ, landOffsetZ, newZ, width, height, numX, numY, alterationHere: integer;
    fileName, landFile: string;
    raw_x, raw_y, raw_z, xHere, yHere, zHere, posXHere, posYHere, posZHere, column_bias, row_bias, outskirts_bias, cellPosX, cellPosY: double;
    previousAlteration: variant;

    joLand, joLandAlteration: TJsonObject;
begin
    //Fix object bounds in case they are all 0
    if ((x1n = 0) and (x2n = 0) and (y1n = 0) and (y2n = 0) and (z1n = 0) and (z2n = 0)) then begin
        x1n := -64;
        x2n := 64;
        y1n := -64;
        y2n := 64;
        z1n := -64;
        z2n := 64;
    end;

    //Okay, so what we need to do is understand that our object is rotated, and we are only given the bounds of the object in the unrotated state.
    //We are going to need to just take the bounding width/height of the object, divide that by 128, to get the number of lines we will use to get all the vertices
    //that we will need to alter.
    //    ||||          ////
    //    ||||         ////
    //    ||||   to   ////
    //    ||||       ////
    //    ||||      ////
    width := x2n - x1n;
    height := y2n - y1n;
    numX := Floor(width/128);
    numY := Floor(height/128);
    if numX = 0 then numX := 1;
    if numY = 0 then numY := 1;
    zHere := z2n;
    //AddMessage(#9 + #9 + 'Width: ' + IntToStr(width) + ' Height: ' + IntToStr(height) + ' NumX: ' + IntToStr(numX) + ' NumY: ' + IntToStr(numY) + ' Rotation: ' + FloatToStr(rotZ) + ' World: ' + wrldEdid);

    //Now we will loop through all the points we need to check, get the rotated position, find the closest land vertex, and alter it.
    for i := 0 to numX do begin
        xHere := x1n + (i * width/numX);
        //xHere := x1 + (i * 128);
        for j := 0 to numY do begin
            yHere := y1n + (j * height/numY);
            //yHere := y1 + (j * 128);
            //AddMessage(#9 + #9 + #9 + 'Processing point ' + FloatToStr(xHere) + ',' + FloatToStr(yHere));
            if xHere > x2n then xHere := x2n;
            if yHere > y2n then yHere := y2n;
            if ((rotX = 0) and (rotY = 0) and (rotZ = 0)) then begin
                raw_x := xHere;
                raw_y := yHere;
                raw_z := zHere;
            end
            else begin
                //continue; //Skipping rotated objects for now.
                rotate_position(
                    xHere, yHere, zHere,            // initial position
                    rotX, rotY, rotZ,              // rotation to apply - x y z
                    raw_x, raw_y, raw_z           // (output) raw final position
                );
            end;
            //AddMessage(#9 + #9 + #9 + 'Rotated point ' + FloatToStr(raw_x) + ',' + FloatToStr(raw_y));
            posXHere := posX + raw_x;
            posYHere := posY + raw_y;
            posZHere := posZ + raw_z;

            //Find the cell this position is in.
            cellXHere := Floor(posXHere/4096);
            cellYHere := Floor(posYHere/4096);

            landFile := wrldEdid + '\x' + IntToStr(cellXHere) + 'y' + IntToStr(cellYHere) + '.json';
            if not FileExists(wbScriptsPath + 'Seasons\LandHeights\' + landFile) then continue;
            joLand := TJsonObject.Create;
            joLandAlteration := TJsonObject.Create;
            try
                joLandAlteration.LoadFromFile(wbScriptsPath + 'Seasons\LandHeightsAltered\' + landFile);

                //Check if cell is in one of the slLandscapeCells, or neighbors.
                if not (bCreateLandscapeSnowMeshes or bCreateLandscapeHeights) then
                    if not IsCellInRange(wrldEdid, cellXHere, cellYHere, True) then continue;

                //AddMessage(#9 + #9 + #9 + 'Position ' + FloatToStr(posXHere) + ',' + FloatToStr(posYHere) + ' is in cell ' + IntToStr(cellXHere) + ',' + IntToStr(cellYHere));

                cellPosX := (posXHere - (cellXHere * 4096))/128;
                cellPosY := (posYHere - (cellYHere * 4096))/128;
                column_closest := Round(cellPosX);
                row_closest := Round(cellPosY);

                //Find the closest vertex in this cell.
                for k := 0 to 3 do begin
                    if k = 0 then begin
                        column := Floor(cellPosX);
                        row := Floor(cellPosY);
                    end
                    else if k = 1 then begin
                        column := Floor(cellPosX);
                        row := Ceil(cellPosY);
                    end
                    else if k = 2 then begin
                        column := Ceil(cellPosX);
                        row := Floor(cellPosY);
                    end
                    else begin
                        column := Ceil(cellPosX);
                        row := Ceil(cellPosY);
                    end;

                    row_bias := (1 - (Max(row, cellPosY) - Min(row, cellPosY))) * 2;
                    column_bias := (1 - (Max(column, cellPosX) - Min(column, cellPosX))) * 2;
                    outskirts_bias := 1;

                    try
                        previousAlteration := joLandAlteration.A[row].S[column];
                    except
                        previousAlteration := '';
                    end;

                    if ((i = 0) or (i = numX) or (j = 0) or (j = numY)) then begin
                        //We are on the outer limits of this alteration.
                        //We will reduce the magnitude of the alteration.
                        //This should allow the alteration to taper at the edges.
                        outskirts_bias := 0.81;
                    end
                    else begin
                        row_bias := 1;
                        column_bias := 1;
                    end;
                    //We set the column and row bias for the alteration based on if it is the closest vertex.
                    if column = column_closest then column_bias := 1;
                    if row = row_closest then row_bias := 1;

                    alterationHere := Round((outskirts_bias * row_bias * column_bias * alterationRefr)/SCALE_FACTOR_TERRAIN) * SCALE_FACTOR_TERRAIN;
                    if alterationHere = 0 then continue;

                    if previousAlteration <> '' then begin
                        if previousAlteration > 0 then begin
                            //AddMessage(#9 + #9 + #9 + 'This vertex has already been altered.');
                            if  ((alterationHere > 0) and (alterationHere <= previousAlteration)) then continue;
                        end
                        else if previousAlteration = 0 then begin
                            if alterationHere > 0 then continue;
                        end
                        else if previousAlteration < 0 then begin
                            //AddMessage(#9 + #9 + #9 + 'This vertex has already been altered.');
                            if alterationHere > 0 then continue;
                            if alterationHere >= previousAlteration then continue;
                        end;
                    end;

                    if alterationHere > 0 then begin
                        //-1 0
                        if (row > 0) and (GetLandAlteration(joLandAlteration, row - 1, column) < 0) then continue;
                        //+1 0
                        if (row < 32) and (GetLandAlteration(joLandAlteration, row + 1, column) < 0) then continue;
                        //0 -1
                        if (column > 0) and (GetLandAlteration(joLandAlteration, row, column - 1) < 0) then continue;
                        //0 +1
                        if (column < 32) and (GetLandAlteration(joLandAlteration, row, column + 1) < 0) then continue;
                        // Check diagonals
                        //-1 -1
                        if (row > 0) and (column > 0) and (GetLandAlteration(joLandAlteration, row - 1, column - 1) < 0) then continue;
                        //-1 +1
                        if (row > 0) and (column < 32) and (GetLandAlteration(joLandAlteration, row - 1, column + 1) < 0) then continue;
                        //+1 -1
                        if (row < 32) and (column > 0) and (GetLandAlteration(joLandAlteration, row + 1, column - 1) < 0) then continue;
                        //+1 +1
                        if (row < 32) and (column < 32) and (GetLandAlteration(joLandAlteration, row + 1, column + 1) < 0) then continue;
                    end
                    else if alterationHere < 0 then begin
                        //-1 0
                        if (row > 0) and (GetLandAlteration(joLandAlteration, row - 1, column) > 0) then
                            AddLandAlteration(joLandAlteration, row - 1, column, 0);
                        //+1 0
                        if (row < 32) and (GetLandAlteration(joLandAlteration, row + 1, column) > 0) then
                            AddLandAlteration(joLandAlteration, row + 1, column, 0);
                        //0 -1
                        if (column > 0) and (GetLandAlteration(joLandAlteration, row, column - 1) > 0) then
                            AddLandAlteration(joLandAlteration, row, column - 1, 0);
                        //0 +1
                        if (column < 32) and (GetLandAlteration(joLandAlteration, row, column + 1) > 0) then
                            AddLandAlteration(joLandAlteration, row, column + 1, 0);
                        // Check diagonals
                        //-1 -1
                        if (row > 0) and (column > 0) and (GetLandAlteration(joLandAlteration, row - 1, column - 1) > 0) then
                            AddLandAlteration(joLandAlteration, row - 1, column - 1, 0);
                        //-1 +1
                        if (row > 0) and (column < 32) and (GetLandAlteration(joLandAlteration, row - 1, column + 1) > 0) then
                            AddLandAlteration(joLandAlteration, row - 1, column + 1, 0);
                        //+1 -1
                        if (row < 32) and (column > 0) and (GetLandAlteration(joLandAlteration, row + 1, column - 1) > 0) then
                            AddLandAlteration(joLandAlteration, row + 1, column - 1, 0);
                        //+1 +1
                        if (row < 32) and (column < 32) and (GetLandAlteration(joLandAlteration, row + 1, column + 1) > 0) then
                            AddLandAlteration(joLandAlteration, row + 1, column + 1, 0);
                    end;

                    joLand.LoadFromFile(wbScriptsPath + 'Seasons\LandHeights\' + landFile);
                    landOffsetZ := joLand.S['offset'];
                    vz := joLand.A[row].S[column];
                    oldZ := (vz + landOffsetZ) * SCALE_FACTOR_TERRAIN;
                    if posZHere < oldZ then begin
                        //AddMessage(#9 + #9 + #9 + 'Object is below the landscape vertex, so skipping it.');
                        continue; //The object is completely below this vertex, so skip it.
                    end;

                    AddLandAlteration(joLandAlteration, row, column, alterationHere);
                    AddMessage(#9 + #9 + #9 + 'Altering land height at ' + IntToStr(column) + ',' + IntToStr(row) + ' in ' + wrldEdid + ' ' + IntToStr(cellXHere) + ' ' + IntToStr(cellYHere) + ' from ' + IntToStr(vz * SCALE_FACTOR_TERRAIN) + ' to ' + FloatToStr(vz * SCALE_FACTOR_TERRAIN + alterationHere));
                end;
            finally
                joLandAlteration.SaveToFile(wbScriptsPath + 'Seasons\LandHeightsAltered\' + wrldEdid + '\x' + IntToStr(cellXHere) + 'y' + IntToStr(cellYHere) + '.json', False, TEncoding.UTF8, True);
                joLandAlteration.Free;
                joLand.Free;
            end;
        end;
    end;
end;

function GetLandAlteration(joLandAlteration: TJsonObject; row, column: integer): integer;
{
    Gets an alteration value from the joLandAlteration.

    Parameters:
      joLandAlteration - JSON object to hold the landscape height alterations.
      row, column      - Row and column within the cell.

    Returns:
      The alteration value at the specified row and column, or 0 if none exists.
}
begin
    Result := 0;
    try
        Result := joLandAlteration.A[row].S[column];
    except
        Result := 0;
    end;
end;

procedure AddLandAlteration(joLandAlteration: TJsonObject; row, column, alteration: integer);
{
    Adds an alteration value to the joLandAlteration.

    Parameters:
      joLandAlteration - JSON object to hold the landscape height alterations.
      row, column      - Row and column within the cell.
      alteration       - The alteration value to add.
}
var
    i: integer;
begin
    try
        joLandAlteration.A[row].S[column] := alteration;
    except
        for i := 0 to 32 do
            joLandAlteration.A[row].Add('');
        joLandAlteration.A[row].S[column] := alteration;
    end;
end;

function IsCellInRange(wrldEdid: string; cellX, cellY: integer; bCheckNeighbors: boolean): boolean;
var
    idx: integer;
begin
    Result := False;
    idx := slLandscapeCells.IndexOf(wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY));
    if idx <> -1 then Result := True;
    if Result then Exit;
    if not bCheckNeighbors then Exit;

    //right x-1
    idx := slLandscapeCells.IndexOf(wrldEdid + ' ' + IntToStr(cellX - 1) + ' ' + IntToStr(cellY));
    if idx <> -1 then Result := True;
    if Result then Exit;

    //left x+1
    idx := slLandscapeCells.IndexOf(wrldEdid + ' ' + IntToStr(cellX + 1) + ' ' + IntToStr(cellY));
    if idx <> -1 then Result := True;
    if Result then Exit;

    //top y+1
    idx := slLandscapeCells.IndexOf(wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY + 1));
    if idx <> -1 then Result := True;
    if Result then Exit;

    //bottom y -1
    idx := slLandscapeCells.IndexOf(wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY - 1));
    if idx <> -1 then Result := True;
    if Result then Exit;

    //top right diagonal x-1 y+1
    idx := slLandscapeCells.IndexOf(wrldEdid + ' ' + IntToStr(cellX - 1) + ' ' + IntToStr(cellY + 1));
    if idx <> -1 then Result := True;
    if Result then Exit;

    //top left diagonal x+1 y+1
    idx := slLandscapeCells.IndexOf(wrldEdid + ' ' + IntToStr(cellX + 1) + ' ' + IntToStr(cellY + 1));
    if idx <> -1 then Result := True;
    if Result then Exit;

    //bottom right diagonal x+1 y-1
    idx := slLandscapeCells.IndexOf(wrldEdid + ' ' + IntToStr(cellX + 1) + ' ' + IntToStr(cellY - 1));
    if idx <> -1 then Result := True;
    if Result then Exit;

    //bottom left diagonal x-1 y-1
    idx := slLandscapeCells.IndexOf(wrldEdid + ' ' + IntToStr(cellX - 1) + ' ' + IntToStr(cellY - 1));
    if idx <> -1 then Result := True;
    if Result then Exit;
end;

procedure ProcessLandRecords;
{
    Process land records to place snow.
}
var
    i, cellX, cellY, count: integer;
    wrldEdid, fileName: string;

    rLand, rCell, rWrld: IwbElement;
begin
    count := tlLandRecords.Count;
    for i:= 0 to Pred(count) do begin
        rLand := ObjectToElement(tlLandRecords[i]);
        rCell := WinningOverride(LinksTo(ElementByIndex(rLand, 0)));
        cellX := GetElementNativeValues(rCell, 'XCLC\X');
        cellY := GetElementNativeValues(rCell, 'XCLC\Y');
        rWrld := WinningOverride(LinksTo(ElementByIndex(rCell, 0)));
        wrldEdid := GetElementEditValues(rWrld, 'EDID');
        if not bCreateLandscapeSnowMeshes then begin
            if not IsCellInRange(wrldEdid, cellX, cellY, False) then continue;
        end;

        if not LandHeightsExist(wrldEdid, cellX, cellY) then continue;
        AddMessage(IntToStr(i + 1) + ' of ' + IntToStr(count) + #9 + ShortName(rLand) + #9 + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY));

        CreateLandscapeSnow(rLand, rCell, rWrld, wrldEdid, fileName, cellX, cellY);
        if bPlaceLandscapeSnow then PlaceLandscapeSnow(rLand, rCell, rWrld, wrldEdid, fileName, cellX, cellY);
    end;
end;

function PlaceLandscapeSnow(land, rCell, rWrld: IwbElement; wrldEdid, fileProvidingLand: string; cellX, cellY: integer): integer;
var
    unitsX, unitsY, landOffsetZ: integer;
    editorIdSnowNif, snowModel, snowLodModel0, snowLodModel1, snowLodModel2, snowStaticFormid: string;

    snowStatic, nCell, snowRef, base: IwbElement;
begin
    Result := 0;
    if not Assigned(SeasonsMainFile) then Exit;
    unitsX := cellX * 4096;
    unitsY := cellY * 4096;
    editorIdSnowNif := wrldEdid + '_' + IntToStr(cellX) + '_' + IntToStr(cellY);

    snowModel := 'LandscapeSnow\' + editorIdSnowNif + '.nif';
    snowLodModel0 := 'LOD\LandscapeSnow\' + editorIdSnowNif + '_lod_0.nif';
    snowLodModel1 := 'LOD\LandscapeSnow\' + editorIdSnowNif + '_lod_1.nif';
    snowLodModel2 := 'LOD\LandscapeSnow\' + editorIdSnowNif + '_lod_2.nif';
    if not FileExists(wbScriptsPath + 'Seasons\output\Meshes\' + snowModel) then begin
        snowModel := 'LandscapeSnow\LandscapeSnow.nif';
        snowLodModel0 := '';
        snowLodModel1 := '';
        snowLodModel2 := '';
        if not Assigned(flatSnowStatic) then flatSnowStatic := CreateNewStat(snowModel, snowLodModel0, snowLodModel1, snowLodModel2, 'FlatSnowStatic01');
        snowStatic := flatSnowStatic;
    end else snowStatic := CreateNewStat(snowModel, snowLodModel0, snowLodModel1, snowLodModel2, editorIdSnowNif);

    fileProvidingLand := GetFileName(GetFile(land));
    landOffsetZ := joLandscapeHeights.O[fileProvidingLand].O[wrldEdid].O[cellX].O[cellY].S['offset'] * SCALE_FACTOR_TERRAIN;

    AddRequiredElementMasters(rWrld, SeasonsMainFile, False, True);
    AddRequiredElementMasters(rCell, SeasonsMainFile, False, True);
  	SortMasters(SeasonsMainFile);
    wbCopyElementToFile(rWrld, SeasonsMainFile, False, True);
    nCell := wbCopyElementToFile(rCell, SeasonsMainFile, False, True);
    snowRef := Add(nCell, 'REFR', True);
    snowStaticFormid := IntToHex(GetLoadOrderFormID(snowStatic), 8);

    SetElementEditValues(snowRef, 'DATA\Position\X', IntToStr(unitsX + 2048));
    SetElementEditValues(snowRef, 'DATA\Position\Y', IntToStr(unitsY + 2048));
    SetElementEditValues(snowRef, 'DATA\Position\Z', IntToStr(landOffsetZ + 16));

    base := ElementByPath(snowRef, 'NAME');
    SetEditValue(base, snowStaticFormid);
end;

function CreateNewStat(model, lod0, lod1, lod2, editorid: string): IwbElement;
var
    newStatic, newStaticModel, newStaticMNAM: IwbElement;
begin
    newStatic := Add(statGroup, 'STAT', True);
    SetEditorID(newStatic, editorid);
    newStaticModel := Add(Add(newStatic, 'Model', True), 'MODL', True);
    SetEditValue(newStaticModel, model);
    newStaticMNAM := Add(newStatic, 'MNAM', True);
    SetElementEditValues(newStaticMNAM, 'LOD #0 (Level 0)\Mesh', lod0);
    SetElementEditValues(newStaticMNAM, 'LOD #1 (Level 1)\Mesh', lod1);
    SetElementEditValues(newStaticMNAM, 'LOD #2 (Level 2)\Mesh', lod2);
    Result := newStatic;
end;


function CreateLandscapeSnow(land, rCell, rWrld: IwbElement; wrldEdid, fileProvidingLand: string; cellX, cellY: integer): integer;
{
    Creates snow mesh for a landscape cell.

    Parameters:
      land              - The LAND record element for the cell.
      rCell             - The CELL record element for the cell.
      rWrld             - The WRLD record element for the worldspace.
      wrldEdid          - EditorID of the worldspace.
      fileProvidingLand - Filename providing the LAND record.
      cellX, cellY      - Cell coordinates.

    Returns:
      Integer indicating success (1) or failure (0).
}
var
    bVertexIsOutsideCell: boolean;
    i, nifFile, row2, column2, row, column, vertexCount, newCellX, newCellY, landOffsetZ, newlandOffsetZ,
    point1, point2, point3, point4, cellOffsetDifference, newVz: integer;
    editorIdSnowNif, fileName, snowNifFile, xyz, vx, vy, vz, newVzStr, fileNameLand, nx, ny, nz: string;

    tsXYZ, tsNormals: TStrings;
    joLand: TJsonObject;

    vertexData, vertex: TdfElement;
    block: TwbNifBlock;
    snowNif, snowLodNif: TwbNifFile;
begin
    Result := 0;
    editorIdSnowNif := wrldEdid + '_' + IntToStr(cellX) + '_' + IntToStr(cellY);

    // Cache lookup results
    joLand := TJsonObject.Create;
    try
        joLand.Assign(joLandscapeHeights.O[fileProvidingLand].O[wrldEdid].O[cellX].O[cellY]);
        landOffsetZ := joLand.S['offset'];

        for nifFile := 0 to 3 do begin
            case nifFile of
                0: begin //Create full model
                    fileName := wbScriptsPath + 'Seasons\LandscapeSnow.nif';
                    snowNifFile := wbScriptsPath + 'Seasons\output\Meshes\LandscapeSnow\' + editorIdSnowNif + '.nif';
                end;
                1: begin //Create lod model
                    fileName := wbScriptsPath + 'Seasons\LandscapeSnow_lod_0.nif';
                    snowNifFile := wbScriptsPath + 'Seasons\output\Meshes\LOD\LandscapeSnow\' + editorIdSnowNif + '_lod_0.nif';
                end;
                2: begin //Create lod model
                    fileName := wbScriptsPath + 'Seasons\LandscapeSnow_lod_1.nif';
                    snowNifFile := wbScriptsPath + 'Seasons\output\Meshes\LOD\LandscapeSnow\' + editorIdSnowNif + '_lod_1.nif';
                end;
                3: begin //Create lod model
                    fileName := wbScriptsPath + 'Seasons\LandscapeSnow_lod_2.nif';
                    snowNifFile := wbScriptsPath + 'Seasons\output\Meshes\LOD\LandscapeSnow\' + editorIdSnowNif + '_lod_2.nif';
                end;
            end;

            snowNif := TwbNifFile.Create;
            try
                snowNif.LoadFromFile(fileName);
                block := snowNif.Blocks[1];
                vertexCount := block.NativeValues['Num Vertices'];
                vertexData := block.Elements['Vertex Data'];

                // Process vertices in batches
                for i := 0 to Pred(vertexCount) do begin
                    bVertexIsOutsideCell := false;
                    vertex := vertexData[i];
                    xyz := vertex.EditValues['Vertex'];
                    tsXYZ := SplitString(xyz, ' ');
                    vx := tsXYZ[0];
                    vy := tsXYZ[1];
                    vz := tsXYZ[2];

                    // Use integer math where possible
                    row2 := Round(StrToFloatDef(vy, 9)) div 64;
                    // dividing by 64 means row2 is twice the size of the actual row
                    // if row2 is even, this row exists
                    column2 := Round(StrToFloatDef(vx, 9)) div 64;
                    // dividing by 64 means column2 is twice the size of the actual column
                    // if column2 is even, this column exists

                    if (IsEven(row2) and IsEven(column2)) then begin
                        row := row2 div 2;
                        column := column2 div 2;
                        if ((row < 0) or (row > 32) or (column < 0) or (column > 32)) then begin
                            // This will only happen for the LOD models. We have an overlap that goes to the neighboring cell so we can close some gaps for LOD.
                            bVertexIsOutsideCell := true;
                            // Here we get the neighboring cell based off these values.
                            newCellX := cellX; //default to no cell offset
                            newCellY := cellY;
                            if column < 0 then newCellX := cellX - 1
                            else if column > 32 then newCellX := cellX + 1;
                            if row < 0 then newCellY := cellY - 1
                            else if row > 32 then newCellY := cellY + 1;
                            fileNameLand := joLandFiles.O[wrldEdid].O[newCellX].S[newCellY];

                            if fileNameLand <> '' then begin
                                //If the neighboring cell does exist, we need to compare the offset values, as that will affect the newVz.
                                newlandOffsetZ := joLandscapeHeights.O[fileNameLand].O[wrldEdid].O[newCellX].O[newCellY].S['offset'];
                                // We will need to add this difference to the final vz value;
                                cellOffsetDifference := landOffsetZ - newlandOffsetZ;

                                //Get the correct row/column for the overlap cell vertex we wish to go by.
                                // Since our LOD only has resolution of 4 rows/columns, we use 3 or 29.
                                if column < 0 then column := 29
                                else if column > 32 then column := 3;
                                if row < 0 then row := 29
                                else if row > 32 then row := 3;

                                //We subtract the cellOffsetDifference from the final z value.
                                newVz := (joLandscapeHeights.O[fileNameLand].O[wrldEdid].O[newCellX].O[newCellY].A[row].S[column] - cellOffsetDifference) * SCALE_FACTOR_TERRAIN;
                            end
                            else begin
                                //If the neighboring cell does not exist, we should change the row/column to the vertex that is closest and use that for the newVz
                                if column < 0 then column := 0
                                else if column > 32 then column := 32;
                                if row < 0 then row := 0
                                else if row > 32 then row := 32;
                                newVz := joLand.A[row].S[column] * SCALE_FACTOR_TERRAIN;
                            end;
                        end
                        else if ((row = 0) or (row = 32) or (column = 0) or (column = 32)) then begin  //protects borders
                            newVz := joLand.A[row].S[column] * SCALE_FACTOR_TERRAIN;
                        end else begin // in case we want to change to some kind of average or max of the multiple surrounding points later... may be useful for lod or some level of smoothing
                            newVz := joLand.A[row].S[column] * SCALE_FACTOR_TERRAIN;
                        end;
                    end else begin
                        // If odd, this row/column does not exist. This will only happen to the full model for the in-between vertices.
                        // Get the average of 4 points around the vertex to set the new height.

                        row := (row2 - 1)/2;
                        column := (column2 - 1)/2;
                        point1 := joLand.A[row].S[column] * SCALE_FACTOR_TERRAIN;

                        row := (row2 - 1)/2;
                        column := (column2 + 1)/2;
                        point2 := joLand.A[row].S[column] * SCALE_FACTOR_TERRAIN;

                        row := (row2 + 1)/2;
                        column := (column2 + 1)/2;
                        point3 := joLand.A[row].S[column] * SCALE_FACTOR_TERRAIN;

                        row := (row2 + 1)/2;
                        column := (column2 - 1)/2;
                        point4 := joLand.A[row].S[column] * SCALE_FACTOR_TERRAIN;

                        //maxPoint := Max(Max(point1, point2), Max(point3, point4));
                        //newVz := (point1 + point2 + point3 + point4 + maxPoint)/5; //We add the max point again to help even out the average a bit more
                        newVz := ComputeInBetweenVertexHeight(point1, point2, point3, point4);
                    end;

                    //We have z offsets built in for the LOD.
                    if nifFile = 0 then newVzStr := FloatToStr(newVz)
                    else if nifFile = 1 then begin
                        if bVertexIsOutsideCell then newVzStr := FloatToStr(newVz)
                        else newVzStr := FloatToStr(newVz + 40);
                    end else if nifFile = 2 then begin
                        if bVertexIsOutsideCell then newVzStr := FloatToStr(newVz + 40)
                        else newVzStr := FloatToStr(newVz + 192);
                    end else if nifFile = 3 then begin
                        if bVertexIsOutsideCell then newVzStr := FloatToStr(newVz + 192)
                        else newVzStr := FloatToStr(newVz + 672);
                    end;
                    vertex.EditValues['Vertex'] := vx + ' ' + vy + ' ' + newVzStr;
                end;
                if nifFile > 1 then begin
                    block.UpdateNormals;
                    block.UpdateTangents;
                end;
                //We need to protect the border vertices' normals.
                if nifFile = 0 then begin
                    for i := 0 to Pred(vertexCount) do begin
                        vertex := vertexData[i];
                        xyz := vertex.EditValues['Vertex'];
                        tsXYZ := SplitString(xyz, ' ');
                        vx := tsXYZ[0];
                        vy := tsXYZ[1];

                        row2 := Round(StrToFloatDef(vy, 9))/64; // dividing by 64 means row2 is twice the size of the actual row
                        // if row2 is even, this row exists
                        column2 := Round(StrToFloatDef(vx, 9))/64; // dividing by 64 means column2 is twice the size of the actual column
                        // if column2 is even, this column exists
                        if not (IsEven(row2) and IsEven(column2)) then continue;
                        row := row2/2;
                        column := column2/2;
                        if not ((row = 0) or (row = 32) or (column = 0) or (column = 32)) then continue;
                        vertex.EditValues['Normal'] := '0.003922 0.003922 1.000000';
                        vertex.EditValues['Bitangent X'] := '1.000000';
                        vertex.EditValues['Bitangent Y'] := '0.003922';
                        vertex.EditValues['Bitangent Z'] := '0.003922';
                        vertex.EditValues['Tangent'] := '0.003922 -1.000000 0.003922';
                    end;
                end;
                snowNif.SaveToFile(snowNifFile);
            finally
                snowNif.Free;
            end;
        end;
    finally
        joLand.Free;
    end;
    Result := 1;
end;

function ComputeInBetweenVertexHeight(point1, point2, point3, point4: integer): integer;
begin
    Result := (point1 + point2 + point3 + point4)/4;
end;

function CreateLandscapeHeights(land, rCell, rWrld: IwbElement; wrldEdid: string): boolean;
{
    Creates and stores the landscape height data for a given LAND record.
    Parameters:
      land      - The LAND record element for the cell.
      rCell     - The CELL record element for the cell.
      rWrld     - The WRLD record element for the worldspace.
      wrldEdid  - EditorID of the worldspace.
    Returns:
      True if new height data was generated, False otherwise.
}
var
    bLandHeightsExist, bHasWater: boolean;
    landOffsetZ, rowColumnOffsetZ, rowStartVal, landValue, landValueScaled, cellWaterHeightFloat,
    cellX, cellY, row, column, iFlat, iHeightAlwaysBelowWater: integer;
    fileProvidingLand, rowColumn, cellWaterHeight: string;

    joLand, joLandAlteration: TJsonObject;

    landHeightData, eRow: IwbElement;
begin
    Result := False;
    if not bCreateLandscapeHeights then Exit;
    cellX := GetElementNativeValues(rCell, 'XCLC\X');
    cellY := GetElementNativeValues(rCell, 'XCLC\Y');
    fileProvidingLand := GetFileName(GetFile(land));
    bHasWater := True;
    if GetElementEditValues(rCell, 'DATA\Has Water') = '0' then bHasWater := False;
    cellWaterHeight := GetElementEditValues(rCell, 'XCLW');
    if cellWaterHeight = 'Default' then cellWaterHeight := GetElementEditValues(rWrld, 'DNAM\Default Water Height');
    cellWaterHeightFloat := StrToFloatDef(cellWaterHeight, 0);

    landOffsetZ := GetElementNativeValues(land, 'VHGT\Offset');
    landHeightData := ElementByPath(land, 'VHGT\Height Data');

    if not bCreateLandscapeHeights then begin
        bLandHeightsExist := LandHeightsExist(wrldEdid, cellX, cellY);
        if bLandHeightsExist then Exit; //If land heights already exist and we are not creating land heights, exit.
    end;
    Result := True;
    joLand := TJsonObject.Create;
    joLandAlteration := TJsonObject.Create;
    try
        joLand.S['offset'] := Round(landOffsetZ);
        iFlat := 4; // assume the cell is flat
        iHeightAlwaysBelowWater := 8; // assume the cell is always below water

        for row := 0 to 32 do begin
            eRow := ElementByPath(landHeightData, 'Row #' + IntToStr(row));
            for column := 0 to 32 do begin
                rowColumn := 'Column #' + IntToStr(column);
                rowColumnOffsetZ := GetElementNativeValues(eRow, rowColumn);
                if rowColumnOffsetZ > 127 then rowColumnOffsetZ := rowColumnOffsetZ - 256;

                if(column = 0) then begin //check if first column
                    if(row = 0) then begin // check if first row of first column
                        rowStartVal := rowColumnOffsetZ; //rowColumnOffsetZ + landOffsetZ; //if first column, first row, height is the LAND's offset + the offset value. We are omitting landOffsetZ since we want to simply place the landscape snow at z height of landOffsetZ.
                    end else begin
                        rowStartVal := rowColumnOffsetZ + rowStartVal; //if first column, but 2nd or higher row, height is the 1st row's offset + the current offset value
                    end;
                    landValue := rowStartVal;
                end else begin
                    // If it is the 2nd or higher column, height is the previous rowColumn's height + the current offset value.
                    landValue := landValue + rowColumnOffsetZ;
                end;
                landValueScaled := (landValue + landOffsetZ) * SCALE_FACTOR_TERRAIN; //This will be the Z height we apply to the vertex of the nif.
                if landValue <> 0 then iFlat := 0; //If at least one land height is not 0, we will not mark this cell not flat.
                if bHasWater and (landValueScaled < cellWaterHeightFloat) then begin
                    //If the land height is below water, we need to lower it more so the snow does not poke above the water.
                    AddLandAlteration(joLandAlteration, row, column, -32);
                end else begin
                    iHeightAlwaysBelowWater := 0; //If at least one land height is above water, we will not mark this cell as always below water.
                end;
                joLand.A[row].Add(Round(landValue));
                bSaveLandHeights := True;
                Result := True;

                // X Y Z coordinates. Shouldn't need these.
                // pX := FloatToStr(column * 128);
                // pY := FloatToStr(row * 128);
                // pZ := FloatToStr(landValueScaled);
            end;
        end;
        if iFlat = 4 then Result := False;
        if iHeightAlwaysBelowWater = 8 then Result := False;
    finally
        if Result then begin
            EnsureDirectoryExists(wbScriptsPath + 'Seasons\LandHeights\' + wrldEdid + '\');
            joLand.SaveToFile(wbScriptsPath + 'Seasons\LandHeights\' + wrldEdid + '\x' + IntToStr(cellX) + 'y' + IntToStr(cellY) + '.json', False, TEncoding.UTF8, True);
            EnsureDirectoryExists(wbScriptsPath + 'Seasons\LandHeightsAltered\' + wrldEdid + '\');
            joLandAlteration.SaveToFile(wbScriptsPath + 'Seasons\LandHeightsAltered\' + wrldEdid + '\x' + IntToStr(cellX) + 'y' + IntToStr(cellY) + '.json', False, TEncoding.UTF8, True);
        end;
        joLand.Free;
        joLandAlteration.Free;
    end;
end;

function LandHeightsExist(wrldEdid: string; cellX, cellY: integer): boolean;
{
    Checks if landscape height data exists for a given worldspace and cell coordinates.

    Parameters:
      wrldEdid - EditorID of the worldspace.
      cellX    - X coordinate of the cell.
      cellY    - Y coordinate of the cell.

    Returns:
      True if the landscape height data file exists, False otherwise.
}
var
    dir: string;
begin
    Result := False;
    dir := wbScriptsPath + 'Seasons\LandHeights\' + wrldEdid + '\';
    try
        if FileExists(dir + 'x' + IntToStr(cellX) + 'y' + IntToStr(cellY) + '.json') then Result := True;
    except
        Result := False;
    end;
end;

procedure ApplyAlterations;
{
    Merges all alterations from joLandscapeHeightsAltered into joLandscapeHeights, modifying the latter in-place.
}
var
    count, cell, r, w, cellX, cellY, row, column, landValue: integer;
    landFile, wrldEdid, fileName: string;
    alteration: variant;

    joLandAlteration, joLand: TJsonObject;

    slWrldsAltered, slFiles: TStringList;
begin
    count := 0;
    slWrldsAltered := TStringList.Create;
    try
        ListDirectoriesInPath(wbScriptsPath + 'Seasons\LandHeightsAltered\', slWrldsAltered);

        for w := 0 to Pred(slWrldsAltered.Count) do begin
            wrldEdid := slWrldsAltered[w];
            slFiles := TStringList.Create;
            try
                ListFilesInPath(wbScriptsPath + 'Seasons\LandHeightsAltered\' + wrldEdid + '\', slFiles);

                for cell := 0 to Pred(slFiles.Count) do begin
                    // Extract cellX and cellY from filename
                    fileName := slFiles[cell];
                    if not FileExists(wbScriptsPath + 'Seasons\LandHeights\' + wrldEdid + '\' + fileName) then continue;
                    //AddMessage(fileName);
                    cellX := StrToIntDef(Copy(fileName, 2, Pos('y', fileName) - 2), 0);
                    cellY := StrToIntDef(Copy(fileName, Pos('y', fileName) + 1, Pos('.json', fileName) - Pos('y', fileName) - 1), 0);

                    // if not bCreateLandscapeSnowMeshes then
                    //     if not IsCellInRange(wrldEdid, cellX, cellY, False) then continue;

                    Inc(count);
                    AddMessage(IntToStr(count) + #9 + 'Merging alterations into land heights: ' + #9 + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY));

                    landFile := wrldEdid + '\x' + IntToStr(cellX) + 'y' + IntToStr(cellY) + '.json';

                    joLand := TJsonObject.Create;
                    joLandAlteration := TJsonObject.Create;
                    try
                        joLand.LoadFromFile(wbScriptsPath + 'Seasons\LandHeights\' + landFile);
                        joLandAlteration.LoadFromFile(wbScriptsPath + 'Seasons\LandHeightsAltered\' + landFile);
                        for r := 0 to Pred(joLandAlteration.Count) do begin
                            row := joLandAlteration.Names[r];
                            for column := 0 to Pred(joLandAlteration.A[row].Count) do begin
                                alteration := joLandAlteration.A[row].S[column];
                                //skip empty alterations
                                if alteration = '' then continue;
                                // Skip if alteration is zero to avoid unnecessary changes and maintain original land height
                                if alteration = 0 then continue;
                                landValue := joLand.A[row].S[column];
                                joLand.A[row].S[column] := (alteration div SCALE_FACTOR_TERRAIN) + landValue;
                            end;
                        end;
                    finally
                        joLand.SaveToFile(wbScriptsPath + 'Seasons\LandHeights\' + landFile, False, TEncoding.UTF8, True);
                        joLand.Free;
                        joLandAlteration.Free;
                    end;
                end;
            finally
                slFiles.Free;
            end;
        end;
    finally
        slWrldsAltered.Free;
    end;
end;

procedure FixLandscapeSeams;
var
    w, cell, cellX, cellY, count: integer;
    wrldEdid, fileName: string;

    slWrlds, slFiles: TStringList;
begin
    count := 0;
    slWrlds := TStringList.Create;
    try
        ListDirectoriesInPath(wbScriptsPath + 'Seasons\LandHeights\', slWrlds);
        for w := 0 to Pred(slWrlds.Count) do begin
            wrldEdid := slWrlds[w];
            slFiles := TStringList.Create;
            ListFilesInPath(wbScriptsPath + 'Seasons\LandHeights\' + wrldEdid + '\', slFiles);
            try
                for cell := 0 to Pred(slFiles.Count) do begin
                    // Extract cellX and cellY from filename
                    fileName := slFiles[cell];
                    AddMessage(fileName);
                    cellX := StrToIntDef(Copy(fileName, 2, Pos('y', fileName) - 2), 0);
                    cellY := StrToIntDef(Copy(fileName, Pos('y', fileName) + 1, Pos('.json', fileName) - Pos('y', fileName) - 1), 0);

                    // if not bCreateLandscapeSnowMeshes then
                    //     if not IsCellInRange(wrldEdid, cellX, cellY, False) then continue;

                    //For every landscape record we will check the edges for seams against the neighboring cell.
                    Inc(count);
                    AddMessage(IntToStr(count) + #9 + ' Checking landscape seams at' + #9 + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY));
                    FixLandscapeSeamsAgainstNeighbors(wrldEdid, cellX, cellY);
                end;
            finally
                slFiles.Free;
            end;
        end;
    finally
        slWrlds.Free;
    end;
end;

procedure FixLandscapeSeamsAgainstNeighbors(wrldEdid: string; cellX, cellY: integer);
{
    Algorithm:
       For each border vertex of the landscape cell, compares its height with the corresponding vertex in neighboring cells.
       If a neighboring vertex is lower, updates the current vertex to match the neighbor, thus eliminating visible seams.

     Expected Side Effects:
       Modifies the land height data in-place for the current cell and its border vertices, potentially lowering them to match adjacent cells.
       Adds messages for each seam fix performed for traceability.
}
var
    bNeedsSaved: boolean;
    row, rowHere, column, columnHere, currentVz, neighborVz: integer;

    joLand: TJsonObject;
begin
    bNeedsSaved := False;
    joLand := TJsonObject.Create;
    try
        joLand.LoadFromFile(wbScriptsPath + 'Seasons\LandHeights\' + wrldEdid + '\x' + IntToStr(cellX) + 'y' + IntToStr(cellY) + '.json');
        // #####################################
        // Bottom-left corner r0 c0
        // #####################################
        currentVz := GetCurrentLandHeightWithOffset(joLand, 0, 0);

        // cell to the left x-1 y r0 c32
        neighborVz := GetLandHeightWithOffset(wrldEdid, cellX - 1, cellY, 0, 32);
        if Assigned(neighborVz) and (neighborVz < currentVz) then begin
            currentVz := neighborVz;
            AddMessage('Seam fixed between ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'and' + #9 + IntToStr(cellX - 1) + ' ' + IntToStr(cellY));
        end;

        // cell below x y-1 r32 c0
        neighborVz := GetLandHeightWithOffset(wrldEdid, cellX, cellY - 1, 32, 0);
        if Assigned(neighborVz) and (neighborVz < currentVz) then begin
            currentVz := neighborVz;
            AddMessage('Seam fixed between ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'and' + #9 + IntToStr(cellX) + ' ' + IntToStr(cellY - 1));
        end;

        // cell diagonal (bottom-left) x-1 y-1 r32 c32
        neighborVz := GetLandHeightWithOffset(wrldEdid, cellX - 1, cellY - 1, 32, 32);
        if Assigned(neighborVz) and (neighborVz < currentVz) then begin
            currentVz := neighborVz;
            AddMessage('Seam fixed between ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'and' + #9 + IntToStr(cellX - 1) + ' ' + IntToStr(cellY - 1));
        end;
        SetLandHeightWithOffset(joLand, bNeedsSaved, wrldEdid, cellX, cellY, 0, 0, currentVz);

        // #####################################
        // Bottom-right corner r0 c32
        // #####################################
        currentVz := GetCurrentLandHeightWithOffset(joLand, 0, 32);

        // cell to the right x+1 y r0 c0
        neighborVz := GetLandHeightWithOffset(wrldEdid, cellX + 1, cellY, 0, 0);
        if Assigned(neighborVz) and (neighborVz < currentVz) then begin
            currentVz := neighborVz;
            AddMessage('Seam fixed between ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'and' + #9 + IntToStr(cellX + 1) + ' ' + IntToStr(cellY));
        end;

        // cell below x y-1 r32 c32
        neighborVz := GetLandHeightWithOffset(wrldEdid, cellX, cellY - 1, 32, 32);
        if Assigned(neighborVz) and (neighborVz < currentVz) then begin
            currentVz := neighborVz;
            AddMessage('Seam fixed between ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'and' + #9 + IntToStr(cellX) + ' ' + IntToStr(cellY - 1));
        end;

        // cell diagonal (bottom-right) x+1 y-1 r32 c0
        neighborVz := GetLandHeightWithOffset(wrldEdid, cellX + 1, cellY - 1, 32, 0);
        if Assigned(neighborVz) and (neighborVz < currentVz) then begin
            currentVz := neighborVz;
            AddMessage('Seam fixed between ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'and' + #9 + IntToStr(cellX + 1) + ' ' + IntToStr(cellY - 1));
        end;
        SetLandHeightWithOffset(joLand, bNeedsSaved, wrldEdid, cellX, cellY, 0, 32, currentVz);

        // #####################################
        // Top-left corner r32 c0
        // #####################################
        currentVz := GetCurrentLandHeightWithOffset(joLand, 32, 0);

        // cell to the left x-1 y r32 c32
        neighborVz := GetLandHeightWithOffset(wrldEdid, cellX - 1, cellY, 32, 32);
        if Assigned(neighborVz) and (neighborVz < currentVz) then begin
            currentVz := neighborVz;
            AddMessage('Seam fixed between ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'and' + #9 + IntToStr(cellX - 1) + ' ' + IntToStr(cellY));
        end;

        // cell above x y+1 r0 c0
        neighborVz := GetLandHeightWithOffset(wrldEdid, cellX, cellY + 1, 0, 0);
        if Assigned(neighborVz) and (neighborVz < currentVz) then begin
            currentVz := neighborVz;
            AddMessage('Seam fixed between ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'and' + #9 + IntToStr(cellX) + ' ' + IntToStr(cellY + 1));
        end;

        // cell diagonal (top-left) x-1 y+1 r0 c32
        neighborVz := GetLandHeightWithOffset(wrldEdid, cellX - 1, cellY + 1, 0, 32);
        if Assigned(neighborVz) and (neighborVz < currentVz) then begin
            currentVz := neighborVz;
            AddMessage('Seam fixed between ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'and' + #9 + IntToStr(cellX - 1) + ' ' + IntToStr(cellY + 1));
        end;
        SetLandHeightWithOffset(joLand, bNeedsSaved, wrldEdid, cellX, cellY, 32, 0, currentVz);

        // #####################################
        // Top-right corner r32 c32
        // #####################################
        currentVz := GetCurrentLandHeightWithOffset(joLand, 32, 32);

        // cell to the right x+1 y r32 c0
        neighborVz := GetLandHeightWithOffset(wrldEdid, cellX + 1, cellY, 32, 0);
        if Assigned(neighborVz) and (neighborVz < currentVz) then begin
            currentVz := neighborVz;
            AddMessage('Seam fixed between ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'and' + #9 + IntToStr(cellX + 1) + ' ' + IntToStr(cellY));
        end;

        // cell above x y+1 r0 c32
        neighborVz := GetLandHeightWithOffset(wrldEdid, cellX, cellY + 1, 0, 32);
        if Assigned(neighborVz) and (neighborVz < currentVz) then begin
            currentVz := neighborVz;
            AddMessage('Seam fixed between ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'and' + #9 + IntToStr(cellX) + ' ' + IntToStr(cellY + 1));
        end;

        // cell diagonal (top-right) x+1 y+1 r0 c0
        neighborVz := GetLandHeightWithOffset(wrldEdid, cellX + 1, cellY + 1, 0, 0);
        if Assigned(neighborVz) and (neighborVz < currentVz) then begin
            currentVz := neighborVz;
            AddMessage('Seam fixed between ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'and' + #9 + IntToStr(cellX + 1) + ' ' + IntToStr(cellY + 1));
        end;
        SetLandHeightWithOffset(joLand, bNeedsSaved, wrldEdid, cellX, cellY, 32, 32, currentVz);

        // #####################################
        // Bottom Row r0 c1 through c31
        // #####################################

        row := 0;
        for column := 1 to 31 do begin
            currentVz := GetCurrentLandHeightWithOffset(joLand, row, column);

            //neighbor cell is below, so y - 1 and r32
            rowHere := 32;
            neighborVz := GetLandHeightWithOffset(wrldEdid, cellX, cellY - 1, rowHere, column);
            if Assigned(neighborVz) and (neighborVz < currentVz) then begin
                currentVz := neighborVz;
                AddMessage('Seam fixed between ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'and' + #9 + IntToStr(cellX) + ' ' + IntToStr(cellY - 1));
            end;
            SetLandHeightWithOffset(joLand, bNeedsSaved, wrldEdid, cellX, cellY, row, column, currentVz);
        end;

        // #####################################
        // Top Row r32 c1 through c31
        // #####################################

        row := 32;
        for column := 1 to 31 do begin
            currentVz := GetCurrentLandHeightWithOffset(joLand, row, column);

            //neighbor cell is above, so y + 1 and r0
            rowHere := 0;
            neighborVz := GetLandHeightWithOffset(wrldEdid, cellX, cellY + 1, rowHere, column);
            if Assigned(neighborVz) and (neighborVz < currentVz) then begin
                currentVz := neighborVz;
                AddMessage('Seam fixed between ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'and' + #9 + IntToStr(cellX) + ' ' + IntToStr(cellY + 1));
            end;
            SetLandHeightWithOffset(joLand, bNeedsSaved, wrldEdid, cellX, cellY, row, column, currentVz);
        end;

        // #####################################
        // Left Column c0 r1 through r31
        // #####################################

        column := 0;
        for row := 1 to 31 do begin
            currentVz := GetCurrentLandHeightWithOffset(joLand, row, column);

            //neighbor cell is to the left, so x - 1 and c32
            columnHere := 32;
            neighborVz := GetLandHeightWithOffset(wrldEdid, cellX - 1, cellY, row, columnHere);
            if Assigned(neighborVz) and (neighborVz < currentVz) then begin
                currentVz := neighborVz;
                AddMessage('Seam fixed between ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'and' + #9 + IntToStr(cellX - 1) + ' ' + IntToStr(cellY));
            end;
            SetLandHeightWithOffset(joLand, bNeedsSaved, wrldEdid, cellX, cellY, row, column, currentVz);
        end;

        // #####################################
        // Right Column c32 r1 through r31
        // #####################################

        column := 32;
        for row := 1 to 31 do begin
            currentVz := GetCurrentLandHeightWithOffset(joLand, row, column);

            //neighbor cell is to the right, so x + 1 and 0
            columnHere := 0;
            neighborVz := GetLandHeightWithOffset(wrldEdid, cellX + 1, cellY, row, columnHere);
            if Assigned(neighborVz) and (neighborVz < currentVz) then begin
                currentVz := neighborVz;
                AddMessage('Seam fixed between ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + #9 + 'and' + #9 + IntToStr(cellX + 1) + ' ' + IntToStr(cellY));
            end;
            SetLandHeightWithOffset(joLand, bNeedsSaved, wrldEdid, cellX, cellY, row, column, currentVz);
        end;

        // #####################################
        // Rows r1 through r31 Columns c1 through c31
        // #####################################
        // for row := 1 to 31 do begin
        //     for column := 1 to 31 do begin
        //         currentVz := GetCurrentLandHeightWithOffset(joLand, row, column);
        //         SetLandHeightWithOffset(joLand, bNeedsSaved, wrldEdid, cellX, cellY, row, column, currentVz);
        //     end;
        // end;
    finally
        if bNeedsSaved then begin
            joLand.SaveToFile(wbScriptsPath + 'Seasons\LandHeights\' + wrldEdid + '\x' + IntToStr(cellX) + 'y' + IntToStr(cellY) + '.json', False, TEncoding.UTF8, True);
        end;
        joLand.Free;
    end;
end;

function GetLandHeightWithOffset(wrldEdid: string; cellX, cellY, row, column: integer): integer;
{
    Get Land Height at this location including offset.
}
var
    offset, landValue: integer;

    joLand: TJsonObject;
begin
    Result := nil;
    if not LandHeightsExist(wrldEdid, cellX, cellY) then Exit;
    joLand := TJsonObject.Create;
    try
        joLand.LoadFromFile(wbScriptsPath + 'Seasons\LandHeights\' + wrldEdid + '\x' + IntToStr(cellX) + 'y' + IntToStr(cellY) + '.json');
        offset := joLand.S['offset'];
        landValue := joLand.A[row].S[column];
        Result := landValue + offset;
    finally
        joLand.Free;
    end;
end;

function GetCurrentLandHeightWithOffset(joLand: TJsonObject; row, column: integer): integer;
{
    Get Land Height at this location including offset.
}
var
    offset, landValue: integer;
begin
    offset := joLand.S['offset'];
    landValue := joLand.A[row].S[column];
    Result := landValue + offset;
end;

procedure SetLandHeightWithOffset(joLand: TJsonObject; var bNeedsSaved: boolean; wrldEdid: string; cellX, cellY, row, column, vz: integer);
var
    offset: integer;
begin
    offset := joLand.S['offset'];
    joLand.A[row].S[column] := vz - offset;
    bNeedsSaved := True;
end;

procedure FetchRules;
{
    Loads the Rule JSON files.
}
var
    a, c, i: integer;
    fileName, j, key, refKey, fileLoadOrderHexPrefix: string;

    f: IwbFile;
begin
    for i := 0 to Pred(FileCount) do begin
        f := FileByIndex(i);
        fileName := GetFileName(f);
        if fileName = 'Fallout4.exe' then continue;
        if fileName = sSeasonsFileName then begin
            SeasonsMainFile := f;
        end;
        slPluginFiles.Add(fileName);
        LoadRules(fileName);

        if not IsEslFile(f) then continue;
        //Need to map the load order hex prefixes for ESLs so we can match load order formids to them.
        fileLoadOrderHexPrefix := GetLoadOrderFormIDFileIDHexPrefix(f);
        if not Assigned(fileLoadOrderHexPrefix) then continue;
        joLoadOrderFormIDFileID.S[fileLoadOrderHexPrefix] := fileName;
    end;

    j := 'Seasons\AlterLandUserRules.json';
    if ResourceExists(j) then begin
        AddMessage('Loaded Alter Land User Rule File: ' + j);
        joUserAlterLandRules.LoadFromResource(j);
        for c := 0 to Pred(joUserAlterLandRules.Count) do begin
            key := joUserAlterLandRules.Names[c];
            joAlterLandRules.O[key].S['editorid'] := joUserAlterLandRules.O[key].S['editorid'];
            joAlterLandRules.O[key].S['alteration'] := joUserAlterLandRules.O[key].S['alteration'];
            for a := 0 to Pred(joUserAlterLandRules.O[key].O['references'].Count) do begin
                refKey := joUserAlterLandRules.O[key].O['references'].Names[a];
                joAlterLandRules.O[key].O['references'].O[refKey].S['alteration'] := joUserAlterLandRules.O[key].O['references'].O[refKey].S['alteration'];
            end;
        end;
    end;
end;

procedure LoadRules(f: string);
{
    Load Rules
}
var
    c, a: integer;
    j, key, refKey: string;

    sub: TJsonObject;
begin
    //Ignore Worldspaces
    j := 'Seasons\' + TrimLeftChars(f, 4) + ' - Ignore Worldspaces.json';
    if ResourceExists(j) then begin
        AddMessage('Loaded Ignore Worldspaces File: ' + j);
        sub := TJsonObject.Create;
        try
            sub.LoadFromResource(j);
            key := 'Ignore Worldspaces';
            for a := 0 to Pred(sub.A[key].Count) do begin
                if sIgnoredWorldspaces = '' then
                    sIgnoredWorldspaces := sub.A[key].S[a]
                else
                    sIgnoredWorldspaces := sIgnoredWorldspaces + ',' + sub.A[key].S[a];
            end;
        finally
            sub.Free;
            AddMessage('Ignored Worldspaces: ' + sIgnoredWorldspaces);
        end;
    end;

    //Alter Land
    j := 'Seasons\' + TrimLeftChars(f, 4) + ' - Alter Land.json';
    if ResourceExists(j) then begin
        AddMessage('Loaded Alter Land File: ' + j);
        sub := TJsonObject.Create;
        try
            sub.LoadFromResource(j);
            for c := 0 to Pred(sub.Count) do begin
                key := sub.Names[c];
                joAlterLandRules.O[key].S['editorid'] := sub.O[key].S['editorid'];
                joAlterLandRules.O[key].S['alteration'] := sub.O[key].S['alteration'];
                for a := 0 to Pred(sub.O[key].O['references'].Count) do begin
                    refKey := sub.O[key].O['references'].Names[a];
                    joAlterLandRules.O[key].O['references'].O[refKey].S['alteration'] := sub.O[key].O['references'].O[refKey].S['alteration'];
                end;
            end;
        finally
            sub.Free;
        end;
    end;
end;

// ----------------------------------------------------
// Record processing Functions and Procedures go below.
// ----------------------------------------------------

function RecordFormIdFileId(e: IwbElement): string;
{
    Returns the record ID of an element.
}
begin
    e := MasterOrSelf(e);
    Result := IntToHex(FormID(e), 8) + ':' + GetFileName(GetFile(e));
end;

function GetRecordFromFormIdFileId(recordId: string): IwbElement;
{
    Returns the record from the given formid:filename.
}
var
    colonPos, recordFormId: integer;
    f: IwbFile;
begin
    colonPos := Pos(':', recordId);
    recordFormId := StrToInt('$' + Copy(recordId, 1, Pred(colonPos)));
    f := FileByName(Copy(recordId, Succ(colonPos), Length(recordId)));
    Result := RecordByFormID(f, recordFormId, False);
end;

function AddLinkedReference(e: IwbElement; keyword, ref: string): integer;
{
  Add a linked reference.
}
var
    i: integer;

    el, linkedrefs, lref: IwbElement;
begin
    if not ElementExists(e, 'Linked References') then begin
        linkedrefs := Add(e, 'Linked References', True);
        lref := ElementByIndex(linkedrefs, 0);
        SetElementEditValues(lref, 'Keyword/Ref', keyword);
        SetElementEditValues(lref, 'Ref', ref);
    end
    else begin
        linkedrefs := ElementByPath(e, 'Linked References');
        lref := ElementAssign(linkedrefs, HighInteger, nil, False);
        SetElementEditValues(lref, 'Keyword/Ref', keyword);
        SetElementEditValues(lref, 'Ref', ref);
    end;
end;

function IsRefPrecombined(r: IwbElement): boolean;
{
    Checks if a reference is precombined.
}
var
    i, t, preCombinedRefsCount, rc: integer;

    rCell, refs: IwbElement;
begin
    Result := false;
    t := ReferencedByCount(r) - 1;
    if t < 0 then Exit;
    for i := 0 to t do begin
        rCell := ReferencedByIndex(r, i);
        if Signature(rCell) <> 'CELL' then continue;
        if not IsWinningOverride(rCell) then continue;
        //AddMessage(ShortName(r) + ' is referenced in ' + Name(c));
        Result := true;
        Exit;
    end;
end;

function GetLandscapeForCell(rCell: IwbElement): IwbElement;
var
    i: integer;

    r: IwbElement;
    cellchild: IwbGroupRecord;
begin
    Result := nil;
    cellchild := FindChildGroup(ChildGroup(rCell), 9, rCell); // get Temporary group of cell
    for i := 0 to Pred(ElementCount(cellchild)) do begin
        r := ElementByIndex(cellchild, i);
        if Signature(r) <> 'LAND' then continue;
        Result := r;
        Exit;
    end;
end;

function GetIsCleanDeleted(r: IwbElement): Boolean;
{
    Checks to see if a reference has an XESP set to opposite of the PlayerRef
}
begin
    Result := False;
    if not ElementExists(r, 'XESP') then Exit;
    if not GetElementEditValues(r, 'XESP\Flags\Set Enable State to Opposite of Parent') = '1' then Exit;
    if GetElementEditValues(r, 'XESP\Reference') <> 'PlayerRef [PLYR:00000014]' then Exit;
    Result := True;
end;

procedure Shuffle(Strings: TStrings);
{
  Shuffles the order of strings.
}
var
    i: Integer;
begin
    for i := Pred(Strings.Count) downto 1 do Strings.Exchange(i, Random(i + 1));
end;

function TrimRightChars(s: string; chars: integer): string;
{
    Returns right string - chars
}
begin
    Result := RightStr(s, Length(s) - chars);
end;

function TrimLeftChars(s: string; chars: integer): string;
{
    Returns left string - chars
}
begin
    Result := LeftStr(s, Length(s) - chars);
end;

procedure EnsureDirectoryExists(f: string);
{
    Create directories if they do not exist.
}
begin
    if not DirectoryExists(f) then
        if not ForceDirectories(f) then
            raise Exception.Create('Can not create destination directory ' + f);
end;

function IsEven(Number: Integer): Boolean;
begin
  Result := false;
  if Number mod 2 = 0 then Result := true;
end;

// normalize an angle (in degrees) to [0.0, 360.0)
function normalize_angle(angle: double): double;
const
  NORMALIZER = 360.0;
begin
  // FMod(a,b) returns a value between -Abs(b) and Abs(b) exclusive, so need to add b and do it again
  // to fully catch negative angles
  Result := FMod(FMod(angle, NORMALIZER) + NORMALIZER, NORMALIZER);
end;

// clamp given value d between min and max (inclusive)
function clamp(d, min, max: double): double;
begin
  if (CompareValue(d, min, EPSILON) = LessThanValue) then begin
    Result := min;
  end else if (CompareValue(d, max, EPSILON) = GreaterThanValue) then begin
    Result := max;
  end else begin
    Result := d;
  end;
end;


procedure quaternion_to_euler(
  qw, qx, qy, qz: double;                    // input quaternion
  var return_x, return_y, return_z: double;  // euler angle (in degrees)
);
var
  p0, p1, p2, p3: double;              // variables representing dynamically-ordered quaternion components
  singularity_check: double;           // contains value used for the singularity check
  e: integer;                          // variable representing sign used in angle calculations
  euler_order: array[0..2] of double;  // holds mapping between rotation sequence angles and output angles
  euler_angle: array[0..2] of double;  // output angles
begin
  // map quaternion components to generic p-variables, and set the sign
  p0 := qw;

  //rotation sequence
  p1 := qz; p2 := qy; p3 := qx; e :=  1;

  // create mapping between the euler angle and the rotation sequence
  euler_order[0] := 2; euler_order[1] := 1; euler_order[2] := 0;

  // calculate the value to be used to check for singularities
  singularity_check := 2.0 * (p0 * p2 - e * p1 * p3);

  // calculate second rotation angle, clamping it to prevent ArcSin from erroring
  euler_angle[euler_order[1]] := ArcSin(clamp(singularity_check, -1.0, 1.0));

  // a singularity exists when the second angle in a rotation sequence is at +/-90 degrees
  if (CompareValue(Abs(singularity_check), 1.0, EPSILON) = LessThanValue) then begin
    euler_angle[euler_order[0]] := ArcTan2(2.0 * (p0 * p1 + e * p2 * p3), 1.0 - 2.0 * (p1 * p1 + p2 * p2));
    euler_angle[euler_order[2]] := ArcTan2(2.0 * (p0 * p3 + e * p1 * p2), 1.0 - 2.0 * (p2 * p2 + p3 * p3));
  end else begin
    // when a singularity is detected, the third angle basically loses all meaning so is set to 0
    euler_angle[euler_order[0]] := ArcTan2(2.0 * (p0 * p1 - e * p2 * p3), 1.0 - 2.0 * (p1 * p1 + p3 * p3));
    euler_angle[euler_order[2]] := 0.0;
  end;

  // convert results to degrees and then normalize them
  return_x := normalize_angle(RadToDeg(euler_angle[0]));
  return_y := normalize_angle(RadToDeg(euler_angle[1]));
  return_z := normalize_angle(RadToDeg(euler_angle[2]));
end;

procedure euler_to_quaternion(
  x, y, z: double;                                         // euler angle in degrees
  var return_qw, return_qx, return_qy, return_qz: double;  // quaternion components
);
var
  cos_x, cos_y, cos_z, sin_x, sin_y, sin_z: double;
  sign_w, sign_x, sign_y, sign_z: integer;
begin
  // normalize angles and convert them to radians
  x := DegToRad(normalize_angle(x));
  y := DegToRad(normalize_angle(y));
  z := DegToRad(normalize_angle(z));

  // calculate cosine and sine of the various angles once instead of multiple times
  cos_x := Cos(x / 2.0); cos_y := Cos(y / 2.0); cos_z := Cos(z / 2.0);
  sin_x := Sin(x / 2.0); sin_y := Sin(y / 2.0); sin_z := Sin(z / 2.0);

  // use the rotation sequence to determine what signs are used when calculating quaternion components
  // Rotation sequence
  sign_w :=  1; sign_x := -1; sign_y :=  1; sign_z := -1;

  // calculate the quaternion components
  return_qw := cos_x * cos_y * cos_z + sign_w * sin_x * sin_y * sin_z;
  return_qx := sin_x * cos_y * cos_z + sign_x * cos_x * sin_y * sin_z;
  return_qy := cos_x * sin_y * cos_z + sign_y * sin_x * cos_y * sin_z;
  return_qz := cos_x * cos_y * sin_z + sign_z * sin_x * sin_y * cos_z;
end;

// multiply two quaternions together - note that quaternion multiplication is NOT commutative, so
// (q1 * q2) != (q2 * q1)
procedure quaternion_multiply(
  qw1, qx1, qy1, qz1: double;                              // input quaternion 1
  qw2, qx2, qy2, qz2: double;                              // input quaternion 2
  var return_qw, return_qx, return_qy, return_qz: double;  // result quaternion
);
begin
  return_qw := qw1 * qw2 - qx1 * qx2 - qy1 * qy2 - qz1 * qz2;
  return_qx := qw1 * qx2 + qx1 * qw2 + qy1 * qz2 - qz1 * qy2;
  return_qy := qw1 * qy2 - qx1 * qz2 + qy1 * qw2 + qz1 * qx2;
  return_qz := qw1 * qz2 + qx1 * qy2 - qy1 * qx2 + qz1 * qw2;
end;

// compute the difference between two quaternions, q1 and q2, using the formula (q_result = q1' * q2),
// where q1' is the inverse (conjugate) of the first quaternion
procedure quaternion_difference(
  qw1, qx1, qy1, qz1: double;                              // first quaternion
  qw2, qx2, qy2, qz2: double;                              // second quaternion
  var return_qw, return_qx, return_qy, return_qz: double;  // difference quaternion
);
var
  qw1i, qx1i, qy1i, qz1i: double;  // inverse (conjugate) of the first quaternion
begin
  quaternion_inverse(  // calculate (q1')
    qw1, qx1, qy1, qz1,
    qw1i, qx1i, qy1i, qz1i
  );
  quaternion_multiply(  // calculate (q1' * q2)
    qw1i, qx1i, qy1i, qz1i,
    qw2, qx2, qy2, qz2,
    return_qw, return_qx, return_qy, return_qz
  );
end;

// get the inverse of a quaternion
procedure quaternion_inverse(
  qw, qx, qy, qz: double;                                  // input quaternion
  var return_qw, return_qx, return_qy, return_qz: double;  // inverted quaternion
);
begin
  return_qw := qw;
  return_qx := -qx;
  return_qy := -qy;
  return_qz := -qz;
end;

procedure rotate_position(
  vx, vy, vz: double;                           // initial position vector (x, y, z coordinates)
  rx, ry, rz: double;                           // rotation to apply (euler angle)
  var return_vx, return_vy, return_vz: double;  // final position vector (x, y, z coordinates)
);
var
  qx, qy, qz, qw: double;      // quaternion representing rotation to be applied
  qwv, qxv, qyv, qzv: double;  // quaternion representing the result of the vector/quaternion multiplication
begin
  euler_to_quaternion(
    rx, ry, rz,
    qw, qx, qy, qz
  );

  // everything i've read says this should be (q * v * q'), but only (q' * (v * q)) gives the correct
  // results *shrug*
  quaternion_multiply(  // calculate (v * q)
    0.0, vx, vy, vz,
    qw, qx, qy, qz,
    qwv, qxv, qyv, qzv
  );
  // instead of computing q', then multiplying that by (v * q) manually, we can compute the
  // difference between them and get the same result (because it's the same math)
  quaternion_difference(  // calculate (q' * (v * q))
    qw, qx, qy, qz,
    qwv, qxv, qyv, qzv,
    nil, return_vx, return_vy, return_vz  // the returned w component is irrelevant and so is discarded
  );
end;

// rotate a rotation (duh) via quaternion math (vs matrix math)
procedure rotate_rotation(
  x, y, z: double;                          // initial rotation (euler angle)
  rx, ry, rz: double;                       // rotation to apply (euler angle)
  var return_x, return_y, return_z: double  // final rotation (euler angle)
);
var
  qw1, qx1, qy1, qz1: double;  // quaternion representing initial rotation
  qw2, qx2, qy2, qz2: double;  // quaternion representing rotation to be applied
  qw3, qx3, qy3, qz3: double;  // quaternion representing final rotation
begin
  euler_to_quaternion(
    x, y, z,
    qw1, qx1, qy1, qz1
  );
  euler_to_quaternion(
    rx, ry, rz,
    qw2, qx2, qy2, qz2
  );

  // everything i've read says this should be (q2 * q1), but only (q1 * q2) gives the correct results
  // *shrug*
  quaternion_multiply(  // calculate (q1 * q2)
    qw1, qx1, qy1, qz1,
    qw2, qx2, qy2, qz2,
    qw3, qx3, qy3, qz3
  );

  quaternion_to_euler(
    qw3, qx3, qy3, qz3,
    return_x, return_y, return_z
  );
end;

// compute the difference between two rotations by converting them to quaternions, using the
// quaternion_difference function, and converting the result back to an euler angle
procedure rotation_difference(
  x1, y1, z1: double;                        // input rotation 1 (euler angle)
  x2, y2, z2: double;                        // input rotation 2 (euler angle)
  var return_x, return_y, return_z: double;  // output rotation (euler angle)
);
var
  qw1, qx1, qy1, qz1: double;  // quaternion representing rotation 1
  qw2, qx2, qy2, qz2: double;  // quaternion representing rotation 2
  qw3, qx3, qy3, qz3: double;  // quaternion representing the difference between the two rotations
begin
  euler_to_quaternion(
    x1, y1, z1,
    qw1, qx1, qy1, qz1
  );
  euler_to_quaternion(
    x2, y2, z2,
    qw2, qx2, qy2, qz2
  );
  quaternion_difference(
    qw1, qx1, qy1, qz1,
    qw2, qx2, qy2, qz2,
    qw3, qx3, qy3, qz3
  );
  quaternion_to_euler(
    qw3, qx3, qy3, qz3,
    return_x, return_y, return_z
  );
end;

function CreateLabel(aParent: TControl; x, y: Integer; aCaption: string): TLabel;
{
    Create a label.
}
begin
    Result := TLabel.Create(aParent);
    Result.Parent := aParent;
    Result.Left := x;
    Result.Top := y;
    Result.Caption := aCaption;
end;

procedure SortJSONObjectKeys(JSONObj: TJsonObject);
{
    Sorts JSON keys alphabetically.
}
var
    SortedKeys: TStringList;
    Key: string;
    NewJSONObj: TJsonObject;
    i: integer;
begin
    // Create a sorted list of keys
    SortedKeys := TStringList.Create;
    NewJSONObj := TJsonObject.Create;
    try
        for i := 0 to Pred(JSONObj.Count) do SortedKeys.Add(JSONObj.Names[i]);
        SortedKeys.Sort; // Sort the keys alphabetically

        for i := 0 to Pred(SortedKeys.Count) do begin
            Key := SortedKeys[i];
            NewJSONObj.O[Key].Assign(JSONObj.O[Key]);
        end;

        // Replace the original JSONObj with the sorted one
        JSONObj.Clear;
        JSONObj.Assign(NewJSONObj);
    finally
        SortedKeys.Free;
        NewJSONObj.Free;
    end;
end;

function IsHexChar(C: Char): Boolean;
var
    hexchars: string;
    i: integer;
begin
    hexchars := '0123456789abcdefABCDEF';
    Result := (Pos(C, hexchars) > 0);
end;

function IsValidHex(s: string): Boolean;
var
    i: Integer;
    charHere: char;
begin
    Result := (s <> '');
    for i := 1 to Length(s) do
        if not IsHexChar(Copy(s, i, 1)) then begin
            Result := False;
            break;
        end;
end;

procedure SortAlterLandJSONObjectKeys;
{
    Sorts Alter Land JSON keys by Editor ID.
}
var
    SortedEDIDs: TStringList;
    Key, edid: string;
    NewJSONObj, joEDIDKeyMap: TJsonObject;
    i: integer;
begin
    // Create a sorted list of keys
    SortedEDIDs := TStringList.Create;
    joEDIDKeyMap := TJsonObject.Create;
    NewJSONObj := TJsonObject.Create;
    try
        for i := 0 to Pred(joAlterLandRules.Count) do begin
            Key := joAlterLandRules.Names[i];
            edid := EditorID(GetRecordFromFormIdFileId(Key));
            SortedEDIDs.Add(edid);
            joEDIDKeyMap.S[edid] := Key;
        end;
        SortedEDIDs.Sort; // Sort the keys alphabetically

        for i := 0 to Pred(SortedEDIDs.Count) do begin
            Key := joEDIDKeyMap.S[SortedEDIDs[i]];
            NewJSONObj.S[Key] := joAlterLandRules.S[Key];
        end;

        // Replace the original joAlterLandRules with the sorted one
        joAlterLandRules.Clear;
        joAlterLandRules.Assign(NewJSONObj);
    finally
        SortedEDIDs.Free;
        joEDIDKeyMap.Free;
        NewJSONObj.Free;
    end;
end;

function GetFileFromLoadOrderFormID(formid: cardinal): IwbFile;
{
    Attempts to get the file from a load order formid.
    Requires joLoadOrderFormIDFileID was made.
}
var
    loadOrderIdx, eslIdx, mask: cardinal;
begin
    Result := nil;
    mask := $FF000000;
    loadOrderIdx := (mask and formid) shr 24;
    if (loadOrderIdx = $FE) then begin
        //ESL
        mask := $FFFFF000;
        eslIdx := (mask and formid) shr 12;
        Result := FileByName(joLoadOrderFormIDFileID.S[IntToHex(eslIdx, 5)]);
    end else begin
        Result := FileByLoadOrderFileID(loadOrderIdx);
    end;
end;

function GetLoadOrderFormIDFileIDHexPrefix(f: IwbFile): string;
{
    Given a file, finds the load order formid prefix in hex.
}
var
    loadOrderIdx, eslIdx, loadOrderFormId: cardinal;
    e: IwbElement;
begin
    Result := nil;
    e := GetFirstMasteredElement(f);
    if not Assigned(e) then Exit;
    loadOrderFormId := GetLoadOrderFormID(e);
    loadOrderIdx := ($FF000000 and loadOrderFormId) shr 24;
    if (loadOrderIdx = $FE) then begin
        eslIdx := (loadOrderFormId and $FFFFF000) shr 12;
        Result := IntToHex(eslIdx, 5);
    end else begin
        Result := IntToHex(loadOrderIdx, 2);
    end;
end;

function GetFirstMasteredElement(f: IwbFile): IwbElement;
{
    Returns the first mastered element in a file.
}
var
    i, j: integer;

    e, g: IwbElement;
begin
    Result := nil;
    //We start with 1 since the element at index 0 is the file header.
    for i := 1 to Pred(ElementCount(f)) do begin
        g := ElementByIndex(f, i);
        if SameText(Signature(g), 'GRUP') then begin
            for j := 0 to Pred(ElementCount(g)) do begin
                e := ElementByIndex(g, j);
                if IsMaster(e) then begin
                    Result := e;
                    Exit;
                end;
            end;
        end;
    end;
end;

function IsEslFile(f: IwbFile): boolean;
begin
    Result := (GetElementEditValues(ElementByIndex(f, 0), 'Record Header\Record Flags\ESL') = '1');
end;

procedure ListStringsInStringList(sl: TStringList);
{
    Given a TStringList, add a message for all items in the list.
}
var
    i, count: integer;
begin
    count := sl.Count;
    if count < 1 then Exit;
    AddMessage('=======================================================================================');
    for i := 0 to Pred(count) do AddMessage(sl[i]);
    AddMessage('=======================================================================================');
end;

procedure ListDirectoriesInPath(path: string; sl: TStringList);
{
    Given a path, list all directories in that path.
}
var
    sr: TSearchRec;
    res: integer;
begin
    res := FindFirst(path + '\*', faDirectory, sr);
    try
        while res = 0 do begin
            if (sr.Attr and faDirectory) = faDirectory then begin
                if (sr.Name <> '.') and (sr.Name <> '..') then begin
                    sl.Add(sr.Name);
                end;
            end;
            res := FindNext(sr);
        end;
    finally
        FindClose(sr);
    end;
end;

procedure ListFilesInPath(path: string; sl: TStringList);
{
    Given a path, list all files in that path.
}
var
    sr: TSearchRec;
    res: integer;
begin
    res := FindFirst(path + '\*', faAnyFile, sr);
    try
        while res = 0 do begin
            if (sr.Attr and faDirectory) <> faDirectory then begin
                if (sr.Name <> '.') and (sr.Name <> '..') then begin
                    sl.Add(sr.Name);
                end;
            end;
            res := FindNext(sr);
        end;
    finally
        FindClose(sr);
    end;
end;


end.
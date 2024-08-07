{
  Generate Season references.
}
unit Seasons;

// ----------------------------------------------------
//Create variables that will need to be used accross multiple functions/procedures.
// ----------------------------------------------------

var
  slBaseRefs, slOld, slNew, slFull, slFall, slWinter, slSpring, slSummer, slOutdoorNonPrecombinedOld, slOutdoorNonPrecombinedFull, slOutdoorOppositeWinterDecals, slOutdoorAutumnDecals, slOutdoorNonPrecombinedSwaps, slSkipEditorIDs : TStringList;
  SeasonsMainFile, SeasonsPatch, iCurrentPlugin: IInterface;
  joRules: TJsonObject;

const
  sSeasonsFileName = 'Seasons.esm';
  sSeasonsPatchPluginName = 'Seasons - Patch.esp';
  sReferenceSignatures = 'STAT,SCOL,TXST';

// ----------------------------------------------------
// Main functions and procedures go immediately below.
// ----------------------------------------------------

function Finalize: integer;
{
    This function is called at the end.
}
begin
  slBaseRefs.Free;
  slOld.Free;
  slNew.Free;
  slFull.Free;
  slFall.Free;
  slWinter.Free;
  slSpring.Free;
  slSummer.Free;
  slOutdoorNonPrecombinedOld.Free;
  slOutdoorNonPrecombinedFull.Free;
  slOutdoorOppositeWinterDecals.Free;
  slOutdoorAutumnDecals.Free;
  slSkipEditorIDs.Free;
  slOutdoorNonPrecombinedSwaps.Free;

  Result := 0;
end;

procedure CreateObjects;
{
    Create objects.
}
begin
  //TLists

  //TStringLists
  slBaseRefs := TStringList.Create;
  slBaseRefs.Sorted := true;
  slBaseRefs.Duplicates := dupIgnore;

  slOld := TStringList.Create;
  slNew := TStringList.Create;
  slFull := TStringList.Create;
  slFall := TStringList.Create;
  slWinter := TStringList.Create;
  slSpring := TStringList.Create;
  slSummer := TStringList.Create;
  slOutdoorNonPrecombinedOld := TStringList.Create;
  slOutdoorNonPrecombinedFull := TStringList.Create;
  slOutdoorOppositeWinterDecals := TStringList.Create;
  slOutdoorAutumnDecals := TStringList.Create;
  slSkipEditorIDs := TStringList.Create;
  slOutdoorNonPrecombinedSwaps := TStringList.Create;

  //TJsonObjects
  joRules := TJsonObject.Create;
end;

procedure LoadRules(f: string);
{
  Load JSON rules
}
var
  sub: TJsonObject;
  c, a: integer;
  j, key: string;
  bFirstRuleJson: Boolean;
begin
  j := 'Seasons\' + TrimLeftChars(f, 4) + '.json';
  if ResourceExists(j) then begin
    AddMessage('Loaded Rule File: ' + j);
    if bFirstRuleJson then begin
      bFirstRuleJson := False;
      joRules.LoadFromResource(j);
    end
    else begin
      sub := TJsonObject.Create;
      sub.LoadFromResource(j);
      for c := 0 to Pred(sub.Count) do begin
        key := sub.Names[c];
        for a := 0 to Pred(sub.A[key].Count) do joRules.A[key].Add(sub.A[key].S[a]);
      end;
      sub.Free;
    end;
  end;
end;

function MainProgram: Boolean;
{
  Main program.
}
begin
  CreateObjects;

  if not LoadPlugins then begin
    Result := False;
    Exit;
  end;

  if not LoadFormLists then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function LoadPlugins: boolean;
{
  Load plugins
}
var
  i: integer;
  f: string;
begin
  for i := 0 to Pred(FileCount) do begin
    f := GetFileName(FileByIndex(i));
    if SameText(f, sSeasonsFileName) then
      SeasonsMainFile := FileByIndex(i)
    else if SameText(f, sSeasonsPatchPluginName) then
      SeasonsPatch := FileByIndex(i);
  end;

  iCurrentPlugin := SeasonsMainFile;

  if not Assigned(SeasonsMainFile) then begin
    MessageDlg('Seasons.esm is not loaded: ' + SeasonsMainFile, mtError, [mbOk], 0);
    Result := False;
    Exit;
  end;

  if not Assigned(SeasonsPatch) then
    SeasonsPatch := AddNewFileName(sSeasonsPatchPluginName, False);

  AddMasterIfMissing(SeasonsPatch, 'Fallout4.esm');
  AddMasterIfMissing(SeasonsPatch, sSeasonsFileName);
  Result := True;
end;

function LoadFormLists: boolean;
{
  Load the formlists in Seasons
}
var
  formLists, formids: IInterface;
  i, n, C: Integer;
  f, editorID, s: String;
begin
  formLists := GroupBySignature(SeasonsMainFile, 'FLST');
  if not Assigned(formLists) then begin
    AddMessage('No Formlists found');
    Result := False;
    Exit;
  end;

  for i := 0 to Pred(ElementCount(formLists)) do begin
    //AddMessage(Name(ElementByIndex(formLists, i)));
    formids := ElementByName(WinningOverride(ElementByIndex(formLists, i)), 'FormIDs');
    editorID := GetElementEditValues(ElementByIndex(formLists, i), 'EDID');
    if editorID = 'workshopScrapRecipe_Tree_seasons' then
      C := 1
    else if editorID = 'workshopScrapRecipe_TreeLarge_seasons' then
      C := 2
    else if editorID = 'original_trees' then
      C := 3
    else if editorID = 'Fall_swaps' then
      C := 4
    else if editorID = 'Winter_swaps' then
      C := 5
    else if editorID = 'Spring_swaps' then
      C := 6
    else if editorID = 'Summer_swaps' then
      C := 7
    else if editorID = 'Outdoor_Only_NonPrecombined_Originals' then
      C := 8
    else if editorID = 'Outdoor_Only_NonPrecombined_Seasons' then
      C := 9
    else if editorID = 'Outdoor_Only_Opposite_Winter_Decals' then
      C := 10
    else if editorID = 'Outdoor_Only_Enable_Autumn_Decals' then
      C := 11
    else if editorID = 'Outdoor_Only_NonPrecombined_SeasonSwapOnly' then
      C := 12
    else
      C := 99;
    //AddMessage(editorID);
    for n := 0 to Pred(ElementCount(formids)) do begin
      s := GetEditValue(ElementByIndex(formids, n));
      //AddMessage(s);
      Case C of
        1 : slNew.Add(s);
        2 : slFull.Add(s);
        3 : slOld.Add(s);
        4 : slFall.Add(s);
        5 : slWinter.Add(s);
        6 : slSpring.Add(s);
        7 : slSummer.Add(s);
        8 : slOutdoorNonPrecombinedOld.Add(s);
        9 : slOutdoorNonPrecombinedFull.Add(s);
        10 : slOutdoorOppositeWinterDecals.Add(s);
        11 : slOutdoorAutumnDecals.Add(s);
        12 : slOutdoorNonPrecombinedSwaps.Add(s);
      else AddMessage('Skipped ' + s);
      end;
    end;
  end;
  Result := True;
end;

function Initialize: Integer;
{
  This function is called at the beginning.
}
var
  i, n, C: Integer;
  formLists, formids: IInterface;
  f, editorID, s: String;
begin
  if not MainMenuForm then begin
    Result := 1;
    Exit;
  end;

  if not MainProgram then begin
    Result := 1;
    Exit;
  end;
end;

// ----------------------------------------------------
// UI functions and procedures go below.
// ----------------------------------------------------

function MainMenuForm: Boolean;
{
  Main menu form.
}
var
  frm: TForm;
  btnStart, btnCancel: TButton;
  pnl: TPanel;
  picSeasons: TPicture;
  fImage: TImage;
  gbOptions: TGroupBox;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'Seasons Change';
    frm.Width := 580;
    frm.Height := 480;
    frm.Position := poMainFormCenter;
    frm.BorderStyle := bsDialog;
    frm.KeyPreview := True;
    frm.OnClose := frmOptionsFormClose;
    frm.OnKeyDown := FormKeyDown;

    picSeasons := TPicture.Create;
    picSeasons.LoadFromFile(DataPath() + 'Seasons\Seasons Change.jpg');

    fImage := TImage.Create(frm);
    fImage.Picture := picSeasons;
    fImage.Parent := frm;
    fImage.Width := 549;
    fImage.Height := 300;
    fImage.Left := 10;
    fImage.Top := 12;

    gbOptions := TGroupBox.Create(frm);
    gbOptions.Parent := frm;
    gbOptions.Left := 10;
    gbOptions.Top := fImage.Top + fImage.Height + 24;
    gbOptions.Width := frm.Width - 30;
    gbOptions.Caption := 'Seasons Change';
    gbOptions.Height := 94;

    btnStart := TButton.Create(gbOptions);
    btnStart.Parent := gbOptions;
    btnStart.Caption := 'Start';
    btnStart.ModalResult := mrOk;
    btnStart.Top := 62;

    btnCancel := TButton.Create(gbOptions);
    btnCancel.Parent := gbOptions;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Top := btnStart.Top;

    btnStart.Left := gbOptions.Width - btnStart.Width - btnCancel.Width - 16;
    btnCancel.Left := btnStart.Left + btnStart.Width + 8;

    pnl := TPanel.Create(gbOptions);
    pnl.Parent := gbOptions;
    pnl.Left := 8;
    pnl.Top := btnStart.Top - 12;
    pnl.Width := gbOptions.Width - 16;
    pnl.Height := 2;

    frm.ActiveControl := btnStart;

    if frm.ShowModal <> mrOk then begin
      Result := False;
      Exit;
    end
    else Result := True;

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

// ----------------------------------------------------
// Record processing Functions and Procedures go below.
// ----------------------------------------------------

procedure ProcessTXST(e: IInterface; season: String);
{
  Process decals.
}
var
  m, n, r, rCell, wrld : IInterface;
  i, idx : Integer;
  baseRecordId, recordId, sSeason : String;
  tlReferences : TList;
begin
  idx := slOutdoorOppositeWinterDecals.IndexOf(Name(e));
  if idx = -1 then Exit;
  baseRecordId := GetFileName(e) + #9 + ShortName(e);

  if Pos(ShortName(e), slBaseRefs.Text) <> 0 then begin
    //AddMessage('Already processed ' + baseRecordId);
    Exit;
  End;
  slBaseRefs.Add(baseRecordId);
  AddMessage(Name(e));
  tlReferences := TList.Create;

  //make list of references to work on
  m := MasterOrSelf(e);
  for i := 0 to ReferencedByCount(m) - 1 do begin
    r := ReferencedByIndex(m, i);
    recordId := GetFileName(r) + #9 + Name(r);

    if Signature(r) <> 'REFR' then begin
      //AddMessage('Skipping non-reference: ' + recordId);
      continue;
    end;
    if not IsWinningOverride(r) then begin
      //AddMessage('Skipping not winning override: ' + recordId);
      continue;
    end;
    if GetIsDeleted(r) then begin
      //AddMessage('Skipping deleted reference: ' + recordId);
      continue;
    end;
    if ElementExists(r, 'XESP') then begin
      //AddMessage('Skipping reference with pre-existing XESP - Enable Parent: ' + recordId);
      continue;
    end;

    rCell := LinksTo(ElementByIndex(r, 0));
    if slSkipEditorIDs.IndexOf(GetElementEditValues(rCell, 'EDID')) <> -1 then begin
      //AddMessage('Skipped reference in blacklisted cell: ' + recordId);
      continue;
    end;

    if (GetElementEditValues(rCell, 'DATA - Flags\Is Interior Cell') = 1) then begin
      //AddMessage('Skipped reference in interior cell: ' + recordId);
      continue;
    end;

    wrld := LinksTo(ElementByIndex(rCell, 0));
    if slSkipEditorIDs.IndexOf(GetElementEditValues(wrld, 'EDID')) <> -1 then begin
      //AddMessage('Skipped reference in blacklisted world: ' + recordId);
      continue;
    end;
    tlReferences.Add(r);
  end;

  //process the references
  for i := 0 to tlReferences.Count - 1 do begin

    r := ObjectToElement(tlReferences[i]);
    recordId := GetFileName(r) + #9 + Name(r);

    ICurrentPlugin := RefMastersDeterminePlugin(r);
    n := wbCopyElementToFile(r, ICurrentPlugin, False, True);

    Add(n, 'XESP', True);

    if season = 'winter' then begin
      sSeason := '000995D6'; //winter
      SetElementEditValues(n, 'XESP\Reference', sSeason);
      SetElementEditValues(n, 'XESP\Flags\Set Enable State to Opposite of Parent', 1);
    end;
    if season = 'autumn' then begin
      sSeason := '0021B1F2'; //autumn
      SetElementEditValues(n, 'XESP\Reference', sSeason);
    end;
  end;

  tlReferences.Free
end;

function ProcessRef(e: IInterface): Integer;
{
  a
}
var
  b, m, r, n, rCell, wrld, ICellPlugin: IInterface;
  i, idx, s, P, countPrecombined, countNotPrecombined : Integer;
  model, baseRecordId, recordId : String;
  OutDoorOnly, NotPrecombinedOnly : Boolean;
  sl : TStringList;
  tlReferences, tlCells : TList;
begin
  if Pos(Signature(e), sReferenceSignatures) = 0 then
    Exit;

  if Signature(e) = 'TXST' then begin
    idx := slOutdoorOppositeWinterDecals.IndexOf(Name(e));
    if idx > -1 then
      ProcessTXST(e, 'winter');
    idx := slOutdoorAutumnDecals.IndexOf(Name(e));
    if idx > -1 then
      ProcessTXST(e, 'autumn');
    Exit;
  End;

  //AddMessage(Name(e));
  idx := slOld.IndexOf(Name(e));
  OutDoorOnly := false;
  NotPrecombinedOnly := false;
  if idx = -1 then begin
    idx := slOutdoorNonPrecombinedOld.IndexOf(Name(e));
    if idx = -1 then
      Exit;
    OutDoorOnly := true;
    NotPrecombinedOnly := true;
  end;

  baseRecordId := GetFileName(e) + #9 + ShortName(e);

  if Pos(ShortName(e), slBaseRefs.Text) <> 0 then begin
    //AddMessage('Already processed ' + baseRecordId);
    Exit;
  End;
  slBaseRefs.Add(baseRecordId);
  AddMessage(Name(e));
  tlReferences := TList.Create;
  tlCells := TList.Create;

  m := MasterOrSelf(e);
  for i := 0 to ReferencedByCount(m) - 1 do begin
    r := ReferencedByIndex(m, i);
    recordId := GetFileName(r) + #9 + Name(r);

    if Signature(r) <> 'REFR' then begin
      //AddMessage('Skipping non-reference: ' + recordId);
      continue;
    end;
    if not IsWinningOverride(r) then begin
      //AddMessage('Skipping not winning override: ' + recordId);
      continue;
    end;
    if GetIsDeleted(r) then begin
      //AddMessage('Skipping deleted reference: ' + recordId);
      continue;
    end;
    if ElementExists(r, 'XESP') then begin
      //AddMessage('Skipping reference with pre-existing XESP - Enable Parent: ' + recordId);
      continue;
    end;

    rCell := WinningOverride(LinksTo(ElementByIndex(r, 0)));
    if slSkipEditorIDs.IndexOf(GetElementEditValues(rCell, 'EDID')) <> -1 then begin
      //AddMessage('Skipped reference in blacklisted cell: ' + recordId);
      continue;
    end;

    if (GetElementEditValues(rCell, 'DATA - Flags\Is Interior Cell') = 1) and OutDoorOnly then begin
      AddMessage('Skipped reference in interior cell: ' + recordId);
      continue;
    end;

    wrld := LinksTo(ElementByIndex(rCell, 0));
    if slSkipEditorIDs.IndexOf(GetElementEditValues(wrld, 'EDID')) <> -1 then begin
      //AddMessage('Skipped reference in blacklisted world: ' + recordId);
      continue;
    end;

    tlReferences.Add(r);

    if tlCells.IndexOf(rCell) < 0 then begin
      tlCells.Add(rCell);
      ICellPlugin := RefMastersDeterminePlugin(rCell);
      wbCopyElementToFile(rCell, ICellPlugin, False, True);
    end;
  end;

  countPrecombined := 0;
  countNotPrecombined := 0;
  for i := 0 to tlReferences.Count - 1 do begin

    r := ObjectToElement(tlReferences[i]);
    ICurrentPlugin := RefMastersDeterminePlugin(r);
    rCell := WinningOverride(LinksTo(ElementByIndex(r, 0)));
    try
      AddRequiredElementMasters(rCell, ICurrentPlugin, False, True);
    except
      on E : Exception do ICurrentPlugin := RefMastersDeterminePlugin(rCell);
    end;

    recordId := GetFileName(r) + #9 + Name(r);
    if IsRefPrecombined(r) then
      begin
        if NotPrecombinedOnly then begin
          AddMessage('Skipped precombined reference: ' + recordId);
          continue;
        end;
        countPrecombined := countPrecombined + 1;
        AddMessage(recordId + ' is precombined');
        P := 1;
      end
    else
      begin
        countNotPrecombined := countNotPrecombined + 1;
        P := 0;
        if OutDoorOnly and NotPrecombinedOnly then P := 2;
        //since not precombined, we can modify it.
        AddMessage(recordId + ' OVERRIDE');
        n := wbCopyElementToFile(r, ICurrentPlugin, False, True);
        ReplaceBase(n, P, idx);
        AssignSeason(n, 3); //winter
      end;
    AddSeasonalVersion(r, ICurrentPlugin, P, idx);
  end;
  AddMessage(#9 + IntToStr(countPrecombined) + ' precombined references.');
  AddMessage(#9 + IntToStr(countNotPrecombined) + ' not precombined references.');
  tlReferences.Free;
  tlCells.Free;
end;

function RefMastersDeterminePlugin(r: IInterface): IInterface;
{
    Sets the output file to either the ESM file or the ESP file based on the required masters for the given reference.
}
begin
  try
    AddRequiredElementMasters(r, SeasonsMainFile, False, True);
    SortMasters(SeasonsMainFile);
    Result := SeasonsMainFile;
  except
    on E : Exception do begin
      AddRequiredElementMasters(r, SeasonsPatch, False, True);
      SortMasters(SeasonsPatch);
      Result := SeasonsPatch;
    end;
  end;
end;

procedure AssignSeason(r: IInterface; s: Integer);
{
  Assign season to reference.
}
var
  sSeason, sMaterial : String;
begin
  Case s of
    0 : sSeason := '00032B07'; //spring
    1 : sSeason := '000847C1'; //summer
    2 : sSeason := '0021B1F2'; //fall
    3 : sSeason := '000995D6'; //winter
  end;
  Add(r, 'XESP', True);
  SetElementEditValues(r, 'XESP\Reference', sSeason);

  if s = 0 then Shuffle(slSpring);
  if s = 1 then Shuffle(slSummer);
  if s = 2 then Shuffle(slFall);
  if s = 3 then Shuffle(slWinter);
  Case s of
    0 : sMaterial := slSpring[0];
    1 : sMaterial := slSummer[0];
    2 : sMaterial := slFall[0];
    3 : sMaterial := slWinter[0];
  end;
  Add(r, 'XMSP', True);
  SetElementEditValues(r, 'XMSP', sMaterial);
end;

procedure ReplaceBase(r: IInterface; P, idx: integer);
{
  Replace Base of reference.
}
begin
  Case P of
    0 : SetElementEditValues(r, 'NAME', slFull[idx]);
    1 : SetElementEditValues(r, 'NAME', slNew[idx]);
    2 : SetElementEditValues(r, 'NAME', slOutdoorNonPrecombinedFull[idx]);
  end;
end;

procedure AddSeasonalVersion(r, PluginFile: IInterface; P, idx: Integer);
{
  Add seasonal versions.
}
var
  season, sMaterial: String;
  n: IInterface;
  s: Integer;
begin
  for s: = 0 to 2 do begin
    n := wbCopyElementToFile(r, PluginFile, True, True);
    ReplaceBase(n, P, idx);
    AssignSeason(n, s);
    AddLinkedReference(n, 'WorkshopStackedItemParentKEYWORD [KYWD:001C5EDD]', Name(r));
  end;
end;

function AddLinkedReference(e: IInterface; keyword, ref: String): Integer;
{
  Add a linked reference.
}
var
  el, linkedrefs, lref: IInterface;
  i: Integer;
begin
  if not ElementExists(e, 'Linked References') then begin
    linkedrefs := Add(e, 'Linked References', True);
    lref := ElementByIndex(linkedrefs, 0);
    SetElementEditValues(lref, 'Keyword/Ref', keyword);
    SetElementEditValues(lref, 'Ref', ref);
  end
  else
    begin
      linkedrefs := ElementByPath(e, 'Linked References');
      lref := ElementAssign(linkedrefs, HighInteger, nil, False);
      SetElementEditValues(lref, 'Keyword/Ref', keyword);
      SetElementEditValues(lref, 'Ref', ref);
    end;
end;

function IsRefPrecombined(r: IInterface): Boolean;
{
  Checks if a reference is precombined.
}
var
  i, t, preCombinedRefsCount, rc: Integer;
  c, refs: IInterface;
begin
  Result := false;
  t := ReferencedByCount(r) - 1;
  if t < 0 then Exit;
  for i := 0 to t do begin
    c := ReferencedByIndex(r, i);
    if Signature(c) <> 'CELL' then continue;
    if not IsWinningOverride(c) then continue;
    //AddMessage(ShortName(r) + ' is referenced in ' + Name(c));
    Result := true;
    Exit;
  end;
end;

procedure Shuffle(Strings: TStrings);
{
  Shuffles the order of strings.
}
var
  i: Integer;
begin
  for i := Strings.Count - 1 downto 1 do
    Strings.Exchange(i, Random(i + 1));
end;

end.

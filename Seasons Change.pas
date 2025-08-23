{
  Generate Seasons.
}
unit Seasons;

// ----------------------------------------------------
//Create variables that will need to be used accross multiple functions/procedures.
// ----------------------------------------------------

var
    bSaveLandHeights: boolean;
    uiScale: integer;
    sIgnoredWorldspaces: string;

    SeasonsMainFile: IwbFile;

    slPluginFiles: TStringList;
    joSeasons, joLandscapeHeights: TJsonObject;

const
    sSeasonsFileName = 'Seasons.esm';
    SCALE_FACTOR_TERRAIN = 8;

// ----------------------------------------------------
// Main functions and procedures go immediately below.
// ----------------------------------------------------

procedure CreateObjects;
{
    Creates objects.
}
begin
    slPluginFiles := TStringList.Create;
    joSeasons := TJsonObject.Create;
    joLandscapeHeights := TJsonObject.Create;
    if FileExists(wbScriptsPath + 'Seasons\LandHeights.json') then
        joLandscapeHeights.LoadFromFile(wbScriptsPath + 'Seasons\LandHeights.json');
end;

function Finalize: integer;
{
    This function is called at the end.
}
begin
    slPluginFiles.Free;
    joSeasons.Free;

    if bSaveLandHeights then joLandscapeHeights.SaveToFile(wbScriptsPath + 'Seasons\LandHeights.json', False, TEncoding.UTF8, True);
    joLandscapeHeights.Free;
    Result := 0;
end;

function Initialize: integer;
{
  This function is called at the beginning.
}
begin
    Result := 0;
    bSaveLandHeights := False;

    //Get scaling
    uiScale := Screen.PixelsPerInch * 100 / 96;
    AddMessage('UI scale: ' + IntToStr(uiScale));

    CreateObjects;
    FetchRules;

    if not MainMenuForm then begin
        Result := 1;
        Exit;
    end;

    CollectRecords;
end;

procedure CollectRecords;
{
    Collect records;
}
var
    i, j, count, blockidx, subblockidx, cellidx: integer;
    recordid: string;

    f: IwbFile;
    g, wrldgroup: IwbGroupRecord;
    r, rWrld, block, subblock, rCell, land: IwbElement;
begin
    count := 0;
    for i := 0 to Pred(FileCount) do begin
        f := FileByIndex(i);

        //Collect LAND
        g := GroupBySignature(f, 'WRLD');
        for j := 0 to Pred(ElementCount(g)) do begin
            rWrld := ElementByIndex(g, j);
            recordid := RecordFormIdFileId(rWrld);
            if Pos(recordid, sIgnoredWorldspaces) <> 0 then continue;

            wrldgroup := ChildGroup(rWrld);
            for blockidx := 0 to Pred(ElementCount(wrldgroup)) do begin
                block := ElementByIndex(wrldgroup, blockidx);
                for subblockidx := 0 to Pred(ElementCount(block)) do begin
                    subblock := ElementByIndex(block, subblockidx);
                    for cellidx := 0 to Pred(ElementCount(subblock)) do begin
                        rCell := ElementByIndex(subblock, cellidx);
                        if (Signature(rCell) <> 'CELL') or GetIsPersistent(rCell) then continue;
                        land := GetLandscapeForCell(rCell);
                        if not Assigned(land) then continue;
                        if not IsWinningOverride(land) then continue;
                        if not ElementExists(land, 'VHGT') then continue;
                        count := count + CreateLandscapeSnow(land, WinningOverride(rCell), WinningOverride(rWrld));
                        //if count = 30 then Exit;
                    end;
                end;
            end;
        end;

    end;
end;

function CreateLandscapeSnow(land, rCell, rWrld: IwbElement): integer;
var
    bFoundInJSON: boolean;
    landOffsetZ, rowColumnOffsetZ, rowStartVal, landValue, landValueScaled: single;
    cellX, CellY, unitsX, unitsY, row, column: integer;
    fileProvidingLand, rowColumn, editorIdSnowNif: string;

    landHeightData: IwbElement;

    snowNif: TwbNifFile;
begin
    Result := 0;
    cellX := GetElementNativeValues(rCell, 'XCLC\X');
    cellY := GetElementNativeValues(rCell, 'XCLC\Y');
    unitsX := cellX * 4096;
    unitsY := cellY * 4096;
    fileProvidingLand := GetFileName(GetFile(land));
    editorIdSnowNif := EditorID(rWrld) + '_' + IntToStr(cellX) + '_' + IntToStr(cellY);

    landOffsetZ := GetElementNativeValues(land, 'VHGT\Offset');
    landHeightData := ElementByPath(land, 'VHGT\Height Data');

    snowNif := TwbNifFile.Create;
    try
        for row := 0 to 32 do begin
            for column := 0 to 32 do begin
                bFoundInJSON := False;

                rowColumn := 'Row #' + IntToStr(row) + '\Column #' + IntToStr(column);
                if joLandscapeHeights.Contains(fileProvidingLand) then begin
                    if joLandscapeHeights.O[fileProvidingLand].Contains(editorIdSnowNif) then begin
                        if joLandscapeHeights.O[fileProvidingLand].O[editorIdSnowNif].Contains(rowColumn) then begin
                            landValueScaled := joLandscapeHeights.O[fileProvidingLand].O[editorIdSnowNif].S[rowColumn];
                            bFoundInJSON := True;
                        end;
                    end;
                end;
                if not bFoundInJSON then begin
                    rowColumnOffsetZ := GetElementNativeValues(landHeightData, rowColumn);
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
                    landValueScaled := landValue * SCALE_FACTOR_TERRAIN; //This will be the Z height we apply to the vertex of the nif.
                    joLandscapeHeights.O[fileProvidingLand].O[editorIdSnowNif].S['offset'] := Round(landOffsetZ) * SCALE_FACTOR_TERRAIN;
                    joLandscapeHeights.O[fileProvidingLand].O[editorIdSnowNif].S[rowColumn] := landValueScaled;
                    bSaveLandHeights := True;
                    Result := 1;
                end;

                // X Y Z coordinates. Shouldn't need these.
                // pX := FloatToStr(column * 128);
                // pY := FloatToStr(row * 128);
                // pZ := FloatToStr(landValueScaled);
            end;
        end;
        //AddMessage(editorIdSnowNif);
    finally
        snowNif.Free;
    end;
end;

procedure FetchRules;
{
    Loads the Rule JSON files.
}
var
    i: integer;
    f: string;
begin
    for i := 0 to Pred(FileCount) do begin
        f := GetFileName(FileByIndex(i));
        slPluginFiles.Add(f);
        LoadRules(f);
    end;
end;

procedure LoadRules(f: string);
{
    Load Rules
}
var
    c, a: integer;
    j, key: string;

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
end;

// ----------------------------------------------------
// UI functions and procedures go below.
// ----------------------------------------------------

function MainMenuForm: Boolean;
{
  Main menu form.
}
var
    btnStart, btnCancel: TButton;
    frm: TForm;
    gbOptions: TGroupBox;
    fImage: TImage;
    pnl: TPanel;
    picSeasons: TPicture;
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
        picSeasons.LoadFromFile(wbScriptsPath + 'Seasons\Seasons Change.jpg');

        fImage := TImage.Create(frm);
        fImage.Picture := picSeasons;
        fImage.Parent := frm;
        fImage.Width := 549;
        fImage.Height := 300;
        fImage.Left := 10;
        fImage.Top := 12;
        fImage.Stretch := True;

        gbOptions := TGroupBox.Create(frm);
        gbOptions.Parent := frm;
        gbOptions.Left := 10;
        gbOptions.Top := fImage.Top + fImage.Height + 24;
        gbOptions.Width := frm.Width - 30;
        gbOptions.Caption := 'Seasons Change';
        gbOptions.Height := 94;

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

        if frm.ShowModal <> mrOk then begin
            Result := False;
            Exit;
        end else Result := True;

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

// ----------------------------------------------------
// Record processing Functions and Procedures go below.
// ----------------------------------------------------

function RecordFormIdFileId(e: IwbElement): string;
{
    Returns the record ID of an element.
}
begin
    Result := TrimRightChars(IntToHex(FixedFormID(e), 8), 2) + ':' + GetFileName(GetFile(MasterOrSelf(e)));
end;

function GetRecordFromFormIdFileId(recordId: string): IwbElement;
{
    Returns the record from the given formid:filename.
}
var
    colonPos, recordFormId, c: integer;
    fileMasterIndex: string;

    f: IwbFile;
begin
    colonPos := Pos(':', recordId);
    f := FileByIndex(slPluginFiles.IndexOf(Copy(recordId, Succ(colonPos), Length(recordId))));
    c := MasterCount(f);
    if c > 9 then fileMasterIndex := IntToStr(c) else fileMasterIndex := '0' + IntToStr(c);
    recordFormId := StrToInt('$' + fileMasterIndex + Copy(recordId, 1, Pred(colonPos)));
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

end.
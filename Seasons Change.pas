{
  Generate Seasons.
}
unit Seasons;

// ----------------------------------------------------
//Create variables that will need to be used accross multiple functions/procedures.
// ----------------------------------------------------

var
    bSaveLandHeights, bCreateLandscapeHeights, bCreateLandscapeSnowMeshes, bPlaceLandscapeSnow, bCreateTestPlugin: boolean;
    uiScale: integer;
    sIgnoredWorldspaces: string;

    SeasonsMainFile, plugin: IwbFile;
    statGroup, scolGroup: IwbGroupRecord;
    flatSnowStatic: IwbElement;

    slPluginFiles: TStringList;
    tlLandRecords, tlBasesThatAlterLand: TList;
    joSeasons, joLandscapeHeights, joLandFiles, joAlterLandRules: TJsonObject;

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
    joLandFiles := TJsonObject.Create;
    joAlterLandRules := TJsonObject.Create;
    tlLandRecords := TList.Create;
    tlBasesThatAlterLand := TList.Create;
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
    joAlterLandRules.Free;
    joLandFiles.Free;
    tlLandRecords.Free;
    tlBasesThatAlterLand.Free;

    joLandscapeHeights.Free;
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

    //Get scaling
    uiScale := Screen.PixelsPerInch * 100 / 96;
    AddMessage('UI scale: ' + IntToStr(uiScale));

    CreateObjects;
    FetchRules;


    if not MainMenuForm then begin
        Result := 1;
        Exit;
    end;
    if bCreateTestPlugin then CreatePlugin;

    EnsureDirectoryExists(wbScriptsPath + 'Seasons\output\Meshes\LandscapeSnow');
    EnsureDirectoryExists(wbScriptsPath + 'Seasons\output\Meshes\LOD\LandscapeSnow');
    CollectRecords;
    AlterLandHeightsForTheseBases;
    ProcessLandRecords;
end;

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

    f: IwbFile;
    g, wrldgroup: IwbGroupRecord;
    r, rWrld, wWrld, block, subblock, rCell, land: IwbElement;
begin
    count := 0;
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
                        land := GetLandscapeForCell(rCell);
                        if not Assigned(land) then continue;
                        if not IsWinningOverride(land) then continue;
                        if not ElementExists(land, 'VHGT') then continue;
                        cellX := GetElementNativeValues(rCell, 'XCLC\X');
                        cellY := GetElementNativeValues(rCell, 'XCLC\Y');
                        AddMessage(IntToStr(count) + #9 + ShortName(land) + #9 + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY));
                        joLandFiles.O[wrldEdid].O[cellX].S[cellY] := fileName;
                        bLandHasChanged := CreateLandscapeHeights(land, WinningOverride(rCell), wWrld, wrldEdid);
                        if bLandHasChanged then begin
                            tlLandRecords.Add(land);
                            Inc(count);
                        end;
                        if bCreateLandscapeSnowMeshes or bPlaceLandscapeSnow then tlLandRecords.Add(land);
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

    end;
    AddMessage('New LAND Records: ' + IntToStr(count));
    if bSaveLandHeights then joLandscapeHeights.SaveToFile(wbScriptsPath + 'Seasons\LandHeights.json', False, TEncoding.UTF8, True);
end;

procedure AlterLandHeightsForTheseBases;
{
    Alters land heights for certain base objects placed near landscape.
}
var
    i: integer;
    base: IwbElement;
begin
    for i:= 0 to Pred(tlBasesThatAlterLand.Count) do begin
        base := ObjectToElement(tlBasesThatAlterLand[i]);
        AddMessage('Processing base ' + #9 + Name(base));
        ProcessBasesThatAlterLand(base, base);
    end;
end;

function ProcessBasesThatAlterLand(base, fromBase: IwbElement): boolean;
{
    Process a base object to see if it alters land heights.
}
var
    i, cellX, cellY: integer;
    wrldEdid, fileName: string;

    r, rCell, wCell, rWrld, rLand: IwbElement;
    c: TwbGridCell;
begin
    Result := False;
    for i := 0 to Pred(ReferencedByCount(base)) do begin
        //Check each reference to see if it is in an exterior cell with landscape, and is close enough to the landscape to alter it.
        //If so, we need to alter the land heights near the reference.
        r := ReferencedByIndex(base, i);
        if Signature(r) = 'SCOL' then begin
            ProcessBasesThatAlterLand(r, base);
            continue;
        end;
        if Signature(r) <> 'REFR' then continue;
        if not IsWinningOverride(r) then continue;
        if GetIsDeleted(r) then continue;
        if ElementExists(r, 'XESP') then continue; //skip enable parented references, since the object is not always present.
        rCell := WinningOverride(LinksTo(ElementByIndex(r, 0)));
        if GetElementEditValues(rCell, 'DATA - Flags\Is Interior Cell') = 1 then continue;
        rWrld := WinningOverride(LinksTo(ElementByIndex(rCell, 0)));
        if Pos(RecordFormIdFileId(rWrld), sIgnoredWorldspaces) <> 0 then continue;

        if GetIsPersistent(rCell) then begin
            c := wbPositionToGridCell(GetPosition(r));
            rCell := WinningOverride(GetCellFromWorldspace(rWrld, c.X, c.Y));
            if not Assigned(rCell) then continue;
        end;

        wrldEdid := GetElementEditValues(rWrld, 'EDID');
        cellX := GetElementNativeValues(rCell, 'XCLC\X');
        cellY := GetElementNativeValues(rCell, 'XCLC\Y');
        if not joLandFiles.O[wrldEdid].O[cellX].Contains(cellY) then continue;
        fileName := joLandFiles.O[wrldEdid].O[cellX].S[cellY];

        if joLandscapeHeights.O[fileName].O[wrldEdid].O[cellX].O[cellY].S['flags'] > 0 then continue; //skip flat landscape cells and underwater landscape cells

        // If we got this far, this REFR is in an exterior cell with landscape, and we will need to alter the land heights IF it is close enough to the landscape.
        AddMessage(#9 + 'Processing reference ' + #9 + ShortName(r) + #9 + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY));
        AlterLandHeightsForThisRefr(r, base, fromBase, wrldEdid, cellX, cellY);
    end;
end;

procedure AlterLandHeightsForThisRefr(r, base, fromBase: IwbElement; wrldEdid: string; cellX, cellY: integer);
{
    Alters land heights for a specific reference.
}
var
    bSCOL: boolean;
    i, x1, y1, z1, x2, y2, z2, alteration: integer;
    scale, posX, posY, posZ: single;

    realBase, scolParts, scolPart, onam, placements, placement: IwbElement;
begin
    //Determine which object to use for bounds. If the base is a SCOL, we need to use the fromBase instead.
    if Signature(base) = 'SCOL' then begin
        realBase := fromBase;
        bSCOL := True;
    end else realBase := base;
    //Get object bounds of the base object.
    x1 := GetElementNativeValues(realBase, 'OBND\X1');
    y1 := GetElementNativeValues(realBase, 'OBND\Y1');
    z1 := GetElementNativeValues(realBase, 'OBND\Z1');
    x2 := GetElementNativeValues(realBase, 'OBND\X2');
    y2 := GetElementNativeValues(realBase, 'OBND\Y2');
    z2 := GetElementNativeValues(realBase, 'OBND\Z2');

    alteration := joAlterLandRules.S[RecordFormIdFileId(realBase)];

    //Get scale and position of the reference.
    if ElementExists(r, 'XSCL') then scale := GetElementNativeValues(r, 'XSCL') else scale := 1;
    posX := GetElementNativeValues(r, 'DATA\Position\X');
    posY := GetElementNativeValues(r, 'DATA\Position\Y');
    posZ := GetElementNativeValues(r, 'DATA\Position\Z');

    //If the object is an SCOL, we need to adjust the position to be the SCOL placement position.
    if bSCOL then begin
        scolParts := ElementByPath(r, 'Parts');
        for i := 0 to Pred(ElementCount(scolParts)) do begin
            scolPart := ElementByIndex(scolParts, i);
            onam := LinksTo(ElementByPath(scolPart, 'ONAM'));
            if onam = realBase then break;
        end;
        placements := ElementByPath(scolPart, 'Placements');
        for i := 0 to Pred(ElementCount(placements)) do begin
            placement := ElementByIndex(placements, i);
            posX := posX + GetElementNativeValues(placement, 'Position\X');
            posY := posY + GetElementNativeValues(placement, 'Position\Y');
            posZ := posZ + GetElementNativeValues(placement, 'Position\Z');
            scale := scale * GetElementNativeValues(placement, 'Scale');
            x1 := Floor(x1 * scale);
            y1 := Floor(y1 * scale);
            z1 := Floor(z1 * scale);
            x2 := Ceil(x2 * scale);
            y2 := Ceil(y2 * scale);
            z2 := Ceil(z2 * scale);
            AlterLandHeightsForThisPlacement(alteration, x1, y1, z1, x2, y2, z2, posX, posY, posZ, wrldEdid, cellX, cellY, realBase);
        end;
    end else begin
        x1 := Floor(x1 * scale);
        y1 := Floor(y1 * scale);
        z1 := Floor(z1 * scale);
        x2 := Ceil(x2 * scale);
        y2 := Ceil(y2 * scale);
        z2 := Ceil(z2 * scale);
        AlterLandHeightsForThisPlacement(alteration, x1, y1, z1, x2, y2, z2, posX, posY, posZ, wrldEdid, cellX, cellY, realBase);
    end;
end;

procedure AlterLandHeightsForThisPlacement(alteration, x1, y1, z1, x2, y2, z2: integer; posX, posY, posZ: single; wrldEdid: string; cellX, cellY: integer; realBase: IwbElement);
{
    Alters land heights for a specific placement of a base object.
}
var
    unitsX, unitsY, aposX, aposY, lowerCornerX, lowerCornerY, upperCornerX, upperCornerY, lowestColumn, lowestRow,
    highestColumn, highestRow, row, column, alteration, cellXHere, cellYHere, maxZ, minZ, vz, newZ, landOffsetZ: integer;
    fileName: string;
    bLandHasChanged: boolean;
begin
    //Get the position in relation to the cell.
    unitsX := cellX * 4096;
    unitsY := cellY * 4096;
    aposX := Round(posX) - unitsX;
    aposY := Round(posY) - unitsY;
    lowerCornerX := aposX - x1;
    lowerCornerY := aposY - y1;
    upperCornerX := aposX + x2;
    upperCornerY := aposY + y2;
    lowestColumn := Floor(lowerCornerX/128);
    highestColumn := Ceil(upperCornerX/128);
    lowestRow := Floor(lowerCornerY/128);
    highestRow := Ceil(upperCornerY/128);
    maxZ := Ceil(posZ) + z2;
    minZ := Floor(posZ) + z1;

    for column := lowestColumn to highestColumn do begin
        if column < 0 then begin
            while column < 0 do begin
                cellXHere := cellX - 1;
                column := column + 32;
            end;
        end else if column > 32 then begin
            while column > 32 do begin
                cellXHere := cellX + 1;
                column := column - 32;
            end;
        end else cellXHere := cellX;
        for row := lowestRow to highestRow do begin
            if row < 0 then begin
                while row < 0 do begin
                    cellYHere := cellY - 1;
                    row := row + 32;
                end;
            end else if row > 32 then begin
                while row > 32 do begin
                    cellYHere := cellY + 1;
                    row := row - 32;
                end;
            end else cellYHere := cellY;

            if joLandFiles.O[wrldEdid].O[cellXHere].Contains(cellYHere) then begin
                fileName := joLandFiles.O[wrldEdid].O[cellXHere].S[cellYHere];
                landOffsetZ := joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].S['offset'] * SCALE_FACTOR_TERRAIN;

                vz := joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].A[row].S[column] * SCALE_FACTOR_TERRAIN + landOffsetZ;
                if minZ - vz > 200 then break;
                newZ := Ceil((vz + alteration)/SCALE_FACTOR_TERRAIN) - landOffsetZ/SCALE_FACTOR_TERRAIN;
                joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].A[row].S[column] := newZ;
                bLandHasChanged := True;
            end;

            if column = 0 then begin
                column := 32;
                cellXHere := cellXHere - 1;
                if joLandFiles.O[wrldEdid].O[cellXHere].Contains(cellYHere) then begin
                    fileName := joLandFiles.O[wrldEdid].O[cellXHere].S[cellYHere];
                    landOffsetZ := joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].S['offset'] * SCALE_FACTOR_TERRAIN;
                    newZ := Ceil((vz + alteration)/SCALE_FACTOR_TERRAIN) - landOffsetZ/SCALE_FACTOR_TERRAIN;
                    joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].A[row].S[column] := newZ;
                    bLandHasChanged := True;
                end;
                if row = 0 then begin
                    row := 32;
                    cellYHere := cellYHere - 1;
                    if joLandFiles.O[wrldEdid].O[cellXHere].Contains(cellYHere) then begin
                        fileName := joLandFiles.O[wrldEdid].O[cellXHere].S[cellYHere];
                        landOffsetZ := joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].S['offset'] * SCALE_FACTOR_TERRAIN;
                        newZ := Ceil((vz + alteration)/SCALE_FACTOR_TERRAIN) - landOffsetZ/SCALE_FACTOR_TERRAIN;
                        joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].A[row].S[column] := newZ;
                        bLandHasChanged := True;
                    end;
                end
                else if row = 32 then begin
                    row := 0;
                    cellYHere := cellYHere + 1;
                    if joLandFiles.O[wrldEdid].O[cellXHere].Contains(cellYHere) then begin
                        fileName := joLandFiles.O[wrldEdid].O[cellXHere].S[cellYHere];
                        landOffsetZ := joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].S['offset'] * SCALE_FACTOR_TERRAIN;
                        newZ := Ceil((vz + alteration)/SCALE_FACTOR_TERRAIN) - landOffsetZ/SCALE_FACTOR_TERRAIN;
                        joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].A[row].S[column] := newZ;
                        bLandHasChanged := True;
                    end;
                end;
            end else if column = 32 then begin
                column := 0;
                cellXHere := cellXHere + 1;
                if joLandFiles.O[wrldEdid].O[cellXHere].Contains(cellYHere) then begin
                    fileName := joLandFiles.O[wrldEdid].O[cellXHere].S[cellYHere];
                    landOffsetZ := joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].S['offset'] * SCALE_FACTOR_TERRAIN;
                    newZ := Ceil((vz + alteration)/SCALE_FACTOR_TERRAIN) - landOffsetZ/SCALE_FACTOR_TERRAIN;
                    joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].A[row].S[column] := newZ;
                    bLandHasChanged := True;
                end;
                if row = 0 then begin
                    row := 32;
                    cellYHere := cellYHere - 1;
                    if joLandFiles.O[wrldEdid].O[cellXHere].Contains(cellYHere) then begin
                        fileName := joLandFiles.O[wrldEdid].O[cellXHere].S[cellYHere];
                        landOffsetZ := joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].S['offset'] * SCALE_FACTOR_TERRAIN;
                        newZ := Ceil((vz + alteration)/SCALE_FACTOR_TERRAIN) - landOffsetZ/SCALE_FACTOR_TERRAIN;
                        joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].A[row].S[column] := newZ;
                        bLandHasChanged := True;
                    end;
                end
                else if row = 32 then begin
                    row := 0;
                    cellYHere := cellYHere + 1;
                    if joLandFiles.O[wrldEdid].O[cellXHere].Contains(cellYHere) then begin
                        fileName := joLandFiles.O[wrldEdid].O[cellXHere].S[cellYHere];
                        landOffsetZ := joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].S['offset'] * SCALE_FACTOR_TERRAIN;
                        newZ := Ceil((vz + alteration)/SCALE_FACTOR_TERRAIN) - landOffsetZ/SCALE_FACTOR_TERRAIN;
                        joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].A[row].S[column] := newZ;
                        bLandHasChanged := True;
                    end;
                end;
            end else if row = 0 then begin
                row := 32;
                cellYHere := cellYHere - 1;
                if joLandFiles.O[wrldEdid].O[cellXHere].Contains(cellYHere) then begin
                    fileName := joLandFiles.O[wrldEdid].O[cellXHere].S[cellYHere];
                    landOffsetZ := joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].S['offset'] * SCALE_FACTOR_TERRAIN;
                    newZ := Ceil((vz + alteration)/SCALE_FACTOR_TERRAIN) - landOffsetZ/SCALE_FACTOR_TERRAIN;
                    joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].A[row].S[column] := newZ;
                    bLandHasChanged := True;
                end;
            end else if row = 32 then begin
                row := 0;
                cellYHere := cellYHere + 1;
                if joLandFiles.O[wrldEdid].O[cellXHere].Contains(cellYHere) then begin
                    fileName := joLandFiles.O[wrldEdid].O[cellXHere].S[cellYHere];
                    landOffsetZ := joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].S['offset'] * SCALE_FACTOR_TERRAIN;
                    newZ := Ceil((vz + alteration)/SCALE_FACTOR_TERRAIN) - landOffsetZ/SCALE_FACTOR_TERRAIN;
                    joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].A[row].S[column] := newZ;
                    bLandHasChanged := True;
                end;
            end;

        end;
    end;
    if bLandHasChanged then AddMessage(#9 + #9 + #9 + 'Altered land heights for ' + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY) + ' due to base object near landscape: ' + ShortName(realBase));
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
        fileName := joLandFiles.O[wrldEdid].O[cellX].S[cellY];

        if joLandscapeHeights.O[fileName].O[wrldEdid].O[cellX].O[cellY].S['flags'] > 0 then continue; //skip flat and underwater landscape cells
        AddMessage(IntToStr(i) + ' of ' + IntToStr(count) + #9 + ShortName(rLand) + #9 + wrldEdid + ' ' + IntToStr(cellX) + ' ' + IntToStr(cellY));

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
var
    bVertexIsOutsideCell: boolean;
    i, nifFile, unitsX, unitsY, landOffsetZ, row2, column2, row, column, vertexCount, point1, point2, point3, point4, maxPoint, newCellX, newCellY, newlandOffsetZ, cellOffsetDifference, newVz: integer;
    editorIdSnowNif, fileName, snowNifFile, xyz, vx, vy, vz, newVzStr, fileNameLand: string;

    tsXYZ: TStrings;

    vertexData, vertex: TdfElement;
    block: TwbNifBlock;
    snowNif, snowLodNif: TwbNifFile;
begin
    Result := 0;
    unitsX := cellX * 4096;
    unitsY := cellY * 4096;
    editorIdSnowNif := wrldEdid + '_' + IntToStr(cellX) + '_' + IntToStr(cellY);

    landOffsetZ := joLandscapeHeights.O[fileProvidingLand].O[wrldEdid].O[cellX].O[cellY].S['offset'];

    for nifFile := 0 to 3 do begin
        if nifFile = 0 then begin //Create full model
            fileName := wbScriptsPath + 'Seasons\LandscapeSnow.nif';
            snowNifFile := wbScriptsPath + 'Seasons\output\Meshes\LandscapeSnow\' + editorIdSnowNif + '.nif';
        end else if nifFile = 1 then begin //Create lod model
            fileName := wbScriptsPath + 'Seasons\LandscapeSnow_lod_0.nif';
            snowNifFile := wbScriptsPath + 'Seasons\output\Meshes\LOD\LandscapeSnow\' + editorIdSnowNif + '_lod_0.nif';
        end else if nifFile = 2 then begin //Create lod model
            fileName := wbScriptsPath + 'Seasons\LandscapeSnow_lod_1.nif';
            snowNifFile := wbScriptsPath + 'Seasons\output\Meshes\LOD\LandscapeSnow\' + editorIdSnowNif + '_lod_1.nif';
        end else if nifFile = 3 then begin //Create lod model
            fileName := wbScriptsPath + 'Seasons\LandscapeSnow_lod_2.nif';
            snowNifFile := wbScriptsPath + 'Seasons\output\Meshes\LOD\LandscapeSnow\' + editorIdSnowNif + '_lod_2.nif';
        end;
        snowNif := TwbNifFile.Create;
        try
            snowNif.LoadFromFile(fileName);
            block := snowNif.Blocks[1];
            vertexCount := block.NativeValues['Num Vertices'];
            vertexData := block.Elements['Vertex Data'];
            for i := 0 to Pred(vertexCount) do begin
                bVertexIsOutsideCell := false;
                vertex := vertexData[i];
                xyz := vertex.EditValues['Vertex'];
                tsXYZ := SplitString(xyz, ' ');
                vx := tsXYZ[0];
                vy := tsXYZ[1];
                vz := tsXYZ[2];

                row2 := Round(StrToFloatDef(vy, 9))/64; // dividing by 64 means row2 is twice the size of the actual row
                // if row2 is even, this row exists
                column2 := Round(StrToFloatDef(vx, 9))/64; // dividing by 64 means column2 is twice the size of the actual column
                // if column2 is even, this column exists

                if (IsEven(row2) and IsEven(column2)) then begin
                    row := row2/2;
                    column := column2/2;
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

                        if not joLandFiles.O[wrldEdid].O[newCellX].Contains(newCellY) then begin
                            //If the neighboring cell does not exist, we should change the row/column to the vertex that is closest and use that for the newVz
                            if column < 0 then column := 0
                            else if column > 32 then column := 32;
                            if row < 0 then row := 0
                            else if row > 32 then row := 32;
                            newVz := joLandscapeHeights.O[fileProvidingLand].O[wrldEdid].O[cellX].O[cellY].A[row].S[column] * SCALE_FACTOR_TERRAIN;
                        end else begin
                            fileNameLand := joLandFiles.O[wrldEdid].O[newCellX].S[newCellY];
                            //If the neighboring cell does exist, we need to compare the offset values, as that will affect the newVz.
                            newlandOffsetZ := joLandscapeHeights.O[fileNameLand].O[wrldEdid].O[newCellX].O[newCellY].S['offset'];
                            // We will need to add this difference to the final vz value;
                            cellOffsetDifference := landOffsetZ - newlandOffsetZ;

                            //Get the correct row/column for the overlap cell vertex we wish to go by.
                            // Since our LOD only has resolution of 4 rows/columns, we use 3 or 29.
                            // For LOD16 we use a scale of 1.1111 so we need to change these to 4 more rows over,
                            // so 7 and 25.
                            if nifFile = 3 then begin
                                if column < 0 then column := 25
                                else if column > 32 then column := 7;
                                if row < 0 then row := 25
                                else if row > 32 then row := 7;
                            end else begin
                                if column < 0 then column := 29
                                else if column > 32 then column := 3;
                                if row < 0 then row := 29
                                else if row > 32 then row := 3;
                            end;

                            //We add the cellOffsetDifference to the final z value.
                            newVz := (joLandscapeHeights.O[fileNameLand].O[wrldEdid].O[newCellX].O[newCellY].A[row].S[column] - cellOffsetDifference) * SCALE_FACTOR_TERRAIN;
                        end;
                    end else begin
                        newVz := joLandscapeHeights.O[fileProvidingLand].O[wrldEdid].O[cellX].O[cellY].A[row].S[column] * SCALE_FACTOR_TERRAIN;
                    end;
                end else begin
                    // If odd, this row/column does not exist. This will only happen to the full model for the in-between vertices.
                    // Get the average of 4 points around the vertex to set the new height.
                    row := (row2 - 1)/2;
                    column := (column2 - 1)/2;
                    point1 := joLandscapeHeights.O[fileProvidingLand].O[wrldEdid].O[cellX].O[cellY].A[row].S[column] * SCALE_FACTOR_TERRAIN;

                    row := (row2 - 1)/2;
                    column := (column2 + 1)/2;
                    point2 := joLandscapeHeights.O[fileProvidingLand].O[wrldEdid].O[cellX].O[cellY].A[row].S[column] * SCALE_FACTOR_TERRAIN;

                    row := (row2 + 1)/2;
                    column := (column2 + 1)/2;
                    point3 := joLandscapeHeights.O[fileProvidingLand].O[wrldEdid].O[cellX].O[cellY].A[row].S[column] * SCALE_FACTOR_TERRAIN;

                    row := (row2 + 1)/2;
                    column := (column2 - 1)/2;
                    point4 := joLandscapeHeights.O[fileProvidingLand].O[wrldEdid].O[cellX].O[cellY].A[row].S[column] * SCALE_FACTOR_TERRAIN;

                    //maxPoint := Max(Max(point1, point2), Max(point3, point4));
                    //newVz := (point1 + point2 + point3 + point4 + maxPoint)/5; //We add the max point again to help even out the average a bit more
                    newVz := (point1 + point2 + point3 + point4)/4;
                end;

                //We have z offsets built in for the LOD.
                if nifFile = 0 then newVzStr := IntToStr(newVz)
                else if nifFile = 1 then begin
                    if bVertexIsOutsideCell then newVzStr := IntToStr(newVz)
                    else newVzStr := IntToStr(newVz + 40);
                end else if nifFile = 2 then begin
                    if bVertexIsOutsideCell then newVzStr := IntToStr(newVz + 40)
                    else newVzStr := IntToStr(newVz + 192);
                end else if nifFile = 3 then begin
                    if bVertexIsOutsideCell then newVzStr := IntToStr(newVz + 192)
                    else newVzStr := IntToStr(newVz + 576);
                end;
                vertex.EditValues['Vertex'] := vx + ' ' + vy + ' ' + newVzStr;
            end;
            block.UpdateNormals;
            block.UpdateTangents;
            snowNif.SaveToFile(snowNifFile);
        finally
            snowNif.Free;
        end;
    end;
    Result := 1;
end;

function CreateLandscapeHeights(land, rCell, rWrld: IwbElement; wrldEdid: string): boolean;
var
    bLandHeightsExist, bHasWater: boolean;
    landOffsetZ, rowColumnOffsetZ, rowStartVal, landValue, landValueScaled, landValueChanged, cellWaterHeightFloat: single;
    cellX, cellY, unitsX, unitsY, row, column, iFlat, iHeightAlwaysBelowWater: integer;
    fileProvidingLand, rowColumn, cellWaterHeight: string;

    landHeightData: IwbElement;
begin
    Result := False;
    cellX := GetElementNativeValues(rCell, 'XCLC\X');
    cellY := GetElementNativeValues(rCell, 'XCLC\Y');
    unitsX := cellX * 4096;
    unitsY := cellY * 4096;
    fileProvidingLand := GetFileName(GetFile(land));
    bHasWater := False;
    if GetElementNativeValues(land, 'DATA\Has Water') = 1 then bHasWater := True;
    cellWaterHeight := GetElementEditValues(rCell, 'XCLW');
    if cellWaterHeight = 'Default' then cellWaterHeight := GetElementEditValues(rWrld, 'DNAM\Default Water Height');
    cellWaterHeightFloat := StrToFloatDef(cellWaterHeight, 9);

    landOffsetZ := GetElementNativeValues(land, 'VHGT\Offset');
    landHeightData := ElementByPath(land, 'VHGT\Height Data');

    bLandHeightsExist := joLandscapeHeights.O[fileProvidingLand].O[wrldEdid].O[cellX].O[cellY].Contains('offset');
    if bLandHeightsExist and not bCreateLandscapeHeights then Exit; //If land heights already exist and we are not creating land heights, exit.
    Result := True;
    joLandscapeHeights.O[fileProvidingLand].O[wrldEdid].O[cellX].O[cellY].S['offset'] := Round(landOffsetZ);
    iFlat := 4; // assume the cell is flat
    iHeightAlwaysBelowWater := 8; // assume the cell is always below water

    for row := 0 to 32 do begin
        for column := 0 to 32 do begin
            rowColumn := 'Row #' + IntToStr(row) + '\Column #' + IntToStr(column);
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
            landValueScaled := (landValue + landOffsetZ) * SCALE_FACTOR_TERRAIN; //This will be the Z height we apply to the vertex of the nif.
            if landValue <> 0 then iFlat := 0; //If at least one land height is not 0, we will not mark this cell not flat.
            if bHasWater and (landValueScaled < cellWaterHeightFloat) then begin
                landValueChanged := landValue - 4; //If the land height is below water, we need to lower it more so the snow does not poke above the water.
            end else begin
                iHeightAlwaysBelowWater := 0; //If at least one land height is above water, we will not mark this cell as always below water.
                landValueChanged := landValue;
            end;
            joLandscapeHeights.O[fileProvidingLand].O[wrldEdid].O[cellX].O[cellY].A[row].Add(landValueChanged);
            bSaveLandHeights := True;
            Result := 1;

            // X Y Z coordinates. Shouldn't need these.
            // pX := FloatToStr(column * 128);
            // pY := FloatToStr(row * 128);
            // pZ := FloatToStr(landValueScaled);
        end;
    end;
    joLandscapeHeights.O[fileProvidingLand].O[wrldEdid].O[cellX].O[cellY].S['flags'] := iFlat + iHeightAlwaysBelowWater;
    if iFlat = 4 then Result := False;
    if iHeightAlwaysBelowWater = 8 then Result := False;
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
        if f = 'Fallout4.exe' then continue;
        if f = sSeasonsFileName then begin
            SeasonsMainFile := FileByIndex(i);
        end;
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

    //Alter Land
    j := 'Seasons\' + TrimLeftChars(f, 4) + ' - Alter Land.json';
    if ResourceExists(j) then begin
        AddMessage('Loaded Alter Land File: ' + j);
        sub := TJsonObject.Create;
        try
            sub.LoadFromResource(j);
            for c := 0 to Pred(sub.Count) do begin
                key := sub.Names[c];
                joAlterLandRules.S[key] := sub.S[key];
            end;
        finally
            sub.Free;
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
    chkCreateTestPlugin, chkCreateLandscapeHeights, chkCreateLandscapeSnowMeshes, chkPlaceLandscapeSnow: TCheckBox;
begin
    frm := TForm.Create(nil);
    try
        frm.Caption := 'Seasons Change';
        frm.Width := 680;
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

        chkCreateLandscapeSnowMeshes := TCheckBox.Create(gbOptions);
        chkCreateLandscapeSnowMeshes.Parent := gbOptions;
        chkCreateLandscapeSnowMeshes.Left := chkCreateLandscapeHeights.Left + chkCreateLandscapeHeights.Width + 16;
        chkCreateLandscapeSnowMeshes.Top := chkCreateLandscapeHeights.Top;
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

        if frm.ShowModal <> mrOk then begin
            Result := False;
            Exit;
        end else Result := True;

        bCreateTestPlugin := chkCreateTestPlugin.Checked;
        bCreateLandscapeHeights := chkCreateLandscapeHeights.Checked;
        bCreateLandscapeSnowMeshes := chkCreateLandscapeSnowMeshes.Checked;
        bPlaceLandscapeSnow := chkPlaceLandscapeSnow.Checked;

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
    Result := IntToHex(FormID(e), 8) + ':' + GetFileName(GetFile(MasterOrSelf(e)));
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

function GetCellFromWorldspace(Worldspace: IInterface; GridX, GridY: integer): IInterface;
var
    blockidx, subblockidx, cellidx: integer;
    wrldgrup, block, subblock, cell: IInterface;
    Grid, GridBlock, GridSubBlock: TwbGridCell;
    LabelBlock, LabelSubBlock: Cardinal;
begin
    Grid := wbGridCell(GridX, GridY);
    GridSubBlock := wbSubBlockFromGridCell(Grid);
    LabelSubBlock := wbGridCellToGroupLabel(GridSubBlock);
    GridBlock := wbBlockFromSubBlock(GridSubBlock);
    LabelBlock := wbGridCellToGroupLabel(GridBlock);

    wrldgrup := ChildGroup(Worldspace);
    // iterate over Exterior Blocks
    for blockidx := 0 to Pred(ElementCount(wrldgrup)) do begin
        block := ElementByIndex(wrldgrup, blockidx);
        if GroupLabel(block) <> LabelBlock then Continue;
        // iterate over SubBlocks
        for subblockidx := 0 to Pred(ElementCount(block)) do begin
            subblock := ElementByIndex(block, subblockidx);
            if GroupLabel(subblock) <> LabelSubBlock then Continue;
            // iterate over Cells
            for cellidx := 0 to Pred(ElementCount(subblock)) do begin
                cell := ElementByIndex(subblock, cellidx);
                if (Signature(cell) <> 'CELL') or GetIsPersistent(cell) then Continue;
                if (GetElementNativeValues(cell, 'XCLC\X') = Grid.x) and (GetElementNativeValues(cell, 'XCLC\Y') = Grid.y) then begin
                    Result := cell;
                    Exit;
                end;
            end;
            Break;
        end;
        Break;
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


end.
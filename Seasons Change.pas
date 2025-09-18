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
    joSeasons, joLandscapeHeights, joLandFiles, joAlterLandRules, joAlteredLandscape: TJsonObject;

const
    sSeasonsFileName = 'Seasons.esm';
    SCALE_FACTOR_TERRAIN = 8;
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
    slPluginFiles := TStringList.Create;
    joSeasons := TJsonObject.Create;
    joLandscapeHeights := TJsonObject.Create;
    joLandFiles := TJsonObject.Create;
    joAlterLandRules := TJsonObject.Create;
    joAlteredLandscape := TJsonObject.Create;
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
    joAlteredLandscape.Free;
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
    //if bSaveLandHeights then joLandscapeHeights.SaveToFile(wbScriptsPath + 'Seasons\LandHeights.json', False, TEncoding.UTF8, True);
end;

procedure AlterLandHeightsForTheseBases;
{
    Alters land heights for certain base objects placed near landscape.
}
var
    i, alteration: integer;
    base: IwbElement;
begin
    for i:= 0 to Pred(tlBasesThatAlterLand.Count) do begin
        base := ObjectToElement(tlBasesThatAlterLand[i]);
        alteration := joAlterLandRules.S[RecordFormIdFileId(base)];
        AddMessage('Processing base ' + #9 + Name(base) + #9 + IntToStr(alteration));
        ProcessBasesThatAlterLand(base, base, alteration);
    end;
end;

function ProcessBasesThatAlterLand(base, fromBase: IwbElement; alteration: integer): boolean;
{
    Process a base object to see if it alters land heights.
}
var
    i: integer;
    wrldEdid: string;

    r, rCell, rWrld: IwbElement;
begin
    Result := False;
    for i := 0 to Pred(ReferencedByCount(base)) do begin
        //Check each reference to see if it is in an exterior cell with landscape, and is close enough to the landscape to alter it.
        //If so, we need to alter the land heights near the reference.
        r := ReferencedByIndex(base, i);
        if Signature(r) = 'SCOL' then begin
            ProcessBasesThatAlterLand(r, base, alteration);
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

        wrldEdid := GetElementEditValues(rWrld, 'EDID');

        // If we got this far, this REFR is in an exterior cell with landscape, and we will need to alter the land heights IF it is close enough to the landscape.
        AddMessage(#9 + 'Processing reference ' + #9 + ShortName(r) + #9 + wrldEdid);
        AlterLandHeightsForThisRefr(r, base, fromBase, wrldEdid, alteration, rWrld);
    end;
end;

procedure AlterLandHeightsForThisRefr(r, base, fromBase: IwbElement; wrldEdid: string; alteration: integer; rWrld: IwbElement);
{
    Alters land heights for a specific reference.
}
var
    bSCOL: boolean;
    i, x1, y1, z1, x2, y2, z2: integer;
    scale, posX, posY, posZ, rotX, rotY, rotZ, pmScale, pmPosX, pmPosY, pmPosZ, pmRotX, pmRotY, pmRotZ: double;

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
        scolParts := ElementByPath(r, 'Parts');
        for i := 0 to Pred(ElementCount(scolParts)) do begin
            scolPart := ElementByIndex(scolParts, i);
            onam := LinksTo(ElementByPath(scolPart, 'ONAM'));
            if onam = realBase then break;
        end;
        placements := ElementByPath(scolPart, 'Placements');
        for i := 0 to Pred(ElementCount(placements)) do begin
            placement := ElementByIndex(placements, i);

            pmPosX := GetElementNativeValues(placement, 'Position\X');
            pmPosY := GetElementNativeValues(placement, 'Position\Y');
            pmPosZ := GetElementNativeValues(placement, 'Position\Z');
            pmRotX := GetElementNativeValues(placement, 'Rotation\X');
            pmRotY := GetElementNativeValues(placement, 'Rotation\Y');
            pmRotZ := GetElementNativeValues(placement, 'Rotation\Z');
            pmScale := GetElementNativeValues(placement, 'Scale');

            rotate_position(
                pmPosX, pmPosY, pmPosZ,         // initial position
                pmRotX, pmRotY, pmRotZ,        // rotation to apply - x y z
                raw_x, raw_y, raw_z           // (output) raw final position
            );

            posX := posX + raw_x;
            posY := posY + raw_y;
            posZ := posZ + raw_z;
            scale := scale * pmScale;
            x1 := Floor(x1 * scale);
            y1 := Floor(y1 * scale);
            z1 := Floor(z1 * scale);
            x2 := Ceil(x2 * scale);
            y2 := Ceil(y2 * scale);
            z2 := Ceil(z2 * scale);
            AlterLandHeightsForThisPlacement(alteration, x1, y1, z1, x2, y2, z2, posX, posY, posZ, rotX, rotY, rotZ, wrldEdid, realBase, rWrld);
        end;
    end
    else begin
        x1 := Floor(x1 * scale);
        y1 := Floor(y1 * scale);
        z1 := Floor(z1 * scale);
        x2 := Ceil(x2 * scale);
        y2 := Ceil(y2 * scale);
        z2 := Ceil(z2 * scale);
        AlterLandHeightsForThisPlacement(alteration, x1, y1, z1, x2, y2, z2, posX, posY, posZ, rotX, rotY, rotZ, wrldEdid, realBase, rWrld);
    end;
end;

procedure AlterLandHeightsForThisPlacement(alteration, x1, y1, z1, x2, y2, z2: integer; posX, posY, posZ, rotX, rotY, rotZ: double; wrldEdid: string; realBase, rWrld: IwbElement);
{
    Alters land heights for a specific placement of a base object.
}
var
    bHasWater: boolean;
    i, j, k, row, column, cellXHere, cellYHere, vz, newZ, oldZ, landOffsetZ, alterationHere, width, height, numX, numY: integer;
    fileName, cellWaterHeight: string;
    raw_x, raw_y, raw_z, xHere, yHere, zHere, posXHere, posYHere, posZHere, cellWaterHeightFloat: double;
    rCell, rWrld: IwbElement;
begin
    //Okay, so what we need to do is understand that our object is rotated, and we are only given the bounds of the object in the unrotated state.
    //We are going to need to just take the bounding width/height of the object, divide that by 128, to get the number of lines we will use to get all the vertices
    //that we will need to alter.
    //    ||||          ////
    //    ||||         ////
    //    ||||   to   ////
    //    ||||       ////
    //    ||||      ////
    width := x2 - x1;
    height := y2 - y1;
    numX := Ceil(width/128);
    numY := Ceil(height/128);
    zHere := z2;
    AddMessage(#9 + #9 + 'Width: ' + IntToStr(width) + ' Height: ' + IntToStr(height) + ' NumX: ' + IntToStr(numX) + ' NumY: ' + IntToStr(numY) + ' Rotation: ' + FloatToStr(rotZ) + ' World: ' + wrldEdid);

    //Now we will loop through all the points we need to check, get the rotated position, find the closest land vertex, and alter it.
    for i := 0 to numX do begin
        xHere := x1 + (i * width/numX);
        //xHere := x1 + (i * 128);
        for j := 0 to numY do begin
            yHere := y1 + (j * height/numY);
            //yHere := y1 + (j * 128);
            AddMessage(#9 + #9 + #9 + 'Processing point ' + FloatToStr(xHere) + ',' + FloatToStr(yHere));
            if xHere > x2 then xHere := x2;
            if yHere > y2 then yHere := y2;
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
            AddMessage(#9 + #9 + #9 + 'Rotated point ' + FloatToStr(raw_x) + ',' + FloatToStr(raw_y));
            posXHere := posX + raw_x;
            posYHere := posY + raw_y;
            posZHere := posZ + raw_z;

            //Find the cell this position is in.
            cellXHere := Floor(posXHere/4096);
            cellYHere := Floor(posYHere/4096);
            AddMessage(#9 + #9 + #9 + 'Position ' + FloatToStr(posXHere) + ',' + FloatToStr(posYHere) + ' is in cell ' + IntToStr(cellXHere) + ',' + IntToStr(cellYHere));
            fileName := joLandFiles.O[wrldEdid].O[cellXHere].S[cellYHere];
            if fileName = '' then begin
                AddMessage(#9 + #9 + #9 + 'No landscape in this cell.');
                continue; //No landscape in this cell.
            end;

            alterationHere := alteration/SCALE_FACTOR_TERRAIN;
            if alterationHere > 0 then begin
                rCell := GetCellFromWorldspace(rWrld, cellXHere, cellYHere);
                bHasWater := True;
                if GetElementEditValues(rCell, 'DATA\Has Water') = '0' then bHasWater := False;
                cellWaterHeight := GetElementEditValues(rCell, 'XCLW');
                if cellWaterHeight = 'Default' then cellWaterHeight := GetElementEditValues(rWrld, 'DNAM\Default Water Height');
                cellWaterHeightFloat := StrToFloatDef(cellWaterHeight, 9);
            end;

            //Find the closest vertex in this cell.
            for k := 0 to 3 do begin


                if k = 0 then begin
                    column := Floor((posXHere - (cellXHere * 4096))/128);
                    row := Floor((posYHere - (cellYHere * 4096))/128);
                end
                else if k = 1 then begin
                    column := Floor((posXHere - (cellXHere * 4096))/128);
                    row := Ceil((posYHere - (cellYHere * 4096))/128);
                end
                else if k = 2 then begin
                    column := Ceil((posXHere - (cellXHere * 4096))/128);
                    row := Floor((posYHere - (cellYHere * 4096))/128);
                end
                else begin
                    column := Ceil((posXHere - (cellXHere * 4096))/128);
                    row := Ceil((posYHere - (cellYHere * 4096))/128);
                end;

                if joAlteredLandscape.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].O[row].S[column] = '1' then begin
                    AddMessage(#9 + #9 + #9 + 'This vertex has already been altered.');
                    if alterationHere > 0 then continue;
                    if alterationHere < 0 then alterationHere := alterationHere - 2;
                end
                else if joAlteredLandscape.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].O[row].S[column] = '-1' then begin
                    AddMessage(#9 + #9 + #9 + 'This vertex has already been altered.');
                    if alterationHere > 0 then continue;
                end;

                landOffsetZ := joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].S['offset'] * SCALE_FACTOR_TERRAIN;
                vz := joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].A[row].S[column];
                oldZ := vz * SCALE_FACTOR_TERRAIN + landOffsetZ;
                if posZHere < oldZ then begin
                    AddMessage(#9 + #9 + #9 + 'Object is below the landscape vertex, so skipping it.');
                    continue; //The object is completely below this vertex, so skip it.
                end;
                if (alterationHere > 0) and (oldZ < cellWaterHeightFloat) then begin
                    AddMessage(#9 + #9 + #9 + 'Landscape is below water height at this vertex, so skipping it.');
                    continue; //The object is below water, so skipping it.
                end;



                //if alterationHere > 0 then continue;

                // if ((alterationHere > 0) and (posZHere > (oldZ + alteration))) then begin
                //     //We will limit the alteration based on the distance from the edges of the object, limiting to a change of 1 from the edges.
                //     //alterationHere := Min(alterationHere, Min(Min(i, numX - i), Min(j, numY - j)) + 1);

                //     while (posZHere > oldZ + (alterationHere * SCALE_FACTOR_TERRAIN)) do Inc(alterationHere);
                //     Inc(alterationHere);
                // end;
                newZ := vz + alterationHere;

                joLandscapeHeights.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].A[row].S[column] := newZ;
                AddMessage(#9 + #9 + #9 + 'Altering land height at ' + IntToStr(column) + ',' + IntToStr(row) + ' in ' + wrldEdid + ' ' + IntToStr(cellXHere) + ' ' + IntToStr(cellYHere) + ' from ' + IntToStr(vz) + ' to ' + IntToStr(newZ));
                if alteration < 0 then joAlteredLandscape.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].O[row].S[column] := '-1'
                else if alteration > 0 then joAlteredLandscape.O[fileName].O[wrldEdid].O[cellXHere].O[cellYHere].O[row].S[column] := '1';

            end;
        end;
    end;
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
    i, nifFile, unitsX, unitsY, landOffsetZ, row2, column2, row, column, vertexCount, point1, point2, point3, point4, maxPoint, newCellX, newCellY, newlandOffsetZ, cellOffsetDifference, newVz, neighborVz: integer;
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
                        fileNameLand := joLandFiles.O[wrldEdid].O[newCellX].S[newCellY];

                        if fileNameLand <> '' then begin
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
                        end
                        else begin
                            //If the neighboring cell does not exist, we should change the row/column to the vertex that is closest and use that for the newVz
                            if column < 0 then column := 0
                            else if column > 32 then column := 32;
                            if row < 0 then row := 0
                            else if row > 32 then row := 32;
                            newVz := joLandscapeHeights.O[fileProvidingLand].O[wrldEdid].O[cellX].O[cellY].A[row].S[column] * SCALE_FACTOR_TERRAIN;
                        end;
                    end
                    else if ((row = 0) or (row = 32) or (column = 0) or (column = 32)) then begin
                        // This is the value here, but we need to check the neighboring cell's same border vertex and use the lowest of the two.
                        newVz := joLandscapeHeights.O[fileProvidingLand].O[wrldEdid].O[cellX].O[cellY].A[row].S[column] * SCALE_FACTOR_TERRAIN;

                        // Find the neighboring cell based off these values.
                        newCellX := cellX; //default to no cell offset
                        newCellY := cellY;
                        if column = 0 then newCellX := cellX - 1
                        else if column = 32 then newCellX := cellX + 1;
                        if row = 0 then newCellY := cellY - 1
                        else if row = 32 then newCellY := cellY + 1;
                        fileNameLand := joLandFiles.O[wrldEdid].O[newCellX].S[newCellY];
                        if fileNameLand <> '' then begin
                            //If the neighboring cell does exist, we need to compare the offset values, as that will affect the height.
                            newlandOffsetZ := joLandscapeHeights.O[fileNameLand].O[wrldEdid].O[newCellX].O[newCellY].S['offset'];
                            cellOffsetDifference := landOffsetZ - newlandOffsetZ;

                            if column = 0 then column := 32
                            else if column = 32 then column := 0;
                            if row = 0 then row := 32
                            else if row = 32 then row := 0;
                            neighborVz := (joLandscapeHeights.O[fileNameLand].O[wrldEdid].O[newCellX].O[newCellY].A[row].S[column] - cellOffsetDifference) * SCALE_FACTOR_TERRAIN;
                            if neighborVz < newVz then newVz := neighborVz;
                        end;
                    end
                    else begin
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
                    newVz := ComputeInBetweenVertexHeight(point1, point2, point3, point4);
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

function ComputeInBetweenVertexHeight(point1, point2, point3, point4: integer): integer;
var
    points: array[0..3] of integer;
    diffs: array[0..3] of double;
    idx: array[0..3] of integer;
    m, n, tmp: integer;
    mean: double;
begin
    Result := (point1 + point2 + point3 + point4)/4;
    Exit;

    //This code is unreachable... it didn't look right.
    points[0] := point1;
    points[1] := point2;
    points[2] := point3;
    points[3] := point4;
    // Calculate difference from each point to the mean
    mean := (point1 + point2 + point3 + point4) / 4;
    for m := 0 to 3 do begin
        diffs[m] := Abs(points[m] - mean);
        idx[m] := m;
    end;
    // Sort idx by diffs ascending (simple bubble sort for 4 elements)
    for m := 0 to 2 do
        for n := m + 1 to 3 do
            if diffs[idx[m]] > diffs[idx[n]] then begin
                tmp := idx[m];
                idx[m] := idx[n];
                idx[n] := tmp;
            end;
    // Take the three closest points
    Result := (points[idx[0]] + points[idx[1]] + points[idx[2]]) / 3;
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
    bHasWater := True;
    if GetElementEditValues(rCell, 'DATA\Has Water') = '0' then bHasWater := False;
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


end.
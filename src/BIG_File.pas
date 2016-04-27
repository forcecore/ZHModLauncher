unit BIG_File;

interface

uses
   BasicDataTypes, SysUtils, Classes, Dialogs;

const
   C_BIG4 = $34474942;
   C_BIGF = $46474942;

type
   TBIGFileUnit = record
      Offset : uint32;
      Size : uint32;
      Filename : string;
      FileWritten : Boolean;
      Index : integer;
   end;

   TBIGPackage = class
      private
         ValidFile : boolean;
         Filename : string;
         BIGType : uint32;
         Size : uint32;
         Files : array of TBIGFileUnit;
         // Constructors and Destructors
         procedure Reset;
         // I/O
         function InvertInteger(_i: uint32): uint32;
         function InvertInteger16(_i: uint16): uint16;
         procedure DecompressData(var _Input,_OutPut: puint8);
      public
         // Constructors and Destructors
         constructor Create;
         // I/O
         procedure LoadFile(const _Filename : string);
         // Gets
         function GetNumFiles : uint32;
         function GetFileInfo (_ID : int32): TBIGFileUnit;
         function GetFileContents( _ID : int32; var _Data : TStream; var _DataSize : uint32) : Boolean;
         function IsValid : boolean;
         // Sets
   end;

implementation

// Constructors and Destructors
constructor TBIGPackage.Create;
begin
   Reset;
end;

procedure TBIGPackage.Reset;
begin
   ValidFile := false;
   SetLength(Files,0);
end;

procedure TBIGPackage.LoadFile(const _Filename : string);
var
   MyFile : TStream;
   FileSize: uint32;
   PData, PCurrentData: pint8;
   HeaderSize,NumFiles,i : uint32;
begin
   Reset;
   if FileExists(_Filename) then
   begin
      Filename := copy(_Filename,1,Length(_Filename));
      MyFile := TFileStream.Create(_Filename, fmOpenRead); // Open file

      // Store the whole file in the memory
      FileSize := MyFile.Size;
      if Filesize > 0 then
      begin
         Getmem(PData, 4);
         MyFile.Read(PData^, 4);
         PCurrentData := PData;
//         MyFile.Free;

         // Let's start reading the file here.
         BIGType := uint32(puint32(PCurrentData)^);
         inc(PCurrentData,4);
         if (BIGType = C_BIG4) or (BIGType = C_BIGF) then
         begin
            // 0.4 Beta Change. This reduces memory load.
            FreeMem(PData);
            Getmem(PData, 16);
            PCurrentData := PData;
            MyFile.Read(PData^, 12);
            // Old code resumes...
            Size := InvertInteger(uint32(puint32(PCurrentData)^));
            inc(PCurrentData,4);
            NumFiles := InvertInteger(uint32(puint32(PCurrentData)^));
            inc(PCurrentData,4);
            // 0.4 code again. Now we get the header size
            HeaderSize := InvertInteger(uint32(puint32(PCurrentData)^)) - 16;
            inc(PCurrentData,4);
            FreeMem(PData);
            GetMem(PData,HeaderSize);
            PCurrentData := PData;
            MyFile.Read(PData^, HeaderSize);
            MyFile.Free;
            // Here we resume the old code.
            SetLength(Files,NumFiles);
            // Let's read each file
            for i := 0 to High(Files) do
            begin
               Files[i].Offset := InvertInteger(uint32(puint32(PCurrentData)^));
               inc(PCurrentData,4);
               Files[i].Size := InvertInteger(uint32(puint32(PCurrentData)^));
               inc(PCurrentData,4);
               // Now we need to read the filename.
               while (PCurrentData^ <> 0) do
               begin
                  Files[i].Filename := Files[i].Filename + char(PCurrentData^);
                  inc(PCurrentData,1);
               end;
               inc(PCurrentData,1);
               Files[i].FileWritten := true;
            end;
            ValidFile := true;
         end
         else
            MyFile.Free;
         FreeMem(PData);
      end;
   end;
end;

function TBIGPackage.InvertInteger(_i: uint32): uint32;
begin
   Result := (_i and $ff) shl 24;
   _i := _i shr 8;
   Result := Result + ((_i and $ff) shl 16);
   _i := _i shr 8;
   Result := Result + ((_i and $ff) shl 8);
   _i := _i shr 8;
   Result := Result + _i;
end;

function TBIGPackage.InvertInteger16(_i: uint16): uint16;
begin
   Result := (_i and $ff) shl 8;
   _i := _i shr 8;
   Result := Result + _i;
end;

// Gets
function TBIGPackage.GetNumFiles: uint32;
begin
   Result := High(Files)+1;
end;

function TBIGPackage.GetFileInfo (_ID : int32): TBIGFileUnit;
begin
   if _ID <= High(Files) then
   begin
      Result := Files[_ID];
   end
   else
      Result := Files[0];
end;

function TBIGPackage.GetFileContents( _ID : int32; var _Data : TStream; var _DataSize : uint32) : Boolean;
var
   MyFile : TStream;
   FileSize: uint32;
   PData,iPData,PCData: puint8;
   FileWord : uint16;
   CompressedSize : uint32;
begin
   Result := false;
   if ValidFile and (_ID <= High(Files)) then
   begin
      MyFile := TFileStream.Create(Filename, fmOpenRead); // Open file

      // Store the whole file in the memory
      FileSize := MyFile.Size;
      if Filesize > 0 then
      begin
         Getmem(PData, Files[_ID].Size);
         MyFile.Seek(Files[_ID].Offset,soFromBeginning);
         MyFile.Read(PData^, Files[_ID].Size);
         MyFile.Free;

         // Now, we create the contents and copy the interesting part.
         iPData := PData;
         _Data := TMemoryStream.Create;
         FileWord := InvertInteger16(uint16(puint16(iPData)^));
         if (FileWord and $3EFF) = $10FB then
         begin
            inc(iPData,2);
            if (FileWord and $8000) = 0 then
            begin
               CompressedSize := iPData^ shl 16;
               inc(iPData);
               CompressedSize := CompressedSize or (iPData^ shl 8);
               inc(iPData);
               CompressedSize := CompressedSize or iPData^;
               inc(iPData);
               if (FileWord and $100) > 0 then
                  inc(iPData,3);
            end
            else
            begin
               CompressedSize := InvertInteger(uint32(puint32(iPData)^));
               if (FileWord and $100) > 0 then
                  inc(iPData,4);
               inc(iPData,4);
            end;
            GetMem(PCData,CompressedSize);
            DecompressData(iPData,PCData);
            _Data.Write(PCData^,CompressedSize);
            FreeMem(PCData);
            _DataSize := CompressedSize;
         end
         else
         begin
            _Data.Write(iPData^,Files[_ID].Size);
            _DataSize := Files[_ID].Size;
         end;
         FreeMem(PData);
      end;
   end;
end;

// RefPack decompression function written by jonwil.
// Conversion from C to Delphi by Banshee.
procedure TBIGPackage.DecompressData(var _Input,_Output : puint8);
var
   CurrInput,CurrOutput : puint8;
   Flags : uint32;
   code,code2,code3,code4 : uint8;
   count : uint32;
   TempBuffer : puint8;
   i : uint32;
begin
	flags := 0;
	code := 0;
	code2 := 0;
	count := 0;
	code3 := 0;
	code4 := 0;
   CurrInput := _Input;
   CurrOutput := _Output;
	while (true) do
	begin
		code := CurrInput^;
   	inc(CurrInput);
		if (code and $80) = 0 then
		begin
			code2 := CurrInput^;
			inc(CurrInput);
			count := code and 3;
			for i := 1 to count do
			begin
				CurrOutput^ := CurrInput^;
				inc(CurrOutput);
				inc(CurrInput);
			end;
			TempBuffer := Puint8((Cardinal(CurrOutput) - 1) - (code2 + (code and $60) * 8));
			count := ((code and $1C) div 4) + 2;
			for i := 0 to count do
			begin
            try
				   CurrOutput^ := TempBuffer^;
            except
               ShowMessage(IntToStr(Cardinal(TempBuffer)));
               exit;
            end;
				inc(CurrOutput);
				inc(TempBuffer);
			end;
		end
		else if (code and $40) = 0 then
		begin
			code2 := CurrInput^;
			inc(CurrInput);
			code3 := CurrInput^;
			inc(CurrInput);
			count := code2 shr 6;
			for i := 1 to count do
			begin
				CurrOutput^ := CurrInput^;
				inc(CurrOutput);
				inc(CurrInput);
			end;
			TempBuffer :=PuInt8((Cardinal(CurrOutput) - 1) - (((code2 and $3F) shl 8) + code3));
			count := (code and $3F) + 3;
			for i := 0 to count do
			begin
				CurrOutput^ := TempBuffer^;
				inc(CurrOutput);
				inc(TempBuffer);
			end;
		end
		else if (code and $20) = 0 then
		begin
			code2 := CurrInput^;
			inc(CurrInput);
			code3 := CurrInput^;
			inc(CurrInput);
			code4 := CurrInput^;
			inc(CurrInput);
			count := code and 3;
			for i := 1 to count do
			begin
				CurrOutput^ := CurrInput^;
				inc(CurrOutput);
				inc(CurrInput);
			end;
			TempBuffer := PuInt8((Cardinal(CurrOutput) - 1) - ((((code and $10) shr 4) shl $10) + (code2 shl 8) + code3));
			count := (((code and $0C) shr 2) shl 8) + code4 + 4;
  			for i := 0 to count do
			begin
            try
				   CurrOutput^ := TempBuffer^;
            except
               ShowMessage(IntToStr(Cardinal(TempBuffer)));
               exit;
            end;
				inc(CurrOutput);
				inc(TempBuffer);
			end;
		end
		else
		begin
			count := ((code and $1F) * 4) + 4;
			if (count <= $70) then
			begin
				for i := 1 to count do
				begin
					CurrOutput^ := CurrInput^;
					inc(CurrOutput);
					inc(CurrInput);
				end;
			end
			else
			begin
				count := code and 3;
				for i := 1 to count do
				begin
					CurrOutput^ := CurrInput^;
					inc(CurrOutput);
					inc(CurrInput);
				end;
				exit;
			end;
		end;
	end;
end;

function TBIGPackage.IsValid : boolean;
begin
   Result := ValidFile;
end;

end.

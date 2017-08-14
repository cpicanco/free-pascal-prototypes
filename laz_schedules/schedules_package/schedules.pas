{
  Schedules
  Copyright (C) 2007 Drausio Capobianco, Universidade Federal de São Carlos.
  Copyright (C) 2010-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Schedules;

{$mode objfpc}{$H+}

interface

uses Classes, Schedules.Abstract;

type

  TScheduleName = (UnknownSchedule,
    EXT, // Extintion = FR MaxtInt
    CRF, // Continuous reinforcement = FR 1
    FR,  // Fixed ratio
    FI,  // Fixed interval
    FT,  // Fixed time
    VR,  // Variable ratio
    VI,  // Variable interval
    VT,  // Variable time
    DRL, // Differential reinforcement of low rates
    DRH  // Differential reinforcement of high rates
  );

//type
//  TScheduleParameters = record
//  case ScheduleName : TScheduleName of
//    UnknownSchedule,
//    EXT, CRF : ();
//          FR : (FixedRatio : Cardinal);
//          FI : (FixedInterval : Cardinal);
//          FT : (FixedTime : Cardinal);
//          VR : (BaseRatio, RatioVariation : Cardinal);
//          VI : (BaseInterval, IntervalVariation : Cardinal);
//          VT : (BaseTime, TimeVariation : Cardinal);
//    DRL, DRH : (Ratio, Interval : Cardinal);
//  end;
//
//  operator=(A, B : TScheduleParameters) : Boolean;
//
// type

  { TSchedule }

  TSchedule = class sealed (TComponent)
  private
    FName : TScheduleName;
    FSchedule: TSchedules;
    FScheduleLoaded: Boolean;
    //FParameters : TScheduleParameters;
    procedure Consequence(Sender: TObject);
    procedure DestroySchedule;
    function GetLoaded: Boolean;
    function GetOnConsequence: TNotifyEvent;
    function GetOnResponse: TNotifyEvent;
    function GetParameter(i : integer): Cardinal;
    //function GetParameters: TScheduleParameters;
    function GetParametersAsString: string; overload;
    function GetParameters(ASchedule: string): string; overload;
    function GetScheduleName: TScheduleName;
    function GetScheduleString: string;
    function GetScheduleNameAsString: string; overload;
    function GetScheduleName(ASchedule: string): string; overload;
    procedure Response(Sender: TObject);
    procedure SetOnConsequence(AValue: TNotifyEvent);
    procedure SetOnResponse(AValue: TNotifyEvent);
    procedure SetParameter(i : integer; AValue: Cardinal);
    procedure SetParameters(ASchedule: string); overload;
    //procedure SetParameters(AValue: TScheduleParameters); overload;
    procedure SetParameters(AParameter1, AParameter2 : Cardinal); overload;
    procedure CreateSchedule(AScheduleName : TScheduleName); overload;
    procedure CreateSchedule(AName : string); overload;
    procedure SetParametersAsString(AValue: string);
    procedure SetScheduleName(AScheduleName: TScheduleName);
    procedure SetScheduleNameAsString(AValue: string);
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadParameters(Reader: TReader);
    procedure WriteParameters(Writer: TWriter);
  public
    constructor Create(AOwner : TComponent; ASchedule : string); overload;
    constructor Create(AOwner : TComponent; AScheduleName : TScheduleName;
      AParameter1: Cardinal = 0; AParameter2: Cardinal = 0); overload;
    destructor Destroy; override;
    procedure DoResponse;
    procedure Load(ASchedule : string); overload;
    procedure Load(AScheduleName : TScheduleName;
      AParameter1: Cardinal = 0; AParameter2: Cardinal = 0); overload;
    procedure Start;
    property AsString : string read GetScheduleString write Load;
    property Loaded : Boolean read GetLoaded;
    property ScheduleNameAsString : string read GetScheduleNameAsString write SetScheduleNameAsString;
    property Parameter[i : integer] : Cardinal read GetParameter write SetParameter;
    //property Parameters : TScheduleParameters read GetParameters write SetParameters;
    property ParametersAsString : string read GetParametersAsString write SetParametersAsString;
  published
    property Schedule : TScheduleName read GetScheduleName write SetScheduleName;
    property OnConsequence: TNotifyEvent read GetOnConsequence write SetOnConsequence;
    property OnResponse: TNotifyEvent read GetOnResponse write SetOnResponse;
  end;

implementation

uses SysUtils, StrUtils, Schedules.Classes;
//
//operator=(A, B: TScheduleParameters) : Boolean;
//begin
//  Result := A.ScheduleName = B.ScheduleName;
//  if not Result then Exit;
//  case A.ScheduleName of
//    UnknownSchedule,
//    EXT, CRF: Exit;
//          FR: Result := A.FixedRatio = B.FixedRatio;
//          FI: Result := A.FixedInterval = B.FixedInterval;
//          FT: Result := A.FixedTime = B.FixedTime;
//          VR:
//            begin
//              Result := A.BaseRatio = B.BaseRatio;
//              Result := A.RatioVariation = B.RatioVariation;
//            end;
//          VI:
//            begin
//              Result := A.BaseInterval = B.BaseInterval;
//              Result := A.IntervalVariation = B.IntervalVariation;
//            end;
//          VT:
//            begin
//              Result := A.BaseTime = B.BaseTime;
//              Result := A.TimeVariation = B.TimeVariation;
//            end;
//    DRL, DRH:
//            begin
//              Result := A.Ratio = A.Ratio;
//              Result := B.Interval = B.Interval;
//            end;
//  end;
//end;

procedure TSchedule.CreateSchedule(AScheduleName: TScheduleName);
begin
  if FName = AScheduleName then Exit;
  if Assigned(FSchedule) then DestroySchedule;
  case AScheduleName of
    EXT, CRF, FR, VR : FSchedule := TRatioSchedule.Create;
              FI, VI : FSchedule := TIntervalSchedule.Create;
              FT, VT : FSchedule := TTimeSchedule.Create;
                 DRL : FSchedule := TDRLSchedule.Create;
                 DRH : FSchedule := TDRHSchedule.Create;
    UnknownSchedule  :
      raise Exception.Create(RSErrorCreatingUnknownSchedule) at
        get_caller_addr(get_frame),
        get_caller_frame(get_frame);
  end;
  FName := AScheduleName;
  FSchedule.OnConsequence := @Consequence;
  FSchedule.OnResponse := @Response;
end;

procedure TSchedule.CreateSchedule(AName: string);
var
  LScheduleName : TScheduleName;
  LName : string;
  ValidScheduleNameFound : Boolean = False;
begin
  for LScheduleName := Succ(Low(TScheduleName)) to High(TScheduleName) do
    begin
      WriteStr(LName,LScheduleName);
      if LName = GetScheduleName(AName) then
        begin
          ValidScheduleNameFound:=True;
          Break;
        end;
    end;
  if ValidScheduleNameFound then
      CreateSchedule(LScheduleName)
  else
    raise Exception.Create(RSErrorCreatingUnknownSchedule) at
      get_caller_addr(get_frame),
      get_caller_frame(get_frame);
end;

procedure TSchedule.SetParametersAsString(AValue: string);
begin

end;

procedure TSchedule.SetScheduleName(AScheduleName: TScheduleName);
begin
  CreateSchedule(AScheduleName);
end;

procedure TSchedule.SetScheduleNameAsString(AValue: string);
begin
  CreateSchedule(AValue);
end;

procedure TSchedule.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
end;

procedure TSchedule.ReadParameters(Reader: TReader);
var
  LP1, LP2 : Cardinal;
begin
  with Reader do
    case FName of
      FR, FI, FT:
        begin
          LP1 := FSchedule.Parameter1;
          ReadListBegin;
          FSchedule.Parameter1 := ReadInteger;
          if FSchedule.Parameter1 > 0 then
            { do nothing }
          else
            FSchedule.Parameter1 := LP1;
          ReadListEnd;
        end;

      VR, VI, VT:
        begin
          LP1 := FSchedule.Parameter1;
          LP2 := FSchedule.Parameter2;
          ReadListBegin;
          FSchedule.Parameter1 := ReadInteger;
          if FSchedule.Parameter1 > 0 then
            begin
              FSchedule.Parameter2 := ReadInteger;
              if FSchedule.Parameter2 < FSchedule.Parameter1 then
                { do nothing }
              else
                FSchedule.Parameter2 := LP2;
            end
          else
            FSchedule.Parameter1 := LP1;
          ReadListEnd;
        end;

      DRH, DRL:
        begin
          LP1 := FSchedule.Parameter1;
          LP2 := FSchedule.Parameter2;
          ReadListBegin;
          FSchedule.Parameter1 := ReadInteger;
          FSchedule.Parameter2 := ReadInteger;
          if (FSchedule.Parameter1 > 0) and (FSchedule.Parameter2 > 0) then
            { do nothing }
          else
            begin
              FSchedule.Parameter1 := LP1;
              FSchedule.Parameter2 := LP2;
            end;
          ReadListEnd;
        end;
    end;
end;

procedure TSchedule.WriteParameters(Writer: TWriter);
begin
  with Writer do
    case FName of
      FR, FI, FT:
        begin
          WriteListBegin;
          if FSchedule.Parameter1 > 0 then
            WriteInteger(FSchedule.Parameter1);;
          WriteListEnd;
        end;

      VR, VI, VT:
        begin
          WriteListBegin;
          if FSchedule.Parameter1 > 0 then
            begin
              WriteInteger(FSchedule.Parameter1);
              if FSchedule.Parameter2 < FSchedule.Parameter1 then
                WriteInteger(FSchedule.Parameter2);

            end;
          WriteListEnd;
        end;

      DRH, DRL:
        begin
          WriteListBegin;
          if (FSchedule.Parameter1 > 0) and (FSchedule.Parameter2 > 0) then
            begin
              WriteInteger(FSchedule.Parameter1);
              WriteInteger(FSchedule.Parameter2);
            end;
          WriteListEnd;
        end;
    end;
end;

procedure TSchedule.SetParameters(AParameter1, AParameter2: Cardinal);
var
  LExceptionMessage : string;
begin
  case FName of
    CRF:
      begin
        FSchedule.Parameter1 := 1; // ratio
        FSchedule.Parameter2 := 0; // no variation
        Exit;
      end;

    EXT:
      begin
        FSchedule.Parameter1 := MaxInt; // ratio
        FSchedule.Parameter2 := 0;      // no variation
        Exit;
      end;

    FR, FI, FT:
      if AParameter1 > 0 then
        begin
          FSchedule.Parameter1 := AParameter1; // ratio or time
          FSchedule.Parameter2 := 0;           // no variation
          Exit;
        end
      else
        LExceptionMessage:=RSErrorInvalidValue;

    VR, VI, VT:
      if AParameter1 > 0 then
        begin
          FSchedule.Parameter1 := AParameter1; // ratio or time
          if AParameter2 < AParameter1 then
            begin
              FSchedule.Parameter2 := AParameter2; // variation
              Exit;
            end
          else
           LExceptionMessage := RSErrorInvalidVariation;
        end
      else
        LExceptionMessage := RSErrorInvalidValue;

    DRH, DRL:
      if (AParameter1 > 0) and (AParameter2 > 0) then
        begin
         FSchedule.Parameter1 := AParameter1; // ratio
         FSchedule.Parameter2 := AParameter2; // time
         Exit;
        end
      else
        LExceptionMessage:=RSErrorInvalidValue;

    UnknownSchedule:
      LExceptionMessage := RSErrorSettingUnknownScheduleParameters;
  end;

  raise Exception.Create(LExceptionMessage) at
    get_caller_addr(get_frame),
    get_caller_frame(get_frame);
end;

procedure TSchedule.SetParameter(i : integer; AValue: Cardinal);
begin
  case i of
    0 : if FSchedule.Parameter1 = AValue then Exit;
    1 : if FSchedule.Parameter2 = AValue then Exit;
  end;
  case FName of
    EXT, CRF: { do nothing };
    FR, FI, FT:
    case i of
      0 :
        begin
          SetParameters(AValue, FSchedule.Parameter2);
          Exit;
        end;
      1 : { do nothing };
    end;

    VR, VI, VT, DRH, DRL:
      begin
        case i of
          0 : SetParameters(AValue,FSchedule.Parameter2);
          1 : SetParameters(FSchedule.Parameter1,AValue);
        end;
        Exit;
      end;

    UnknownSchedule:
       raise Exception.Create(RSErrorGettingUnknownScheduleParameters) at
          get_caller_addr(get_frame),
          get_caller_frame(get_frame);
  end;

  raise Exception.Create(RSErrorParameterDoesNotExist) at
     get_caller_addr(get_frame),
     get_caller_frame(get_frame);
end;

procedure TSchedule.SetParameters(ASchedule: string);
begin
  if ParametersAsString = GetParameters(ASchedule) then Exit;
  SetParameters(StrToIntDef(ExtractDelimited(2,ASchedule,[#32]), 0),
                StrToIntDef(ExtractDelimited(3,ASchedule,[#32]), 0));
end;

//procedure TSchedule.SetParameters(AValue: TScheduleParameters);
//begin
//  if FParameters = AValue then Exit;
//  FParameters := AValue;
//  case FParameters.ScheduleName of
//    UnknownSchedule,
//    EXT, CRF : { do nothing };
//    FR: SetParameters(FParameters.FixedRatio, 0);
//    FI: SetParameters(FParameters.FixedInterval, 0);
//    FT: SetParameters(FParameters.FixedTime, 0);
//    VR: SetParameters(FParameters.BaseRatio, FParameters.RatioVariation);
//    VI: SetParameters(FParameters.BaseInterval, FParameters.IntervalVariation);
//    VT: SetParameters(FParameters.BaseTime, FParameters.TimeVariation);
//    DRL, DRH: SetParameters(FParameters.Ratio, FParameters.Interval);
//  end;
//end;

constructor TSchedule.Create(AOwner: TComponent; ASchedule: string);
begin
  inherited Create(AOwner);
  CreateSchedule(ASchedule);
end;

constructor TSchedule.Create(AOwner: TComponent; AScheduleName: TScheduleName;
  AParameter1: Cardinal; AParameter2: Cardinal);
begin
  inherited Create(AOwner);
  Load(AScheduleName, AParameter1, AParameter2);
end;

destructor TSchedule.Destroy;
begin
  if Assigned(FSchedule) then;
    FSchedule.Free;
  inherited Destroy;
end;

procedure TSchedule.Consequence(Sender: TObject);
begin
  if Assigned(OnConsequence) then OnConsequence(Self);
end;

procedure TSchedule.DestroySchedule;
begin
  FSchedule.OnConsequence := nil;
  FSchedule.OnResponse := nil;
  FreeAndNil(FSchedule);
end;

function TSchedule.GetLoaded: Boolean;
begin
  Result := FName <> UnknownSchedule;
end;

function TSchedule.GetOnConsequence: TNotifyEvent;
begin
  Result := FSchedule.OnConsequence;
end;

function TSchedule.GetOnResponse: TNotifyEvent;
begin
  Result := FSchedule.OnResponse;
end;

function TSchedule.GetParameter(i : integer): Cardinal;
begin
  case FName of
    EXT, CRF: { do nothing };
    FR, FI, FT:
    case i of
      0 :
        begin
          Result := FSchedule.Parameter1;
          Exit;
        end;
      1 : { do nothing };
    end;

    VR, VI, VT, DRH, DRL:
      begin
        case i of
          0 : Result := FSchedule.Parameter1;
          1 : Result := FSchedule.Parameter2;
        end;
        Exit;
      end;

    UnknownSchedule:
       raise Exception.Create(RSErrorGettingUnknownScheduleParameters) at
          get_caller_addr(get_frame),
          get_caller_frame(get_frame);
  end;

  raise Exception.Create(RSErrorParameterDoesNotExist) at
     get_caller_addr(get_frame),
     get_caller_frame(get_frame);
end;

//function TSchedule.GetParameters: TScheduleParameters;
//begin
//  Result := FParameters;
//end;

function TSchedule.GetParametersAsString: string;
begin
  case FName of
    EXT, CRF: Result := '';                              // no parameter required
    FR, FI, FT: Result:= IntToStr(FSchedule.Parameter1); // one parameter required
    VR, VI, VT, DRH, DRL:                                // two parameters required
      Result:= IntToStr(FSchedule.Parameter1)+#32+IntToStr(FSchedule.Parameter2);

    UnknownSchedule:
       raise Exception.Create(RSErrorGettingUnknownScheduleParameters) at
          get_caller_addr(get_frame),
          get_caller_frame(get_frame);
  end;
end;

function TSchedule.GetParameters(ASchedule: string): string;
begin
  Result := ExtractDelimited(2,ASchedule,[#32])+#32+ExtractDelimited(3,ASchedule,[#32]);
end;

function TSchedule.GetScheduleName: TScheduleName;
begin
  Result := FName;
end;

function TSchedule.GetScheduleString: string;
begin
  Result := GetScheduleNameAsString+#32+GetParametersAsString;
end;

function TSchedule.GetScheduleNameAsString: string;
var
  LName : string;
begin
  WriteStr(LName, FName);
  Result := LName;
end;

function TSchedule.GetScheduleName(ASchedule: string): string;
begin
  ASchedule := ASchedule+#32;
  Result := ExtractDelimited(1, ASchedule, [#32]);
end;

procedure TSchedule.Response(Sender: TObject);
begin
  if Assigned(OnResponse) then OnResponse(Self);
end;

procedure TSchedule.SetOnConsequence(AValue: TNotifyEvent);
begin
  FSchedule.OnConsequence:=AValue;
end;

procedure TSchedule.SetOnResponse(AValue: TNotifyEvent);
begin
  FSchedule.OnResponse:=AValue;
end;


procedure TSchedule.DoResponse;
begin
  if FScheduleLoaded then
    FSchedule.DoResponse;
end;

procedure TSchedule.Load(ASchedule: string);
begin
  CreateSchedule(GetScheduleName(ASchedule));
  SetParameters(ASchedule);
end;
procedure TSchedule.Load(AScheduleName: TScheduleName; AParameter1: Cardinal;
  AParameter2: Cardinal);
begin
  CreateSchedule(AScheduleName);
  SetParameters(AParameter1, AParameter2);
end;
procedure TSchedule.Start;
begin
  if FScheduleLoaded then
    FSchedule.Start;
end;



end.


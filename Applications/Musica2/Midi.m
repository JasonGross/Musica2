(* :Title: Midi *)

(* :Summary: Functions for Midi *)

(* :Author: Bo C. Herlin *)

(* :Licence: GPL

Copyright (C) 2004  Bo C. Herlin

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*)

(* :Contact: bo@gcab.net *)

(* :Context: Musica2`Midi` *)

(* :History:
  2004-08-14  bch :  yet another rest/tie scheme, now with MidiDataAnyValue and MidiDataNoValue, MidiRest* is removed
                     added MidiMilliSec as an integer MidiTimeUnit
                     added MidiPar and MidiSeq
  2004-08-13  bch :  added Midi(Add|Get|Rem|Set)(Notes|QPM) and MidiEmpty
                     renamed MidiEqualizeEOT to MidiAddEOT
  2004-08-10  bch :  removed MidiAddEOT, now incorporated into MidiEqualizeEOT
  2004-08-10  bch :  added MidiRest* and MidiTie*
  2004-08-09  bch :  used Reap/Sow and AppendTo as a speed boost, changed param to MidiVoiceReleaseTimeFunction
  2004-08-08  bch :  added this whole "state"-thing, all the MidiSetXxx called by a MidiSetStateLow will have its code moved to its caller and then be deleted
  2004-08-06  bch :  extended MidiVoiceReleaseTimeFunction to also take pitch and velocity
                     added some help/usage-text
  2004-08-04  bch :  first release
  2004-07-28  bch :  created
*)

(* :Keywords: music, midi *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Midi`",
  {
    "Utilities`BinaryFiles`",
    "Musica2`Utils`"
    }
]

Unprotect[
  Midi,
  MidiAbsolute,
  MidiAddEOT,
  MidiAddEvents,
  MidiAddNotes,
  MidiAddQPM,
  MidiChord,
  MidiControlChange,
  MidiDataAnyValue,
  MidiDataAnyValueQ,
  MidiDataNoValue,
  MidiDataNoValueQ,
  MidiDelta,
  MidiEmpty,
  MidiEOT,
  MidiExpandStates,
  MidiExpandStatePaths,
  MidiExportSMF,
  MidiFile,
  MidiFileFormat,
  MidiGetChannels,
  MidiGetDuration,
  MidiGetDurations,
  MidiGetInfo,
  MidiGetNotes,
  MidiGetQPM,
  MidiGetSecToTickFunction,
  MidiGetShape,
  MidiGetState,
  MidiGetTickToSecFunction,
  MidiGetTimeUnit,
  MidiGetTiming,
  MidiGetTPQ,
  MidiImportSMF,
  MidiKeySignature,
  MidiMeta,
  MidiMilliSec,
  MidiNormalizeNoteOff,
  MidiNoteOff,
  MidiNoteOn,
  MidiPar,
  MidiPatternData,
  MidiPatternChord,
  MidiPatternFile,
  MidiPatternInfo,
  MidiPatternMidi,
  MidiPatternMusic,
  MidiPatternTiming,
  MidiPatternTrack,
  MidiPatternType,
  MidiPatternVoice,
  MidiRemEvents,
  MidiRemNotes,
  MidiRemQPM,
  MidiSec,
  MidiSeq,
  MidiSetNotes,
  MidiSetQPM,
  MidiSetState,
  MidiSetStateLow,
  MidiShape,
  MidiStates,
  MidiStatePaths,
  MidiStateRoutes,
  MidiStatesExpanded,
  MidiStatePathsExpanded,
  MidiSysX0,
  MidiSysX7,
  MidiTempo,
  MidiTick,
  MidiTie,
  MidiTieQ,
  MidiTimeSignature,
  MidiTimeUnit,
  MidiTiming,
  MidiTPQ,
  MidiUnTie,
  MidiVoice,
  MidiVoiceReleaseTimeFunction,
  MidiQPM
  ];

Midi::usage = "Midi[i, d] represents a midi object where i is the info about d, the midi data."
MidiAbsolute::usage = "MidiTiming can be either MidiAbsolute or MidiDelta. If MidiTiming is MidiAbsolute, all timing information are absolute values. Observe that when in shape MidiVoice or MidiChord, MidiAbsolute means end-timing"
MidiAddEOT::usage = "MidiAddEOT[m_Midi]"
MidiAddEvents::usage = "MidiAddEvents[mx_Midi,e:MidiPatternFile]"
MidiAddNotes::usage = "MidiAddNotes[m_Midi,n : {{{{_,_},{_,_,_}}...}...}]"
MidiAddQPM::usage = "MidiAddQPM[m_Midi, q : {{_, _} ...}]"
MidiChord::usage = ""
MidiControlChange::usage = ""
MidiDataAnyValue::usage = ""
MidiDataAnyValueQ::usage = ""
MidiDataNoValue::usage = ""
MidiDataNoValueQ::usage = ""
MidiDelta::usage = "MidiTiming can be either MidiAbsolute or MidiDelta. If MidiTiming is MidiDelta, all timing information are delta values."
MidiEmpty::usage = ""
MidiEOT::usage = ""
MidiExpandStates::usage = "MidiExpandStates[s_]"
MidiExpandStatePaths::usage = "MidiExpandStatePaths[p_]"
MidiExportSMF::usage = "MidiExportSMF[fn_String,m_Midi,opts___]"
MidiFile::usage = ""
MidiFileFormat::usage = ""
MidiGetChannels::usage = "MidiGetChannels[m_Midi]"
MidiGetDuration::usage = "MidiGetDuration[m_Midi]"
MidiGetDurations::usage = "MidiGetDurations[m_Midi]"
MidiGetInfo::usage = ""
MidiGetNotes::usage = "MidiGetNotes[mx_Midi]"
MidiGetQPM::usage = "MidiGetQPM[m_Midi]"
MidiGetSecToTickFunction::usage = "MidiGetSecToTickFunction[m_Midi]"
MidiGetShape::usage = "MidiGetShape[m_Midi]"
MidiGetState::usage = "MidiGetState[m_Midi]"
MidiGetTickToSecFunction::usage = "MidiGetTickToSecFunction[m_Midi]"
MidiGetTimeUnit::usage = "MidiGetTimeUnit[m_Midi]"
MidiGetTiming::usage = "MidiGetTiming[m_Midi]"
MidiGetTPQ::usage = "MidiGetTPQ[m_Midi]"
MidiImportSMF::usage = "MidiImportSMF[fn_String,opts___]"
MidiKeySignature::usage = ""
MidiMeta::usage = ""
MidiMilliSec::usage = "MidiMilliSec"
MidiNormalizeNoteOff::usage = "MidiNormalizeNoteOff[m_Midi, v2z_:False]"
MidiNoteOff::usage = ""
MidiNoteOn::usage = ""
MidiPar::usage = "MidiPar[m:{_Midi,_Midi...}, opts___]"
MidiPatternData::usage = ""
MidiPatternChord::usage = ""
MidiPatternFile::usage = ""
MidiPatternInfo::usage = ""
MidiPatternMidi::usage = ""
MidiPatternMusic::usage = ""
MidiPatternTiming::usage = ""
MidiPatternTrack::usage = ""
MidiPatternType::usage = ""
MidiPatternVoice::usage = ""
MidiRemEvents::usage = "MidiRemEvents[mx_Midi,p_]"
MidiRemNotes::usage = "MidiRemNotes[m_Midi]"
MidiRemQPM::usage = "MidiRemQPM[m_Midi]"
MidiSec::usage = ""
MidiSeq::usage = "MidiSeq[m:{_Midi,_Midi...}, opts___]"
MidiSetNotes::usage = "MidiSetNotes[m_Midi,n : {{{{_,_},{_,_,_}}...}...}]"
MidiSetQPM::usage = "MidiSetQPM[m_Midi, q : {{_, _} ...}]"
MidiSetState::usage = "MidiSetState[m_Midi, s_, opts___]"
MidiSetStateLow::usage = "MidiSetState[m_Midi, s_, opts___]"
MidiShape::usage = ""
MidiStates::usage = ""
MidiStatePaths::usage = ""
MidiStateRoutes::usage = ""
MidiStatesExpanded::usage = ""
MidiStatePathsExpanded::usage = ""
MidiSysX0::usage = ""
MidiSysX7::usage = ""
MidiTempo::usage = ""
MidiTick::usage = ""
MidiTie::usage = ""
MidiTieQ::usage = ""
MidiTimeSignature::usage = ""
MidiTimeUnit::usage = ""
MidiTiming::usage = ""
MidiTPQ::usage = ""
MidiUnTie::usage = ""
MidiVoice::usage = ""
MidiVoiceReleaseTimeFunction::usage = "default is Function[{track,note:{{on, off}, {ch, p, v}}},0]"
MidiQPM::usage = ""

Begin["`Private`"]

EOT = {MidiEOT,{}};

Format[m_Midi] :=
  If[MidiGetShape[m]===MidiFile,
    StringForm["Midi[`1`,{{{timing, {type, data}}...}...}]",m[[1]]],
    If[MidiGetShape[m]===MidiVoice,
      StringForm["Midi[`1`,{{{type, track},{{timing, data}...}}...}]",m[[1]]],
      If[MidiGetShape[m]===MidiChord,
        StringForm["Midi[`1`,{{{type, {track...}},{{timing, {data...}}...}}...}]",m[[1]]],
        StringForm["Midi[`1`,<unknown shape>]",m[[1]]]
        ]
      ]
    ]

Options[Midi]=
  {
    MidiQPM -> 120,
    MidiFileFormat -> 1,
    MidiTPQ -> 960
    }

MidiAddEOT[mx_Midi,keep_:False] := (* this function also adds EOT if missing. if the keep parameter is true the max-duration is kept *)
  Module[{d,m=MidiSetState[mx,{MidiShape->MidiFile,MidiTiming->MidiAbsolute}]},
    If[keep,d=MidiGetDuration[m]];
    m[[2]] = Cases[Sort[#],e$_/;!MatchQ[e$,{_,EOT}]->e$]& /@ m[[2]];
    If[!keep,d=MidiGetDuration[m]];
    MidiSetState[
      Midi[
        Sort[m[[1]]],
        Append[#,{d,EOT}]& /@ m[[2]]
        ],
      MidiGetState[mx]
      ]
    ]

MidiAddEvents[mx_Midi,e_]:=
  Module[{m=MidiSetState[mx,{MidiShape->MidiFile,MidiTiming->MidiAbsolute}],t,te,tm},
    t=Max[te=Length[e],tm=Length[m[[2]]]];
    MidiSetState[
      Midi[
        m[[1]],
        MapThread[Join,{Join[m[[2]],Table[{},{t-tm}]],Join[e,Table[{},{t-te}]]}]
        ],
      MidiGetState[mx]
      ]
    ]

MidiAddNotes[m_Midi,n:{{{{_,_},{_,_,_}}...}...}]:=
  MidiAddEvents[m,
    Flatten[
      {
        {#[[1,1]],{MidiNoteOn,#[[2]]}},
        {#[[1,2]],{MidiNoteOff,ReplacePart[#[[2]],0,{3}]}}
        }&
        /@#,1]&/@n
    ]

MidiAddQPM[m_Midi, q : {{_, _} ...}] :=
  MidiAddEvents[m, {{
    #[[1]],
    Round[
      {MidiTempo, {
        Mod[IntegerPart[#/65536], 256],
        Mod[IntegerPart[#/256], 256],
        Mod[#, 256]
        } &[60000000/#[[2]]]}
      ]
    } & /@ q}]

MidiControlChange = 3;

(*MidiDataAnyValue = 128;*)

MidiDataAnyValueQ[expr_] := If[AtomQ[expr],expr===MidiDataAnyValue||expr===MidiTie[MidiDataAnyValue],Or@@(MidiDataAnyValueQ/@expr)]

(*MidiDataNoValue = 129;*)

MidiDataNoValueQ[expr_] := If[AtomQ[expr],expr===MidiDataNoValue||expr===MidiTie[MidiDataNoValue],Or@@(MidiDataNoValueQ/@expr)]

MidiEmpty = Midi[{MidiShape->MidiFile,MidiTiming->MidiAbsolute,MidiTimeUnit->MidiTick},{{}}]

MidiEOT = {MidiMeta,16^^2F};

MidiExpandStates[s_] :=
  Module[{r},
    r = Flatten[Array[{##} &, Length[#[[2]]] & /@ s], Length[s] - 1];
    r = Sort[MapIndexed[(s[[#2[[1]], 1]] -> s[[#2[[1]], 2, #1]]) &, #]] & /@ r;
    Sort[r]
    ]

(* fill path *)
FP[f_,t_] := (* return t and all in f not in t *)
  Module[{r=t},
    Scan[If[!MemberQ[t,#[[1]]->_],AppendTo[r,#];]&,f];
    Sort[r]
    ]

(* fill state *)
FS[s_] :=
  Module[{miss},
    (* wich state-parts is missing in s *)
    miss=Complement[#[[1]]&/@MidiStates,#[[1]]&/@s];
    (* fill in missing state-parts with a _ *)
    Sort[Join[s,(#->_)&/@miss]]
    ]

MidiExpandStatePaths[p_] :=
  Module[{r = p, from, to},
    r = (
      from = #[[1]];
      to = #[[2]];
      (* fill in missing state - parts with a _ *)
      from = FS[from];
      (* make "from" a list of from's *)
      from = Cases[MidiStatesExpanded, from];
      (* make "to" a list of to's *)
      to = FP[#, to] & /@ from;
      (* make a list of {from, to} *)
      Transpose[{from, to}]
      ) & /@ r;
    Sort[Flatten[r, 1]]
    ]

MidiExportSMF[fn_String,mx_Midi, opts___] :=
  Module[{m=mx,f=Null},
    m = MidiSetState[m,{MidiShape->MidiFile,MidiTiming->MidiDelta,MidiTimeUnit->MidiTick}];
    m = MidiAddEOT[m];(* an option for this? especially since it sorts the events! *)
    f = OpenWriteBinary[fn];
    Catch[
      WriteString[f,"MThd"];
      WriteInt[f,4,6];
      WriteInt[f,2,MidiFileFormat /. m[[1]] /. Options[Midi]];
      WriteInt[f,2,Length[m[[2]]]];
      WriteInt[f,2,MidiTPQ /. m[[1]] /. Options[Midi]];
      WriteTrack[f,#]&/@(m[[2]]);
      Flush[f];
      Close[f];
      ,
      _,
      (Print["failure ",#1];Close[f];#1)&
      ];
    m
    ]

MidiGetChannels[m_Midi] :=
  Union[
    Cases[#,{_,{MidiNoteOn,{c$_,_,_}}}->c$]
    ]& /@ m[[2]] /; MidiGetShape[m]===MidiFile

MidiGetDuration[m_Midi] := Max[MidiGetDurations[m]]

MidiGetDurations[m_Midi] := (Max[#[[1]]& /@ #]& /@ MidiSetState[m,{MidiShape->MidiFile,MidiTiming->MidiAbsolute}][[2]])

MidiGetInfo[m_Midi] := m[[1]]

MidiGetNotes[mx_Midi] :=
  Module[{m = MidiNormalizeNoteOff[MidiSetState[mx, {MidiShape -> MidiFile, MidiTiming -> MidiAbsolute}]], on, off},
    Function[t,
      (* get note-on as {{ch, p, tick, v} ...} *)
      on = Cases[t, {tick$_, {MidiNoteOn, {c$_, p$_, v$_}}} -> {c$, p$, tick$, v$}];
      (* get note-off as {{ch, p, tick, v} ...} *)
      off = Cases[t, {tick$_, {MidiNoteOff, {c$_, p$_, v$_}}} -> {c$, p$, tick$, v$}];
      (* set to {{{on, off}, {ch, p, v}} ...} *)
      MapThread[{{#1[[3]], #2[[3]]}, {#1[[1]], #1[[2]], #1[[4]]}} &, {on, off}]
      ] /@ m[[2]]
    ]

MidiGetQPMLow[m_Midi] :=
  Module[{ma = MidiSetState[m, {MidiTiming->MidiAbsolute}],u},
    (* get all tempo events as {{timing,data}...} *)
    u = Cases[ma[[2, 1]], {t$_, {MidiTempo, u$_}} -> {t$, u$}];
    u
    ] /; MidiGetShape[m] === MidiFile

MidiGetQPMLow[m_Midi] :=
  Module[{ma = MidiSetState[m, {MidiTiming->MidiAbsolute}],u},
    (* get the tempo events as {{{end-timing,data}...}...} *)
    u = Cases[ma[[2]], {{MidiTempo,1},d$_} -> d$];
    (* set to {{end-timing,data}...} *)
    u = Flatten[u,1];
    If[0<Length[u],
      (* convert to start timing *)
      u = Transpose[u]; u[[1]] = Prepend[Drop[u[[1]],-1],0]; u = Transpose[u];
      ];
    u
    ] /; MidiGetShape[m] === MidiVoice

MidiGetQPMLow[m_Midi] :=
  Module[{ma = MidiSetState[m, {MidiTiming->MidiAbsolute}],u,p},
    (* get the tempo events as {{{track...},{end-timing,{data...}}...}...} *)
    u = Cases[ma[[2]], {{MidiTempo,t$_},d$_} -> {t$,d$}];
    (* set to {{track...},{end-timing,{data...}}...} *)
    u = Flatten[u,1];
    If[0<Length[u],
      (* localize track 1 *)
      p = Position[u[[1]],1];
      If[0<Length[p],
        p = p[[1,1]];
        (* set to {{end-timing,data}...} *)
        u = {#[[1]],#[[2,p]]}&/@u[[2]];
        (* convert to start timing *)
        u = Transpose[u]; u[[1]] = Prepend[Drop[u[[1]],-1],0]; u = Transpose[u],
        u = {}
        ];
      ];
    u
    ] /; MidiGetShape[m] === MidiChord

MidiGetQPM[m_Midi] :=
  Module[
    {
      u = MidiGetQPMLow[m],
      tpq = MidiTPQ /. m[[1]] /. Options[Midi]
      },
    (* convert to USPQ *)
    u = {#[[1]], Total[{65536, 256, 1}*#[[2]]]} & /@ u;
    (* convert to QPM *)
    u = {#[[1]], 60000000/#[[2]]} & /@ u;
    (* add a default tempo if needed *)
    If[Length[u] == 0 || u[[1, 1]] != 0, u = Prepend[u, {0, MidiQPM /. m[[1]] /. Options[Midi]}]];
    u
    ]

MidiGetSecToTickFunction[m_Midi] :=
  Module[
    {
      tpq = MidiGetTPQ[m],
      u = MidiGetQPM[m], k, sa, sd, ta, td, f
      },
    (* convert to TPM *)
    u = {#[[1]], tpq#[[2]]} & /@ u;
    (* convert to TPS *)
    u = {#[[1]], #[[2]]/60} & /@ u;
    (* transpose u *)
    u = Transpose[u];

    (* get k *)
    k = u[[2]];
    (* get sec *)
    sa = u[[1]];
    sd = ValuesToDeltas[sa];
    (* get tick *)
    td = Drop[k, -1]sd;
    ta = DeltasToValues[td];

    (* make a list of {s, k, t} *)
    f = Transpose[{sa, k, ta}];
    (* make a list of functions *)
    f = Function[sec, Evaluate[N[(sec - #[[1]])#[[2]] + #[[3]]]]] & /@ f;

    (* make the resulting function *)
    Function[t, Evaluate[Round[MakeNestedIfs[Transpose[{td, Drop[f, -1]}], f[[1]], f[[-1]]][t][t]]]]
    ] /; MidiGetTimeUnit[m] === MidiSec

MidiGetShape[m_Midi] := MidiShape /. m[[1]]

MidiGetState[s_List] := Module[{t = #[[1]] & /@ MidiStates}, Sort[Cases[s, Rule[p$_, _] /; MemberQ[t, p$]]]]

MidiGetState[m_Midi] := MidiGetState[MidiGetInfo[m]]

MidiGetTickToSecFunction[m_Midi] :=
  Module[
    {
      tpq = MidiGetTPQ[m],
      u = MidiGetQPM[m], k, sa, sd, ta, td, f
      },
    (* convert to TPM *)
    u = {#[[1]], tpq#[[2]]} & /@ u;
    (* convert to TPS *)
    u = {#[[1]], #[[2]]/60} & /@ u;
    (* convert to SPT *)
    u = {#[[1]], 1/#[[2]]} & /@ u;
    (* transpose u *)
    u = Transpose[u];

    (* get k *)
    k = u[[2]];
    (* get tick *)
    ta = u[[1]];
    td = ValuesToDeltas[ta];
    (* get sec *)
    sd = Drop[k, -1]td;
    sa = DeltasToValues[sd];

    (* make a list of {t, k, s} *)
    f = Transpose[{ta, k, sa}];
    (* make a list of functions *)
    f = Function[tick, Evaluate[N[(tick - #[[1]])#[[2]] + #[[3]]]]] & /@ f;

    (* make the resulting function *)
    Function[t, Evaluate[MakeNestedIfs[Transpose[{td, Drop[f, -1]}], f[[1]], f[[-1]]][t][t]]]
    ] /; MidiGetTimeUnit[m] === MidiTick

MidiGetTimeUnit[m_Midi] := MidiTimeUnit /. m[[1]]

MidiGetTiming[m_Midi] := MidiTiming /. m[[1]]

MidiGetTPQ[m_Midi] := MidiTPQ /. m[[1]] /. Options[Midi]

MidiImportSMF[fn_String,opts___]:=
  Module[
    {
      f=Null,
      h={MidiShape->MidiFile, MidiTiming->MidiDelta, MidiTimeUnit->MidiTick},
      t={},
      n
      },
    f=OpenReadBinary[fn];(*,DOSTextFormat->False];*)
    Catch[
      If[StringJoin@@ReadList[f,Character,4]!="MThd",error];
      If[ReadInt[f,4]!=6,error];
      AppendTo[h,MidiFileFormat->ReadInt[f,2]];
      n=ReadInt[f,2];
      AppendTo[h,MidiTPQ->ReadInt[f,2]];
      Do[AppendTo[t,ReadTrack[f]],{n}];
      Close[f];
      (*
      If[Verbose/.{opts}/.{Verbose->False},
        Print["FileFormat: ",MidiFileFormat/.h];
        Print["TPQ: ",MidiTPQ/.h];
        Print["Tracks: ",Length[t]];
        ];
      *)
      Midi[Sort[h],t],
      _,
      (Print["failure ",#1];Close[f];#1)&
      ]
    ]

MidiKeySignature = {MidiMeta,16^^59};

MidiMeta = 16^^FF;

MidiNormalizeNoteOff[m_Midi,v2z_:False]:=
  Midi[
    Sort[m[[1]]],
    (
      If[MatchQ[#,{_,{MidiNoteOn,{_,_,0}}}],
        ReplacePart[#,MidiNoteOff,{2,1}],
        If[v2z && MatchQ[#,{_,{MidiNoteOff,{_,_,_}}}],
          ReplacePart[#,0,{2,2,3}],
          #
          ]
        ]& /@ #
      )& /@ m[[2]]
    ] /; MidiGetShape[m]===MidiFile

MidiNoteOff = 0;

MidiNoteOn = 1;

MidiPar[mx:{_Midi,_Midi...}, opts___] :=
  Module[{m=MidiSetState[#,{MidiSetShape->MidiFile,MidiTiming->MidiAbsolute,MidiTimeUnit->MidiSec}]&/@mx,d},
    d = Flatten[Join[{m[[1,2]]},Drop[#[[2]],1]&/@Drop[m,1]],1];
    MidiSetState[
      MidiAddEOT[Midi[m[[1,1]],d]],
      MidiGetState[mx[[1]]]
      ]
    ]

MidiPatternData = ({MidiPatternDataAtom...}|MidiPatternDataAtom|_MidiTie);
MidiPatternDataAtom = (_Integer|MidiDataNoValue|MidiDataAnyValue|_MidiTie);
MidiPatternChord = {{{MidiPatternType,{MidiPatternTrack...}},{{MidiPatternTiming,{MidiPatternData...}}...}}...};
MidiPatternFile = {{{MidiPatternTiming,{MidiPatternType,MidiPatternData}}...}...};
MidiPatternInfo = {(
  (MidiQPM->(_Integer|_Real)) |
  (MidiFileFormat->(0|1)) |
  (MidiShape->(MidiFile|MidiVoice|MidiChord)) |
  (MidiTiming->(MidiDelta|MidiAbsolute)) |
  (MidiTimeUnit->(MidiTick|MidiSec|MidiMilliSec)) |
  (MidiTPQ->_Integer)
  )...};
MidiPatternMidi = Midi[MidiPatternInfo,MidiPatternMusic];
MidiPatternMusic = (MidiPatternFile|MidiPatternVoice|MidiPatternChord);
MidiPatternTiming = (_Integer|_Real);
MidiPatternTrack = (_Integer|{_Integer,_Integer,_Integer});
MidiPatternType = (MidiNoteOff|MidiNoteOn|2|MidiControlChange|4|5|6|7|MidiSysX0|MidiSysX7|{MidiMeta,_Integer});
MidiPatternVoice = {{{MidiPatternType,MidiPatternTrack},{{MidiPatternTiming,MidiPatternData}...}}...};

MidiRemEvents[mx_Midi,p_] :=
  Module[{m=MidiSetState[mx,{MidiShape->MidiFile,MidiTiming->MidiAbsolute}]},
    MidiSetState[
      Midi[
        m[[1]],
        Cases[#,e$_/;!MatchQ[e$,p]]&/@m[[2]]
        ],
      MidiGetState[mx]
      ]
    ]

MidiRemNotes[m_Midi] := MidiRemEvents[m,{_, {(MidiNoteOn | MidiNoteOff), _}}]

MidiRemQPM[m_Midi] := MidiRemEvents[m,{_, {MidiTempo, _}}]

MidiSeq[mx:{_Midi,_Midi...}, opts___] :=
  Module[{m=MidiSetState[#,{MidiSetShape->MidiFile,MidiTiming->MidiAbsolute,MidiTimeUnit->MidiSec}]&/@mx,d,s=0,t,q,x},
    x=Max @@ (Length[#[[2]]]&/@m);
    d=Reap[
      Scan[(* per midi *)
        (
          q=MidiGetDuration[#];
          t=1;
          Scan[( (* per track *)
            Scan[ (* per event *)
              Sow[{#[[1]]+s,#[[2]]},t]&,
              #
              ];
            t++;
            )&,
            #[[2]]
            ];
          s+=q;
          )&,
        m
        ],
      Range[x]
      ][[2]];
    d=If[#==={},#,#[[1]]]&/@d;
    MidiSetState[
      MidiAddEOT[Midi[m[[1,1]],d]],
      MidiGetState[mx[[1]]]
      ]
    ]

MidiSetNotes[m_Midi, e : {{{{_,_},{_,_,_}}...}...}]:=
  MidiSetState[MidiAddNotes[MidiRemNotes[MidiSetState[m,{MidiShape->MidiFile,MidiTiming->MidiAbsolute}]],e],MidiGetState[m]]

MidiSetQPM[m_Midi, e : {{_, _} ...}] :=
  MidiSetState[MidiAddQPM[MidiRemQPM[MidiSetState[m,{MidiShape->MidiFile,MidiTiming->MidiAbsolute}]],e],MidiGetState[m]]

MidiSetState[m_Midi, s_, opts___] :=
  Module[
    {
      pr = Verbose /. {opts} /. {Verbose->False},
      f,
      t = FP[MidiGetState[m], MidiGetState[s]],
      p,
      r = m
      },
    f = Position[MidiStatesExpanded, MidiGetState[m]][[1, 1]];
    t = Position[MidiStatesExpanded, t][[1, 1]];
    p = Drop[MidiStateRoutes[[f, t]], 1];
    If[pr,Print[Prepend[p,f]]];
    p = MidiStatesExpanded[[#]] & /@ p;
    If[pr,Print[ColumnForm[p]]];
    Scan[(r = MidiSetStateLow[r, #, opts]) &, p];
    r
    ]

MidiSetStateLow[m_Midi,s_, opts___]:=
  Module[
    {
      am = MidiNormalizeNoteOff[m],
      tr, trm, tron, troff, trn,
      (* get a list of durations, one for each track *)
      d = MidiGetDurations[m],
      rtf = MidiVoiceReleaseTimeFunction /. {opts} /. {MidiVoiceReleaseTimeFunction -> (0&)}
      },
    (* get meta and sysex as {{{{type, data}, tick}...}...} *)
    trm = Sort[Cases[#, {t$_, {h$_, d$_}} /; MatchQ[h$,MidiSysX0|MidiSysX7|{MidiMeta,_}] -> {{h$,d$}, t$}]] & /@ am[[2]];
    (* set to {{{{{type1, data},tick}...}, {{{type2, data}, tick}...}...}...} *)
    trm = Split[#, (#1[[1,1]] === #2[[1,1]]) &] & /@ trm;
    (* set to {{{{type, track}, {{duration...}, {data...}}}...}...} *)
    trm = MapIndexed[
      (
        Function[tt,
          Module[{r = tt, ty = tt[[1, 1, 1]]},
            (* r is {{{type, data}, tick}...}, sort on tick *)
            r = Sort[r, OrderedQ[{#1[[2]], #2[[2]]}] &];
            (* prepend a MidiDataNoValue at 0 if tick 0 is absent *)
            If[r[[1, 2]] != 0, r = Prepend[r, {{ty, MidiDataNoValue}, 0}]];
            (* set to {{{type, data}...}, {tick...}} *)
            r = Transpose[r];
            (* set to {{data...}, {tick...}} *)
            r[[1]] = #[[2]] & /@ r[[1]];
            (* set to {{data...}, {duration...}} *)
            r[[2]] = ValuesToDeltas[Append[r[[2]], d[[#2[[1]]]]]];
            (* set to {{type, track}, {{duration...}, {data...}}} *)
            {{ty, #2[[1]]}, {r[[2]], r[[1]]}}
            ]
          ] /@ #1
        ) &, trm];
    (* set to {{{type, track}, {{duration...}, {data...}}}...} *)
    trm = Sort[Flatten[trm, 1]];

    (* get note - on as {{{track, ch, p, tick, v}...}...} *)
    tron = MapIndexed[Cases[#1, {t$_, {MidiNoteOn, {c$_, p$_, v$_}}} -> {#2[[1]], c$, p$, t$, v$}] &, am[[2]]];
    (* set to {{track, ch, p, tick, v}...} *)
    tron = Sort[Flatten[tron, 1]];
    (* get note - off as {{{track, ch, p, tick, v}...}...} *)
    troff = MapIndexed[Cases[#1, {t$_, {MidiNoteOff, {c$_, p$_, v$_}}} -> {#2[[1]], c$, p$, t$, v$}] &,am[[2]]];
    (* set to {{track, ch, p, tick, v}...} *)
    troff = Sort[Flatten[troff, 1]];
    (* set to {{{track, ch}, {p, v}, {on, off}}...} *)
    trn = MapThread[{{#1[[1]], #1[[2]]}, {#1[[3]],#1[[5]]}, {#1[[4]], #2[[4]]}} &, {tron, troff}];
    (* group by {track,ch} to {{{{track, ch1},{p, v}, {on, off}}...}, {{{track, ch2},{p, v}, {on, off}}...}...} *)
    trn = Split[trn, (#1[[1]] === #2[[1]]) &];
    (* then reverse each event to get {{{{on, off}, {p, v}, {track, ch}}...}...} *)
    trn = (Reverse /@ #) & /@ trn;
    (* make monophonic voices, set to {{{{{on, off}, {p, v}, {track, ch, voice}}...}...}...} *)
    trn =
      Function[trni, (* trni is {{{on, off}, {p, v}, {track, ch}}...} *)
        Reap[
          Module[{tim={{0,1}}},(* tim is {{off,voice}...} *)
            Scan[
              Module[{pos,rt,v}, (* # is {{on, off}, {p, v}, {track, ch}} *)
                pos=Cases[tim,{e$_,_}/;(e$<=#[[1,1]])];
                If[0<Length[pos],
                  v=Sort[pos][[-1,2]],
                  AppendTo[tim,{0,v=(Length[tim]+1)}]
                  ];
                (* add a rest ? *)
                If[tim[[v,1]]!=#[[1,1]],
                  (* add a tuple, not just an atom, so that Transpose works when trying to get pitch *)
                  Sow[{{tim[[v,1]],#[[1,1]]},{MidiDataNoValue,MidiDataNoValue},Append[#[[3]],v]},v]
                  ];
                (* calculate release-time *)
                rt = rtf[#[[3,1]],{#[[1]],Prepend[#[[2]],#[[3,2]]]}];
                (* update tim *)
                tim[[v]]={#[[1,2]]+rt,v};
                (* add the note *)
                Sow[ReplacePart[#,Append[#[[3]],v],{3}],v];
                ]&,
              Sort[trni]
              ];
            ];
          ][[2]]
        ] /@ trn;
    (* set to {{{{on, off}, {p, v}, {track, ch, voice}}...}...} *)
    trn = Flatten[trn, 1];
    (* set to {{{MidiNoteOn, {track, ch, voice}}, {{{p, +/-v}, tick}...}}...} *)
    trn = {{MidiNoteOn,#[[1, 3]]}, Flatten[{{{#[[2, 1]], #[[2, 2]]}, #[[1, 1]]}, {{#[[2, 1]], -#[[2, 2]]}, #[[1, 2]]}} & /@ #, 1]} & /@ trn;
    (* sort *)
    (*trn = {#[[1]], Sort[#[[2]], OrderedQ[{#1[[2]], #2[[2]]}] &]} & /@ trn;*)
    (* add a preceding rest if necesary *)
    trn = {#[[1]], If[#[[2, 1, 2]] == 0, #[[2]], Prepend[#[[2]], {{0, 0}, 0}]]} & /@ trn;
    (* transpose to {{type, {{{p, v}...}, {tick...}}}...} *)
    trn = {#[[1]], Transpose[#[[2]]]} & /@ trn;
    (* switch to durations and chop off the last rest {{type, {{duration...}, {data...}}}...} *)
    trn = {#[[1]], {ValuesToDeltas[#[[2, 2]]], Drop[#[[2, 1]], -1]}} & /@ trn;

    (* merge trm and trn to be one big {{{type, track}, {{duration...}, {data...}}}...} *)
    tr = Join[trm, trn];

    (* transpose to {{{type, track}, {{duration, data}...}}...} *)
    tr = {#[[1]], Transpose[#[[2]]]} & /@ tr;
    (* get all with duration != 0 *)
    tr = {#[[1]], Select[#[[2]], (#[[1]] != 0) &]} & /@ tr;
    (* get all nonempty voices *)
    tr = Select[tr, (#[[2]] != {}) &];

    Midi[
      Sort[m[[1]] /. (MidiShape -> _) -> (MidiShape -> MidiVoice) /. (MidiTiming -> _) -> (MidiTiming -> MidiDelta)],
      Sort[tr]
      ]
    ] /; MatchQ[MidiGetState[m],FS[{MidiShape->MidiFile,MidiTiming->MidiAbsolute}]]&&
         Complement[s,MidiGetState[m]]=={MidiShape->MidiVoice,MidiTiming->MidiDelta}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Module[{trm, trn, trx, tr},
    (* get all meta and sysx as {{{type, track}, {{end-tick, data}...}}...} *)
    trm = Select[m[[2]], MatchQ[#[[1, 1]], MidiSysX0 | MidiSysX7 | {MidiMeta,_}] &];
    (* set to {{{type, track}, {{tick, data}...}}...} *)
    trm = {#[[1]], Module[{td=Transpose[#[[2]]]},Transpose[{Prepend[td[[1]],0],Append[td[[2]],MidiDataNoValue]}]]} & /@ trm;
    (* fix EOT *)
    trm = If[#[[1, 1]] == MidiEOT, ReplacePart[#, {}, {2, -1, 2}], #] & /@ trm;
    (* remove all MidiDataNoValue *)
    trm = {#[[1]], Select[#[[2]], ! MidiDataNoValueQ[#[[2]]] &]} & /@ trm;
    (* set to {{track, {{tick, {type, data}}...}}...} *)
    trm = {#[[1, 2]], Function[e, {e[[1]], {#[[1, 1]], e[[2]]}}] /@ #[[2]]} & /@ trm;

    (* get all notes as {{{type, track}, {{end-tick, data}...}}...} *)
    trn = Select[m[[2]], MatchQ[#[[1, 1]], MidiNoteOn] &];
    (* set to {{{track,c}, {{tick, end-tick, data}...}}...} and ignore the voice info *)
    trn = {{#[[1, 2, 1]],#[[1,2,2]]}, Module[{td=Transpose[#[[2]]]},Transpose[{Prepend[Drop[td[[1]],-1],0],td[[1]],td[[2]]}]]} & /@ trn;
    (* set to {{track, {{tick, {type, data}}...}}...} *)
    trn = {
      #[[1,1]],
      Flatten[
        Cases[
          #[[2]],
          {t$_, e$_, d$:{p$_,v$_}} /; (!MidiDataAnyValueQ[d$] && !MidiDataNoValueQ[d$]) -> {
              {t$, {MidiNoteOn, {#[[1,2]],p$,v$}}},
              {e$, {MidiNoteOff, {#[[1,2]],p$,0}}}
              }
          ],
        1
        ]
      } & /@ trn;

    (* collect all data into one *)
    trx = Join[trm, trn];
    (* sort on track - nr *)
    trx = Sort[trx];

    (* put every event into the right track *)
    tr=Reap[Scan[Function[t,Scan[Sow[#,t[[1]]]&, t[[2]]]],trx];][[2]];

    (* sort each track *)
    tr = Sort /@ tr;

    Midi[
      Sort[m[[1]] /. (MidiShape -> _) -> (MidiShape -> MidiFile)],
      tr
      ]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->MidiVoice,MidiTiming->MidiAbsolute}]]&&
         Complement[s,MidiGetState[m]]=={MidiShape->MidiFile}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Module[{g},
    (* group each type together, set g to {{{{type,track},{{timing,data}...}}...}...} *)
    g = Split[Sort[m[[2]]],(#1[[1,1]]===#2[[1,1]])&];
    (* set to {{{type,{track...}},{{timing,{data...}}...}}...} *)
    g = Function[{gt},
      Module[{n = gt,nd, t, tt},
        (* n is {{{type,track},{{timing,data}...}}...} *)

        (* get the total duration *)
        tt = Max[Total[Transpose[#[[2]]][[1]]]& /@ n];

        (* adjust any voice to make equal lengths? *)
        n = {
          (* # is {{type,track},{{timing,data}...}} *)
          #[[1]],
          Module[{vtt = Total[Transpose[#[[2]]][[1]]]},
            If[vtt != tt,
              If[n[[1,1,1]]===MidiNoteOn,
                Append[#[[2]],{tt - vtt,{MidiDataNoValue,MidiDataNoValue}}],
                ReplacePart[#[[2]],#[[2,-1,1]]+ tt - vtt,{-1,1}]
                ],
              #[[2]]
              ]
            ]
          } & /@ n;

        (* get all durations as {{timing...}...} *)
        t = Transpose[#[[2]]][[1]]& /@ n;
        (* set to {timing...} *)
        t = ValuesToDeltas[Union[Flatten[DeltasToValues /@ t]]];

        (* divide all data to the proper duration to be {{data...}...} *)
        nd = Module[{vt, j = 1, k},(* for each voice as {{type,track},{{timing,data}...}} *)
          Flatten[
            ( (* for each note as {timing,data} *)
              k = j;
              While[Total[Take[t, {j, k}]] < #[[1]], k++];
              vt = Take[t, {j, k}];
              j = k + 1;
              Prepend[
                Table[
                  MidiTie[#[[2]]],
                  {k, 2, Length[vt]}
                  ],
                #[[2]]
                ]
              ) & /@ #[[2]],
            1]
          ] & /@ n;

        (* return {{type,{track...}},{{timing,{data...}}...}} *)
        {{n[[1,1,1]],#[[1,2]]& /@ n}, Transpose[{t, Transpose[nd]}]}
        ]
      ] /@ g;

    Midi[
      Sort[m[[1]] /. (MidiShape -> _) -> (MidiShape -> MidiChord)],
      g
      ]
    ] /; MatchQ[MidiGetState[m],FS[{MidiShape->MidiVoice,MidiTimeUnit->MidiTick,MidiTiming->MidiDelta}]]&&(* DONT ALLOW MidiSec HERE!!! *)
         Complement[s,MidiGetState[m]]=={MidiShape->MidiChord}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Module[{g=m[[2]]},
    (* g is {{{type,{track...}},{{timing,{data...}}...}}...}, remember? *)
    (* set to {{{{type,track}...},{{{timing,data}...}...}}...} *)
    g = {Function[track,{#[[1,1]],track}]/@#[[1,2]],Transpose[Function[td,Function[data,{td[[1]],data}]/@td[[2]]]/@#[[2]]]}& /@ g;
    (* set to {{{{type,track},{{timing,data}...}}...}...} *)
    g = Transpose /@ g;
    (* set to {{{type,track},{{timing,data}...}}...} *)
    g = Flatten[g,1];
    (* remove all ties *)
    g = {
      #[[1]],
      Module[{p=Null},
        Reap[
          Scan[
            Function[td,
              If[MidiTieQ[td[[2]]],
                If[p===Null,
                  Print["Odd, a tie at the beginning of a voice? There is a bug here somewhere!"],
                  p={p[[1]]+td[[1]],p[[2]]}
                  ],
                If[p===Null,
                  p=td,
                  Sow[p];(* use Reap/Sow *)
                  p=td
                  ]
                ]
              ],
            #[[2]]
            ];
          Sow[p](* use Reap/Sow *)
          ][[2,1]]
        ]
      }& /@ g;

    Midi[
      Sort[m[[1]] /. (MidiShape -> _) -> (MidiShape -> MidiVoice)],
      g
      ]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->MidiChord, MidiTiming->MidiDelta}]]&&
         Complement[s,MidiGetState[m]]=={MidiShape->MidiVoice}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Module[{f=MidiGetSecToTickFunction[m]},
    Midi[
      Sort[m[[1]]/.(MidiTimeUnit->_)->(MidiTimeUnit->MidiTick)],
      ({f[#[[1]]],#[[2]]}& /@ #)&/@m[[2]]
      ]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->MidiFile,MidiTimeUnit->MidiSec, MidiTiming->MidiAbsolute}]]&&
         Complement[s,MidiGetState[m]]=={MidiTimeUnit->MidiTick}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Module[{f=MidiGetSecToTickFunction[m]},
    Midi[
      Sort[m[[1]]/.(MidiTimeUnit->_)->(MidiTimeUnit->MidiTick)],
      {#[[1]],{f[#[[1]]],#[[2]]}& /@ #[[2]]}&/@m[[2]]
      ]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->(MidiVoice|MidiChord), MidiTimeUnit->MidiSec,MidiTiming->MidiAbsolute}]]&&
         Complement[s,MidiGetState[m]]=={MidiTimeUnit->MidiTick}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Module[{f=MidiGetTickToSecFunction[m]},
    Midi[
      Sort[m[[1]]/.(MidiTimeUnit->_)->(MidiTimeUnit->MidiSec)],
      ({f[#[[1]]],#[[2]]}& /@ #)& /@ m[[2]]
      ]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->MidiFile,MidiTimeUnit->MidiTick, MidiTiming->MidiAbsolute}]]&&
         Complement[s,MidiGetState[m]]=={MidiTimeUnit->MidiSec}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Module[{f=MidiGetTickToSecFunction[m]},
    Midi[
      Sort[m[[1]]/.(MidiTimeUnit->_)->(MidiTimeUnit->MidiSec)],
      {#[[1]],{f[#[[1]]],#[[2]]}& /@ #[[2]]}&/@m[[2]]
      ]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->(MidiVoice|MidiChord), MidiTimeUnit->MidiTick,MidiTiming->MidiAbsolute}]]&&
         Complement[s,MidiGetState[m]]=={MidiTimeUnit->MidiSec}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Midi[
    Sort[m[[1]]/.(MidiTimeUnit->_)->(MidiTimeUnit->MidiMilliSec)],
    ({Round[1000 * #[[1]]],#[[2]]}& /@ #)& /@ m[[2]]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->MidiFile,MidiTimeUnit->MidiSec}]]&&
         Complement[s,MidiGetState[m]]=={MidiTimeUnit->MidiMilliSec}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Midi[
    Sort[m[[1]]/.(MidiTimeUnit->_)->(MidiTimeUnit->MidiMilliSec)],
    {#[[1]],{Round[1000 * #[[1]]],#[[2]]}& /@ #[[2]]}&/@m[[2]]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->(MidiVoice|MidiChord), MidiTimeUnit->MidiSec}]]&&
         Complement[s,MidiGetState[m]]=={MidiTimeUnit->MidiMilliSec}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Midi[
    Sort[m[[1]]/.(MidiTimeUnit->_)->(MidiTimeUnit->MidiSec)],
    ({#[[1]]/1000,#[[2]]}& /@ #)& /@ m[[2]]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->MidiFile,MidiTimeUnit->MidiMilliSec}]]&&
         Complement[s,MidiGetState[m]]=={MidiTimeUnit->MidiSec}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Midi[
    Sort[m[[1]]/.(MidiTimeUnit->_)->(MidiTimeUnit->MidiSec)],
    {#[[1]],{#[[1]]/1000,#[[2]]}& /@ #[[2]]}&/@m[[2]]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->(MidiVoice|MidiChord), MidiTimeUnit->MidiMilliSec}]]&&
         Complement[s,MidiGetState[m]]=={MidiTimeUnit->MidiSec}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Midi[
    Sort[m[[1]]/.(MidiTiming->_)->(MidiTiming->MidiAbsolute)],
    If[0<Length[#],Module[{td=Transpose[#]},td[[1]]=Drop[DeltasToValues[td[[1]]],1];Transpose[td]],#]& /@ m[[2]]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->MidiFile,MidiTiming->MidiDelta}]]&&
         Complement[s,MidiGetState[m]]=={MidiTiming->MidiAbsolute}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Midi[
    Sort[m[[1]]/.(MidiTiming->_)->(MidiTiming->MidiAbsolute)],
    {#[[1]],Module[{td=Transpose[#[[2]]]},Transpose[{Drop[DeltasToValues[td[[1]]],1],td[[2]]}]]}&/@m[[2]]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->(MidiVoice|MidiChord), MidiTiming->MidiDelta}]]&&
         Complement[s,MidiGetState[m]]=={MidiTiming->MidiAbsolute}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Midi[
    Sort[m[[1]]/.(MidiTiming->_)->(MidiTiming->MidiDelta)],
    If[0<Length[#],Module[{td=Transpose[Sort[#]]},td[[1]]=ValuesToDeltas[Prepend[td[[1]],0]];Transpose[td]],#]& /@ m[[2]]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->MidiFile,MidiTiming->MidiAbsolute}]]&&
         Complement[s,MidiGetState[m]]=={MidiTiming->MidiDelta}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Midi[
    Sort[m[[1]]/.(MidiTiming->_)->(MidiTiming->MidiDelta)],
    {#[[1]],Module[{td=Transpose[Sort[#[[2]]]]},Transpose[{ValuesToDeltas[Prepend[td[[1]],0]],td[[2]]}]]}&/@m[[2]]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->(MidiVoice|MidiChord), MidiTiming->MidiAbsolute}]]&&
         Complement[s,MidiGetState[m]]=={MidiTiming->MidiDelta}

(* if you change this, you must run CalcMidiStateRoutes, at least twice (bug?) *)
MidiStates = {
  {MidiShape, {MidiFile, MidiVoice, MidiChord}},
  {MidiTimeUnit, {MidiTick, MidiSec, MidiMilliSec}},
  {MidiTiming, {MidiDelta, MidiAbsolute}}
  };

MidiStatesExpanded = MidiExpandStates[MidiStates]

(* if you change this, you must run CalcMidiStateRoutes, at least twice (bug?) *)
MidiStatePaths = {
  (* Shape *)
  {{MidiShape -> MidiFile, MidiTiming -> MidiAbsolute}, {MidiShape -> MidiVoice, MidiTiming->MidiDelta}},
  {{MidiShape -> MidiVoice, MidiTiming -> MidiAbsolute}, {MidiShape -> MidiFile}},
  {{MidiShape -> MidiVoice, MidiTimeUnit -> MidiTick, MidiTiming -> MidiDelta}, {MidiShape -> MidiChord}},
  {{MidiShape -> MidiChord, MidiTiming -> MidiDelta}, {MidiShape -> MidiVoice}},
  (* TimeUnit *)
  {{MidiShape -> MidiFile, MidiTimeUnit -> MidiSec, MidiTiming -> MidiAbsolute}, {MidiTimeUnit -> MidiTick}},
  {{MidiShape -> (MidiVoice | MidiChord), MidiTimeUnit -> MidiSec, MidiTiming -> MidiAbsolute}, {MidiTimeUnit -> MidiTick}},
  {{MidiShape -> MidiFile, MidiTimeUnit -> MidiTick, MidiTiming -> MidiAbsolute}, {MidiTimeUnit -> MidiSec}},
  {{MidiShape -> (MidiVoice | MidiChord), MidiTimeUnit -> MidiTick, MidiTiming -> MidiAbsolute}, {MidiTimeUnit -> MidiSec}},
  {{MidiShape -> MidiFile, MidiTimeUnit -> MidiSec}, {MidiTimeUnit -> MidiMilliSec}},
  {{MidiShape -> (MidiVoice | MidiChord), MidiTimeUnit -> MidiSec}, {MidiTimeUnit -> MidiMilliSec}},
  {{MidiShape -> MidiFile, MidiTimeUnit -> MidiMilliSec}, {MidiTimeUnit -> MidiSec}},
  {{MidiShape -> (MidiVoice | MidiChord), MidiTimeUnit -> MidiMilliSec}, {MidiTimeUnit -> MidiSec}},
  (* Timing *)
  {{MidiShape -> MidiFile, MidiTiming -> MidiDelta}, {MidiTiming -> MidiAbsolute}},
  {{MidiShape -> MidiFile, MidiTiming -> MidiAbsolute}, {MidiTiming -> MidiDelta}},
  {{MidiShape -> (MidiVoice | MidiChord), MidiTiming -> MidiDelta}, {MidiTiming -> MidiAbsolute}},
  {{MidiShape -> (MidiVoice | MidiChord), MidiTiming -> MidiAbsolute}, {MidiTiming -> MidiDelta}}
  };

MidiStatePathsExpanded = MidiExpandStatePaths[MidiStatePaths]

(* this data is generated by CalcMidiStateRoutes and then manually pasted in here *)
MidiStateRoutes = {{{1}, {1, 2}, {1, 3}, {1, 2, 4}, {1, 3, 5}, {1, 3, 5, 6}, {1, 2, 14, 13,
      7}, {1, 2, 14, 13, 7, 8}, {1, 2, 14, 13, 15, 9}, {1, 2, 14, 13, 7, 8,
      10}, {1, 3, 5, 6, 18, 17, 11}, {1, 3, 5, 6, 18, 17, 11, 12}, {1, 2, 14,
      13}, {1, 2, 14}, {1, 2, 14, 13, 15}, {1, 2, 4, 16}, {1, 3, 5, 6, 18,
      17}, {1, 3, 5, 6, 18}}, {{2, 1}, {2}, {2, 1, 3}, {2, 4}, {2, 1, 3,
      5}, {2, 1, 3, 5, 6}, {2, 14, 13, 7}, {2, 14, 13, 7, 8}, {2, 14, 13, 15,
      9}, {2, 14, 13, 7, 8, 10}, {2, 1, 3, 5, 6, 18, 17, 11}, {2, 1, 3, 5, 6,
      18, 17, 11, 12}, {2, 14, 13}, {2, 14}, {2, 14, 13, 15}, {2, 4, 16}, {2,
      1, 3, 5, 6, 18, 17}, {2, 1, 3, 5, 6, 18}}, {{3, 1}, {3, 1, 2}, {3}, {3,
      4}, {3, 5}, {3, 5, 6}, {3, 1, 2, 14, 13, 7}, {3, 1, 2, 14, 13, 7,
      8}, {3, 4, 16, 15, 9}, {3, 4, 16, 15, 9, 10}, {3, 5, 6, 18, 17, 11}, {3,
       5, 6, 18, 17, 11, 12}, {3, 1, 2, 14, 13}, {3, 1, 2, 14}, {3, 4, 16,
      15}, {3, 4, 16}, {3, 5, 6, 18, 17}, {3, 5, 6, 18}}, {{4, 2, 1}, {4,
      2}, {4, 3}, {4}, {4, 3, 5}, {4, 3, 5, 6}, {4, 2, 14, 13, 7}, {4, 2, 14,
      13, 7, 8}, {4, 16, 15, 9}, {4, 16, 15, 9, 10}, {4, 3, 5, 6, 18, 17,
      11}, {4, 3, 5, 6, 18, 17, 11, 12}, {4, 2, 14, 13}, {4, 2, 14}, {4, 16,
      15}, {4, 16}, {4, 3, 5, 6, 18, 17}, {4, 3, 5, 6, 18}}, {{5, 3, 1}, {5,
      3, 1, 2}, {5, 3}, {5, 3, 4}, {5}, {5, 6}, {5, 3, 1, 2, 14, 13, 7}, {5,
      3, 1, 2, 14, 13, 7, 8}, {5, 3, 4, 16, 15, 9}, {5, 3, 4, 16, 15, 9,
      10}, {5, 6, 18, 17, 11}, {5, 6, 18, 17, 11, 12}, {5, 3, 1, 2, 14,
      13}, {5, 3, 1, 2, 14}, {5, 3, 4, 16, 15}, {5, 3, 4, 16}, {5, 6, 18,
      17}, {5, 6, 18}}, {{6, 5, 3, 1}, {6, 5, 3, 1, 2}, {6, 5, 3}, {6, 5, 3,
      4}, {6, 5}, {6}, {6, 5, 3, 1, 2, 14, 13, 7}, {6, 5, 3, 1, 2, 14, 13, 7,
      8}, {6, 18, 17, 15, 9}, {6, 18, 17, 15, 9, 10}, {6, 18, 17, 11}, {6, 18,
       17, 11, 12}, {6, 5, 3, 1, 2, 14, 13}, {6, 5, 3, 1, 2, 14}, {6, 18, 17,
      15}, {6, 5, 3, 4, 16}, {6, 18, 17}, {6, 18}}, {{7, 9, 16, 15, 17, 18, 6,
       5, 3, 1}, {7, 9, 16, 15, 17, 18, 6, 5, 3, 1, 2}, {7, 9, 16, 15, 17, 18,
       6, 5, 3}, {7, 9, 16, 15, 17, 18, 6, 5, 3, 4}, {7, 9, 16, 15, 17, 18, 6,
       5}, {7, 9, 16, 15, 17, 18, 6}, {7}, {7, 8}, {7, 9}, {7, 8, 10}, {7, 9,
      11}, {7, 9, 11, 12}, {7, 14, 13}, {7, 14}, {7, 9, 16, 15}, {7, 9,
      16}, {7, 9, 16, 15, 17}, {7, 9, 16, 15, 17, 18}}, {{8, 7, 9, 16, 15, 17,
       18, 6, 5, 3, 1}, {8, 7, 9, 16, 15, 17, 18, 6, 5, 3, 1, 2}, {8, 7, 9,
      16, 15, 17, 18, 6, 5, 3}, {8, 7, 9, 16, 15, 17, 18, 6, 5, 3, 4}, {8, 7,
      9, 16, 15, 17, 18, 6, 5}, {8, 7, 9, 16, 15, 17, 18, 6}, {8, 7}, {8}, {8,
       7, 9}, {8, 10}, {8, 7, 9, 11}, {8, 7, 9, 11, 12}, {8, 7, 14, 13}, {8,
      7, 14}, {8, 7, 9, 16, 15}, {8, 7, 9, 16}, {8, 7, 9, 16, 15, 17}, {8, 7,
      9, 16, 15, 17, 18}}, {{9, 16, 15, 17, 18, 6, 5, 3, 1}, {9, 16, 15, 17,
      18, 6, 5, 3, 1, 2}, {9, 16, 15, 17, 18, 6, 5, 3}, {9, 16, 15, 17, 18, 6,
       5, 3, 4}, {9, 16, 15, 17, 18, 6, 5}, {9, 16, 15, 17, 18, 6}, {9,
      7}, {9, 7, 8}, {9}, {9, 10}, {9, 11}, {9, 11, 12}, {9, 16, 14, 13}, {9,
      16, 14}, {9, 16, 15}, {9, 16}, {9, 16, 15, 17}, {9, 16, 15, 17,
      18}}, {{10, 9, 16, 15, 17, 18, 6, 5, 3, 1}, {10, 9, 16, 15, 17, 18, 6,
      5, 3, 1, 2}, {10, 9, 16, 15, 17, 18, 6, 5, 3}, {10, 9, 16, 15, 17, 18,
      6, 5, 3, 4}, {10, 9, 16, 15, 17, 18, 6, 5}, {10, 9, 16, 15, 17, 18,
      6}, {10, 8, 7}, {10, 8}, {10, 9}, {10}, {10, 9, 11}, {10, 9, 11,
      12}, {10, 9, 16, 14, 13}, {10, 9, 16, 14}, {10, 9, 16, 15}, {10, 9,
      16}, {10, 9, 16, 15, 17}, {10, 9, 16, 15, 17, 18}}, {{11, 18, 6, 5, 3,
      1}, {11, 18, 6, 5, 3, 1, 2}, {11, 18, 6, 5, 3}, {11, 18, 6, 5, 3,
      4}, {11, 18, 6, 5}, {11, 18, 6}, {11, 9, 7}, {11, 9, 7, 8}, {11,
      9}, {11, 9, 10}, {11}, {11, 12}, {11, 18, 17, 15, 13}, {11, 18, 17, 15,
      13, 14}, {11, 18, 17, 15}, {11, 9, 16}, {11, 18, 17}, {11, 18}}, {{12,
      11, 18, 6, 5, 3, 1}, {12, 11, 18, 6, 5, 3, 1, 2}, {12, 11, 18, 6, 5,
      3}, {12, 11, 18, 6, 5, 3, 4}, {12, 11, 18, 6, 5}, {12, 11, 18, 6}, {12,
      11, 9, 7}, {12, 11, 9, 7, 8}, {12, 11, 9}, {12, 11, 9, 10}, {12,
      11}, {12}, {12, 11, 18, 17, 15, 13}, {12, 11, 18, 17, 15, 13, 14}, {12,
      11, 18, 17, 15}, {12, 11, 9, 16}, {12, 11, 18, 17}, {12, 11, 18}}, {{13,
       15, 17, 18, 6, 5, 3, 1}, {13, 15, 17, 18, 6, 5, 3, 1, 2}, {13, 15, 17,
      18, 6, 5, 3}, {13, 15, 17, 18, 6, 5, 3, 4}, {13, 15, 17, 18, 6, 5}, {13,
       15, 17, 18, 6}, {13, 7}, {13, 7, 8}, {13, 15, 9}, {13, 7, 8, 10}, {13,
      15, 17, 11}, {13, 15, 17, 11, 12}, {13}, {13, 14}, {13, 15}, {13, 14,
      16}, {13, 15, 17}, {13, 15, 17, 18}}, {{14, 13, 15, 17, 18, 6, 5, 3,
      1}, {14, 13, 15, 17, 18, 6, 5, 3, 1, 2}, {14, 13, 15, 17, 18, 6, 5,
      3}, {14, 13, 15, 17, 18, 6, 5, 3, 4}, {14, 13, 15, 17, 18, 6, 5}, {14,
      13, 15, 17, 18, 6}, {14, 13, 7}, {14, 13, 7, 8}, {14, 13, 15, 9}, {14,
      13, 7, 8, 10}, {14, 13, 15, 17, 11}, {14, 13, 15, 17, 11, 12}, {14,
      13}, {14}, {14, 13, 15}, {14, 16}, {14, 13, 15, 17}, {14, 13, 15, 17,
      18}}, {{15, 17, 18, 6, 5, 3, 1}, {15, 17, 18, 6, 5, 3, 1, 2}, {15, 17,
      18, 6, 5, 3}, {15, 17, 18, 6, 5, 3, 4}, {15, 17, 18, 6, 5}, {15, 17, 18,
       6}, {15, 13, 7}, {15, 13, 7, 8}, {15, 9}, {15, 9, 10}, {15, 17,
      11}, {15, 17, 11, 12}, {15, 13}, {15, 13, 14}, {15}, {15, 16}, {15,
      17}, {15, 17, 18}}, {{16, 15, 17, 18, 6, 5, 3, 1}, {16, 15, 17, 18, 6,
      5, 3, 1, 2}, {16, 15, 17, 18, 6, 5, 3}, {16, 15, 17, 18, 6, 5, 3,
      4}, {16, 15, 17, 18, 6, 5}, {16, 15, 17, 18, 6}, {16, 14, 13, 7}, {16,
      14, 13, 7, 8}, {16, 15, 9}, {16, 15, 9, 10}, {16, 15, 17, 11}, {16, 15,
      17, 11, 12}, {16, 14, 13}, {16, 14}, {16, 15}, {16}, {16, 15, 17}, {16,
      15, 17, 18}}, {{17, 18, 6, 5, 3, 1}, {17, 18, 6, 5, 3, 1, 2}, {17, 18,
      6, 5, 3}, {17, 18, 6, 5, 3, 4}, {17, 18, 6, 5}, {17, 18, 6}, {17, 15,
      13, 7}, {17, 15, 13, 7, 8}, {17, 15, 9}, {17, 15, 9, 10}, {17, 11}, {17,
       11, 12}, {17, 15, 13}, {17, 15, 13, 14}, {17, 15}, {17, 15,
      16}, {17}, {17, 18}}, {{18, 6, 5, 3, 1}, {18, 6, 5, 3, 1, 2}, {18, 6, 5,
       3}, {18, 6, 5, 3, 4}, {18, 6, 5}, {18, 6}, {18, 17, 15, 13, 7}, {18,
      17, 15, 13, 7, 8}, {18, 17, 15, 9}, {18, 17, 15, 9, 10}, {18, 17,
      11}, {18, 17, 11, 12}, {18, 17, 15, 13}, {18, 17, 15, 13, 14}, {18, 17,
      15}, {18, 17, 15, 16}, {18, 17}, {18}}};

MidiSysX0 = 16^^F0;

MidiSysX7 = 16^^F7;

MidiTempo = {MidiMeta,16^^51};

MidiTie[d_MidiTie] := d

MidiTie[d_?NumberQ] := -d-1

MidiTie[d_List] := MidiTie /@ d

MidiTieQ[d_] := MatchQ[d,_MidiTie] || (NumberQ[d]&&d<0) || (!AtomQ[d]&&Or@@(MidiTieQ/@d))

MidiTimeSignature = {MidiMeta,16^^58};

MidiUnTie[d_MidiTie] := d[[1]]

MidiUnTie[d_?NumberQ] := -d-1

MidiUnTie[d_List] := MidiUnTie /@ d

MidiUnTie[d_] := d

(******** private functions used by MidiImportSMF and MidiExportSMF ********)

ReadInt[f_,n_]:=Total[ReadList[f,Byte,n] Table[256^i,{i,n-1,0,-1}]]

WriteInt[f_,n_,i_]:=
  Module[{b=IntegerDigits[i,256]},
    b=Join[Table[0,{n-Length[b]}],b];
    WriteBinary[f,b,ByteConversion->Identity]
    ]

ReadVarLen[f_]:=
  Module[{r=0,b=16^^80},
    While[16^^80<=b,
      b=Read[f,Byte];
      r=r*16^^80+BitAnd[b,16^^7F]
      ];
    r
    ]

ListVarLen[i_]:=
  Module[{r={}},
    r=IntegerDigits[i,16^^80];
    Join[16^^80+Drop[r,-1],Take[r,-1]]
    ]

ReadSysX[f_,type_] :=
  Module[{s},
    s=ReadVarLen[f];
    If[s!=0,
      {type,ReadList[f,Byte,s]},
      {type,{}}
      ]
    ]

ReadMeta[f_] :=
  Module[{subtype,s},
    subtype=Read[f,Byte];
    s=ReadVarLen[f];
    If[s!=0,
      {{MidiMeta,subtype},ReadList[f,Byte,s]},
      {{MidiMeta,subtype},{}}
      ]
    ]

ReadChannel[f_,r_] :=
  Module[{type,channel,s,x={}},
    type=BitAnd[IntegerPart[r[[1]]/2^4],16^^7];
    channel=BitAnd[r[[1]],16^^F];
    s={3,3,3,3,2,2,3}[[type+1]]-Length[r];
    If[s!=0,x=ReadList[f,Byte,s]];
    {type,Join[{channel},Drop[r,1],x]}
    ]

ReadEvent[f_,rt_]:=
  Module[{d=ReadVarLen[f],b,r},
    b=Read[f,Byte];
    If[b==MidiSysX0||b==MidiSysX7,
      {rt,{d,ReadSysX[f,b]}},
      If[b==MidiMeta,
        {rt,{d,ReadMeta[f]}},
        r=If[b<16^^80,{rt,b},{b}];
        {r[[1]],{d,ReadChannel[f,r]}}
        ]
      ]
    ]

ReadTrack[f_]:=
  Module[{e={0,{0,{}}},rt=0,pos},
    If[StringJoin@@ReadList[f,Character,4]!="MTrk",error];
    pos=StreamPosition[f]+ReadInt[f,4]+4;
    If[0<Length[#],#[[1]],{}]&[
      Reap[
        While[e[[2,2]]!=EOT && StreamPosition[f]<pos,
          e=ReadEvent[f,rt];
          rt=e[[1]];
          Sow[e[[2]]];
          ];
        If[StreamPosition[f]!=pos,error];
        ][[2]]
      ]
    ]

ListSysX[{type_,data_}]:=
  {type,Length[data],data}

ListMeta[{type:{_,sybtype_},data_}]:=
  {type,Length[data],data}

ListChannel[{type_,{channel_,data__}}]:=
  {16^^80+2^4type+channel,data}

ListTrack[t_]:=
  Function[e,
    {
      ListVarLen[e[[1]]],
      If[MatchQ[e[[2,1]],MidiSysX0|MidiSysX7],
        ListSysX[e[[2]]],
        If[MatchQ[e[[2,1]],{MidiMeta,_}],
          ListMeta[e[[2]]],
          ListChannel[e[[2]]]
          ]
        ]
      }
    ]/@t

WriteTrack[f_,t_]:=
  Module[{w=t},
    w=ListTrack[w];
    w=Mod[#,256]&/@(Abs/@(Round/@Flatten[w]));
    WriteString[f,"MTrk"];
    WriteInt[f,4,Length[w]];
    WriteBinary[f,w,ByteConversion->Identity]
    ]

End[]

Protect[
  Midi,
  MidiAbsolute,
  MidiAddEOT,
  MidiAddEvents,
  MidiAddNotes,
  MidiAddQPM,
  MidiChord,
  MidiControlChange,
  MidiDataAnyValue,
  MidiDataAnyValueQ,
  MidiDataNoValue,
  MidiDataNoValueQ,
  MidiDelta,
  MidiEmpty,
  MidiEOT,
  MidiExpandStates,
  MidiExpandStatePaths,
  MidiExportSMF,
  MidiFile,
  MidiFileFormat,
  MidiGetChannels,
  MidiGetDuration,
  MidiGetDurations,
  MidiGetInfo,
  MidiGetNotes,
  MidiGetQPM,
  MidiGetSecToTickFunction,
  MidiGetShape,
  MidiGetState,
  MidiGetTickToSecFunction,
  MidiGetTimeUnit,
  MidiGetTiming,
  MidiGetTPQ,
  MidiImportSMF,
  MidiKeySignature,
  MidiMeta,
  MidiMilliSec,
  MidiNormalizeNoteOff,
  MidiNoteOff,
  MidiNoteOn,
  MidiPar,
  MidiPatternData,
  MidiPatternChord,
  MidiPatternFile,
  MidiPatternInfo,
  MidiPatternMidi,
  MidiPatternMusic,
  MidiPatternTiming,
  MidiPatternTrack,
  MidiPatternType,
  MidiPatternVoice,
  MidiRemEvents,
  MidiRemNotes,
  MidiRemQPM,
  MidiSec,
  MidiSeq,
  MidiSetNotes,
  MidiSetQPM,
  MidiSetState,
  MidiSetStateLow,
  MidiShape,
  MidiStates,
  MidiStatePaths,
  MidiStateRoutes,
  MidiStatesExpanded,
  MidiStatePathsExpanded,
  MidiSysX0,
  MidiSysX7,
  MidiTempo,
  MidiTick,
  MidiTie,
  MidiTieQ,
  MidiTimeSignature,
  MidiTimeUnit,
  MidiTiming,
  MidiTPQ,
  MidiUnTie,
  MidiVoice,
  MidiVoiceReleaseTimeFunction,
  MidiQPM
  ];

EndPackage[ ]


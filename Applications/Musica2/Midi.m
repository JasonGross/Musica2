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
  MidiChord,
  MidiControlChange,
  MidiDelta,
  MidiEOT,
  MidiEqualizeEOT,
  MidiExpandStates,
  MidiExpandStatePaths,
  MidiExportSMF,
  MidiFile,
  MidiFileFormat,
  MidiGetChannels,
  MidiGetDuration,
  MidiGetDurations,
  MidiGetInfo,
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
  MidiNormalizeNoteOff,
  MidiNoteOff,
  MidiNoteOn,
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
  (*MidiPrintHeader,*)
  MidiSec,
  MidiSetShape,
  MidiSetState,
  MidiSetStateLow,
  MidiSetTimeUnit,
  MidiSetTiming,
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
  MidiTimeSignature,
  MidiTimeUnit,
  MidiTiming,
  MidiTPQ,
  MidiVoice,
  MidiVoiceReleaseTimeFunction,
  MidiQPM
  ];

Midi::usage = "Midi[i, d] represents a midi object where i is the info about d, the midi data."
MidiAbsolute::usage = "MidiTiming can be either MidiAbsolute or MidiDelta. If MidiTiming is MidiAbsolute, all timing information are absolute values. Observe that when in shape MidiVoice or MidiChord, MidiAbsolute means end-timing"
MidiAddEOT::usage = "MidiAddEOT[m:Midi[_,_]]"
MidiChord::usage = ""
MidiControlChange::usage = ""
MidiDelta::usage = "MidiTiming can be either MidiAbsolute or MidiDelta. If MidiTiming is MidiDelta, all timing information are delta values."
MidiEOT::usage = ""
MidiEqualizeEOT::usage = "MidiEqualizeEOT[m:Midi[_,_]]"
MidiExpandStates::usage = "MidiExpandStates[s_]"
MidiExpandStatePaths::usage = "MidiExpandStatePaths[p_]"
MidiExportSMF::usage = "MidiExportSMF[fn_String,m:Midi[_,_],opts___]"
MidiFile::usage = ""
MidiFileFormat::usage = ""
MidiGetChannels::usage = "MidiGetChannels[m:Midi[_,_]]"
MidiGetDuration::usage = "MidiGetDuration[m:Midi[_,_]]"
MidiGetDurations::usage = "MidiGetDurations[m:Midi[_,_]]"
MidiGetInfo::usage = ""
MidiGetQPM::usage = "MidiGetQPM[m : Midi[_, _]]"
MidiGetSecToTickFunction::usage = "MidiGetSecToTickFunction[m : Midi[_, _]]"
MidiGetShape::usage = "MidiGetShape[m:Midi[_,_]]"
MidiGetState::usage = "MidiGetState[m : Midi[_, _]]"
MidiGetTickToSecFunction::usage = "MidiGetTickToSecFunction[m : Midi[_, _]]"
MidiGetTimeUnit::usage = "MidiGetTimeUnit[m:Midi[_,_]]"
MidiGetTiming::usage = "MidiGetTiming[m:Midi[_,_]]"
MidiGetTPQ::usage = "MidiGetTPQ[m:Midi[_,_]]"
MidiImportSMF::usage = "MidiImportSMF[fn_String,opts___]"
MidiKeySignature::usage = ""
MidiMeta::usage = ""
MidiNormalizeNoteOff::usage = "MidiNormalizeNoteOff[m:Midi[_,_], v2z_:False]"
MidiNoteOff::usage = ""
MidiNoteOn::usage = ""
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
(*MidiPrintHeader::usage = ""*)
MidiSec::usage = ""
MidiSetShape::usage = "MidiSetShape[m:Midi[_,_],s_]"
MidiSetState::usage = "MidiSetState[m : Midi[_, _], s_]"
MidiSetStateLow::usage = ""
MidiSetTimeUnit::usage = "MidiSetTimeUnit[m:Midi[_,_],u_]"
MidiSetTiming::usage = "MidiSetTiming[m:Midi[_,_],t_]"
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
MidiTimeSignature::usage = ""
MidiTimeUnit::usage = ""
MidiTiming::usage = ""
MidiTPQ::usage = ""
MidiVoice::usage = ""
MidiVoiceReleaseTimeFunction::usage = "Function[{on,off,track,channel,pitch,velocity},0]"
MidiQPM::usage = ""

Begin["`Private`"]

ST[{}] := {}
ST[x_List] := Transpose[x]/;0<Length[x]

EOT = {MidiEOT,{}};

Format[m:Midi[_,_]] :=
  If[MidiGetShape[m]===MidiFile,
    StringForm["Midi[`1`,{{{timing, {type, data}}...}...}]",m[[1]]],
    If[MidiGetShape[m]===MidiVoice,
      StringForm["Midi[`1`,{{{type, track},{{timing, data}...}}...}]",m[[1]]],
      If[MidiGetShape[m]===MidiChord,
        StringForm["Midi[`1`,{{{type, {track...}},{{timing, {data...}}...}}...}]",m[[1]]],
        StringForm["Midi[`1`,<unknown data shape>]",m[[1]]]
        ]
      ]
    ]

Options[Midi]=
  {
    MidiQPM -> 120,
    MidiFileFormat -> 1,
    MidiTPQ -> 960
    }

MidiAddEOT[m:Midi[_,_]]:=
  Midi[
    Sort[m[[1]]],
    If[
      Length[#]==0||#[[-1,2]]!=EOT,
      Append[#,{0,EOT}],
      #
      ]& /@ m[[2]]
    ] /; MidiGetShape[m]===MidiFile && MidiGetTiming[m]===MidiDelta

MidiAddEOT[m:Midi[_,_]]:=
  MidiSetTiming[
    MidiAddEOT[
      MidiSetTiming[m,MidiDelta]
      ],
    MidiAbsolute
    ] /; MidiGetShape[m]===MidiFile && MidiGetTiming[m]===MidiAbsolute

MidiControlChange = 3;

MidiEOT = {MidiMeta,16^^2F};

MidiEqualizeEOT[m:Midi[_,_]] :=
  Module[{d=MidiGetDuration[m]},
    Midi[
      Sort[m[[1]]],
      If[
        Length[#]!=0&&#[[-1,2]]==EOT,
        ReplacePart[#,d,{-1,1}],
        #
        ]& /@ m[[2]]
      ]
    ] /; MidiGetShape[m]===MidiFile && MidiGetTiming[m]===MidiAbsolute

MidiEqualizeEOT[m:Midi[_,_]] :=
  MidiSetTiming[
    MidiEqualizeEOT[
      MidiSetTiming[m,MidiAbsolute]
      ],
    MidiDelta
  ] /; MidiGetShape[m]===MidiFile && MidiGetTiming[m]===MidiDelta

MidiExpandStates[s_] :=
  Module[{r},
    r = Flatten[Array[{##} &, Length[#[[2]]] & /@ s], Length[s] - 1];
    r = Sort[MapIndexed[(s[[#2[[1]], 1]] -> s[[#2[[1]], 2, #1]]) &, #]] & /@ r;
    Sort[r]
    ]

FillPath[f_,t_] := (* return all in f not in t *)
  Module[{r=t},
    Scan[If[!MemberQ[t,#[[1]]\[Rule]_],r=Append[r,#]]&,f];
    Sort[r]
    ]

FillState[s_]:=
  Module[{miss},
    (* wich state-parts is missing in s *)
    miss=Complement[#[[1]]&/@MidiStates,#[[1]]&/@s];
    (* fill in missing state-parts with a _ *)
    Sort[Join[s,(#\[Rule]_)&/@miss]]
    ]

MidiExpandStatePaths[p_] :=
  Module[{r = p, from, to},
    r = (
      from = #[[1]];
      to = #[[2]];
      (* fill in missing state - parts with a _ *)
      from = FillState[from];
      (* make "from" a list of from's *)
      from = Cases[MidiStatesExpanded, from];
      (* make "to" a list of to's *)
      to = FillPath[#, to] & /@ from;
      (* make a list of {from, to} *)
      Transpose[{from, to}]
      ) & /@ r;
    Sort[Flatten[r, 1]]
    ]

MidiExportSMF[fn_String,mx:Midi[_,_], opts___] :=
  Module[{m=mx,f=Null},
    m = MidiSetShape[m,MidiFile];
    m = MidiSetTiming[m,MidiDelta];
    m = MidiSetTimeUnit[m,MidiTick];
    m = MidiAddEOT[m];
    f=OpenWriteBinary[fn];
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

MidiGetChannels[m:Midi[_,_]] :=
  Union[
    Cases[#,{_,{MidiNoteOn,{c$_,_,_}}}->c$]
    ]& /@ m[[2]] /; MidiGetShape[m]===MidiFile

MidiGetDuration[m:Midi[_,_]] := Max[MidiGetDurations[m]] /; MidiGetShape[m]===MidiFile

MidiGetDurations[m:Midi[_,_]] := (Max[#[[1]]&/@#]&/@MidiSetTiming[m,MidiAbsolute][[2]]) /; MidiGetShape[m]===MidiFile

MidiGetInfo[m:Midi[_,_]] := m[[1]]

MidiGetQPMLow[m : Midi[_, _]] :=
  Module[{ma = MidiSetTiming[m, MidiAbsolute],u},
    (* get all tempo events as {{timing,data}...} *)
    u = Cases[ma[[2, 1]], {t$_, {MidiTempo, u$_}} -> {t$, u$}];
    u
    ] /; MidiGetShape[m] === MidiFile

MidiGetQPMLow[m : Midi[_, _]] :=
  Module[{ma = MidiSetTiming[m, MidiAbsolute],u},
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

MidiGetQPMLow[m : Midi[_, _]] :=
  Module[{ma = MidiSetTiming[m, MidiAbsolute],u,p},
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

MidiGetQPM[m : Midi[_, _]] :=
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

MidiGetSecToTickFunction[m : Midi[_, _]] :=
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

MidiGetShape[m:Midi[_,_]] := MidiShape /. m[[1]]

MidiGetState[s_List] := Module[{t = #[[1]] & /@ MidiStates}, Sort[Cases[s, Rule[p$_, _] /; MemberQ[t, p$]]]]

MidiGetState[m : Midi[_, _]] := MidiGetState[MidiGetInfo[m]]

MidiGetTickToSecFunction[m : Midi[_, _]] :=
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

MidiGetTimeUnit[m:Midi[_,_]] := MidiTimeUnit /. m[[1]]

MidiGetTiming[m:Midi[_,_]] := MidiTiming /. m[[1]]

MidiGetTPQ[m:Midi[_,_]] := MidiTPQ /. m[[1]] /. Options[Midi]

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
      h=Append[h,MidiFileFormat->ReadInt[f,2]];
      n=ReadInt[f,2];
      h=Append[h,MidiTPQ->ReadInt[f,2]];
      Do[t=Append[t,ReadTrack[f]],{n}];
      Close[f];
      (*
      If[MidiPrintHeader/.{opts}/.{MidiPrintHeader->False},
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

MidiNormalizeNoteOff[m:Midi[_,_],v2z_:False]:=
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

MidiPatternData = ({_Integer...}|Null);
MidiPatternChord = {{{MidiPatternType,{MidiPatternTrack...}},{{MidiPatternTiming,{MidiPatternData...}}...}}...};
MidiPatternFile = {{{MidiPatternTiming,{MidiPatternType,MidiPatternData}}...}...};
MidiPatternInfo = {(
  (MidiQPM->(_Integer|_Real)) |
  (MidiFileFormat->(0|1)) |
  (MidiShape->(MidiFile|MidiVoice|MidiChord)) |
  (MidiTiming->(MidiDelta|MidiAbsolute)) |
  (MidiTimeUnit->(MidiTick|MidiSec)) |
  (MidiTPQ->_Integer)
  )...};
MidiPatternMidi = Midi[MidiPatternInfo,MidiPatternMusic];
MidiPatternMusic = (MidiPatternFile|MidiPatternVoice|MidiPatternChord);
MidiPatternTiming = (_Integer|_Real);
MidiPatternTrack = (_Integer|{_Integer,_Integer,_Integer});
MidiPatternType = (MidiNoteOff|MidiNoteOn|2|MidiControlChange|4|5|6|7|MidiSysX0|MidiSysX7|{MidiMeta,_Integer});
MidiPatternVoice = {{{MidiPatternType,MidiPatternTrack},{{MidiPatternTiming,MidiPatternData}...}}...};

MidiSetShape[m:Midi[_,_],s_] := m /; MidiGetShape[m] === s

MidiSetShape[m : Midi[_, _], MidiVoice, opts___] := (* TODO, handle all channel messages not just notes *)
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
            (* prepend a null at 0 if tick 0 is absent *)
            If[r[[1, 2]] != 0, r = Prepend[r, {{ty, Null}, 0}]];
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
      Function[trni,
        Module[{time, coll, done = trni, next, v = 1, r = {}},
          While[done != {},
            time = 0;
            coll = {};
            next = Sort[done];
            done = {};
            While[next != {},
              coll = Append[
                coll,
                ReplacePart[
                  next[[1]],
                  Append[next[[1, 3]], v],
                  {3}
                  ]
                ];
              time = next[[1, 1, 2]] + rtf[next[[1,1,1]],next[[1,1,2]],next[[1,3,1]],next[[1,3,2]],next[[1,2,1]],next[[1,2,2]]];
              next = Drop[next, 1];
              done = Join[done, Select[next, (#[[1, 1]] < time) &]];
              next = Sort[Select[next, (time <= #[[1, 1]]) &]];
              ];
            r = Append[r, coll];
            v += 1;
            ];
          r
          ]
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
    (* transpose to {{{type, track}, {{duration...}, {data...}}}...} *)
    tr = {#[[1]], Transpose[#[[2]]]} & /@ tr;
    (* set to {{{type, track}, {{end-tick...}, {data...}}}...} *)
    tr = {#[[1]], {Drop[DeltasToValues[#[[2,1]]],1], #[[2,2]]}} & /@ tr;
    (* transpose to {{{type, track}, {{end-tick, data}...}}...} *)
    tr = {#[[1]], Transpose[#[[2]]]} & /@ tr;

    Midi[
      Sort[m[[1]] /. (MidiShape -> _) -> (MidiShape -> MidiVoice)],
      Sort[tr]
      ]
    ] /; MidiGetShape[m]===MidiFile && MidiGetTiming[m]===MidiAbsolute

MidiSetShape[m : Midi[_, _], MidiVoice, opts___] :=
  MidiSetTiming[
    MidiSetShape[
      MidiSetTiming[m,MidiAbsolute],
      MidiVoice,
      opts
      ],
    MidiDelta
    ] /; MidiGetShape[m]===MidiFile && MidiGetTiming[m]===MidiDelta

MidiSetShape[m : Midi[_, _], MidiFile] :=
  Module[{trm, trn, trx, tr},
    (* get all meta and sysx as {{{type, track}, {{end-tick, data}...}}...} *)
    trm = Select[m[[2]], MatchQ[#[[1, 1]], MidiSysX0 | MidiSysX7 | {MidiMeta,_}] &];
    (* set to {{{type, track}, {{tick, data}...}}...} *)
    trm = {#[[1]], Module[{td=Transpose[#[[2]]]},Transpose[{Prepend[td[[1]],0],Append[td[[2]],Null]}]]} & /@ trm;
    (* fix EOT *)
    trm = If[#[[1, 1]] == MidiEOT, ReplacePart[#, {}, {2, -1, 2}], #] & /@ trm;
    (* remove all Null *)
    trm = {#[[1]], Select[#[[2]], ! (#[[2]] === Null) &]} & /@ trm;
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
          {t$_, e$_, {p$_,v$_}} /; (0 < v$) -> {
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
    (* prepare tr to contain the tracks needed *)
    tr = Array[{} &, trx[[-1, 1]]];
    (* add all events to the right track *)
    Scan[Function[t, Scan[Function[e, tr[[t[[1]]]] = Append[tr[[t[[1]]]], e]], t[[2]]]], trx];
    (* sort each track *)
    tr = Sort /@ tr;

    Midi[
      Sort[m[[1]] /. (MidiShape -> _) -> (MidiShape -> MidiFile)],
      tr
      ]
    ] /; MidiGetShape[m]===MidiVoice && MidiGetTiming[m]===MidiAbsolute

MidiSetShape[m : Midi[_, _], MidiFile] :=
  MidiSetTiming[
    MidiSetShape[
      MidiSetTiming[m,MidiAbsolute],
      MidiFile
      ],
    MidiDelta
    ] /; MidiGetShape[m]===MidiVoice && MidiGetTiming[m]===MidiDelta

MidiSetShape[m : Midi[_, _], MidiChord] :=
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
                Append[#[[2]],{tt - vtt,{#[[2, -1, 2, 1]], -Abs[#[[2, -1, 2, 2]]]}}],
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
        nd = Module[{vt, i = 1, j},(* for each voice as {{type,track},{{timing,data}...}} *)
          Flatten[
            ( (* for each note as {timing,data} *)
              j = i;
              While[Total[Take[t, {i, j}]] < #[[1]], j++];
              vt = Take[t, {i, j}];
              i = j + 1;
              Prepend[
                Table[
                  -#[[2]], (* this is a tie *)
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
    ] /; MidiGetShape[m]===MidiVoice && MidiGetTiming[m]===MidiDelta

MidiSetShape[m : Midi[_, _], MidiChord] :=
  MidiSetTiming[
    MidiSetShape[
      MidiSetTiming[m,MidiDelta],
      MidiVoice
      ],
    MidiAbsolute
    ] /; MidiGetShape[m]===MidiVoice && MidiGetTiming[m]===MidiAbsolute

TieQ[d_] := (IntegerQ[d]&&d<0) || (ListQ[d]&&d[[1]]<0) || (d===-Null)

MidiSetShape[m : Midi[_, _], MidiVoice] :=
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
      Module[{r={},p=Null},
        Scan[
          Function[td,
            If[TieQ[td[[2]]],
              If[p===Null,
                Print["Odd, a tie at the beginning of a voice? There is a bug here somewhere!"],
                p={p[[1]]+td[[1]],p[[2]]}
                ],
              If[p===Null,
                p=td,
                r=Append[r,p];
                p=td
                ]
              ]
            ],
          #[[2]]
          ];
        Append[r,p]
        ]
      }& /@ g;

    Midi[
      Sort[m[[1]] /. (MidiShape -> _) -> (MidiShape -> MidiVoice)],
      g
      ]
    ] /; MidiGetShape[m]===MidiChord && MidiGetTiming[m]===MidiDelta

MidiSetShape[m : Midi[_, _], MidiVoice] :=
  MidiSetTiming[
    MidiSetShape[
      MidiSetTiming[m,MidiDelta],
      MidiVoice
      ],
    MidiAbsolute
    ] /; MidiGetShape[m]===MidiChord && MidiGetTiming[m]===MidiAbsolute

MidiSetShape[m : Midi[_, _], MidiFile] :=
  MidiSetShape[
    MidiSetShape[
      m,
      MidiVoice
      ],
    MidiFile
    ] /; MidiGetShape[m]===MidiChord

MidiSetShape[m : Midi[_, _], MidiChord] :=
  MidiSetShape[
    MidiSetShape[
      m,
      MidiVoice
      ],
    MidiChord
    ] /; MidiGetShape[m]===MidiFile

MidiSetStateLow[m:Midi[_,_],i_]:=
  MidiSetShape[m,MidiVoice]/;
    MatchQ[MidiGetState[m],
        FillState[{MidiShape\[Rule]MidiFile,MidiTiming\[Rule]MidiAbsolute}]]&&
            Complement[i,MidiGetState[m]]\[Equal]{MidiShape\[Rule]MidiVoice}

MidiSetStateLow[m:Midi[_,_],i_]:=
  MidiSetShape[m,MidiFile]/;
    MatchQ[MidiGetState[m],
        FillState[{MidiShape\[Rule]MidiVoice,MidiTiming\[Rule]MidiAbsolute}]]&&
            Complement[i,MidiGetState[m]]\[Equal]{MidiShape\[Rule]MidiFile}

MidiSetStateLow[m:Midi[_,_],i_]:=
  MidiSetShape[m,MidiChord]/;
    MatchQ[MidiGetState[m],
        FillState[{MidiShape\[Rule]MidiVoice,MidiTimeUnit\[Rule]MidiTick,
            MidiTiming\[Rule]MidiDelta}]]&&
      Complement[i,MidiGetState[m]]\[Equal]{MidiShape\[Rule]MidiChord}

MidiSetStateLow[m:Midi[_,_],i_]:=
  MidiSetShape[m,MidiVoice]/;
    MatchQ[MidiGetState[m],
        FillState[{MidiShape\[Rule]MidiChord,MidiTimeUnit\[Rule]MidiTick,
            MidiTiming\[Rule]MidiDelta}]]&&
      Complement[i,MidiGetState[m]]\[Equal]{MidiShape\[Rule]MidiVoice}

MidiSetStateLow[m:Midi[_,_],i_]:=
  MidiSetTimeUnit[m,MidiSec]/;
    MatchQ[MidiGetState[m],
        FillState[{MidiShape\[Rule]MidiFile,MidiTimeUnit\[Rule]MidiTick,
            MidiTiming\[Rule]MidiAbsolute}]]&&
      Complement[i,MidiGetState[m]]\[Equal]{MidiTimeUnit\[Rule]MidiSec}

MidiSetStateLow[m:Midi[_,_],i_]:=
  MidiSetTimeUnit[m,MidiSec]/;
    MatchQ[MidiGetState[m],
        FillState[{MidiShape\[Rule](MidiVoice|MidiChord),
            MidiTimeUnit\[Rule]MidiTick,MidiTiming\[Rule]MidiAbsolute}]]&&
         Complement[i,MidiGetState[m]]\[Equal]{MidiTimeUnit\[Rule]MidiSec}

MidiSetStateLow[m:Midi[_,_],i_]:=
  MidiSetTimeUnit[m,MidiTick]/;
    MatchQ[MidiGetState[m],
        FillState[{MidiShape\[Rule]MidiFile,MidiTimeUnit\[Rule]MidiSec,
            MidiTiming\[Rule]MidiAbsolute}]]&&
      Complement[i,MidiGetState[m]]\[Equal]{MidiTimeUnit\[Rule]MidiTick}

MidiSetStateLow[m:Midi[_,_],i_]:=
  MidiSetTimeUnit[m,MidiTick]/;
    MatchQ[MidiGetState[m],
        FillState[{MidiShape\[Rule](MidiVoice|MidiChord),
            MidiTimeUnit\[Rule]MidiSec,MidiTiming\[Rule]MidiAbsolute}]]&&
        Complement[i,MidiGetState[m]]\[Equal]{MidiTimeUnit\[Rule]MidiTick}

MidiSetStateLow[m:Midi[_,_],i_]:=
  MidiSetTiming[m,MidiAbsolute]/;
    MatchQ[MidiGetState[m],
        FillState[{MidiShape\[Rule]MidiFile,MidiTiming\[Rule]MidiDelta}]]&&
          Complement[i,MidiGetState[m]]\[Equal]{MidiTiming\[Rule]MidiAbsolute}

MidiSetStateLow[m:Midi[_,_],i_]:=
  MidiSetTiming[m,MidiAbsolute]/;
    MatchQ[MidiGetState[m],
        FillState[{MidiShape\[Rule](MidiVoice|MidiChord),
            MidiTiming\[Rule]MidiDelta}]]&&
      Complement[i,MidiGetState[m]]\[Equal]{MidiTiming\[Rule]MidiAbsolute}

MidiSetStateLow[m:Midi[_,_],i_]:=
  MidiSetTiming[m,MidiDelta]/;
    MatchQ[MidiGetState[m],
        FillState[{MidiShape\[Rule]MidiFile,MidiTiming\[Rule]MidiAbsolute}]]&&
            Complement[i,MidiGetState[m]]\[Equal]{MidiTiming\[Rule]MidiDelta}

MidiSetStateLow[m:Midi[_,_],i_]:=
  MidiSetTiming[m,MidiDelta]/;
    MatchQ[MidiGetState[m],
        FillState[{MidiShape\[Rule](MidiVoice|MidiChord),
            MidiTiming\[Rule]MidiAbsolute}]]&&
      Complement[i,MidiGetState[m]]\[Equal]{MidiTiming\[Rule]MidiDelta}

MidiSetState[m : Midi[_, _], s_] :=
  Module[{f, t = FillPath[MidiGetState[m], MidiGetState[s]], p, r = m},
    f = Position[MidiStatesExpanded, MidiGetState[m]][[1, 1]];
    t = Position[MidiStatesExpanded, t][[1, 1]];
    Print[{f, t}];
    p = Drop[MidiStateRoutes[[f, t]], 1];
    Print[p];
    p = MidiStatesExpanded[[#]] & /@ p;
    Print[ColumnForm[p]];
    Scan[(r = MidiSetStateLow[r, #]) &, p];
    r
    ]

MidiSetTimeUnit[m:Midi[_,_],u_] := m /; MidiGetTimeUnit[m]===u

MidiSetTimeUnit[m:Midi[_,_],MidiSec] :=
  Module[{f=MidiGetTickToSecFunction[m]},
    Midi[
      Sort[m[[1]]/.(MidiTimeUnit->_)->(MidiTimeUnit->MidiSec)],
      ({f[#[[1]]],#[[2]]}&/@#)&/@m[[2]]
      ]
    ] /; MidiGetShape[m]===MidiFile && MidiGetTimeUnit[m]===MidiTick && MidiGetTiming[m]===MidiAbsolute

MidiSetTimeUnit[m:Midi[_,_],MidiSec] :=
  Module[{f=MidiGetTickToSecFunction[m]},
    Midi[
      Sort[m[[1]]/.(MidiTimeUnit->_)->(MidiTimeUnit->MidiSec)],
      {#[[1]],{f[#[[1]]],#[[2]]}&/@#[[2]]}&/@m[[2]]
      ]
    ] /; (MidiGetShape[m]===MidiVoice || MidiGetShape[m]===MidiChord) && MidiGetTimeUnit[m]===MidiTick && MidiGetTiming[m]===MidiAbsolute

MidiSetTimeUnit[m:Midi[_,_],MidiTick] :=
  Module[{f=MidiGetSecToTickFunction[m]},
    Midi[
      Sort[m[[1]]/.(MidiTimeUnit->_)->(MidiTimeUnit->MidiTick)],
      ({f[#[[1]]],#[[2]]}&/@#)&/@m[[2]]
      ]
    ] /; MidiGetShape[m]===MidiFile && MidiGetTimeUnit[m]===MidiSec && MidiGetTiming[m]===MidiAbsolute

MidiSetTimeUnit[m:Midi[_,_],MidiTick] :=
  Module[{f=MidiGetSecToTickFunction[m]},
    Midi[
      Sort[m[[1]]/.(MidiTimeUnit->_)->(MidiTimeUnit->MidiTick)],
      {#[[1]],{f[#[[1]]],#[[2]]}&/@#[[2]]}&/@m[[2]]
      ]
    ] /; (MidiGetShape[m]===MidiVoice || MidiGetShape[m]===MidiChord) && MidiGetTimeUnit[m]===MidiSec && MidiGetTiming[m]===MidiAbsolute

MidiSetTimeUnit[m:Midi[_,_],u_] :=
  MidiSetTiming[
    MidiSetTimeUnit[
      MidiSetTiming[m,MidiAbsolute],
      u
      ],
    MidiDelta
    ] /; MidiGetTiming[m]===MidiDelta

MidiSetTiming[m:Midi[_,_],t_] := m /; MidiGetTiming[m]===t

MidiSetTiming[m:Midi[_,_],MidiAbsolute] := (
  Midi[
    Sort[m[[1]]/.(MidiTiming->_)->(MidiTiming->MidiAbsolute)],
    If[0<Length[#],Module[{td=Transpose[#]},td[[1]]=Drop[DeltasToValues[td[[1]]],1];Transpose[td]],#]& /@ m[[2]]
    ]) /; MidiGetShape[m]===MidiFile && MidiGetTiming[m]===MidiDelta

MidiSetTiming[m:Midi[_,_],MidiAbsolute] :=(
  Midi[
    Sort[m[[1]]/.(MidiTiming->_)->(MidiTiming->MidiAbsolute)],
    {#[[1]],Module[{td=Transpose[#[[2]]]},Transpose[{Drop[DeltasToValues[td[[1]]],1],td[[2]]}]]}&/@m[[2]]
    ]) /; (MidiGetShape[m]===MidiVoice||MidiGetShape[m]===MidiChord) && MidiGetTiming[m]===MidiDelta

MidiSetTiming[m:Midi[_,_],MidiDelta] :=(
  Midi[
    Sort[m[[1]]/.(MidiTiming->_)->(MidiTiming->MidiDelta)],
    If[0<Length[#],Module[{td=Transpose[Sort[#]]},td[[1]]=ValuesToDeltas[Prepend[td[[1]],0]];Transpose[td]],#]& /@ m[[2]]
    ]) /; MidiGetShape[m]===MidiFile && MidiGetTiming[m]===MidiAbsolute

MidiSetTiming[m:Midi[_,_],MidiDelta] :=(
  Midi[
    Sort[m[[1]]/.(MidiTiming->_)->(MidiTiming->MidiDelta)],
    {#[[1]],Module[{td=Transpose[Sort[#[[2]]]]},Transpose[{ValuesToDeltas[Prepend[td[[1]],0]],td[[2]]}]]}&/@m[[2]]
    ]) /; (MidiGetShape[m]===MidiVoice||MidiGetShape[m]===MidiChord) && MidiGetTiming[m]===MidiAbsolute

MidiStates = {
  {MidiShape, {MidiFile, MidiVoice, MidiChord}},
  {MidiTimeUnit, {MidiTick, MidiSec}},
  {MidiTiming, {MidiDelta, MidiAbsolute}}
  };

MidiStatesExpanded = MidiExpandStates[MidiStates]

MidiStatePaths = {
  (* Shape *)
  {{MidiShape -> MidiFile, MidiTiming -> MidiAbsolute}, {MidiShape -> MidiVoice}},
  {{MidiShape -> MidiVoice, MidiTiming -> MidiAbsolute}, {MidiShape -> MidiFile}},
  {{MidiShape -> MidiVoice, MidiTimeUnit -> MidiTick, MidiTiming -> MidiDelta}, {MidiShape -> MidiChord}},
  {{MidiShape -> MidiChord, MidiTimeUnit -> MidiTick, MidiTiming -> MidiDelta}, {MidiShape -> MidiVoice}},
  (* TimeUnit *)
  {{MidiShape -> MidiFile, MidiTimeUnit -> MidiTick, MidiTiming -> MidiAbsolute}, {MidiTimeUnit -> MidiSec}},
  {{MidiShape -> MidiFile, MidiTimeUnit -> MidiSec, MidiTiming -> MidiAbsolute}, {MidiTimeUnit -> MidiTick}},
  {{MidiShape -> (MidiVoice | MidiChord), MidiTimeUnit -> MidiTick, MidiTiming -> MidiAbsolute}, {MidiTimeUnit -> MidiSec}},
  {{MidiShape -> (MidiVoice | MidiChord), MidiTimeUnit -> MidiSec, MidiTiming -> MidiAbsolute}, {MidiTimeUnit -> MidiTick}},
  (* Timing *)
  {{MidiShape -> MidiFile, MidiTiming -> MidiDelta}, {MidiTiming -> MidiAbsolute}},
  {{MidiShape -> MidiFile, MidiTiming -> MidiAbsolute}, {MidiTiming -> MidiDelta}},
  {{MidiShape -> (MidiVoice | MidiChord), MidiTiming -> MidiDelta}, {MidiTiming -> MidiAbsolute}},
  {{MidiShape -> (MidiVoice | MidiChord), MidiTiming -> MidiAbsolute}, {MidiTiming -> MidiDelta}}
  };

MidiStatePathsExpanded = MidiExpandStatePaths[MidiStatePaths]

MidiStateRoutes = {
  {{1},{1,2},{1,3},{1,3,4},{1,3,4,12,11,9,5},{1,3,4,12,11,9,5,6},{1,3,4,12,11,
  7},{1,3,4,12,11,7,8},{1,3,4,12,11,9},{1,3,4,12,11,9,10},{1,3,4,12,
  11},{1,3,4,12}},{{2,1},{2},{2,1,3},{2,1,3,4},{2,1,3,4,12,11,9,5},{2,1,3,
  4,12,11,9,5,6},{2,1,3,4,12,11,7},{2,1,3,4,12,11,7,8},{2,1,3,4,12,11,
  9},{2,1,3,4,12,11,9,10},{2,1,3,4,12,11},{2,1,3,4,12}},{{3,1},{3,1,
  2},{3},{3,4},{3,4,12,11,9,5},{3,4,12,11,9,5,6},{3,4,12,11,7},{3,4,12,11,
  7,8},{3,4,12,11,9},{3,4,12,11,9,10},{3,4,12,11},{3,4,12}},{{4,3,1},{4,3,
  1,2},{4,3},{4},{4,12,11,9,5},{4,12,11,9,5,6},{4,12,11,7},{4,12,11,7,
  8},{4,12,11,9},{4,12,11,9,10},{4,12,11},{4,12}},{{5,9,11,12,4,3,1},{5,9,
  11,12,4,3,1,2},{5,9,11,12,4,3},{5,9,11,12,4},{5},{5,6},{5,7},{5,7,8},{5,
  9},{5,9,10},{5,9,11},{5,9,11,12}},{{6,5,9,11,12,4,3,1},{6,5,9,11,12,4,3,
  1,2},{6,5,9,11,12,4,3},{6,5,9,11,12,4},{6,5},{6},{6,5,7},{6,5,7,8},{6,5,
  9},{6,5,9,10},{6,5,9,11},{6,5,9,11,12}},{{7,5,9,11,12,4,3,1},{7,5,9,11,
  12,4,3,1,2},{7,5,9,11,12,4,3},{7,5,9,11,12,4},{7,5},{7,5,6},{7},{7,
  8},{7,5,9},{7,5,9,10},{7,5,9,11},{7,5,9,11,12}},{{8,7,5,9,11,12,4,3,
  1},{8,7,5,9,11,12,4,3,1,2},{8,7,5,9,11,12,4,3},{8,7,5,9,11,12,4},{8,7,
  5},{8,7,5,6},{8,7},{8},{8,7,5,9},{8,7,5,9,10},{8,7,5,9,11},{8,7,5,9,11,
  12}},{{9,11,12,4,3,1},{9,11,12,4,3,1,2},{9,11,12,4,3},{9,11,12,4},{9,
  5},{9,5,6},{9,11,7},{9,11,7,8},{9},{9,10},{9,11},{9,11,12}},{{10,9,11,
  12,4,3,1},{10,9,11,12,4,3,1,2},{10,9,11,12,4,3},{10,9,11,12,4},{10,9,
  5},{10,9,5,6},{10,9,11,7},{10,9,11,7,8},{10,9},{10},{10,9,11},{10,9,11,
  12}},{{11,12,4,3,1},{11,12,4,3,1,2},{11,12,4,3},{11,12,4},{11,9,5},{11,
  9,5,6},{11,7},{11,7,8},{11,9},{11,9,10},{11},{11,12}},{{12,4,3,1},{12,4,
  3,1,2},{12,4,3},{12,4},{12,11,9,5},{12,11,9,5,6},{12,11,7},{12,11,7,
  8},{12,11,9},{12,11,9,10},{12,11},{12}}
  };

MidiSysX0 = 16^^F0;

MidiSysX7 = 16^^F7;

MidiTempo = {MidiMeta,16^^51};

MidiTimeSignature = {MidiMeta,16^^58};

(* private functions used by MidiImportSMF and MidiExportSMF *)

ReadInt[f_,n_]:=Total[ReadList[f,Byte,n] Table[256^i,{i,n-1,0,-1}]]

WriteInt[f_,n_,i_]:=
  Module[{b=IntegerDigits[i,256]},
    b=Join[Array[0&,n-Length[b]],b];
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

ReadEvent[f_,rt_]:=
  Module[{d=ReadVarLen[f],b,s,t,c,r,z={3,3,3,3,2,2,3}},
    b=Read[f,Byte];
    If[b==MidiSysX0||b==MidiSysX7,
      s=ReadVarLen[f];
      If[s!=0,
        r={rt,{d,{b,ReadList[f,Byte,s]}}},
        r={rt,{d,{b,{}}}}
        ],
      If[b==MidiMeta,
        t=Read[f,Byte];
        s=ReadVarLen[f];
        If[s!=0,
          r={rt,{d,{{b,t},ReadList[f,Byte,s]}}},
          r={rt,{d,{{b,t},{}}}}
          ],
        If[b<16^^80,
          r={rt,b},
          r={b}
          ];
        t=BitAnd[IntegerPart[r[[1]]/2^4],16^^7];
        c=BitAnd[r[[1]],16^^F];
        s=z[[t+1]]-Length[r];
        If[s!=0,r=Join[r,ReadList[f,Byte,s]]];
        r={r[[1]],{d,{t,Prepend[Drop[r,1],c]}}};
        ]
      ];
    r
    ]

ReadTrack[f_]:=
  Module[{t={},e={0,{0,{}}},rt=0,pos},
    If[StringJoin@@ReadList[f,Character,4]!="MTrk",error];
    pos=StreamPosition[f]+ReadInt[f,4]+4;
    While[e[[2,2]]!=EOT&&StreamPosition[f]<pos,
      e=ReadEvent[f,rt];
      rt=e[[1]];
      t=Append[t,e[[2]]]
      ];
    If[StreamPosition[f]!=pos,error];
    t
    ]

ListTrack[t_]:=
  Function[e,
    If[MatchQ[e[[2,1]],MidiSysX0|MidiSysX7|{MidiMeta,_}],
      {ListVarLen[e[[1]]],e[[2,1]],Length[e[[2,2]]],e[[2,2]]},
      {ListVarLen[e[[1]]],16^^80+2^4e[[2,1]]+e[[2,2,1]],Drop[e[[2,2]],1]}
      ]
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
  MidiChord,
  MidiControlChange,
  MidiDelta,
  MidiEOT,
  MidiEqualizeEOT,
  MidiExpandStates,
  MidiExpandStatePaths,
  MidiExportSMF,
  MidiFile,
  MidiFileFormat,
  MidiGetChannels,
  MidiGetDuration,
  MidiGetDurations,
  MidiGetInfo,
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
  MidiNormalizeNoteOff,
  MidiNoteOff,
  MidiNoteOn,
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
  (*MidiPrintHeader,*)
  MidiSec,
  MidiSetShape,
  MidiSetState,
  MidiSetStateLow,
  MidiSetTimeUnit,
  MidiSetTiming,
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
  MidiTimeSignature,
  MidiTimeUnit,
  MidiTiming,
  MidiTPQ,
  MidiVoice,
  MidiVoiceReleaseTimeFunction,
  MidiQPM
  ];

EndPackage[ ]


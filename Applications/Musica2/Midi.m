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
  2004-09-12  bch :  using Common.m, MidiGetDuration(s) is now GetDuration(s), MidiGetInfo is now GetInfo
  2004-09-11  bch :  added use of Note.m's Chord- and Melody-types
                     added use of Util.m's ParOfSeqToSeqOfPar and SeqOfParToParOfSeq
  2004-09-??  bch :  changed all Moeldys to Melodies, ok?
  2004-09-04  bch :  added MidiQ
  2004-09-02  bch :  renamed MidiData*Value* to Data*Value* and moved them to Utils.m
                     renamed MidiTie* to DataTie* and moved them to Utils.m
                     renamed MidiVoice to MidiMelody
  2004-09-01  bch :  added MidiPitchCenter
  2004-08-27  bch :  simplified Format so that ?? wont get messy, need help here...
                     added message todo
  2004-08-26  bch :  added some help/usage-text
  2004-08-23  bch :  added MidiFixTime, MidiSet(Pitch|Time)
                     bugfix in going from shape file to voice
                     added MidiGetPitchRange(s)
  2004-08-22  bch :  added Midi(Pitch|Time)(Bend|Flip|Shift)
  2004-08-17  bch :  added MidiSetTPQ
  2004-08-17  bch :  added Midi(Add|Get|Rem|Set)(Chords|Voices)
                     corrected trackhandling when changing from shape MidiVoice to MidiFile
                     added MidiOfSilence and MidiPitchShift
  2004-08-15  bch :  dropped the voice-nr from shape voice and chord
                     renamed MidiAddEOT to MidiFixEOT and MidiNormalizeNoteOff to MidiFixNoteOff
  2004-08-14  bch :  yet another rest/tie scheme, now with MidiDataAnyValue and MidiDataNoValue, MidiRest* is removed
                     added MidiMilliSec as an integer MidiTimeUnit
                     added MidiPar and MidiSeq
                     changed MidiEmpty to have one empty track rather than no track at all
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
    "Musica2`Common`",
    "Musica2`Note`",
    "Musica2`Utils`"
    }
]

Unprotect[
  GetData,
  GetDuration,
  GetDurations,
  GetInfo
  ];

Unprotect[
  Midi,
  MidiAbsolute,
  MidiAddChords,
  MidiAddEvents,
  MidiAddMelodies,
  MidiAddNotes,
  MidiAddQPM,
  MidiChord,
  MidiControlChange,
  MidiDelta,
  MidiEmpty,
  MidiEOT,
  MidiExportSMF,
  MidiFile,
  MidiFileFormat,
  MidiFixEOT,
  MidiFixNoteOff,
  MidiFixTime,
  MidiGetChannels,
  MidiGetChords,
  MidiGetMelodies,
  MidiGetNotes,
  MidiGetPitchRange,
  MidiGetPitchRanges,
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
  MidiMelody,
  MidiMelodyReleaseTimeFunction,
  MidiMeta,
  MidiMilliSec,
  MidiMix,
  MidiNoteOff,
  MidiNoteOn,
  MidiOfSilence,
  MidiPar,
  MidiPatternData,
  MidiPatternChord,
  MidiPatternFile,
  MidiPatternInfo,
  MidiPatternMelody,
  MidiPatternMidi,
  MidiPatternMusic,
  MidiPatternTiming,
  MidiPatternTrack,
  MidiPatternType,
  MidiPitchCenter,
  MidiPitchFlip,
  MidiPitchShift,
  MidiQ,
  MidiQPM,
  MidiRemChords,
  MidiRemEvents,
  MidiRemMelodies,
  MidiRemNotes,
  MidiRemQPM,
  MidiSec,
  MidiSeq,
  MidiSetChords,
  MidiSetMelodies,
  MidiSetNotes,
  MidiSetPitch,
  MidiSetQPM,
  MidiSetState,
  MidiSetStateLow,
  MidiSetTime,
  MidiSetTPQ,
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
  MidiTimeBend,
  MidiTimeFlip,
  MidiTimeShift,
  MidiTimeSignature,
  MidiTimeUnit,
  MidiTiming,
  MidiTPQ,
  MidiUnPar,
  MidiUnSeq
  ];

Midi::todo = "It would be great to have a Format that is informative WHITHOUT messing with ??."
MidiAddChords::todo = "What about the track- and channel-info when they already exist? Error handling is missing."
MidiExportSMF::todo = "Check if MidiFileFormat is OK in respect to the number of tracks?"
MidiFileFormat::todo = "Shall there be checks in Midi(Add|Set)(Notes|Melodies|Chords) and other functions that MidiFileFormat is OK in respect to the number of tracks?"
MidiMix::todo = "Everything."
MidiPar::todo = "An option for weither the first track is a master-track and should be discarded or not."
MidiQ::todo = "Make the use of MidiPattern* work."
MidiSetStateLow::todo = "Changing shape from MidiFile to MidiMelody and back discards all channel-messages but notes."
MidiUnPar::todo = "An option for weither the first track is a master-track and should be copied into all Midi-objects or not."
MidiUnSeq::todo = "Everything."

Midi::usage = "Midi[i, d] represents a midi object where i is the info about d, the midi data. Midi is also the holder of some default values/options."<>ToDoString<>Midi::todo
MidiAbsolute::usage = "MidiTiming can be either MidiAbsolute or MidiDelta. If MidiTiming is MidiAbsolute, all timing information are absolute values. Observe that when in shape MidiMelody or MidiChord, MidiAbsolute means end-timing"
MidiAddChords::usage = "MidiAddChords[m_Midi,n:{{{_,_}...},{{_,{{_,_}...}}...}}] adds the chords in n to m. The chords is appended to m, so if m already contains chords, the polyphony in n and m must match. The timing is supposed to be MidiDelta."<>ToDoString<>MidiAddChords::todo
MidiAddEvents::usage = "MidiAddEvents[m_Midi,n:{{{_,{_,_}}...}...}] adds the events in e to m. The timing is supposed to be MidiAbsolute."
MidiAddMelodies::usage = "MidiAddMelodies[m_Midi,n:{{{_,_},{{_,{_,_}}...}}...}] "
MidiAddNotes::usage = "MidiAddNotes[m_Midi,n : {{{{_,_},{_,_,_}}...}...}] adds the notes in n to m. The timing is supposed to be MidiAbsolute. MidiAddNotes calls MidiAddEvents after creating a NoteOn and a NoteOff for each note."
MidiAddQPM::usage = "MidiAddQPM[m_Midi, q : {{_, _} ...}] adds tempo events in QuartersPerMinute to m. The timing is supposed to be MidiAbsolute. MidiAddQPM calls MidiAddEvents after reformatting each QPM."
MidiChord::usage = "One possible shape for a Midi-object. Part of a Midi's state."
MidiControlChange::usage = "The constant for event-type control-change."
MidiDelta::usage = "MidiTiming can be either MidiAbsolute or MidiDelta. If MidiTiming is MidiDelta, all timing information are delta values."
MidiEmpty::usage = "An empty Midi-object."
MidiEOT::usage = "The constant for event-type EndOfTrack."
MidiExportSMF::usage = "MidiExportSMF[fn_String, m_Midi, opts___] writes m to the file named fn in StandardMidiFile-format."<>ToDoString<>MidiExportSMF::todo
MidiFile::usage = "One possible shape for a Midi-object. Part of a Midi's state."
MidiFileFormat::usage = "Part of a Midi's info. Either the integer 0 or 1. Will be used more later."<>ToDoString<>MidiFileFormat::todo
MidiFixEOT::usage = "MidiFixEOT[m_Midi, keep_] adds EOT to each track in m, and removes any duplicates or extra EOT's. If keep is true the duration in m will be preserved, if false the duration will be trimmed."
MidiFixNoteOff::usage = "MidiFixNoteOff[m_Midi, v2z_:False] changes all NoteOn's with velocity 0 (zero) to NoteOff's. If v2z it true all velocity's of NoteOff's will be set to 0 (zero) as well."
MidiFixTime::usage = "MidiFixTime[m_Midi, q_] quantifies the timing's in m. MidiFixTime calls MidiSetTime with function-parameter (q*Round[#/q])&."
MidiGetChannels::usage = "MidiGetChannels[m_Midi] returns a nested list with channel-numbers for each track for which there are notes."
MidiGetChords::usage = "MidiGetChords[m_Midi] returns the chords in m. The timing will be MidiDelta."
MidiGetMelodies::usage = "MidiGetMelodies[m_Midi] returns the voices in m. THe timing will be MidiDelta."
MidiGetNotes::usage = "MidiGetNotes[m_Midi] returns the notes in m. The timing will be MidiAbsolute."
MidiGetPitchRange::usage = "MidiGetPitchRange[m_Midi] returns the pitch-range in m. See also MidiGetPitchRanges."
MidiGetPitchRanges::usage = "MidiGetPitchRanges[m_Midi] returns a list with the pitch-range's for each track in m. See also MidiGetPitchRange."
MidiGetQPM::usage = "MidiGetQPM[m_Midi] returns the tempo events in m. The timing will be MidiAbsolute."
MidiGetSecToTickFunction::usage = "MidiGetSecToTickFunction[m_Midi] returns a conversion-function reflecting the tempo-changes in m."
MidiGetShape::usage = "MidiGetShape[m_Midi] returns the shape m is in."
MidiGetState::usage = "MidiGetState[m_Midi] returns the state m is in."
MidiGetTickToSecFunction::usage = "MidiGetTickToSecFunction[m_Midi] returns a conversion-function reflecting the tempo-changes in m."
MidiGetTimeUnit::usage = "MidiGetTimeUnit[m_Midi] returns the time-unit used in m."
MidiGetTiming::usage = "MidiGetTiming[m_Midi] returns the timing used in m."
MidiGetTPQ::usage = "MidiGetTPQ[m_Midi] returns the resolution in TicksPerQuarter used in m."
MidiImportSMF::usage = "MidiImportSMF[fn_String, opts___] reads a StandardMidiFile and returns a Midi-object."
MidiKeySignature::usage = "The constant for event-type key-signature."
MidiMelody::usage = "One possible shape for a Midi-object. Part of a Midi's state."
MidiMelodyReleaseTimeFunction::usage = "default is Function[{track,note:{{on, off}, {ch, p, v}}},0]"
MidiMeta::usage = "The constant for the first part of event-type's that are meta-events."
MidiMilliSec::usage = "One possible time-unit for a Midi-object. Part of a Midi's state."
MidiMix::usage = "Does nothing yet."<>ToDoString<>MidiMix::todo
MidiNoteOff::usage = "The constant for event-type note-on."
MidiNoteOn::usage = "The constant for event-type note-off."
MidiOfSilence::usage = "MidiOfSilence[tracks_,duration_, opts___] creates an almost empty Midi-object. The opts-argument can be MidiTimeUnit, MidiTPQ and MidiQPM, which if omitted will default to Options[Midi]."
MidiPar::usage = "MidiPar[m:{_Midi,_Midi...}, opts___] concatenates all Midi-objects in m into one. The first track in the resulting Midi-object will be the one from the first Midi-object in the list in m. All the other Midi-objects first track's will be discarded."<>ToDoString<>MidiPar::todo
MidiPatternData::usage = ""
MidiPatternChord::usage = ""
MidiPatternFile::usage = ""
MidiPatternInfo::usage = ""
MidiPatternMelody::usage = ""
MidiPatternMidi::usage = ""
MidiPatternMusic::usage = ""
MidiPatternTiming::usage = ""
MidiPatternTrack::usage = ""
MidiPatternType::usage = ""
MidiPitchCenter::usage = "MidiPitchCenter[m_Midi, c_]"
MidiPitchFlip::usage = "MidiPitchFlip[m_Midi, o_] inverts all pitch's in m around the pitch in argument o. MidiPitchFlip calls MidiSetPitch. If the o-argument is omitted, it will be calculated by calling Mean[MidiGetPitchRange[m]]. MidiPitchFlip calls MidiSetPitch[m,(o-(#-o))&]."
MidiPitchShift::usage = "MidiPitchShift[m_Midi, s_] simply calls MidiSetPitch[m,(#+s)&]."
MidiQ::usage = "MidiQ[expr_] test if expr is a proper Midi-object."<>ToDoString<>MidiQ::todo
MidiQPM::usage = "A part of the info of a Midi-object and Options[Midi], indicates the default tempo if there is no tempo-events in the data."
MidiRemChords::usage = "MidiRemChords[m_Midi] removes all notes in m by first setting shape to MidiChord."
MidiRemEvents::usage = "MidiRemEvents[m_Midi,p_] removes all events in m that match the pattern p."
MidiRemMelodies::usage = "MidiRemMelodies[m_Midi] removes all notes in m by first setting shape to MidiMelody."
MidiRemNotes::usage = "MidiRemNotes[m_Midi] removes all notes in m. MidiRemNotes calls MidiRemEvents[m,{_, {(MidiNoteOn | MidiNoteOff), _}}]."
MidiRemQPM::usage = "MidiRemQPM[m_Midi] removes all tempo-changes in m. MidiRemQPM calls MidiRemEvents[m,{_, {MidiTempo, _}}]."
MidiSec::usage = "One possible time-unit for a Midi-object. Part of a Midi's state."
MidiSeq::usage = "MidiSeq[m:{_Midi,_Midi...}, opts___] concatenates all Midi-objects in m into one."
MidiSetChords::usage = "MidiSetChords[m_Midi,n:{{{_,_}...},{{_,{{_,_}...}}...}}] calls MidiRemChords and MidiAddChords."
MidiSetMelodies::usage = "MidiSetMelodies[m_Midi,n:{{{_,_},{{_,{_,_}}...}}...}] calls MidiRemMelodies and MidiAddMelodies."
MidiSetNotes::usage = "MidiSetNotes[m_Midi,n : {{{{_,_},{_,_,_}}...}...}] calls MidiRemNotes and MidiAddNotes."
MidiSetPitch::usage = "MidiSetPitch[m_Midi, f_] sets all pitch in m to the result from calling the function in f taking the original pitch as parameter."
MidiSetQPM::usage = "MidiSetQPM[m_Midi, q : {{_, _} ...}] calls MidiRemQPM and MidiAddQPM."
MidiSetState::usage = "MidiSetState[m_Midi, s_, opts___] sets m to the state in s, which can be incomplete. The s-argument is the list of options/rules MidiShape, MidiTimeUnit and MidiTiming. MidiSetState repeatedly calls MidiSetStateLow according to MidiStateRoutes, calculated by CalcMidiStateRoutes."
MidiSetStateLow::usage = "MidiSetStateLow[m_Midi, s_, opts___] does the hard work in changing the state in m, called by MidiSetState."<>ToDoString<>MidiSetStateLow::todo
MidiSetTime::usage = "MidiSetTime[m_Midi, f_] sets all timing in m to the result from calling the function in f taking the original timing as parameter."
MidiSetTPQ::usage = "MidiSetTPQ[m_Midi,tpq_] sets the TPQ in m without changing the data whatsoever, only the info-part is affected."
MidiShape::usage = "A part of the state a Midi-object can be in. Possible values are MidiFile, MidiMelody and MidiChord."
MidiStates::usage = "The list of all possible states a Midi-object can be in."
MidiStatePaths::usage = "Low-level stuff, needed by CalcMidiStateRoutes."
MidiStateRoutes::usage = "Low-level stuff, needed by MidiSetState, calculated by CalcMidiStateRoutes."
MidiStatesExpanded::usage = "Low-level stuff, needed by CalcMidiStateRoutes."
MidiStatePathsExpanded::usage = "Low-level stuff, needed by CalcMidiStateRoutes."
MidiSysX0::usage = "The constant for event-type sys-ex starting with F0."
MidiSysX7::usage = "The constant for event-type sys-ex starting with F7."
MidiTempo::usage = "The constant for event-type tempo."
MidiTick::usage = "One possible time-unit for a Midi-object. Part of a Midi's state."
MidiTimeBend::usage = "MidiTimeBend[m_Midi, b_] calls MidiSetTime[m,(#*b)&]. Call it augment or diminish if you wish."
MidiTimeFlip::usage = "MidiTimeFlip[m_Midi] flips the time in m, retragrade if you wish."
MidiTimeShift::usage = "MidiTimeShift[m_Midi, s_] transposes (not in the Mathematica sense) m by calling MidiSetTime[m,(#+s)&]."
MidiTimeSignature::usage = "The constant for event-type time-signature."
MidiTimeUnit::usage = "A part of the state a Midi-object can be in. Possible values are MidiTick, MidiSec and MidiMilliSec."
MidiTiming::usage = "A part of the state a Midi-object can be in. Possible values are MidiDelta and MidiAbsolute."
MidiTPQ::usage = "A part of the info of a Midi-object and Options[Midi], indicates the resolution."
MidiUnPar::usage = "MidiUnPar[m_Midi] creates a list of Midi-objects, one for each track in m but track 1 which is copied into all Midi-objects."<>ToDoString<>MidiUnPar::todo
MidiUnSeq::usage = "Does nothing yet."<>ToDoString<>MidiUnSeq::todo

Begin["`Private`"]

EOT = {MidiEOT,{}};

Format[m_Midi] := "\[SkeletonIndicator]Midi\[SkeletonIndicator]"

(*
Format[m_Midi] :=
  If[Length[m]==2 && OptionQ[m[[1]]],
    If[MidiGetShape[m]===MidiFile,
      StringForm["Midi[`1`,{{{timing, {type, data}}...}...}]",m[[1]]],
      If[MidiGetShape[m]===MidiMelody,
        StringForm["Midi[`1`,{{{type, track},{{timing, data}...}}...}]",m[[1]]],
        If[MidiGetShape[m]===MidiChord,
          StringForm["Midi[`1`,{{{type, {track...}},{{timing, {data...}}...}}...}]",m[[1]]],
          StringForm["Midi[`1`,<unknown shape>]",m[[1]]]
          ]
        ]
      ],
    StringForm["Midi[<unknown form>]"]
    ] /; MidiQ[m]
*)

GetData[m_?MidiQ] := m[[2]]

GetInfo[m_?MidiQ] := m[[1]]

Options[Midi]=
  {
    MidiQPM -> 120,
    MidiFileFormat -> 1,
    MidiTPQ -> 960
    }

Midi[c_?ChordQ] := Midi[{c}]
Midi[c:{_?ChordQ...}] := MidiAddChords[MidiOfSilence[1,0],c]
Midi[m_?MelodyQ] := Midi[{m}]
Midi[m:{_?MelodyQ...}] := MidiAddMelodies[MidiOfSilence[1,0],m]

MidiAddChords[mx_Midi,c_?ChordQ] := MidiAddChords[mx,{c}]
MidiAddChords[mx_Midi,c:{_?ChordQ...}] := MidiAddChords[mx,{Table[{i+1,0},{i,Max[GetNoteCount/@c]}],c}]
MidiAddChords[mx_Midi,n:{{{_,_}...},{_?ChordQ...}}] :=
  Module[{m = MidiSetState[mx,{MidiShape->MidiChord,MidiTiming->MidiDelta}],c,d,g},
    g = GetData[#]& /@ n[[2]];
    (* get the polyphony(s) in n *)
    c = Union[Length[#[[2]]]& /@ g];
    (* there can only be one! *)
    If[Length[c]==1 && Length[n[[1]]] == c[[1]],
      d = Sort[m[[2]]];
      If[0 < Length[d] && d[[1,1,1]]===MidiNoteOn,
        If[c[[1]]==Length[d[[1,1,2]]],
          (* todo, what about the track- and channel-info? *)
          d[[1,2]] = Join[d[[1,2]],g],
          (* todo, error! what to do? *)
          Null
          ],
        d = Prepend[d,{{MidiNoteOn,n[[1]]},g}]
        ];
      MidiSetState[
        Midi[
          m[[1]],
          d
          ],
        MidiGetState[mx]
        ],
      (* todo, error1, what to do? *)
      Null
      ]
    ]

MidiAddEvents[mx_Midi,e:{{{_,{_,_}}...}...}]:=
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

MidiAddMelodies[mx_Midi,m_?MelodyQ] := MidiAddMelodies[mx,{m}]
MidiAddMelodies[mx_Midi,m:{_?MelodyQ...}] := MidiAddMelodies[mx,Table[{{i+1,0},m[[i]]},{i,Length[m]}]] (* todo, better setting of default-track-nr *)
MidiAddMelodies[mx_Midi,n:{{{_,_},_?MelodyQ}...}] :=
  Module[{m = MidiSetState[mx,{MidiShape->MidiMelody,MidiTiming->MidiDelta}],d},
    d = {{MidiNoteOn,#[[1]]},GetData[#[[2]]]}& /@ n;
    MidiSetState[
      Midi[
        m[[1]],
        Sort[Join[m[[2]],d]]
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

MidiEmpty = Midi[{MidiShape->MidiFile,MidiTiming->MidiAbsolute,MidiTimeUnit->MidiTick},{}]

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
    m = MidiFixEOT[m,True];(* an option for this? especially since it sorts the events! *)
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

MidiFixEOT[mx_Midi, keep_:False] := (* this function also adds EOT if missing. if the keep parameter is true the max-duration is kept *)
  Module[{d,m=MidiSetState[mx,{MidiShape->MidiFile,MidiTiming->MidiAbsolute}]},
    If[keep,d=GetDuration[m]];
    m[[2]] = Cases[Sort[#],e$_/;!MatchQ[e$,{_,EOT}]->e$]& /@ m[[2]];
    If[!keep,d=GetDuration[m]];
    MidiSetState[
      Midi[
        Sort[m[[1]]],
        Append[#,{d,EOT}]& /@ m[[2]]
        ],
      MidiGetState[mx]
      ]
    ]

MidiFixNoteOff[m_Midi,v2z_:False]:=
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

MidiFixTime[m_Midi, q_] := MidiSetTime[m,(q*Round[#/q])&]

MidiGetChannels[m_Midi] :=
  Union[
    Cases[#,{_,{MidiNoteOn,{c$_,_,_}}}->c$]
    ]& /@ m[[2]] /; MidiGetShape[m]===MidiFile

MidiGetChords[mx_Midi] :=
  Module[{m = MidiSetState[mx, {MidiShape -> MidiChord, MidiTiming -> MidiDelta}],d},
    d = Sort[m[[2]]];
    If [0 < Length[d] && d[[1,1,1]] === MidiNoteOn, {d[[1,1,2]],Chord /@ d[[1,2]]}, {{},{}}]
    ]

GetDuration[m_Midi] := Max[GetDurations[m]]

GetDurations[m_Midi] := (Max[#[[1]]& /@ #]& /@ MidiSetState[m,{MidiShape->MidiFile,MidiTiming->MidiAbsolute}][[2]])

MidiGetMelodies[mx_Midi] :=
  Module[{m = MidiSetState[mx,{MidiShape->MidiMelody,MidiTiming->MidiDelta}],d},
    d = Cases[Sort[m[[2]]],{{MidiNoteOn,{_,_}},_}];
    {#[[1,2]],Melody[#[[2]]]}& /@ d
    ]

MidiGetNotes[mx_Midi] :=
  Module[{m = MidiFixNoteOff[MidiSetState[mx, {MidiShape -> MidiFile, MidiTiming -> MidiAbsolute}]], on, off},
    Function[t,
      (* get note-on as {{ch, p, tick, v} ...} *)
      on = Cases[t, {tick$_, {MidiNoteOn, {c$_, p$_, v$_}}} -> {c$, p$, tick$, v$}];
      (* get note-off as {{ch, p, tick, v} ...} *)
      off = Cases[t, {tick$_, {MidiNoteOff, {c$_, p$_, v$_}}} -> {c$, p$, tick$, v$}];
      (* set to {{{on, off}, {ch, p, v}} ...} *)
      MapThread[{{#1[[3]], #2[[3]]}, {#1[[1]], #1[[2]], #1[[4]]}} &, {on, off}]
      ] /@ m[[2]]
    ]

MidiGetPitchRange[m_Midi] := {Min[#],Max[#]}&[Flatten[MidiGetPitchRanges[m]]]

MidiGetPitchRanges[m_Midi] :=
  Module[{c=MidiGetChannels[m],n=MidiGetNotes[m],p,mi,ma},
    MapThread[
      Function[{cc,nn},
        (
          p = Cases[nn,{{_,_},{#,p$_,_}}->p$];
          {Min[p],Max[p]}
          )& /@ cc
        ],
      {c,n}
      ]
    ]

MidiGetQPMLow[m_Midi] :=
  Module[{ma = MidiSetState[m, {MidiTiming->MidiAbsolute}],u},
    If[0<Length[ma[[2]]],
      (* get all tempo events as {{timing,data}...} *)
      u = Cases[ma[[2, 1]], {t$_, {MidiTempo, u$_}} -> {t$, u$}];
      u,
      {}
      ]
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
    ] /; MidiGetShape[m] === MidiMelody

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

MidiGetState[m_Midi] := MidiGetState[GetInfo[m]]

MidiGetTickToSecFunction[m_Midi] :=
  Module[
    {
      tpq = MidiGetTPQ[m],
      u = MidiGetQPM[m],
      k, sa, sd, ta, td, f
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

MidiMix[m_Midi] := m

MidiNoteOff = 0;

MidiNoteOn = 1;

MidiOfSilence[t_,d_, opts___] :=
  Module[{m = MidiEmpty},
    m = MidiSetState[m,{MidiTimeUnit->(MidiTimeUnit/.{opts}/.{MidiTimeUnit->MidiGetTimeUnit[MidiEmpty]})}];
    m = MidiSetTPQ[m,MidiTPQ/.{opts}/.Options[Midi]];
    m = Midi[
      m[[1]],
      Table[{{d,EOT}},{t}]
      ];
    m = MidiSetQPM[m,{{0,MidiQPM/.{opts}/.Options[Midi]}}];
    m
    ]

MidiPar[mx:{_Midi,_Midi...}, opts___] :=
  Module[{m=MidiSetState[#,{MidiShape->MidiFile,MidiTiming->MidiAbsolute,MidiTimeUnit->MidiSec}]&/@mx,d},
    d = Flatten[Join[{m[[1,2]]},Drop[#[[2]],1]&/@Drop[m,1]],1];
    MidiSetState[
      MidiFixEOT[Midi[m[[1,1]],d],True],
      MidiGetState[mx[[1]]]
      ]
    ]

MidiPatternData = ({MidiPatternDataAtom...}|MidiPatternDataAtom|_DataTie);
MidiPatternDataAtom = (_Integer|DataNoValue|DataAnyValue|_DataTie);
MidiPatternChord = {{{MidiPatternType,{MidiPatternTrack...}},{{MidiPatternTiming,{MidiPatternData...}}...}}...};
MidiPatternFile = {{{MidiPatternTiming,{MidiPatternType,MidiPatternData}}...}...};
MidiPatternInfo = {(
  (MidiQPM->(_Integer|_Real)) |
  (MidiFileFormat->(0|1)) |
  (MidiShape->(MidiFile|MidiMelody|MidiChord)) |
  (MidiTiming->(MidiDelta|MidiAbsolute)) |
  (MidiTimeUnit->(MidiTick|MidiSec|MidiMilliSec)) |
  (MidiTPQ->_Integer)
  )...};
MidiPatternMelody = {{{MidiPatternType,MidiPatternTrack},{{MidiPatternTiming,MidiPatternData}...}}...};
MidiPatternMidi = Midi[MidiPatternInfo,MidiPatternMusic];
MidiPatternMusic = (MidiPatternFile|MidiPatternMelody|MidiPatternChord);
MidiPatternTiming = (_Integer|_Real);
MidiPatternTrack = (_Integer|{_Integer,_Integer});
MidiPatternType = (MidiNoteOff|MidiNoteOn|2|MidiControlChange|4|5|6|7|MidiSysX0|MidiSysX7|{MidiMeta,_Integer});

MidiPitchCenter[m_Midi] := MidiPitchCenter[m, 63]

MidiPitchCenter[m_Midi, c_] := MidiSetPitch[m,Evaluate[#+(c-IntegerPart[Mean[MidiGetPitchRange[m]]])]&]

MidiPitchFlip[m_Midi] := MidiPitchFlip[m,Mean[MidiGetPitchRange[m]]]

MidiPitchFlip[m_Midi, o_] := MidiSetPitch[m,(o-(#-o))&]

MidiPitchShift[m_Midi, s_] := MidiSetPitch[m,(#+s)&]

MidiQ[expr_] :=
  MatchQ[expr,
    Midi[_?OptionQ,
      {{{_, {_, _}} ...} ...} |
      {{{_, _}, {{_, _} ...}} ...} |
      {{{_, {__}}, {{_, {__}} ...}} ...}
      ]
    ]

MidiRemChords[mx_Midi] :=
  Module[{m = MidiSetState[mx,{MidiShape->MidiChord}],d},
    d = Sort[m[[2]]];
    If[0<Length[d] && d[[1,1,1]]===MidiNoteOn,d=Drop[d,1]];
    MidiSetState[
      Midi[
        m[[1]],
        d
        ],
      MidiGetState[mx]
      ]
    ]

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

MidiRemMelodies[mx_Midi] :=
  Module[{m = MidiSetState[mx,{MidiShape->MidiMelody}]},
    MidiSetState[
      Midi[
        m[[1]],
        Cases[m[[2]],{{t$,_},_}/;(t$_!=MidiNoteOn)]
        ],
      MidiGetState[mx]
      ]
    ]

MidiRemNotes[m_Midi] := MidiRemEvents[m,{_, {(MidiNoteOn | MidiNoteOff), _}}]

MidiRemQPM[m_Midi] := MidiRemEvents[m,{_, {MidiTempo, _}}]

MidiSeq[mx:{_Midi,_Midi...}, opts___] :=
  Module[{m=MidiSetState[#,{MidiShape->MidiFile,MidiTiming->MidiAbsolute,MidiTimeUnit->MidiSec}]&/@mx,d,s=0,t,q,x},
    x=Max @@ (Length[#[[2]]]&/@m);
    d=Reap[
      Scan[(* per midi *)
        (
          q=GetDuration[#];
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
      MidiFixEOT[Midi[m[[1,1]],d],True],
      MidiGetState[mx[[1]]]
      ]
    ]

MidiSetChords[m_Midi,n:{{{_,_}...},{_?ChordQ...}}]:=
  MidiSetState[MidiAddChords[MidiRemChords[MidiSetState[m,{MidiShape->MidiChord,MidiTiming->MidiDelta}]],n],MidiGetState[m]]

MidiSetMelodies[m_Midi,n:{{{_,_},_?MelodyQ}...}]:=
  MidiSetState[MidiAddMelodies[MidiRemMelodies[MidiSetState[m,{MidiShape->MidiMelody,MidiTiming->MidiDelta}]],n],MidiGetState[m]]

MidiSetNotes[m_Midi, n : {{{{_,_},{_,_,_}}...}...}]:=
  MidiSetState[MidiAddNotes[MidiRemNotes[MidiSetState[m,{MidiShape->MidiFile,MidiTiming->MidiAbsolute}]],n],MidiGetState[m]]

MidiSetPitch[m_Midi, f_] :=
  Module[{n = MidiGetNotes[m]},
    n = ({#[[1]], {#[[2, 1]], f[#[[2, 2]]], #[[2, 3]]}} & /@ #) & /@ n;
    MidiSetNotes[m, n]
    ]

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
      am = MidiFixNoteOff[m],
      tr, trm, tron, troff, trn,
      (* get a list of durations, one for each track *)
      d = GetDurations[m],
      rtf = MidiMelodyReleaseTimeFunction /. {opts} /. {MidiMelodyReleaseTimeFunction -> (0&)}
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
            (* prepend a DataNoValue at 0 if tick 0 is absent *)
            If[r[[1, 2]] != 0, r = Prepend[r, {{ty, DataNoValue}, 0}]];
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
    (* make monophonic voices, set to {{{{{on, off}, {p, v}, {track, ch}}...}...}...} *)
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
                  Sow[{{tim[[v,1]],#[[1,1]]},{DataNoValue,DataNoValue},#[[3]]},v]
                  ];
                (* calculate release-time *)
                rt = rtf[#[[3,1]],{#[[1]],Prepend[#[[2]],#[[3,2]]]}];
                (* update tim *)
                tim[[v]]={#[[1,2]]+rt,v};
                (* add the note *)
                Sow[#,v];
                ]&,
              Sort[trni]
              ];
            ];
          ][[2]]
        ] /@ trn;
    (* set to {{{{on, off}, {p, v}, {track, ch, voice}}...}...} *)
    trn = Flatten[trn, 1];
    (* set to {{{MidiNoteOn, {track, ch, voice}}, {{({p, v}|{DataNoValue, DataNoValue}), tick}...}}...} *)
    trn = {{MidiNoteOn,#[[1, 3]]}, Flatten[{{{#[[2, 1]], #[[2, 2]]}, #[[1, 1]]}, {{DataNoValue, DataNoValue}, #[[1, 2]]}} & /@ #, 1]} & /@ trn;
    (* sort *)
    (*trn = {#[[1]], Sort[#[[2]], OrderedQ[{#1[[2]], #2[[2]]}] &]} & /@ trn;*)
    (* add a preceding rest if necesary, is it ever? *)
    trn = {#[[1]], If[#[[2, 1, 2]] == 0, #[[2]], Prepend[#[[2]], {{DataNoValue, DataNoValue}, 0}]]} & /@ trn;
    (* add a trailing dummy-rest *)
    trn = {#[[1]], Append[#[[2]], {{DataNoValue, DataNoValue}, d[[#[[1,2,1]]]]}]} & /@ trn;
    (* transpose to {{type, {{{p, v}...}, {tick...}}}...} *)
    trn = {#[[1]], Transpose[#[[2]]]} & /@ trn;
    (* switch to durations and chop off the dummy-rest {{type, {{duration...}, {data...}}}...} *)
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
      Sort[m[[1]] /. (MidiShape -> _) -> (MidiShape -> MidiMelody) /. (MidiTiming -> _) -> (MidiTiming -> MidiDelta)],
      Sort[tr]
      ]
    ] /; MatchQ[MidiGetState[m],FS[{MidiShape->MidiFile,MidiTiming->MidiAbsolute}]]&&
         Complement[s,MidiGetState[m]]=={MidiShape->MidiMelody,MidiTiming->MidiDelta}

MidiSetStateLow[m_Midi,s_, opts___] :=
  Module[{trm, trn, trx, tr, tc},
    (* get all meta and sysx as {{{type, track}, {{end-tick, data}...}}...} *)
    trm = Select[m[[2]], MatchQ[#[[1, 1]], MidiSysX0 | MidiSysX7 | {MidiMeta,_}] &];
    (* set to {{{type, track}, {{tick, data}...}}...} *)
    trm = {#[[1]], Module[{td=Transpose[#[[2]]]},Transpose[{Prepend[td[[1]],0],Append[td[[2]],DataNoValue]}]]} & /@ trm;
    (* fix EOT *)
    trm = If[#[[1, 1]] == MidiEOT, ReplacePart[#, {}, {2, -1, 2}], #] & /@ trm;
    (* remove all DataNoValue *)
    trm = {#[[1]], Select[#[[2]], ! DataNoValueQ[#[[2]]] &]} & /@ trm;
    (* set to {{track, {{tick, {type, data}}...}}...} *)
    trm = {#[[1, 2]], Function[e, {e[[1]], {#[[1, 1]], e[[2]]}}] /@ #[[2]]} & /@ trm;

    (* get all notes as {{{type, track}, {{end-tick, data}...}}...} *)
    trn = Select[m[[2]], MatchQ[#[[1, 1]], MidiNoteOn] &];
    (* set to {{{track,c}, {{tick, end-tick, data}...}}...} *)
    trn = {{#[[1, 2, 1]],#[[1,2,2]]}, Module[{td=Transpose[#[[2]]]},Transpose[{Prepend[Drop[td[[1]],-1],0],td[[1]],td[[2]]}]]} & /@ trn;
    (* set to {{track, {{tick, {type, data}}...}}...} *)
    trn = {
      #[[1,1]],
      Flatten[
        Cases[
          #[[2]],
          {t$_, e$_, d$:{p$_,v$_}} /; (!DataAnyValueQ[d$] && !DataNoValueQ[d$]) -> {
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

    If[0<Length[trx],
      (* get highest track nr *)
      tc = trx[[-1,1]];

      (* put every event into the right track *)
      tr = Reap[Scan[Function[t,Scan[Sow[#,t[[1]]]&, t[[2]]]],trx],Range[tc]][[2]];
      tr = If[# === {}, {}, #[[1]]]& /@ tr;

      (* sort each track *)
      tr = Sort /@ tr,
      tr = {}
      ];

    Midi[
      Sort[m[[1]] /. (MidiShape -> _) -> (MidiShape -> MidiFile)],
      tr
      ]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->MidiMelody,MidiTiming->MidiAbsolute}]]&&
         Complement[s,MidiGetState[m]]=={MidiShape->MidiFile}

MidiSetStateLowOld[m_Midi,s_, opts___]:= (* todo, use ParOfSeqToSeqOfPar, (done below), then delete this one when it works *)
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
                Append[#[[2]],{tt - vtt,{DataNoValue,DataNoValue}}],
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
                  DataTie[#[[2]]],
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
    ] /; MatchQ[MidiGetState[m],FS[{MidiShape->MidiMelody,MidiTimeUnit->MidiTick,MidiTiming->MidiDelta}]]&&(* DONT ALLOW MidiSec HERE!!! *)
         Complement[s,MidiGetState[m]]=={MidiShape->MidiChord}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Module[{g},
    (* group each type together, set g to {{{{type,track},{{timing,data}...}}...}...} *)
    g = Split[Sort[m[[2]]],(#1[[1,1]]===#2[[1,1]])&];
    (* set to {{{type,{track...}},{{timing,{data...}}...}}...} *)
    g = {
      {#[[1,1,1]],#[[1,2]]& /@ #},
      ParOfSeqToSeqOfPar[#[[2]]& /@ #]
      }& /@ g;
    Midi[
      Sort[m[[1]] /. (MidiShape -> _) -> (MidiShape -> MidiChord)],
      g
      ]
    ] /; MatchQ[MidiGetState[m],FS[{MidiShape->MidiMelody,MidiTimeUnit->MidiTick,MidiTiming->MidiDelta}]]&&(* DONT ALLOW MidiSec HERE!!! *)
         Complement[s,MidiGetState[m]]=={MidiShape->MidiChord}

MidiSetStateLowOld[m_Midi,s_, opts___]:= (* todo, use SeqOfParToParOfSeq, (done below), then delete this one when it works *)
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
              If[DataTieQ[td[[2]]],
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
      Sort[m[[1]] /. (MidiShape -> _) -> (MidiShape -> MidiMelody)],
      g
      ]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->MidiChord, MidiTiming->MidiDelta}]]&&
         Complement[s,MidiGetState[m]]=={MidiShape->MidiMelody}

MidiSetStateLow[m_Midi,s_, opts___]:=
  Module[{g=m[[2]]},
    (* g is {{{type,{track...}},{{timing,{data...}}...}}...}, remember? *)
    g = Transpose[{
      Function[t,{#[[1,1]],t}] /@ #[[1,2]],
      SeqOfParToParOfSeq[#[[2]]]
      }]& /@ g;
    g = Flatten[g,1];
    Midi[
      Sort[m[[1]] /. (MidiShape -> _) -> (MidiShape -> MidiMelody)],
      g
      ]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->MidiChord, MidiTiming->MidiDelta}]]&&
         Complement[s,MidiGetState[m]]=={MidiShape->MidiMelody}

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
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->(MidiMelody|MidiChord), MidiTimeUnit->MidiSec,MidiTiming->MidiAbsolute}]]&&
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
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->(MidiMelody|MidiChord), MidiTimeUnit->MidiTick,MidiTiming->MidiAbsolute}]]&&
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
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->(MidiMelody|MidiChord), MidiTimeUnit->MidiSec}]]&&
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
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->(MidiMelody|MidiChord), MidiTimeUnit->MidiMilliSec}]]&&
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
    {#[[1]],Module[{td=Transpose[#[[2]]]},Transpose[{Drop[DeltasToValues[td[[1]]],1],td[[2]]}]]}& /@ m[[2]]
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->(MidiMelody|MidiChord), MidiTiming->MidiDelta}]]&&
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
    ] /; MatchQ[MidiGetState[m], FS[{MidiShape->(MidiMelody|MidiChord), MidiTiming->MidiAbsolute}]]&&
         Complement[s,MidiGetState[m]]=={MidiTiming->MidiDelta}

MidiSetTime[mx_Midi, f_] :=
  Module[{m = MidiSetState[mx, {MidiShape->MidiFile, MidiTiming->MidiAbsolute}]},
    MidiSetState[
      Midi[
        m[[1]],
        ({f[#[[1]]],#[[2]]}& /@ #)& /@ m[[2]]
        ],
      MidiGetState[mx]
      ]
    ]

MidiSetTPQ[m_Midi,tpq_]:=
  Midi[
    Sort[
      If[(MidiTPQ /. m[[1]]) === MidiTPQ,
        Append[m[[1]],MidiTPQ->tpq],
        m[[1]]/.(MidiTPQ->_)->(MidiTPQ->tpq)
        ]
      ],
    m[[2]]
    ]

(* if you change this, you must run CalcMidiStateRoutes, at least twice (bug?) *)
MidiStates = {
  {MidiShape, {MidiFile, MidiMelody, MidiChord}},
  {MidiTimeUnit, {MidiTick, MidiSec, MidiMilliSec}},
  {MidiTiming, {MidiDelta, MidiAbsolute}}
  };

MidiStatesExpanded = MidiExpandStates[MidiStates]

(* if you change this, you must run CalcMidiStateRoutes, at least twice (bug?) *)
MidiStatePaths = {
  (* Shape *)
  {{MidiShape -> MidiFile, MidiTiming -> MidiAbsolute}, {MidiShape -> MidiMelody, MidiTiming->MidiDelta}},
  {{MidiShape -> MidiMelody, MidiTiming -> MidiAbsolute}, {MidiShape -> MidiFile}},
  {{MidiShape -> MidiMelody, MidiTimeUnit -> MidiTick, MidiTiming -> MidiDelta}, {MidiShape -> MidiChord}},
  {{MidiShape -> MidiChord, MidiTiming -> MidiDelta}, {MidiShape -> MidiMelody}},
  (* TimeUnit *)
  {{MidiShape -> MidiFile, MidiTimeUnit -> MidiSec, MidiTiming -> MidiAbsolute}, {MidiTimeUnit -> MidiTick}},
  {{MidiShape -> (MidiMelody | MidiChord), MidiTimeUnit -> MidiSec, MidiTiming -> MidiAbsolute}, {MidiTimeUnit -> MidiTick}},
  {{MidiShape -> MidiFile, MidiTimeUnit -> MidiTick, MidiTiming -> MidiAbsolute}, {MidiTimeUnit -> MidiSec}},
  {{MidiShape -> (MidiMelody | MidiChord), MidiTimeUnit -> MidiTick, MidiTiming -> MidiAbsolute}, {MidiTimeUnit -> MidiSec}},
  {{MidiShape -> MidiFile, MidiTimeUnit -> MidiSec}, {MidiTimeUnit -> MidiMilliSec}},
  {{MidiShape -> (MidiMelody | MidiChord), MidiTimeUnit -> MidiSec}, {MidiTimeUnit -> MidiMilliSec}},
  {{MidiShape -> MidiFile, MidiTimeUnit -> MidiMilliSec}, {MidiTimeUnit -> MidiSec}},
  {{MidiShape -> (MidiMelody | MidiChord), MidiTimeUnit -> MidiMilliSec}, {MidiTimeUnit -> MidiSec}},
  (* Timing *)
  {{MidiShape -> MidiFile, MidiTiming -> MidiDelta}, {MidiTiming -> MidiAbsolute}},
  {{MidiShape -> MidiFile, MidiTiming -> MidiAbsolute}, {MidiTiming -> MidiDelta}},
  {{MidiShape -> (MidiMelody | MidiChord), MidiTiming -> MidiDelta}, {MidiTiming -> MidiAbsolute}},
  {{MidiShape -> (MidiMelody | MidiChord), MidiTiming -> MidiAbsolute}, {MidiTiming -> MidiDelta}}
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

MidiTimeBend[m_Midi, b_] := MidiSetTime[m,(#*b)&]

MidiTimeFlip[mx_Midi] :=
  Module[{m = MidiSetState[MidiFixEOT[mx,True], {MidiShape->MidiMelody,MidiTiming->MidiDelta}]},
    MidiSetState[
      Midi[
        m[[1]],
        {#[[1]],Reverse[#[[2]]]}&/@m[[2]]
        ],
      MidiGetState[mx]
      ]
    ]

MidiTimeShift[m_Midi, s_] := MidiSetTime[m,(#+s)&]

MidiTimeSignature = {MidiMeta,16^^58};

MidiUnPar[mx_Midi] :=
  Module[{m=MidiSetState[mx,{MidiShape->MidiFile}]},
    Table[
      MidiSetState[Midi[m[[1]],{m[[2,1]],m[[2,t]]}],MidiGetState[mx]],
      {t,2,Length[m[[2]]]}
      ]
    ]

MidiUnSeq[m_Midi] := m

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
  GetData,
  GetDuration,
  GetDurations,
  GetInfo
  ];

Protect[
  Midi,
  MidiAbsolute,
  MidiAddChords,
  MidiAddEvents,
  MidiAddMelodies,
  MidiAddNotes,
  MidiAddQPM,
  MidiChord,
  MidiControlChange,
  MidiDelta,
  MidiEmpty,
  MidiEOT,
  MidiExportSMF,
  MidiFile,
  MidiFileFormat,
  MidiFixEOT,
  MidiFixNoteOff,
  MidiFixTime,
  MidiGetChannels,
  MidiGetChords,
  MidiGetMelodies,
  MidiGetNotes,
  MidiGetPitchRange,
  MidiGetPitchRanges,
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
  MidiMelody,
  MidiMelodyReleaseTimeFunction,
  MidiMeta,
  MidiMilliSec,
  MidiMix,
  MidiNoteOff,
  MidiNoteOn,
  MidiOfSilence,
  MidiPar,
  MidiPatternData,
  MidiPatternChord,
  MidiPatternFile,
  MidiPatternInfo,
  MidiPatternMelody,
  MidiPatternMidi,
  MidiPatternMusic,
  MidiPatternTiming,
  MidiPatternTrack,
  MidiPatternType,
  MidiPitchCenter,
  MidiPitchFlip,
  MidiPitchShift,
  MidiQ,
  MidiQPM,
  MidiRemChords,
  MidiRemEvents,
  MidiRemMelodies,
  MidiRemNotes,
  MidiRemQPM,
  MidiSec,
  MidiSeq,
  MidiSetChords,
  MidiSetMelodies,
  MidiSetNotes,
  MidiSetPitch,
  MidiSetQPM,
  MidiSetState,
  MidiSetStateLow,
  MidiSetTime,
  MidiSetTPQ,
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
  MidiTimeBend,
  MidiTimeFlip,
  MidiTimeShift,
  MidiTimeSignature,
  MidiTimeUnit,
  MidiTiming,
  MidiTPQ,
  MidiUnPar,
  MidiUnSeq
  ];

EndPackage[ ]


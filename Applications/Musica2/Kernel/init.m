(* :Title: Master Declarations File for Musica2 *)

(* :Summary: This file contains declarations of all the major symbols contained in files in this directory.
When loaded, it sets up the symbols with attribute Stub, so the correct package will be loaded when the symbol is called. *)

(* :Author: This file was created by the function Musica2`Setup`MakeInitDotEm[], written by Bo C. Herlin *)

(* :History: File created 2004-8-14 at 17:37 *)

If[!MemberQ[$Packages,"Musica2`"],
  System`Private`p = Unprotect[$Packages];
  PrependTo[$Packages,"Musica2`"];
  Protect @@ System`Private`p  
];

DeclarePackage["Musica2`EventList`",
{"EventListToMidi", "MidiToEventList"}
];

DeclarePackage["Musica2`Midi`",
{"Midi", "MidiAbsolute", "MidiAddEOT", "MidiAddEvents", "MidiAddNotes", 
 "MidiAddQPM", "MidiChord", "MidiControlChange", "MidiDataAnyValue", 
 "MidiDataAnyValueQ", "MidiDataNoValue", "MidiDataNoValueQ", "MidiDelta", 
 "MidiEmpty", "MidiEOT", "MidiExpandStatePaths", "MidiExpandStates", 
 "MidiExportSMF", "MidiFile", "MidiFileFormat", "MidiGetChannels", 
 "MidiGetDuration", "MidiGetDurations", "MidiGetInfo", "MidiGetNotes", 
 "MidiGetQPM", "MidiGetSecToTickFunction", "MidiGetShape", "MidiGetState", 
 "MidiGetTickToSecFunction", "MidiGetTimeUnit", "MidiGetTiming", 
 "MidiGetTPQ", "MidiImportSMF", "MidiKeySignature", "MidiMeta", 
 "MidiMilliSec", "MidiNormalizeNoteOff", "MidiNoteOff", "MidiNoteOn", 
 "MidiPar", "MidiPatternChord", "MidiPatternData", "MidiPatternFile", 
 "MidiPatternInfo", "MidiPatternMidi", "MidiPatternMusic", 
 "MidiPatternTiming", "MidiPatternTrack", "MidiPatternType", 
 "MidiPatternVoice", "MidiQPM", "MidiRemEvents", "MidiRemNotes", 
 "MidiRemQPM", "MidiSec", "MidiSeq", "MidiSetNotes", "MidiSetQPM", 
 "MidiSetState", "MidiSetStateLow", "MidiShape", "MidiStatePaths", 
 "MidiStatePathsExpanded", "MidiStateRoutes", "MidiStates", 
 "MidiStatesExpanded", "MidiSysX0", "MidiSysX7", "MidiTempo", "MidiTick", 
 "MidiTie", "MidiTieQ", "MidiTimeSignature", "MidiTimeUnit", "MidiTiming", 
 "MidiTPQ", "MidiUnTie", "MidiVoice", "MidiVoiceReleaseTimeFunction"}
];

DeclarePackage["Musica2`Setup`",
{"CalcMidiStateRoutes", "MakeInitDotEm"}
];

DeclarePackage["Musica2`Sound`",
{"FuncToList", "ListToFunc", "SoundChannelCount", "SoundDuration", 
 "SoundFuncQ", "SoundGetChannelCount", "SoundGetDuration", "SoundGetFunc", 
 "SoundGetInfo", "SoundGetList", "SoundGetSampleCount", "SoundGetSampleRate", 
 "SoundImportWav", "SoundListQ", "SoundLoop", "SoundMakeFunc", 
 "SoundMakeList", "SoundMix", "SoundOfSilence", "SoundPar", 
 "SoundPitchShift", "SoundSampleCount", "SoundSeq", "SoundSetDuration", 
 "SoundType", "SoundUnPar", "SoundUnSeq", "Zound"}
];

DeclarePackage["Musica2`Utils`",
{"DeltasToValues", "FunctionQ", "MakeNestedIfs", "NormalizeList", 
 "UnCompile", "ValuesToDeltas"}
];

Null

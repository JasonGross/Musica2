(* :Title: Master Declarations File for Musica2 *)

(* :Summary: This file contains declarations of all the major symbols contained in files in this directory.
When loaded, it sets up the symbols with attribute Stub, so the correct package will be loaded when the symbol is called. *)

(* :Author: This file was created by the function Musica2`Utils`MakeInitDotEm[], written by Bo C. Herlin *)

(* :History: File created 2004-8-8 at 13:47 *)

If[!MemberQ[$Packages,"Musica2`"],
  System`Private`p = Unprotect[$Packages];
  PrependTo[$Packages,"Musica2`"];
  Protect @@ System`Private`p  
];

DeclarePackage["Musica2`EventList`",
{"EventListToMidi", "MidiToEventList"}
];

DeclarePackage["Musica2`Midi`",
{"Midi", "MidiAbsolute", "MidiAddEOT", "MidiChord", "MidiControlChange", 
 "MidiDelta", "MidiEOT", "MidiEqualizeEOT", "MidiExpandStatePaths", 
 "MidiExpandStates", "MidiExportSMF", "MidiFile", "MidiFileFormat", 
 "MidiGetChannels", "MidiGetDuration", "MidiGetDurations", "MidiGetInfo", 
 "MidiGetQPM", "MidiGetSecToTickFunction", "MidiGetShape", "MidiGetState", 
 "MidiGetTickToSecFunction", "MidiGetTimeUnit", "MidiGetTiming", 
 "MidiGetTPQ", "MidiImportSMF", "MidiKeySignature", "MidiMeta", 
 "MidiNormalizeNoteOff", "MidiNoteOff", "MidiNoteOn", "MidiPatternChord", 
 "MidiPatternData", "MidiPatternFile", "MidiPatternInfo", "MidiPatternMidi", 
 "MidiPatternMusic", "MidiPatternTiming", "MidiPatternTrack", 
 "MidiPatternType", "MidiPatternVoice", "MidiQPM", "MidiSec", "MidiSetShape", 
 "MidiSetState", "MidiSetStateLow", "MidiSetTimeUnit", "MidiSetTiming", 
 "MidiShape", "MidiStatePaths", "MidiStatePathsExpanded", "MidiStateRoutes", 
 "MidiStates", "MidiStatesExpanded", "MidiSysX0", "MidiSysX7", "MidiTempo", 
 "MidiTick", "MidiTie", "MidiTimeSignature", "MidiTimeUnit", "MidiTiming", 
 "MidiTPQ", "MidiVoice", "MidiVoiceReleaseTimeFunction"}
];

DeclarePackage["Musica2`Setup`",
{"CalcMidiStateRoutes", "MakeInitDotEm"}
];

DeclarePackage["Musica2`Sound`",
{"SoundChannelCount", "SoundDuration", "SoundFuncQ", "SoundGetChannelCount", 
 "SoundGetDuration", "SoundGetFunc", "SoundGetInfo", "SoundGetList", 
 "SoundGetSampleCount", "SoundGetSampleRate", "SoundImportWav", "SoundListQ", 
 "SoundLoop", "SoundMakeFunc", "SoundMakeList", "SoundMix", "SoundOfSilence", 
 "SoundPar", "SoundSampleCount", "SoundSeq", "SoundSmooth", "SoundType", 
 "SoundUnPar", "SoundUnSeq", "Zound"}
];

DeclarePackage["Musica2`Utils`",
{"DeltasToValues", "Func1ListQ", "Func1Normalize", "Func1Q", "Func1ToList", 
 "FunctionQ", "ListToFunc1", "MakeNestedIfs", "ReArg1", "UnCompile", 
 "ValuesToDeltas"}
];

Null

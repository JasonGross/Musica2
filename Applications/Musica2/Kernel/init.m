(* :Title: Master Declarations File for Musica2 *)

(* :Summary: This file contains declarations of all the major symbols contained in files in this directory.
When loaded, it sets up the symbols with attribute Stub, so the correct package will be loaded when the symbol is called. *)

(* :Author: This file was created by the function Musica2`Setup`MakeInitDotEm[], written by Bo C. Herlin *)

(* :History: File created 2004-8-23 at 12:56 *)

If[!MemberQ[$Packages,"Musica2`"],
  System`Private`p = Unprotect[$Packages];
  PrependTo[$Packages,"Musica2`"];
  Protect @@ System`Private`p  
];

DeclarePackage["Musica2`EventList`",
{"EventListToMidi", "MidiToEventList"}
];

DeclarePackage["Musica2`Midi`",
{"Midi", "MidiAbsolute", "MidiAddChords", "MidiAddEOT", "MidiAddEvents", 
 "MidiAddNotes", "MidiAddQPM", "MidiAddVoices", "MidiChord", 
 "MidiControlChange", "MidiDataAnyValue", "MidiDataAnyValueQ", 
 "MidiDataNoValue", "MidiDataNoValueQ", "MidiDelta", "MidiEmpty", "MidiEOT", 
 "MidiExpandStatePaths", "MidiExpandStates", "MidiExportSMF", "MidiFile", 
 "MidiFileFormat", "MidiFixEOT", "MidiFixNoteOff", "MidiFixTime", 
 "MidiGetChannels", "MidiGetChords", "MidiGetDuration", "MidiGetDurations", 
 "MidiGetInfo", "MidiGetNotes", "MidiGetPitchRange", "MidiGetPitchRanges", 
 "MidiGetQPM", "MidiGetSecToTickFunction", "MidiGetShape", "MidiGetState", 
 "MidiGetTickToSecFunction", "MidiGetTimeUnit", "MidiGetTiming", 
 "MidiGetTPQ", "MidiGetVoices", "MidiImportSMF", "MidiKeySignature", 
 "MidiMeta", "MidiMilliSec", "MidiNormalizeNoteOff", "MidiNoteOff", 
 "MidiNoteOn", "MidiOfSilence", "MidiPar", "MidiPatternChord", 
 "MidiPatternData", "MidiPatternFile", "MidiPatternInfo", "MidiPatternMidi", 
 "MidiPatternMusic", "MidiPatternTiming", "MidiPatternTrack", 
 "MidiPatternType", "MidiPatternVoice", "MidiPitchChange", "MidiPitchFlip", 
 "MidiPitchShift", "MidiQPM", "MidiRemChords", "MidiRemEvents", 
 "MidiRemNotes", "MidiRemQPM", "MidiRemVoices", "MidiSec", "MidiSeq", 
 "MidiSetChords", "MidiSetNotes", "MidiSetPitch", "MidiSetQPM", 
 "MidiSetState", "MidiSetStateLow", "MidiSetTime", "MidiSetTPQ", 
 "MidiSetVoices", "MidiShape", "MidiStatePaths", "MidiStatePathsExpanded", 
 "MidiStateRoutes", "MidiStates", "MidiStatesExpanded", "MidiSysX0", 
 "MidiSysX7", "MidiTempo", "MidiTick", "MidiTie", "MidiTieQ", "MidiTimeBend", 
 "MidiTimeChange", "MidiTimeFlip", "MidiTimeShift", "MidiTimeSignature", 
 "MidiTimeUnit", "MidiTiming", "MidiTPQ", "MidiUnTie", "MidiVoice", 
 "MidiVoiceReleaseTimeFunction"}
];

DeclarePackage["Musica2`MidiPlay`",
{"MidiExportWav", "MidiPlay", "MidiToSound", "SoundBySample", "SoundBySine"}
];

DeclarePackage["Musica2`Setup`",
{"CalcMidiStateRoutes", "MakeInitDotEm"}
];

DeclarePackage["Musica2`Sound`",
{"FuncToList", "ListToFunc", "SoundChannelCount", "SoundDuration", 
 "SoundExportWav", "SoundFuncQ", "SoundGetChannelCount", "SoundGetDuration", 
 "SoundGetFunc", "SoundGetInfo", "SoundGetList", "SoundGetSampleCount", 
 "SoundGetSampleRate", "SoundImportWav", "SoundListQ", "SoundLoop", 
 "SoundMakeFunc", "SoundMakeList", "SoundMix", "SoundOfSilence", "SoundPar", 
 "SoundPitchShift", "SoundSampleCount", "SoundSeq", "SoundSetDuration", 
 "SoundType", "SoundUnPar", "SoundUnSeq", "Zound"}
];

DeclarePackage["Musica2`Utils`",
{"DeltasToValues", "FunctionQ", "MakeNestedIfs", "NormalizeList", 
 "UnCompile", "ValuesToDeltas"}
];

Null

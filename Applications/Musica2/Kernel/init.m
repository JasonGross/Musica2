(* :Title: Master Declarations File for Musica2 *)

(* :Summary: This file contains declarations of all the major symbols contained in files in this directory.
When loaded, it sets up the symbols with attribute Stub, so the correct package will be loaded when the symbol is called. *)

(* :Author: This file was created by the function Musica2`Setup`MakeInitDotEm[], written by Bo C. Herlin *)

(* :History: File created 2004-9-13 at 16:49 *)

If[!MemberQ[$Packages,"Musica2`"],
  System`Private`p = Unprotect[$Packages];
  PrependTo[$Packages,"Musica2`"];
  Protect @@ System`Private`p  
];

DeclarePackage["Musica2`Common`",
{"Duration", "GetData", "GetDuration", "GetDurations", "GetInfo", "SetData", 
 "SetDuration", "SetDurations", "SetInfo"}
];

DeclarePackage["Musica2`Midi`",
{"AddEvents", "AddTracks", "Event", "EventQ", "GetEvents", "GetTracks", 
 "Midi", "MidiAbsolute", "MidiAddChords", "MidiAddMelodies", "MidiAddNotes", 
 "MidiAddQPM", "MidiChord", "MidiControlChange", "MidiDelta", "MidiEmpty", 
 "MidiEOT", "MidiExportSMF", "MidiFile", "MidiFileFormat", "MidiFixEOT", 
 "MidiFixNoteOff", "MidiFixTime", "MidiGetChannels", "MidiGetChords", 
 "MidiGetMelodies", "MidiGetNotes", "MidiGetPitchRange", 
 "MidiGetPitchRanges", "MidiGetQPM", "MidiGetSecToTickFunction", 
 "MidiGetShape", "MidiGetState", "MidiGetTickToSecFunction", 
 "MidiGetTimeUnit", "MidiGetTiming", "MidiGetTPQ", "MidiImportSMF", 
 "MidiKeySignature", "MidiMelody", "MidiMelodyReleaseTimeFunction", 
 "MidiMeta", "MidiMilliSec", "MidiMix", "MidiNoteOff", "MidiNoteOn", 
 "MidiOfSilence", "MidiPar", "MidiPatternChord", "MidiPatternData", 
 "MidiPatternFile", "MidiPatternInfo", "MidiPatternMelody", 
 "MidiPatternMidi", "MidiPatternMusic", "MidiPatternTiming", 
 "MidiPatternTrack", "MidiPatternType", "MidiPitchCenter", "MidiPitchFlip", 
 "MidiPitchShift", "MidiQ", "MidiQPM", "MidiRemChords", "MidiRemEvents", 
 "MidiRemMelodies", "MidiRemNotes", "MidiRemQPM", "MidiSec", "MidiSeq", 
 "MidiSetChords", "MidiSetMelodies", "MidiSetNotes", "MidiSetPitch", 
 "MidiSetQPM", "MidiSetState", "MidiSetStateLow", "MidiSetTime", 
 "MidiSetTPQ", "MidiShape", "MidiStatePaths", "MidiStatePathsExpanded", 
 "MidiStateRoutes", "MidiStates", "MidiStatesExpanded", "MidiSysX0", 
 "MidiSysX7", "MidiTempo", "MidiTick", "MidiTimeBend", "MidiTimeFlip", 
 "MidiTimeShift", "MidiTimeSignature", "MidiTimeUnit", "MidiTiming", 
 "MidiTPQ", "MidiUnPar", "MidiUnSeq", "Track", "TrackQ"}
];

DeclarePackage["Musica2`MidiPlay`",
{"MidiExportWav", "MidiPlay", "MidiToSound", "SoundBySample", "SoundBySine"}
];

DeclarePackage["Musica2`Note`",
{"Chord", "ChordOfSilence", "ChordQ", "ChordsToMelodies", 
 "GetDurationCenter", "GetDurationGCD", "GetDurationLCM", "GetDurationMean", 
 "GetDurationRange", "GetNoteCount", "GetPitchCodeCenter", 
 "GetPitchCodeMean", "GetPitchCodeRange", "GetPitchCodes", 
 "GetVelocityCenter", "GetVelocityMean", "GetVelocityRange", "GetVelocitys", 
 "MelodiesToChords", "Melody", "MelodyOfSilence", "MelodyQ", "Note", "Scale", 
 "ScaleQ", "ScaleStepToPitchCode", "SetPitchCode", "SetPitchCodes", 
 "SetVelocity", "SetVelocitys", "Velocity"}
];

DeclarePackage["Musica2`Setup`",
{"CalcMidiStateRoutes", "ClearInitDotEm", "MakeInitDotEm", "ToDo"}
];

DeclarePackage["Musica2`Sound`",
{"FuncToList", "ListToFunc", "SoundChannelCount", "SoundExportWav", 
 "SoundFuncQ", "SoundGetChannelCount", "SoundGetFunc", "SoundGetList", 
 "SoundGetSampleCount", "SoundGetSampleRate", "SoundImportWav", "SoundListQ", 
 "SoundLoop", "SoundMakeFunc", "SoundMakeList", "SoundMix", "SoundMixStereo", 
 "SoundOfSilence", "SoundPar", "SoundPitchShift", "SoundSampleCount", 
 "SoundSeq", "SoundType", "SoundUnPar", "SoundUnSeq", "Zound"}
];

DeclarePackage["Musica2`Utils`",
{"DataAnyValue", "DataAnyValueQ", "DataNoValue", "DataNoValueQ", "DataTie", 
 "DataTieQ", "DataUnTie", "DeltasToValues", "FunctionQ", "MakeNestedIfs", 
 "NormalizeList", "ParOfSeqToSeqOfPar", "SeqOfParToParOfSeq", "ToDoString", 
 "UnCompile", "ValuesToDeltas"}
];

Null

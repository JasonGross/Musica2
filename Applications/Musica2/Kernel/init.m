(* :Title: Master Declarations File for Musica2 *)

(* :Summary: This file contains declarations of all the major symbols contained in files in this directory.
When loaded, it sets up the symbols with attribute Stub, so the correct package will be loaded when the symbol is called. *)

(* :Author: This file was created by the function Musica2`Setup`MakeInitDotEm[], written by Bo C. Herlin *)

(* :History: File created 2004-9-27 at 17:9 *)

If[!MemberQ[$Packages,"Musica2`"],
  System`Private`p = Unprotect[$Packages];
  PrependTo[$Packages,"Musica2`"];
  Protect @@ System`Private`p  
];

DeclarePackage["Musica2`Common`",
{"Content", "Convert", "Duration", "Mix", "Octave", "Par", "Play2", "Seq"}
];

DeclarePackage["Musica2`Midi`",
{"Event", "EventData", "EventQ", "EventTime", "EventType", "EventTypeEOT", 
 "EventTypeMeta", "EventTypeNoteOff", "EventTypeNoteOn", "EventTypeSysX0", 
 "EventTypeSysX7", "EventTypeTempo", "Midi", "MidiChannel", "MidiQ", 
 "MilliSecond", "QPM", "Tempo", "TempoFunction", "TempoQ", "TempoTime", 
 "TempoTrack", "TempoTrackQ", "Tick", "TimeUnit", "TPQ", "Track", "TrackQ"}
];

DeclarePackage["Musica2`Note`",
{"Chord", "ChordQ", "Counterpoint", "CounterpointQ", "Melody", "MelodyQ", 
 "ModeAeolian", "ModeDorian", "ModeIonian", "ModeLocrian", "ModeLydian", 
 "ModeMajor", "ModeMinor", "ModeMixolydian", "ModePhrygian", "Note", 
 "NoteDuration", "NoteFunction", "NotePlot", "NoteQ", "PcV", "PitchCode", 
 "Progression", "ProgressionQ", "Scale", "ScaleFunction", "ScaleQ", 
 "Velocity"}
];

DeclarePackage["Musica2`Setup`",
{"ClearInitDotEm", "MakeInitDotEm"}
];

DeclarePackage["Musica2`Sound`",
{"SampleCount", "Snippet", "SnippetQ", "SoundQ", "SoundType"}
];

DeclarePackage["Musica2`Type`",
{"ContainerQ", "CreateContainer", "CreateElement", "Data", "DataQ", 
 "ElementType", "Members", "Opts", "Pack", "Pos", "Tidy", "TypeQ", "UnPack", 
 "UnPackOpts"}
];

DeclarePackage["Musica2`Utils`",
{"AddOpts", "DataAnyValue", "DataAnyValueQ", "DataApply", "DataNoValue", 
 "DataNoValueQ", "DataPlainValueQ", "DataTie", "DataTieQ", "DataUnTie", 
 "DeltasToValues", "FunctionQ", "MakeNestedIfs", "NormalizeList", 
 "ParOfSeqToSeqOfPar", "RemOpts", "SeqOfParToParOfSeq", "UnCompile", 
 "ValuesToDeltas"}
];

Null

(* :Title: Master Declarations File for Musica2 *)

(* :Summary: This file contains declarations of all the major symbols contained in files in this directory.
When loaded, it sets up the symbols with attribute Stub, so the correct package will be loaded when the symbol is called. *)

(* :Author: This file was created by the function Musica2`Setup`MakeInitDotEm[], written by Bo C. Herlin *)

(* :History: File created 2004-9-21 at 18:15 *)

If[!MemberQ[$Packages,"Musica2`"],
  System`Private`p = Unprotect[$Packages];
  PrependTo[$Packages,"Musica2`"];
  Protect @@ System`Private`p  
];

DeclarePackage["Musica2`Common`",
{"Convert", "Duration", "Mix", "Par", "Seq"}
];

DeclarePackage["Musica2`Midi`",
{"Event", "EventData", "EventQ", "EventTime", "EventType", "EventTypeEOT", 
 "EventTypeMeta", "EventTypeNoteOff", "EventTypeNoteOn", "EventTypeSysX0", 
 "EventTypeSysX7", "EventTypeTempo", "Midi", "MidiQ", "MilliSecond", "QPM", 
 "Tempo", "TempoFunction", "TempoQ", "TempoTime", "TempoTrack", 
 "TempoTrackQ", "Tick", "TimeUnit", "TPQ", "Track", "TrackQ"}
];

DeclarePackage["Musica2`Note`",
{"Chord", "ChordQ", "Counterpoint", "CounterpointQ", "Melody", "MelodyQ", 
 "Note", "NoteDuration", "NoteFunction", "NoteQ", "PcV", "PitchCode", 
 "Progression", "ProgressionQ", "Velocity"}
];

DeclarePackage["Musica2`Setup`",
{"ClearInitDotEm", "MakeInitDotEm", "ToDo"}
];

DeclarePackage["Musica2`Sound`",
{"SampleCount", "Snippet", "SnippetData", "SnippetQ", "SoundQ", "SoundType"}
];

DeclarePackage["Musica2`Type`",
{"ContainerQ", "CreateContainer", "CreateElement", "Data", "DataQ", 
 "ElementType", "Members", "Opts", "Pack", "Pos", "Tidy", "TypeQ", "UnPack", 
 "UnPackOpts"}
];

DeclarePackage["Musica2`Utils`",
{"DataAnyValue", "DataAnyValueQ", "DataNoValue", "DataNoValueQ", "DataTie", 
 "DataTieQ", "DataUnTie", "DeltasToValues", "FunctionQ", "MakeNestedIfs", 
 "NormalizeList", "ParOfSeqToSeqOfPar", "SeqOfParToParOfSeq", "ToDoString", 
 "UnCompile", "ValuesToDeltas"}
];

Null

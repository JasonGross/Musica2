(* :Title: Master Declarations File for Musica2 *)

(* :Summary: This file contains declarations of all the major symbols contained in files in this directory.
When loaded, it sets up the symbols with attribute Stub, so the correct package will be loaded when the symbol is called. *)

(* :Author: This file was created by the function Musica2`Setup`MakeInitDotEm[], written by Bo C. Herlin *)

(* :History: File created 2004-11-28 at 12:43 *)

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
 "EventTypeKeySignature", "EventTypeMeta", "EventTypeNoteOff", 
 "EventTypeNoteOn", "EventTypeSysX0", "EventTypeSysX7", "EventTypeTempo", 
 "EventTypeTimeSignature", "Midi", "MidiChannel", "MidiQ", "MilliSecond", 
 "QPM", "Tempo", "TempoFunction", "TempoQ", "TempoTime", "TempoTrack", 
 "TempoTrackQ", "Tick", "TimeUnit", "TPQ", "Track", "TrackQ"}
];

DeclarePackage["Musica2`Naming`",
{"ChordNames", "ChordNaming", "ChordNamingFunction", "FlatsCount", 
 "FlatSymbols", "Helix", "HelixChordNaming", "HelixChordNamingQ", 
 "HelixNoteNaming", "HelixNoteNamingQ", "NameCodeStart", "Naming", 
 "NoteNames", "NoteNaming", "NoteNamingFunction", "PitchCodeInterval", 
 "PitchCodeStart", "PrepareString", "RootNaming", "SharpSymbols"}
];

DeclarePackage["Musica2`Note`",
{"Base", "Bass", "Chord", "ChordQ", "Code", "Counterpoint", "CounterpointQ", 
 "FigBass", "FigBassQ", "Intervals", "IntervalsQ", "Melody", "MelodyQ", 
 "ModeAeolian", "ModeDorian", "ModeIonian", "ModeLocrian", "ModeLydian", 
 "ModeMajor", "ModeMinor", "ModeMixolydian", "ModePhrygian", "Note", 
 "NoteDuration", "NoteFunction", "NotePlot", "NoteQ", "NoteRestQ", 
 "NoteTieQ", "PcV", "PitchCode", "Progression", "ProgressionQ", "Scale", 
 "ScaleFunction", "ScaleQ", "ThirdStack", "ThirdStackQ", "Velocity"}
];

DeclarePackage["Musica2`PianoRoll`",
{"MaxNote", "MaxTime", "MinNote", "MinTime", "PianoRoll"}
];

DeclarePackage["Musica2`Setup`",
{"ClearInitDotEm", "MakeInitDotEm"}
];

DeclarePackage["Musica2`Sound`",
{"SampleCount", "Snippet", "SnippetQ", "SoundQ", "SoundType"}
];

DeclarePackage["Musica2`Spectrum`",
{"Amplitude", "Frequency", "Phase", "Spectrum", "SpectrumQ", "Tone", "ToneQ"}
];

DeclarePackage["Musica2`Tuning`",
{"EqualTemperament", "EqualTemperamentQ", "FrequencyOctave", "FrequencyRef", 
 "PitchCodeOctave", "PitchCodeRef", "Tuning", "TuningFunction"}
];

DeclarePackage["Musica2`Type`",
{"ContainerQ", "CreateContainer", "CreateElement", "Data", "DataQ", 
 "ElementType", "Members", "Opts", "Pack", "Pos", "Struct", "Tidy", "TypeQ", 
 "UnPack", "UnPackOpts"}
];

DeclarePackage["Musica2`Utils`",
{"AddOpts", "Circular", "DataAnyValue", "DataAnyValueQ", "DataApply", 
 "DataNoValue", "DataNoValueQ", "DataPlainValueQ", "DataTie", "DataTieQ", 
 "DataUnTie", "DeltasToValues", "FunctionQ", "MakeNestedIfs", 
 "NormalizeList", "ParOfSeqToSeqOfPar", "RemOpts", "SeqOfParToParOfSeq", 
 "UnCompile", "ValuesToDeltas"}
];

Null

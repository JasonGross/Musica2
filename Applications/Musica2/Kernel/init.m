(* :Title: Master Declarations File for Musica2 *)

(* :Summary: This file contains declarations of all the major symbols contained in files in this directory.
When loaded, it sets up the symbols with attribute Stub, so the correct package will be loaded when the symbol is called. *)

(* :Author: This file was created by the function Musica2`Setup`MakeInitDotEm[], written by Bo C. Herlin *)

(* :History: File created 2005-2-17 at 11:58 *)

If[!MemberQ[$Packages,"Musica2`"],
  System`Private`p = Unprotect[$Packages];
  PrependTo[$Packages,"Musica2`"];
  Protect @@ System`Private`p  
];

DeclarePackage["Musica2`Common`",
{"Content", "Convert", "Damping", "Duration", "Frequency", "MidiChannel", 
 "Mix", "Musica2", "Octave", "Overtone", "Par", "PitchCode", "Play2", "Seq", 
 "Time", "TotalDuration", "UnPar", "UnSeq"}
];

DeclarePackage["Musica2`Instrument`",
{"BasicInstrument", "BasicInstrumentQ", "Instrument", "PitchCodeToFrequency", 
 "TimeToSample", "VelocityToAmplitude"}
];

DeclarePackage["Musica2`Midi`",
{"Event", "EventData", "EventQ", "EventTime", "EventType", 
 "EventTypeChannelPressure", "EventTypeControlChange", "EventTypeEOT", 
 "EventTypeKeyPressure", "EventTypeKeySignature", "EventTypeMeta", 
 "EventTypeNoteOff", "EventTypeNoteOn", "EventTypePitchBend", 
 "EventTypeProgramChange", "EventTypeSysX0", "EventTypeSysX7", 
 "EventTypeTempo", "EventTypeTimeSignature", "Midi", "MidiQ", "MilliSecond", 
 "QPM", "Tempo", "TempoQ", "TempoTime", "TempoTrack", "TempoTrackQ", "Tick", 
 "TimeUnit", "TPQ", "Track", "TrackQ"}
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
 "FigBass", "FigBassQ", "Intervals", "IntervalsQ", "KeyMode", "KeyModeQ", 
 "Melody", "MelodyQ", "ModeAeolian", "ModeDorian", "ModeIonian", 
 "ModeLocrian", "ModeLydian", "ModeMajor", "ModeMinor", "ModeMixolydian", 
 "ModePhrygian", "Note", "NotePlot", "NoteQ", "NoteRest", "NoteRestQ", 
 "NoteTie", "NoteTieQ", "Progression", "ProgressionQ", "ScaleStep", "Steps", 
 "ThirdStack", "ThirdStackQ", "Tonic", "Velocity"}
];

DeclarePackage["Musica2`ObjectType`",
{"ContainerQ", "CreateContainer", "CreateElement", "Data", "DataDeep", 
 "DataQ", "DataToRules", "ElementType", "Members", "ObjectType", 
 "ObjectTypeQ", "Opts", "Pack", "Pos", "Struct", "Tidy", "UnPack", 
 "UnPackOpts"}
];

DeclarePackage["Musica2`PianoRoll`",
{"MaxNote", "MaxTime", "MinNote", "MinTime", "PianoRoll"}
];

DeclarePackage["Musica2`Setup`",
{"ClearInitDotEm", "MakeInitDotEm", "Packages", "PrintUsage", "Setup", 
 "Symbols"}
];

DeclarePackage["Musica2`Sound`",
{"SampleCount", "Snippet", "SnippetQ", "SoundQ", "SoundType", "TestOne", 
 "TestTwo"}
];

DeclarePackage["Musica2`Spectrum`",
{"Amplitude", "Phase", "Tone", "ToneQ", "ToneSpectrum", "ToneSpectrumQ"}
];

DeclarePackage["Musica2`Test`",
{"TestCase", "TestCreate", "TestRun", "TestSuite"}
];

DeclarePackage["Musica2`Tuning`",
{"CustomTuning", "CustomTuningQ", "EqualTemperament", "EqualTemperamentQ", 
 "FrequencyOctave", "FrequencyRatios", "FrequencyRef", "PitchCodeOctave", 
 "PitchCodeRef", "Tuning"}
];

DeclarePackage["Musica2`Usage`",
{"Usage"}
];

DeclarePackage["Musica2`Utils`",
{"AddOpts", "Circular", "DataAnyValue", "DataAnyValueQ", "DataApply", 
 "DataNoValue", "DataNoValueQ", "DataPlainValueQ", "DataTie", "DataTieQ", 
 "DataUnTie", "DeltasToValues", "Deprecated", "FunctionQ", "GetOpts", 
 "MakeNestedIfs", "NormalizeList", "ParOfSeqToSeqOfPar", "RatiosToValues", 
 "RemOpts", "SeqOfParToParOfSeq", "UnCompile", "Utils", "ValuesToDeltas", 
 "ValuesToRatios"}
];

DeclarePackage["Musica2`ValueObject`",
{"DurationObjectTrack", "DurationValue", "DurationValueQ", 
 "DurationValueTrack", "DurationValueTrackQ", "EventMessage", 
 "EventMessageQ", "Object", "TimedObject", "TimedObjectQ", 
 "TimedObjectTrack", "TimedObjectTrackQ", "TypeValue", "TypeValueQ", 
 "ValueObject", "ValueType"}
];

Null

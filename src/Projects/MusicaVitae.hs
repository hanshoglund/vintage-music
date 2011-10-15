                                           
                                           
module Music.Projects.MusicaVitae where



{-
    Introduktion
        Passages
        Passager
        ====
        
        Instrumentation

            Violin I-IV (a: I, III, b : II, IV)
            Viola I-II (a: I, b: II)
            Cello I-II (a: I, b: II)
            Double Bass

        
        Tuning

            The orchestra is split into sections:
                Section (a) tunes A4 to 442 Hz (A3 to 217 Hz)
                Section (b) tunes A4 to 437 Hz (A3 to 222 Hz)
                Double bass (c) tunes A1 to 57 Hz (?)
                
            The other strings are tuned using the harmonics of the A-string
            Other frequences may be picked, as long as the following holds:
                a + b / 16 = 55 + (a - b)
            
        

        Intonation

            Many playing techiniques in the score calls for open
            strings. In this case intonation is determined solely
            by the differencing tuning.
            
            In some cases, open-string techniques are used with an
            above first-position stop. This should make the open string
            pitch rise about a quarter-tone step (or at least less than
            a half-tone step).
            
            Where stopped strings are used, intonation is determined by context:

                 * In solo passages, intonation is individual
                   Do not attempt to synchronize intontation (on long notes et al) 
                   at overlapping solo cues.

                 * In unison passages, intonation should be synchronized.

    ====

    Öppna strängar/Bakgrund

        nat flageolett
        nat flageolett gliss
        trem (halvflageolett)
        öppen sträng

    Kvartsstoppade strängar

        trem kvarsttopp/öppen
        gliss + jete
        pizz
        snap
    
    
    Stoppade strängar/Förgrund

        Melodik (diatonisk, kromatisk?)
            Hur representera förhållandet till omgivningen?

        Fördela mellan grupper


-}

tuning1 = 442
tuning2 = 437
tuning3 = 57

data OpenStringTechnique    = NaturalHarmonic
                            | NaturalHarmonicGliss
                            | HalfHarmonicTrem
                            | OpenStringNote
                            | Jete
                            | Pizz
                            | Snap

data QuarterStringTechnique = OpenQuarterTrem
                            | QuarterStoppedStringNote
                                
data StoppedStringTechnique = StoppedStringNote
                            | StoppedStringPhrase


                         


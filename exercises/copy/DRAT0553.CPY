      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0553.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0553.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:08:22   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      * MEMBER DRAT0553
      *
           05 EINGABE.
              07 VERGLEICHSDATUM.
                 09 TTMMJJJJ-TAG        PICTURE 99.
                 09 FILLER              PICTURE X.
                 09 TTMMJJJJ-MONAT      PICTURE 99.
                 09 FILLER              PICTURE X.
                 09 TTMMJJJJ-JAHR       PICTURE 9(4).
              07 ANFANGSDATUM.
                 09 TTMMJJJJ-TAG        PICTURE 99.
                 09 FILLER              PICTURE X.
                 09 TTMMJJJJ-MONAT      PICTURE 99.
                 09 FILLER              PICTURE X.
                 09 TTMMJJJJ-JAHR       PICTURE 9(4).
              07 ENDDATUM.
                 09 TTMMJJJJ-TAG        PICTURE 99.
                 09 FILLER              PICTURE X.
                 09 TTMMJJJJ-MONAT      PICTURE 99.
                 09 FILLER              PICTURE X.
                 09 TTMMJJJJ-JAHR       PICTURE 9(4).
              07 AUSWAHL-FUNKTION       PICTURE XXX.
              07 ARBEITSTAG-OFFSET      PICTURE 99.
              07 KALENDERTAGS-OFFSET.
                 09 ANZAHL-TAGE         PICTURE S9(5)  COMPUTATIONAL-3.
                 09 ANZAHL-MONATE       PICTURE S9(5)  COMPUTATIONAL-3.
                 09 ANZAHL-JAHRE        PICTURE S9(5)  COMPUTATIONAL-3.

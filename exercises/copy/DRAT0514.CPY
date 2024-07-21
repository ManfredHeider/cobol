      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0514.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0514.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:00:42   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRAT0514.
000020        07 DTANR                  PICTURE 9(5).
000030        07 DTANR-ALT              PICTURE 9(4).
000040        07 PMSTKZN                PICTURE XX.
000050        07 PMDATUM.
000060           09 TTMMJJJJ-TAG        PICTURE 99.
000070           09 FILLER              PICTURE X.
000080           09 TTMMJJJJ-MONAT      PICTURE 99.
000090           09 FILLER              PICTURE X.
000100           09 TTMMJJJJ-JAHR       PICTURE 9(4).
000110        07 DTERSTELLUNG.
000120           09 TTMMJJJJ-TAG        PICTURE 99.
000130           09 FILLER              PICTURE X.
000140           09 TTMMJJJJ-MONAT      PICTURE 99.
000150           09 FILLER              PICTURE X.
000160           09 TTMMJJJJ-JAHR       PICTURE 9(4).
000170        07 DTSTANDVON.
000180           09 TTMMJJJJ-TAG        PICTURE 99.
000190           09 FILLER              PICTURE X.
000200           09 TTMMJJJJ-MONAT      PICTURE 99.
000210           09 FILLER              PICTURE X.
000220           09 TTMMJJJJ-JAHR       PICTURE 9(4).
000230        07 DTSTAND.
000240           09 TTMMJJJJ-TAG        PICTURE 99.
000250           09 FILLER              PICTURE X.
000260           09 TTMMJJJJ-MONAT      PICTURE 99.
000270           09 FILLER              PICTURE X.
000280           09 TTMMJJJJ-JAHR       PICTURE 9(4).
000290        07 DTSTANDBIS REDEFINES DTSTAND.
000300           09 TTMMJJJJ-TAG        PICTURE 99.
000310           09 FILLER              PICTURE X.
000320           09 TTMMJJJJ-MONAT      PICTURE 99.
000330           09 FILLER              PICTURE X.
000340           09 TTMMJJJJ-JAHR       PICTURE 9(4).
000350        07 DTWRGKZN               PICTURE XXX.

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0544.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0544.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:05:28   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRAT0544.
000020        07 DATUM-TTMMJJJJ.
000030           09 TTMMJJJJ-TAG        PICTURE 99.
000040           09 FILLER              PICTURE X.
000050           09 TTMMJJJJ-MONAT      PICTURE 99.
000060           09 FILLER              PICTURE X.
000070           09 TTMMJJJJ-JAHR       PICTURE 9(4).
000080        07 FILLER                 OCCURS 10 TIMES
000090                                  PICTURE X.
000100        07 STATUS-BEREICH.
000110           09 PROGRAMMNAME        PICTURE X(8).
000120           09 RC                  PICTURE 99.
000130           09 MELDUNGS-BESCHREIBUNG.
000140              11 MELDUNGS-NR      PICTURE 9(8).
000150              11 SUB-VAR-1        PICTURE X(20).
000160              11 SUB-VAR-2        PICTURE X(20).
000170           09 FEHL-BER-ALT        PICTURE X(45).
000180           09 FILLER              OCCURS 47 TIMES
000190                                  PICTURE X.

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0506.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0506.CPv  $
      *
      *   Rev 2.2   Sep 03 2013 10:58:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jul 02 2013 08:01:28   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRAT0506.
000020*    Eingabestruktur für Datumvergleich        UPAT0056
000030        07 AUSWAHL-FUNKTION       PICTURE X(16).
000040*    AUSWAHL-FUNKTION
000050        07 DATUM1-TTMMJJJJ.
000060*    DATUM-TT.MM.JJJJ
000070           09 TTMMJJJJ-TAG        PICTURE 99.
000080*    Tagesangabe eines Datum
000090           09 FILLER              PICTURE X.
000100*    Filler-1
000110           09 TTMMJJJJ-MONAT      PICTURE 99.
000120*    Monatsangabe eines Datum
000130           09 FILLER              PICTURE X.
000140*    Filler-1
000150           09 TTMMJJJJ-JAHR       PICTURE 9(4).
000160*    Jahresangabe eines Datum
000170        07 DATUM2-TTMMJJJJ.
000180*    DATUM-TT.MM.JJJJ
000190           09 TTMMJJJJ-TAG        PICTURE 99.
000200*    Tagesangabe eines Datum
000210           09 FILLER              PICTURE X.
000220*    Filler-1
000230           09 TTMMJJJJ-MONAT      PICTURE 99.
000240*    Monatsangabe eines Datum
000250           09 FILLER              PICTURE X.
000260*    Filler-1
000270           09 TTMMJJJJ-JAHR       PICTURE 9(4).
000280*    Jahresangabe eines Datum
000290        07 ANZAHL-TT              PICTURE S9(5)  COMPUTATIONAL-3.
000300*    ANZAHL
000310        07 ANZAHL-MM              PICTURE S9(5)  COMPUTATIONAL-3.
000320*    ANZAHL
000330        07 ANZAHL-JJ              PICTURE S9(5)  COMPUTATIONAL-3.
000340*    ANZAHL

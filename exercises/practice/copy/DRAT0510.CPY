      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0510.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0510.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:00:06   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRAT0510.
000020*    Eingabestruktur für Add/Sub Wert v. Datum UPAT0057
000030        07 DATUM-TTMMJJJJ.
000040*    DATUM-TT.MM.JJJJ
000050           09 TTMMJJJJ-TAG        PICTURE 99.
000060*    Tagesangabe eines Datum
000070           09 FILLER              PICTURE X.
000080*    Filler-1
000090           09 TTMMJJJJ-MONAT      PICTURE 99.
000100*    Monatsangabe eines Datum
000110           09 FILLER              PICTURE X.
000120*    Filler-1
000130           09 TTMMJJJJ-JAHR       PICTURE 9(4).
000140*    Jahresangabe eines Datum
000150        07 ANZAHL-TT              PICTURE S9(5)  COMPUTATIONAL-3.
000160*    ANZAHL
000170        07 ANZAHL-MM              PICTURE S9(5)  COMPUTATIONAL-3.
000180*    ANZAHL
000190        07 ANZAHL-JJ              PICTURE S9(5)  COMPUTATIONAL-3.
000200*    ANZAHL

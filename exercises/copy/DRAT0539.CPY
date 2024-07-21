      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0539.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0539.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:04:24   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRAT0539.
000020*    Ausgabeparameter UPAT0067
000030        07 DGKM0349.
000040*    Ausgabestruktur für Datumsprüfung         PMKM0229
000050           09 DATUM-JJJJMM.
000060*    DATUM-JJJJMM
000070              11 JJJJMM-JAHR      PICTURE 9(4).
000080*    Jahresangabe eines Datum
000090              11 JJJJMM-MONAT     PICTURE 99.
000100*    Monatsangabe eines Datum
000110           09 DATUM-MMJJJJ.
000120*    DATUM-MM.JJJJ
000130              11 MMJJJJ-MONAT     PICTURE 99.
000140*    Monatsangabe eines Datum
000150              11 FILLER           PICTURE X.
000160*    Filler-1
000170              11 MMJJJJ-JAHR      PICTURE 9(4).
000180*    Jahresangabe eines Datum
000190           09 DATUM-JJJJMMTT.
000200*    DATUM-JJJJMMTT
000210              11 JJJJMMTT-JAHR    PICTURE 9(4).
000220*    Jahresangabe eines Datum
000230              11 JJJJMMTT-MONAT   PICTURE 99.
000240*    Monatsangabe eines Datum
000250              11 JJJJMMTT-TAG     PICTURE 99.
000260*    Tagesangabe eines Datum
000270           09 DATUM-TTMMJJJJ.
000280*    DATUM-TT.MM.JJJJ
000290              11 TTMMJJJJ-TAG     PICTURE 99.
000300*    Tagesangabe eines Datum
000310              11 FILLER           PICTURE X.
000320*    Filler-1
000330              11 TTMMJJJJ-MONAT   PICTURE 99.
000340*    Monatsangabe eines Datum
000350              11 FILLER           PICTURE X.
000360*    Filler-1
000370              11 TTMMJJJJ-JAHR    PICTURE 9(4).
000380*    Jahresangabe eines Datum
000390        07 RC                     PICTURE 99.
000400*    RC Returncode aus Unterprogrammen
000410        07 MELDUNGS-NR            PICTURE 9(8).
000420*    Meldungsnummer

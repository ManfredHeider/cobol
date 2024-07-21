      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0508.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0508.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 14:59:52   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DATUM-TTMMJJJJ.
000020*    DATUM-TT.MM.JJJJ
000030        07 TTMMJJJJ-TAG           PICTURE 99.
000040*    Tagesangabe eines Datum
000050        07 FILLER                 PICTURE X.
000060*    Filler-1
000070        07 TTMMJJJJ-MONAT         PICTURE 99.
000080*    Monatsangabe eines Datum
000090        07 FILLER                 PICTURE X.
000100*    Filler-1
000110        07 TTMMJJJJ-JAHR          PICTURE 9(4).
000120*    Jahresangabe eines Datum

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0515.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0515.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:00:52   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRAT0515.
000020        07 SATZLAENGE             PICTURE 9(4).
000030        07 FACH-RC                PICTURE 99.
000040        07 FACH-TEXT              PICTURE X(80).
000050        07 SOLL-IST-TEXT          PICTURE X(80).
000060        07 STATUS-BEREICH.
000070           09 PROGRAMMNAME        PICTURE X(8).
000080           09 RC                  PICTURE 99.
000090           09 MELDUNGS-BESCHREIBUNG.
000100              11 MELDUNGS-NR      PICTURE 9(8).
000110              11 SUB-VAR-1        PICTURE X(20).
000120              11 SUB-VAR-2        PICTURE X(20).
000130           09 FEHL-BER-ALT        PICTURE X(45).
000140           09 FILLER              OCCURS 47 TIMES
000150                                  PICTURE X.

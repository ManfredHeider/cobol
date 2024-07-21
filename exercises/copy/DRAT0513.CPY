      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0513.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0513.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:00:30   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DTAINFO-SATZ.
000020        07 DTANR                  PICTURE X(5).
000030        07 DTANR-ALT              PICTURE X(4).
000040        07 ABSENDER               PICTURE X(27).
000050        07 EMPFAENGER             PICTURE X(27).
000060        07 INHALT                 PICTURE X(20).
000070        07 VSFORMAT               PICTURE X.
000080        07 NSFORMAT               PICTURE X.
000090        07 SPERRKZN               PICTURE X.
000100        07 SPERRDATUM             PICTURE X(10).
000110        07 DSN                    PICTURE X(44).
000120        07 RECFM                  PICTURE XX.
000130        07 LRECL                  PICTURE 9(5).
000140        07 KONTROLLWERT           OCCURS 5 TIMES.
000150           09 KW-START            PICTURE 9(5).
000160           09 KW-LAENGE           PICTURE 99.
000170           09 KW-FORMAT           PICTURE X.
000180           09 KW-ANZNKST          PICTURE 9.
000190        07 BANK-VS-BLZ1           PICTURE 9(8).
000200        07 BANK-VS-BLZ2           PICTURE 9(8).
000210        07 BANK-VS-KTONR          PICTURE 9(10).
000220        07 COBA-KOPKDNR           PICTURE S9(11) PACKED-DECIMAL.
000230        07 FILLER                 OCCURS 26 TIMES
000240                                  PICTURE X.

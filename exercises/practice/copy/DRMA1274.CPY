      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1274.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1274.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:24:04   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRMA1274.
000020        07 VBNR                   PICTURE X(10).
000030        07 STRUK-DIR              PICTURE 999.
000040        07 PROVEINH               OCCURS 13 TIMES
000050                                  INDEXED BY IND
000060                                  PICTURE S9(9)V9
000070                                                 PACKED-DECIMAL.

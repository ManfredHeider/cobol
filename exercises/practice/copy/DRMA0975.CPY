      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0975.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA0975.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:10:38   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRMA0975.
000020        07 HAUPT-KONTO            PICTURE X(10).
000030        07 EROEFF-DAT             PICTURE 9(8).
000040        07 SCHLIE-DAT             PICTURE 9(8).
000050        07 KONTO-DAT              PICTURE 9(8).
000060        07 HAUPT                  PICTURE S9(9)V99.
000070        07 STORNO                 PICTURE S9(9)V99.
000080        07 VORSCHUSS              PICTURE S9(9)V99.
000090        07 ABSCHREIBUNG           PICTURE S9(9)V99.
000100        07 UEBERDECKUNG           PICTURE S9(9)V99.
000110        07 HAFTUNGSVOLUMEN        PICTURE S9(9)V99.
000120        07 KENNZEICHEN            PICTURE X(9).
000130        07 FILLER                 PICTURE X(16).

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0543.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0543.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:05:14   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRAT0543.
000020        07 DATUM.
000030           09 TAG                 PICTURE 99.
000040           09 FILLER              PICTURE X.
000050           09 MONAT               PICTURE 99.
000060           09 FILLER              PICTURE X.
000070           09 JAHR                PICTURE 9(4).

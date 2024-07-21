      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0541.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0541.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:04:48   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRAT0541.
000020        07 ANZAHL-TAGE            PICTURE S9(4)  BINARY
000030          SYNCHRONIZED.
000040        07 DRAT0531.
000050           09 RC                  PICTURE 99.
000060           09 MELDUNGS-BESCHREIBUNG.
000070              11 MELDUNGS-NR      PICTURE 9(8).
000080              11 SUB-VAR-1        PICTURE X(20).
000090              11 SUB-VAR-2        PICTURE X(20).

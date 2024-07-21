      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0531.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0531.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:02:50   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRAT0531.
000020*    Ausgabeparameter UPAT0062
000030        07 RC                     PICTURE 99.
000040*    RC Returncode aus Unterprogrammen
000050        07 MELDUNGS-BESCHREIBUNG.
000060*    allgemeine Meldungs-Beschreibung
000070           09 MELDUNGS-NR         PICTURE 9(8).
000080*    Meldungsnummer
000090           09 SUB-VAR-1           PICTURE X(20).
000100*    Substitutionsvariable
000110           09 SUB-VAR-2           PICTURE X(20).
000120*    Substitutionsvariable

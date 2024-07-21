      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0547.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0547.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:06:04   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      * MEMBER DRAT0547
      *
000010     05 MELDUNGS-BESCHREIBUNG.
000020*    allgemeine Meldungs-Beschreibung
000030        07 MELDUNGS-NR            PICTURE 9(8).
000040*    Meldungsnummer
000050        07 SUB-VAR-1              PICTURE X(20).
000060*    Substitutionsvariable
000070        07 SUB-VAR-2              PICTURE X(20).
000080*    Substitutionsvariable

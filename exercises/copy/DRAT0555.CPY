      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0555.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0555.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:07:22   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000010     05 DRAT0555.
000020*    Ausgabeschnittstelle von UPAT0073 (Arbeitstag erm)
000030        07 FEIERTAG               PICTURE X(18).
000040*    Filler-18
000050        07 RC                     PICTURE 99.
000060*    RC Returncode aus Unterprogrammen
000070        07 MELDUNGS-NR            PICTURE 9(8).
000080*    Meldungsnummer

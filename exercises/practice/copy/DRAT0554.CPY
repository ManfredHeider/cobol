      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0554.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0554.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:07:10   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
           05 DRAT0554.
      *    Ausgabeparameter UPAT0065
              07 PRUEF-SCHALTJAHR     PIC X.
                 88 SCHALTJAHR        VALUE 'J'.
      *    Bedingungsnamen SCHALTJAHR
              07 RC                   PIC S99.
      *    RC Returncode aus Unterprogrammen
              07 MELDUNGS-NR          PIC S9(8).
      *    Meldungsnummer

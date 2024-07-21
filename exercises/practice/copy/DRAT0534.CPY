      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0534.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0534.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:03:30   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01  :XX:-MONATSLETZTE-TABELLE.
           05 :XX:-ANZAHL-TAGE-JE-MONAT PIC X(24)
                           VALUE '312831303130313130313031'.
           05 FILLER REDEFINES :XX:-ANZAHL-TAGE-JE-MONAT.
              10 :XX:-MONATSLETZTER PIC S99 OCCURS 12.

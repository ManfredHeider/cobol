      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1194.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1194.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:16:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1193 - SELECT
      *
      *     SOURCE-NAME      :  DRMA1194
      *     DMR-NAME         :  DG-OV1194
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  15.11.10/15:15
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA1194.
              07 FUNKTION               PIC X(8).
              07 VERSION                PIC X.
              07 VBNR                   PIC S9(9)      BINARY.
              07 GUELTIG-AB             PIC X(10).
      *
      **************************************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3350.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA3350.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 11:48:30   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA3335 - SELECT-2
      *
      *     SOURCE-NAME      :  DRMA3350
      *     DMR-NAME         :  DG-OV3350
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  14.05.08/10:18
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA3350.
              07 FUNKTION               PIC X(8).
              07 VERSION                PIC X.
              07 VBNR                   PIC S9(9)      BINARY.
              07 STICHTAG               PIC X(10).
      *
      **************************************************************

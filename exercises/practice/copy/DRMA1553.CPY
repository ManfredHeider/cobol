      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1553.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1553.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 09:06:52   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1193 - SELECT-1
      *
      *     SOURCE-NAME      :  DRMA1553
      *     DMR-NAME         :  DG-OV1553
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  15.11.10/15:22
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA1553.
              07 FUNKTION               PIC X(8).
              07 VERSION                PIC X.
              07 VBNR                   PIC S9(9)      BINARY.
      *
      **************************************************************

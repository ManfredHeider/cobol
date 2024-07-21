      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4287.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA4287.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:25:54   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe: UPMA4286   Funktion V1
      *     RDz
      *     SOURCE-NAME      :  DRMA4287
      *     DMR-NAME         :  DG-OV4287
      *
      *     AUTOR            :  WOLFF,STEFAN
      *     GENERIERUNG      :  21.09.2011/13:30:45
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA4287.
              07 FUNKTION               PIC X(8).
              07 VG                     PIC 9(4).
              07 WERTUNGSZEITRAUM       PIC 9.
              07 EH-AP-NT-EG            OCCURS 3 TIMES
                                        PIC S9(10)V9   PACKED-DECIMAL.
      *
      **************************************************************

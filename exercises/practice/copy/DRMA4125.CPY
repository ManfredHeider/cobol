      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4125.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA4125.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:20:40   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA4124
      *
      *     SOURCE-NAME      :  DRMA4125
      *     DMR-NAME         :  DG-OV4125
      *
      *     AUTOR            :  CONSOIR, ANNA
      *     GENERIERUNG      :  28.01.11/14:44
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA4125.
              07 DGOV4123.
                 09 FUNKTION            PIC X(8).
                 09 DGOV4122.
                    11 VERSION          PIC X.
                    11 VG               PIC 9(4).
              07 FACH-KENNUNG           PIC X(10).
              07 AUSWAHL-FUNKTION       PIC X(10).
              07 BUCH-DAT               PIC X(10).
      *
      **************************************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1995.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1995.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:41:14   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1994
      *
      *     SOURCE-NAME      :  DRMA1995
      *     DMR-NAME         :  DG-OV1995
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  21.08.08/12:01
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA1995.
              07 DIR-NR                 PIC S9(4)      BINARY.
              07 VBNR                   PIC S9(9)      BINARY.
              07 ANREDE                 PIC X.
              07 ZUNAME                 PIC X(40).
              07 VORNAME                PIC X(20).
              07 TITEL-AKAD             PIC X(10).
              07 TITEL-ADEL             PIC X(10).
              07 AUSL-KZ                PIC X(4).
              07 PLZ                    PIC X(9).
              07 ORT                    PIC X(40).
              07 STR                    PIC X(40).
      *
      **************************************************************

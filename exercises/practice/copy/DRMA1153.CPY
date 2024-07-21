      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1153.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1153.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:13:52   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datensatz: OVBETREUUNG00
      *
      *     SOURCE-NAME      :  DRMA1153
      *     DMR-NAME         :  DR-OV1153
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  15.05.08/11:04
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA1153.
              07 BETREUUNG.
                 09 VERSION             PIC X.
                 09 VG                  PIC S9(4)      BINARY.
                 09 VBNR                PIC S9(9)      BINARY.
                 09 HIST-KZN            PIC X.
                 09 GUELTIG-AB          PIC X(10).
                 09 GUELTIG-BIS         PIC X(10).
                 09 UE-BETREUER-VBNR    PIC S9(9)      BINARY.
                 09 D-BETREUER-VBNR     PIC S9(9)      BINARY.
                 09 DIR-BETREUER-VBNR   PIC S9(9)      BINARY.
                 09 ED-BESITZER-VBNR    PIC S9(9)      BINARY.
                 09 DIR-NR              PIC S9(4)      BINARY.
                 09 AENDERUNGSSYSZEIT   PIC X(26).
                 09 AENDERUNGSKENNUNG   PIC X(8).
      *
      **************************************************************

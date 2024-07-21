      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1196.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1196.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:16:24   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1193 - INSERT
      *
      *     SOURCE-NAME      :  DRMA1196
      *     DMR-NAME         :  DG-OV1196
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  15.11.10/15:18
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA1196.
              07 FUNKTION               PIC X(8).
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

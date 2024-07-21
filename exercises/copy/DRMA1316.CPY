      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1316.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1316.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 08:52:06   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     mitCopy: Tabelle OVANALYSEDATEN00
      *
      *     SOURCE-NAME      :  DRMA1316
      *     DMR-NAME         :  DG-OV1316
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  21.05.08/07:46
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 ANALYSEDATEN.
              07 ANAL-LFDNR             PIC S9(4)      BINARY.
              07 SEL-DE-VG-WERT         PIC S9(4)      BINARY.
              07 SEL-DE-DAT-WERT        PIC X(10).
              07 DGOV1954.
                 09 SEL-DE-1-WERT       PIC X(15).
                 09 SEL-DE-2-WERT       PIC X(15).
                 09 SEL-DE-3-WERT       PIC X(15).
                 09 SEL-DE-4-WERT       PIC X(15).
              07 DGOV1955 REDEFINES DGOV1954.
                 09 SEL-DE-WERT         OCCURS 4 TIMES
                                        PIC X(15).
              07 DGOV1956.
                 09 ERG-DE-0-WERT       PIC S9(15)V99  PACKED-DECIMAL.
                 09 ERG-DE-1-WERT       PIC S9(15)V99  PACKED-DECIMAL.
                 09 ERG-DE-2-WERT       PIC S9(15)V99  PACKED-DECIMAL.
                 09 ERG-DE-3-WERT       PIC S9(15)V99  PACKED-DECIMAL.
                 09 ERG-DE-4-WERT       PIC S9(15)V99  PACKED-DECIMAL.
                 09 ERG-DE-5-WERT       PIC S9(15)V99  PACKED-DECIMAL.
                 09 ERG-DE-6-WERT       PIC S9(15)V99  PACKED-DECIMAL.
                 09 ERG-DE-7-WERT       PIC S9(15)V99  PACKED-DECIMAL.
                 09 ERG-DE-8-WERT       PIC S9(15)V99  PACKED-DECIMAL.
                 09 ERG-DE-9-WERT       PIC S9(15)V99  PACKED-DECIMAL.
              07 DGOV1957 REDEFINES DGOV1956.
                 09 ERG-DE-WERT         OCCURS 10 TIMES
                                        PIC S9(15)V99  PACKED-DECIMAL.
              07 AENDERUNGSSYSZEIT      PIC X(26).
              07 AENDERUNGSKENNUNG      PIC X(8).
      *
      **************************************************************

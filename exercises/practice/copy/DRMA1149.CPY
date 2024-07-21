      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1149.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1149.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:13:40   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     mitCopy: Tabelle OVWORK100
      *
      *     SOURCE-NAME      :  DRMA1149
      *     DMR-NAME         :  DG-OV1149
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  20.05.08/17:50
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 WORK1.
              07 WK1-HEADER.
                 09 USERID              PIC X(8).
                 09 TRANS               PIC X(8).
                 09 SEQUENZNUMMER       PIC S99        BINARY.
                 09 ENDE-INDIKATOR      PIC S99        BINARY.
                 09 AENDERUNGSSYSZEIT   PIC X(26).
                 09 AENDERUNGSKENNUNG   PIC X(8).
              07 DATENBEREICH.
                 09 DATENBEREICH-LEN    PIC S9(4)      BINARY.
                 09 DATENBEREICH-TEXT   PIC X(3822).
      *
      **************************************************************

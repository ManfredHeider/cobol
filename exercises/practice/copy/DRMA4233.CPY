      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4233.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA4233.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:23:06   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datensatz: Referenzbestand der Verträge aus VBI
      *
      *     SOURCE-NAME      :  DRMA4233
      *     DMR-NAME         :  DR-OV4233
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  31.05.11/12:56
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA4233.
              07 DGOV4193.
                 09 BUCHDAT-VBI         PIC X(10).
                 09 VBNR                PIC 9(9).
                 09 PG                  PIC 99.
                 09 VERTRAGS-ID-VBI     PIC X(15).
                 09 KDNR                PIC 9(9).
                 09 AUSLOESE-KZN        PIC X.
                 09 BUCHDAT-ERST        PIC X(10).
      *
      **************************************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2519.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA2519.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 11:33:18   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe: UPMA2518   Funktion V1
      *
      *     SOURCE-NAME      :  DRMA2519
      *     DMR-NAME         :  DG-OV2519
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  26.02.09/11:00
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA2519.
              07 FUNKTION               PIC X(8).
              07 VG                     PIC 9(4).
              07 FTPNR                  PIC 999.
              07 TIMESTAMP-BEGINN       PIC X(26).
              07 TIMESTAMP-ENDE         PIC X(26).
              07 ANZAHL                 PIC 9(9).
      *
      **************************************************************

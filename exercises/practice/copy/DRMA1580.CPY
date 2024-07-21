      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1580.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1580.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 09:10:26   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DRUCKZUS - Seitenanzahl je VB/FACH-KENNUNG
      *
      *     SOURCE-NAME      :  DRMA1580
      *     DMR-NAME         :  DR-OV1580
      *
      *     AUTOR            :  WILHELM, OLIVER
      *     GENERIERUNG      :  08.08.00/12:21
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1580.
000020        07 VBNR                   PICTURE 9(9).
000030        07 FACH-KENNUNG           PICTURE X(10).
000040        07 BUCHDAT                PICTURE X(10).
000050        07 SEITENANZAHL           PICTURE S9(7)  PACKED-DECIMAL.
000060        07 FILLER                 PICTURE X(17).
      *
      **************************************************************

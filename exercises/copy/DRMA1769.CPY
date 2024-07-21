      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1769.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1769.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:29:24   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     IDENTBEREICH
      *
      *     SOURCE-NAME      :  DRMA1769
      *     DMR-NAME         :  DG-OV1769
      *
      *     AUTOR            :  NIEßEN, SVEN
      *     GENERIERUNG      :  31.08.00/17:33
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 IDENTBEREICH.
000020        07 IDENTKZN               PICTURE X(5).
000030        07 FILLER                 PICTURE X.
000040        07 REPORTNAME             PICTURE X(20).
000050        07 FILLER                 PICTURE XXX.
000060        07 INDEX1-VBNR            PICTURE 9(9).
000070        07 FILLER                 OCCURS 61 TIMES
000080                                  PICTURE X.
000090        07 FACH-KENNUNG           PICTURE X(10).
000100        07 VBNR                   PICTURE S9(9)  BINARY.
000110        07 SEITENANZAHL           PICTURE S9(7)  PACKED-DECIMAL.
000120        07 BUCHDAT                PICTURE X(10).
000130        07 FILLER                 OCCURS 5 TIMES
000140                                  PICTURE X.
      *
      **************************************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1439.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1439.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 08:57:56   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     IDENTZEILE
      *
      *     SOURCE-NAME      :  DRMA1439
      *     DMR-NAME         :  DR-OV1439
      *
      *     AUTOR            :  NIEßEN, SVEN
      *     GENERIERUNG      :  31.08.00/17:33
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 IDENTZEILE.
000020        07 DRUCK-DATEIKZN         PICTURE X.
000030        07 DRUCK-STEUERKZN        PICTURE X.
000040        07 IDENTBEREICH.
000050           09 IDENTKZN            PICTURE X(5).
000060           09 FILLER              PICTURE X.
000070           09 REPORTNAME          PICTURE X(20).
000080           09 FILLER              PICTURE XXX.
000090           09 INDEX1-VBNR         PICTURE 9(9).
000100           09 FILLER              OCCURS 61 TIMES
000110                                  PICTURE X.
000120           09 FACH-KENNUNG        PICTURE X(10).
000130           09 VBNR                PICTURE S9(9)  BINARY.
000140           09 SEITENANZAHL        PICTURE S9(7)  PACKED-DECIMAL.
000150           09 BUCHDAT             PICTURE X(10).
000160           09 FILLER              OCCURS 5 TIMES
000170                                  PICTURE X.
      *
      **************************************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1456.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1456.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 08:59:12   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     AnalysedatenVB für AUSW 52
      *
      *     SOURCE-NAME      :  DRMA1456
      *     DMR-NAME         :  DG-OV1456
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  05.04.01/18:54
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 ANALYSEDATENVB-52.
000020        07 SEL-DE-STUFE-WERT      PICTURE X(4).
000030        07 DGOV1956.
000040           09 ERG-DE-0-WERT       PICTURE S9(15)V99
000050                                                 PACKED-DECIMAL.
000060           09 ERG-DE-1-WERT       PICTURE S9(15)V99
000070                                                 PACKED-DECIMAL.
000080           09 ERG-DE-2-WERT       PICTURE S9(15)V99
000090                                                 PACKED-DECIMAL.
000100           09 ERG-DE-3-WERT       PICTURE S9(15)V99
000110                                                 PACKED-DECIMAL.
000120           09 ERG-DE-4-WERT       PICTURE S9(15)V99
000130                                                 PACKED-DECIMAL.
000140           09 ERG-DE-5-WERT       PICTURE S9(15)V99
000150                                                 PACKED-DECIMAL.
000160           09 ERG-DE-6-WERT       PICTURE S9(15)V99
000170                                                 PACKED-DECIMAL.
000180           09 ERG-DE-7-WERT       PICTURE S9(15)V99
000190                                                 PACKED-DECIMAL.
000200           09 ERG-DE-8-WERT       PICTURE S9(15)V99
000210                                                 PACKED-DECIMAL.
000220           09 ERG-DE-9-WERT       PICTURE S9(15)V99
000230                                                 PACKED-DECIMAL.
000240        07 DGOV1957 REDEFINES DGOV1956.
000250           09 ERG-DE-WERT         OCCURS 10 TIMES
000260                                  PICTURE S9(15)V99
000270                                                 PACKED-DECIMAL.
      *
      **************************************************************

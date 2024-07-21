      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1949.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1949.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:37:28   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     AnalysedatenVB für AUSW 51
      *
      *     SOURCE-NAME      :  DRMA1949
      *     DMR-NAME         :  DG-OV1949
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  27.11.00/16:54
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 ANALYSEDATENVB-51.
000020        07 SEL-DE-VG-WERT         PICTURE S9(4)  BINARY.
000030        07 SEL-DE-DAT-WERT        PICTURE X(10).
000040        07 SEL-DE-VBNR-WERT       PICTURE S9(9)  BINARY.
000050        07 SEL-DE-DIR-WERT        PICTURE X(4).
000060        07 SEL-DE-STUFE-WERT      PICTURE X(4).
000070        07 DGOV1954.
000080           09 SEL-DE-1-WERT       PICTURE X(15).
000090           09 SEL-DE-2-WERT       PICTURE X(15).
000100           09 SEL-DE-3-WERT       PICTURE X(15).
000110           09 SEL-DE-4-WERT       PICTURE X(15).
000120        07 DGOV1955 REDEFINES DGOV1954.
000130           09 SEL-DE-WERT         OCCURS 4 TIMES
000140                                  PICTURE X(15).
000150        07 DGOV1958.
000160           09 ERG-DE-0-WERT       PICTURE S9(15)V99
000170                                                 PACKED-DECIMAL.
000180           09 ERG-DE-1-WERT       PICTURE S9(15)V99
000190                                                 PACKED-DECIMAL.
000200           09 ERG-DE-2-WERT       PICTURE S9(15)V99
000210                                                 PACKED-DECIMAL.
000220           09 ERG-DE-3-WERT       PICTURE S9(15)V99
000230                                                 PACKED-DECIMAL.
000240           09 ERG-DE-4-WERT       PICTURE S9(15)V99
000250                                                 PACKED-DECIMAL.
000260           09 ERG-DE-5-WERT       PICTURE S9(15)V99
000270                                                 PACKED-DECIMAL.
000280           09 ERG-DE-6-WERT       PICTURE S9(15)V99
000290                                                 PACKED-DECIMAL.
000300           09 ERG-DE-7-WERT       PICTURE S9(15)V99
000310                                                 PACKED-DECIMAL.
000320           09 ERG-DE-8-WERT       PICTURE S9(15)V99
000330                                                 PACKED-DECIMAL.
000340           09 ERG-DE-9-WERT       PICTURE S9(15)V99
000350                                                 PACKED-DECIMAL.
000360           09 ERG-DE-10-WERT      PICTURE S9(15)V99
000370                                                 PACKED-DECIMAL.
000380           09 ERG-DE-11-WERT      PICTURE S9(15)V99
000390                                                 PACKED-DECIMAL.
000400           09 ERG-DE-12-WERT      PICTURE S9(15)V99
000410                                                 PACKED-DECIMAL.
000420           09 ERG-DE-13-WERT      PICTURE S9(15)V99
000430                                                 PACKED-DECIMAL.
000440           09 ERG-DE-14-WERT      PICTURE S9(15)V99
000450                                                 PACKED-DECIMAL.
000460           09 ERG-DE-15-WERT      PICTURE S9(15)V99
000470                                                 PACKED-DECIMAL.
000480           09 ERG-DE-16-WERT      PICTURE S9(15)V99
000490                                                 PACKED-DECIMAL.
000500           09 ERG-DE-17-WERT      PICTURE S9(15)V99
000510                                                 PACKED-DECIMAL.
000520           09 ERG-DE-18-WERT      PICTURE S9(15)V99
000530                                                 PACKED-DECIMAL.
000540           09 ERG-DE-19-WERT      PICTURE S9(15)V99
000550                                                 PACKED-DECIMAL.
000560        07 DGOV1959 REDEFINES DGOV1958.
000570           09 ERG-DE-WERT         OCCURS 20 TIMES
000580                                  PICTURE S9(15)V99
000590                                                 PACKED-DECIMAL.
      *
      **************************************************************

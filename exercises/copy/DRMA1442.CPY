      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1442.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1442.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 08:58:10   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich UPMA1142 - Daten AnalysedatenVB-51
      *
      *     SOURCE-NAME      :  DRMA1442
      *     DMR-NAME         :  DG-OV1442
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  22.03.01/15:28
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1442.
000020        07 ANALYSEDATENVB-51      OCCURS 8 TIMES.
000030           09 SEL-DE-VG-WERT      PICTURE S9(4)  BINARY.
000040           09 SEL-DE-DAT-WERT     PICTURE X(10).
000050           09 SEL-DE-VBNR-WERT    PICTURE S9(9)  BINARY.
000060           09 SEL-DE-DIR-WERT     PICTURE X(4).
000070           09 SEL-DE-STUFE-WERT   PICTURE X(4).
000080           09 DGOV1954.
000090              11 SEL-DE-1-WERT    PICTURE X(15).
000100              11 SEL-DE-2-WERT    PICTURE X(15).
000110              11 SEL-DE-3-WERT    PICTURE X(15).
000120              11 SEL-DE-4-WERT    PICTURE X(15).
000130           09 DGOV1955 REDEFINES DGOV1954.
000140              11 SEL-DE-WERT      OCCURS 4 TIMES
000150                                  PICTURE X(15).
000160           09 DGOV1958.
000170              11 ERG-DE-0-WERT    PICTURE S9(15)V99
000180                                                 PACKED-DECIMAL.
000190              11 ERG-DE-1-WERT    PICTURE S9(15)V99
000200                                                 PACKED-DECIMAL.
000210              11 ERG-DE-2-WERT    PICTURE S9(15)V99
000220                                                 PACKED-DECIMAL.
000230              11 ERG-DE-3-WERT    PICTURE S9(15)V99
000240                                                 PACKED-DECIMAL.
000250              11 ERG-DE-4-WERT    PICTURE S9(15)V99
000260                                                 PACKED-DECIMAL.
000270              11 ERG-DE-5-WERT    PICTURE S9(15)V99
000280                                                 PACKED-DECIMAL.
000290              11 ERG-DE-6-WERT    PICTURE S9(15)V99
000300                                                 PACKED-DECIMAL.
000310              11 ERG-DE-7-WERT    PICTURE S9(15)V99
000320                                                 PACKED-DECIMAL.
000330              11 ERG-DE-8-WERT    PICTURE S9(15)V99
000340                                                 PACKED-DECIMAL.
000350              11 ERG-DE-9-WERT    PICTURE S9(15)V99
000360                                                 PACKED-DECIMAL.
000370              11 ERG-DE-10-WERT   PICTURE S9(15)V99
000380                                                 PACKED-DECIMAL.
000390              11 ERG-DE-11-WERT   PICTURE S9(15)V99
000400                                                 PACKED-DECIMAL.
000410              11 ERG-DE-12-WERT   PICTURE S9(15)V99
000420                                                 PACKED-DECIMAL.
000430              11 ERG-DE-13-WERT   PICTURE S9(15)V99
000440                                                 PACKED-DECIMAL.
000450              11 ERG-DE-14-WERT   PICTURE S9(15)V99
000460                                                 PACKED-DECIMAL.
000470              11 ERG-DE-15-WERT   PICTURE S9(15)V99
000480                                                 PACKED-DECIMAL.
000490              11 ERG-DE-16-WERT   PICTURE S9(15)V99
000500                                                 PACKED-DECIMAL.
000510              11 ERG-DE-17-WERT   PICTURE S9(15)V99
000520                                                 PACKED-DECIMAL.
000530              11 ERG-DE-18-WERT   PICTURE S9(15)V99
000540                                                 PACKED-DECIMAL.
000550              11 ERG-DE-19-WERT   PICTURE S9(15)V99
000560                                                 PACKED-DECIMAL.
000570           09 DGOV1959 REDEFINES DGOV1958.
000580              11 ERG-DE-WERT      OCCURS 20 TIMES
000590                                  PICTURE S9(15)V99
000600                                                 PACKED-DECIMAL.
      *
      **************************************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1662.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1662.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:19:40   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1659 - INSERT
      *
      *     SOURCE-NAME      :  DRMA1662
      *     DMR-NAME         :  DG-OV1662
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  28.11.00/16:02
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1662.
000020        07 FUNKTION               PICTURE X(8).
000030        07 ANALYSEDATENVB.
000040           09 ANAL-LFDNR          PICTURE S9(4)  BINARY.
000050           09 SEL-DE-VG-WERT      PICTURE S9(4)  BINARY.
000060           09 SEL-DE-DAT-WERT     PICTURE X(10).
000070           09 SEL-DE-VBNR-WERT    PICTURE S9(9)  BINARY.
000080           09 SEL-DE-DIR-WERT     PICTURE X(4).
000090           09 SEL-DE-STUFE-WERT   PICTURE X(4).
000100           09 DGOV1954.
000110              11 SEL-DE-1-WERT    PICTURE X(15).
000120              11 SEL-DE-2-WERT    PICTURE X(15).
000130              11 SEL-DE-3-WERT    PICTURE X(15).
000140              11 SEL-DE-4-WERT    PICTURE X(15).
000150           09 DGOV1955 REDEFINES DGOV1954.
000160              11 SEL-DE-WERT      OCCURS 4 TIMES
000170                                  PICTURE X(15).
000180           09 DGOV1956.
000190              11 ERG-DE-0-WERT    PICTURE S9(15)V99
000200                                                 PACKED-DECIMAL.
000210              11 ERG-DE-1-WERT    PICTURE S9(15)V99
000220                                                 PACKED-DECIMAL.
000230              11 ERG-DE-2-WERT    PICTURE S9(15)V99
000240                                                 PACKED-DECIMAL.
000250              11 ERG-DE-3-WERT    PICTURE S9(15)V99
000260                                                 PACKED-DECIMAL.
000270              11 ERG-DE-4-WERT    PICTURE S9(15)V99
000280                                                 PACKED-DECIMAL.
000290              11 ERG-DE-5-WERT    PICTURE S9(15)V99
000300                                                 PACKED-DECIMAL.
000310              11 ERG-DE-6-WERT    PICTURE S9(15)V99
000320                                                 PACKED-DECIMAL.
000330              11 ERG-DE-7-WERT    PICTURE S9(15)V99
000340                                                 PACKED-DECIMAL.
000350              11 ERG-DE-8-WERT    PICTURE S9(15)V99
000360                                                 PACKED-DECIMAL.
000370              11 ERG-DE-9-WERT    PICTURE S9(15)V99
000380                                                 PACKED-DECIMAL.
000390           09 DGOV1957 REDEFINES DGOV1956.
000400              11 ERG-DE-WERT      OCCURS 10 TIMES
000410                                  PICTURE S9(15)V99
000420                                                 PACKED-DECIMAL.
000430           09 AENDERUNGSSYSZEIT   PICTURE X(26).
000440           09 AENDERUNGSKENNUNG   PICTURE X(8).
      *
      **************************************************************

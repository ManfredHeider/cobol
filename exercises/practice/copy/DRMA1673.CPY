      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1673.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1673.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:21:22   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich UPMA1142 - Daten AnalysedatenVB
      *
      *     SOURCE-NAME      :  DRMA1673
      *     DMR-NAME         :  DG-OV1673
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  28.11.00/16:08
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1673.
000020        07 ANALYSEDATENVB         OCCURS 8 TIMES.
000030           09 ANAL-LFDNR          PICTURE S9(4)  BINARY.
000040           09 SEL-DE-VG-WERT      PICTURE S9(4)  BINARY.
000050           09 SEL-DE-DAT-WERT     PICTURE X(10).
000060           09 SEL-DE-VBNR-WERT    PICTURE S9(9)  BINARY.
000070           09 SEL-DE-DIR-WERT     PICTURE X(4).
000080           09 SEL-DE-STUFE-WERT   PICTURE X(4).
000090           09 DGOV1954.
000100              11 SEL-DE-1-WERT    PICTURE X(15).
000110              11 SEL-DE-2-WERT    PICTURE X(15).
000120              11 SEL-DE-3-WERT    PICTURE X(15).
000130              11 SEL-DE-4-WERT    PICTURE X(15).
000140           09 DGOV1955 REDEFINES DGOV1954.
000150              11 SEL-DE-WERT      OCCURS 4 TIMES
000160                                  PICTURE X(15).
000170           09 DGOV1956.
000180              11 ERG-DE-0-WERT    PICTURE S9(15)V99
000190                                                 PACKED-DECIMAL.
000200              11 ERG-DE-1-WERT    PICTURE S9(15)V99
000210                                                 PACKED-DECIMAL.
000220              11 ERG-DE-2-WERT    PICTURE S9(15)V99
000230                                                 PACKED-DECIMAL.
000240              11 ERG-DE-3-WERT    PICTURE S9(15)V99
000250                                                 PACKED-DECIMAL.
000260              11 ERG-DE-4-WERT    PICTURE S9(15)V99
000270                                                 PACKED-DECIMAL.
000280              11 ERG-DE-5-WERT    PICTURE S9(15)V99
000290                                                 PACKED-DECIMAL.
000300              11 ERG-DE-6-WERT    PICTURE S9(15)V99
000310                                                 PACKED-DECIMAL.
000320              11 ERG-DE-7-WERT    PICTURE S9(15)V99
000330                                                 PACKED-DECIMAL.
000340              11 ERG-DE-8-WERT    PICTURE S9(15)V99
000350                                                 PACKED-DECIMAL.
000360              11 ERG-DE-9-WERT    PICTURE S9(15)V99
000370                                                 PACKED-DECIMAL.
000380           09 DGOV1957 REDEFINES DGOV1956.
000390              11 ERG-DE-WERT      OCCURS 10 TIMES
000400                                  PICTURE S9(15)V99
000410                                                 PACKED-DECIMAL.
000420           09 AENDERUNGSSYSZEIT   PICTURE X(26).
000430           09 AENDERUNGSKENNUNG   PICTURE X(8).
      *
      **************************************************************

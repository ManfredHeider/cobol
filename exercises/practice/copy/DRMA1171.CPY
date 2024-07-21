      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1171.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1171.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:15:18   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1166 - UPDATE
      *
      *     SOURCE-NAME      :  DRMA1171
      *     DMR-NAME         :  DG-OV1171
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  28.11.00/16:09
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1171.
000020        07 FUNKTION               PICTURE X(8).
000030        07 ANALYSEDATEN.
000040           09 ANAL-LFDNR          PICTURE S9(4)  BINARY.
000050           09 SEL-DE-VG-WERT      PICTURE S9(4)  BINARY.
000060           09 SEL-DE-DAT-WERT     PICTURE X(10).
000070           09 DGOV1954.
000080              11 SEL-DE-1-WERT    PICTURE X(15).
000090              11 SEL-DE-2-WERT    PICTURE X(15).
000100              11 SEL-DE-3-WERT    PICTURE X(15).
000110              11 SEL-DE-4-WERT    PICTURE X(15).
000120           09 DGOV1955 REDEFINES DGOV1954.
000130              11 SEL-DE-WERT      OCCURS 4 TIMES
000140                                  PICTURE X(15).
000150           09 DGOV1956.
000160              11 ERG-DE-0-WERT    PICTURE S9(15)V99
000170                                                 PACKED-DECIMAL.
000180              11 ERG-DE-1-WERT    PICTURE S9(15)V99
000190                                                 PACKED-DECIMAL.
000200              11 ERG-DE-2-WERT    PICTURE S9(15)V99
000210                                                 PACKED-DECIMAL.
000220              11 ERG-DE-3-WERT    PICTURE S9(15)V99
000230                                                 PACKED-DECIMAL.
000240              11 ERG-DE-4-WERT    PICTURE S9(15)V99
000250                                                 PACKED-DECIMAL.
000260              11 ERG-DE-5-WERT    PICTURE S9(15)V99
000270                                                 PACKED-DECIMAL.
000280              11 ERG-DE-6-WERT    PICTURE S9(15)V99
000290                                                 PACKED-DECIMAL.
000300              11 ERG-DE-7-WERT    PICTURE S9(15)V99
000310                                                 PACKED-DECIMAL.
000320              11 ERG-DE-8-WERT    PICTURE S9(15)V99
000330                                                 PACKED-DECIMAL.
000340              11 ERG-DE-9-WERT    PICTURE S9(15)V99
000350                                                 PACKED-DECIMAL.
000360           09 DGOV1957 REDEFINES DGOV1956.
000370              11 ERG-DE-WERT      OCCURS 10 TIMES
000380                                  PICTURE S9(15)V99
000390                                                 PACKED-DECIMAL.
000400           09 AENDERUNGSSYSZEIT   PICTURE X(26).
000410           09 AENDERUNGSKENNUNG   PICTURE X(8).
      *
      **************************************************************

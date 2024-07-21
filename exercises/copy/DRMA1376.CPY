      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1376.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1376.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 08:54:10   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1371 - UPDATE
      *
      *     SOURCE-NAME      :  DRMA1376
      *     DMR-NAME         :  DG-OV1376
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  28.11.00/15:54
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1376.
000020        07 FUNKTION               PICTURE X(8).
000030        07 AUSWERTUNG.
000040           09 AUSW-LFDNR          PICTURE S9(4)  BINARY.
000050           09 ANAL-LFDNR          PICTURE S9(4)  BINARY.
000060           09 GUELT-AB            PICTURE X(10).
000070           09 GUELT-BIS           PICTURE X(10).
000080           09 SEL-DE-VG-LFDNR     PICTURE S9(4)  BINARY.
000090           09 SEL-DE-DAT-LFDNR    PICTURE S9(4)  BINARY.
000100           09 DRMA1950.
000110              11 SEL-DE-1-LFDNR   PICTURE S9(4)  BINARY.
000120              11 SEL-DE-2-LFDNR   PICTURE S9(4)  BINARY.
000130              11 SEL-DE-3-LFDNR   PICTURE S9(4)  BINARY.
000140              11 SEL-DE-4-LFDNR   PICTURE S9(4)  BINARY.
000150           09 DRMA1951 REDEFINES DRMA1950.
000160              11 SEL-DE-LFDNR     OCCURS 4 TIMES
000170                                  PICTURE S9(4)  BINARY.
000180           09 DRMA1952.
000190              11 ERG-DE-0-LFDNR   PICTURE S9(4)  BINARY.
000200              11 ERG-DE-1-LFDNR   PICTURE S9(4)  BINARY.
000210              11 ERG-DE-2-LFDNR   PICTURE S9(4)  BINARY.
000220              11 ERG-DE-3-LFDNR   PICTURE S9(4)  BINARY.
000230              11 ERG-DE-4-LFDNR   PICTURE S9(4)  BINARY.
000240              11 ERG-DE-5-LFDNR   PICTURE S9(4)  BINARY.
000250              11 ERG-DE-6-LFDNR   PICTURE S9(4)  BINARY.
000260              11 ERG-DE-7-LFDNR   PICTURE S9(4)  BINARY.
000270              11 ERG-DE-8-LFDNR   PICTURE S9(4)  BINARY.
000280              11 ERG-DE-9-LFDNR   PICTURE S9(4)  BINARY.
000290              11 ERG-DE-10-LFDNR  PICTURE S9(4)  BINARY.
000300              11 ERG-DE-11-LFDNR  PICTURE S9(4)  BINARY.
000310              11 ERG-DE-12-LFDNR  PICTURE S9(4)  BINARY.
000320              11 ERG-DE-13-LFDNR  PICTURE S9(4)  BINARY.
000330              11 ERG-DE-14-LFDNR  PICTURE S9(4)  BINARY.
000340              11 ERG-DE-15-LFDNR  PICTURE S9(4)  BINARY.
000350              11 ERG-DE-16-LFDNR  PICTURE S9(4)  BINARY.
000360              11 ERG-DE-17-LFDNR  PICTURE S9(4)  BINARY.
000370              11 ERG-DE-18-LFDNR  PICTURE S9(4)  BINARY.
000380              11 ERG-DE-19-LFDNR  PICTURE S9(4)  BINARY.
000390           09 DRMA1953 REDEFINES DRMA1952.
000400              11 ERG-DE-LFDNR     OCCURS 20 TIMES
000410                                  PICTURE S9(4)  BINARY.
000420           09 AENDERUNGSSYSZEIT   PICTURE X(26).
000430           09 AENDERUNGSKENNUNG   PICTURE X(8).
      *
      **************************************************************

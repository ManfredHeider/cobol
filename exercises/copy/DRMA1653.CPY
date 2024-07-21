      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1653.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1653.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:18:12   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1648 - UPDATE
      *
      *     SOURCE-NAME      :  DRMA1653
      *     DMR-NAME         :  DG-OV1653
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  28.11.00/15:50
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1653.
000020        07 FUNKTION               PICTURE X(8).
000030        07 AUSWERTUNGVB.
000040           09 AUSW-LFDNR          PICTURE S9(4)  BINARY.
000050           09 ANAL-LFDNR          PICTURE S9(4)  BINARY.
000060           09 GUELT-AB            PICTURE X(10).
000070           09 GUELT-BIS           PICTURE X(10).
000080           09 SEL-DE-VG-LFDNR     PICTURE S9(4)  BINARY.
000090           09 SEL-DE-DAT-LFDNR    PICTURE S9(4)  BINARY.
000100           09 SEL-DE-VBNR-LFDNR   PICTURE S9(4)  BINARY.
000110           09 SEL-DE-DIR-LFDNR    PICTURE S9(4)  BINARY.
000120           09 SEL-DE-STUFE-LFDNR  PICTURE S9(4)  BINARY.
000130           09 DRMA1950.
000140              11 SEL-DE-1-LFDNR   PICTURE S9(4)  BINARY.
000150              11 SEL-DE-2-LFDNR   PICTURE S9(4)  BINARY.
000160              11 SEL-DE-3-LFDNR   PICTURE S9(4)  BINARY.
000170              11 SEL-DE-4-LFDNR   PICTURE S9(4)  BINARY.
000180           09 DRMA1951 REDEFINES DRMA1950.
000190              11 SEL-DE-LFDNR     OCCURS 4 TIMES
000200                                  PICTURE S9(4)  BINARY.
000210           09 DRMA1952.
000220              11 ERG-DE-0-LFDNR   PICTURE S9(4)  BINARY.
000230              11 ERG-DE-1-LFDNR   PICTURE S9(4)  BINARY.
000240              11 ERG-DE-2-LFDNR   PICTURE S9(4)  BINARY.
000250              11 ERG-DE-3-LFDNR   PICTURE S9(4)  BINARY.
000260              11 ERG-DE-4-LFDNR   PICTURE S9(4)  BINARY.
000270              11 ERG-DE-5-LFDNR   PICTURE S9(4)  BINARY.
000280              11 ERG-DE-6-LFDNR   PICTURE S9(4)  BINARY.
000290              11 ERG-DE-7-LFDNR   PICTURE S9(4)  BINARY.
000300              11 ERG-DE-8-LFDNR   PICTURE S9(4)  BINARY.
000310              11 ERG-DE-9-LFDNR   PICTURE S9(4)  BINARY.
000320              11 ERG-DE-10-LFDNR  PICTURE S9(4)  BINARY.
000330              11 ERG-DE-11-LFDNR  PICTURE S9(4)  BINARY.
000340              11 ERG-DE-12-LFDNR  PICTURE S9(4)  BINARY.
000350              11 ERG-DE-13-LFDNR  PICTURE S9(4)  BINARY.
000360              11 ERG-DE-14-LFDNR  PICTURE S9(4)  BINARY.
000370              11 ERG-DE-15-LFDNR  PICTURE S9(4)  BINARY.
000380              11 ERG-DE-16-LFDNR  PICTURE S9(4)  BINARY.
000390              11 ERG-DE-17-LFDNR  PICTURE S9(4)  BINARY.
000400              11 ERG-DE-18-LFDNR  PICTURE S9(4)  BINARY.
000410              11 ERG-DE-19-LFDNR  PICTURE S9(4)  BINARY.
000420           09 DRMA1953 REDEFINES DRMA1952.
000430              11 ERG-DE-LFDNR     OCCURS 20 TIMES
000440                                  PICTURE S9(4)  BINARY.
000450           09 AENDERUNGSSYSZEIT   PICTURE X(26).
000460           09 AENDERUNGSKENNUNG   PICTURE X(8).
      *
      **************************************************************

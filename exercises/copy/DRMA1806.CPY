      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1806.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1806.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:34:06   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich UPMA1142 - Daten Analysedaten-7
      *
      *     SOURCE-NAME      :  DRMA1806
      *     DMR-NAME         :  DG-OV1806
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  28.11.00/15:57
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1806.
000020        07 ANALYSEDATEN-7         OCCURS 8 TIMES.
000030           09 SEL-DE-VG-WERT      PICTURE S9(4)  BINARY.
000040           09 SEL-DE-DAT-WERT     PICTURE X(10).
000050           09 DGOV1958.
000060              11 ERG-DE-0-WERT    PICTURE S9(15)V99
000070                                                 PACKED-DECIMAL.
000080              11 ERG-DE-1-WERT    PICTURE S9(15)V99
000090                                                 PACKED-DECIMAL.
000100              11 ERG-DE-2-WERT    PICTURE S9(15)V99
000110                                                 PACKED-DECIMAL.
000120              11 ERG-DE-3-WERT    PICTURE S9(15)V99
000130                                                 PACKED-DECIMAL.
000140              11 ERG-DE-4-WERT    PICTURE S9(15)V99
000150                                                 PACKED-DECIMAL.
000160              11 ERG-DE-5-WERT    PICTURE S9(15)V99
000170                                                 PACKED-DECIMAL.
000180              11 ERG-DE-6-WERT    PICTURE S9(15)V99
000190                                                 PACKED-DECIMAL.
000200              11 ERG-DE-7-WERT    PICTURE S9(15)V99
000210                                                 PACKED-DECIMAL.
000220              11 ERG-DE-8-WERT    PICTURE S9(15)V99
000230                                                 PACKED-DECIMAL.
000240              11 ERG-DE-9-WERT    PICTURE S9(15)V99
000250                                                 PACKED-DECIMAL.
000260              11 ERG-DE-10-WERT   PICTURE S9(15)V99
000270                                                 PACKED-DECIMAL.
000280              11 ERG-DE-11-WERT   PICTURE S9(15)V99
000290                                                 PACKED-DECIMAL.
000300              11 ERG-DE-12-WERT   PICTURE S9(15)V99
000310                                                 PACKED-DECIMAL.
000320              11 ERG-DE-13-WERT   PICTURE S9(15)V99
000330                                                 PACKED-DECIMAL.
000340              11 ERG-DE-14-WERT   PICTURE S9(15)V99
000350                                                 PACKED-DECIMAL.
000360              11 ERG-DE-15-WERT   PICTURE S9(15)V99
000370                                                 PACKED-DECIMAL.
000380              11 ERG-DE-16-WERT   PICTURE S9(15)V99
000390                                                 PACKED-DECIMAL.
000400              11 ERG-DE-17-WERT   PICTURE S9(15)V99
000410                                                 PACKED-DECIMAL.
000420              11 ERG-DE-18-WERT   PICTURE S9(15)V99
000430                                                 PACKED-DECIMAL.
000440              11 ERG-DE-19-WERT   PICTURE S9(15)V99
000450                                                 PACKED-DECIMAL.
000460           09 DGOV1959 REDEFINES DGOV1958.
000470              11 ERG-DE-WERT      OCCURS 20 TIMES
000480                                  PICTURE S9(15)V99
000490                                                 PACKED-DECIMAL.
000500           09 SEL-DE-DAT-WERT1    PICTURE X(10).
000510           09 SEL-DE-DAT-WERT2    PICTURE X(10).
000520           09 SEL-DE-DAT-WERT3    PICTURE X(10).
000530           09 SEL-DE-DAT-WERT4    PICTURE X(10).
000540           09 SEL-DE-DAT-WERT5    PICTURE X(10).
000550           09 SEL-DE-DAT-WERT6    PICTURE X(10).
000560           09 SEL-DE-DAT-WERT7    PICTURE X(10).
000570           09 SEL-DE-DAT-WERT8    PICTURE X(10).
000580           09 SEL-DE-DAT-WERT9    PICTURE X(10).
000590           09 SEL-DE-DAT-WERT10   PICTURE X(10).
000600           09 SEL-DE-DAT-WERT11   PICTURE X(10).
000610           09 SEL-DE-DAT-WERT12   PICTURE X(10).
000620           09 SEL-DE-DAT-WERT13   PICTURE X(10).
000630           09 SEL-DE-DAT-WERT14   PICTURE X(10).
000640           09 SEL-DE-DAT-WERT15   PICTURE X(10).
000650           09 SEL-DE-DAT-WERT16   PICTURE X(10).
000660           09 SEL-DE-DAT-WERT17   PICTURE X(10).
000670           09 SEL-DE-DAT-WERT18   PICTURE X(10).
000680           09 SEL-DE-DAT-WERT19   PICTURE X(10).
000690           09 SEL-DE-DAT-WERT20   PICTURE X(10).
      *
      **************************************************************

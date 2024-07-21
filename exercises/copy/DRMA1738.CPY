      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1738.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1738.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:26:00   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Analysedaten komplett
      *
      *     SOURCE-NAME      :  DRMA1738
      *     DMR-NAME         :  DG-OV1738
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  11.08.00/13:57
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 ANALYSEDATEN-KOMPLETT.
000020        07 ANAL-LFDNR             PICTURE S9(4)  BINARY.
000030        07 SEL-DE-VG-WERT         PICTURE S9(4)  BINARY.
000040        07 SEL-DE-DAT-WERT        PICTURE X(10).
000050        07 SEL-DE-VBNR-WERT       PICTURE S9(9)  BINARY.
000060        07 SEL-DE-DIR-WERT        PICTURE X(4).
000070        07 SEL-DE-STUFE-WERT      PICTURE X(4).
000080        07 SEL-DE-1-WERT          PICTURE X(15).
000090        07 SEL-DE-2-WERT          PICTURE X(15).
000100        07 SEL-DE-3-WERT          PICTURE X(15).
000110        07 SEL-DE-4-WERT          PICTURE X(15).
000120        07 ERG-DE-0-WERT          PICTURE S9(15)V99
000130                                                 PACKED-DECIMAL.
000140        07 ERG-DE-1-WERT          PICTURE S9(15)V99
000150                                                 PACKED-DECIMAL.
000160        07 ERG-DE-2-WERT          PICTURE S9(15)V99
000170                                                 PACKED-DECIMAL.
000180        07 ERG-DE-3-WERT          PICTURE S9(15)V99
000190                                                 PACKED-DECIMAL.
000200        07 ERG-DE-4-WERT          PICTURE S9(15)V99
000210                                                 PACKED-DECIMAL.
000220        07 ERG-DE-5-WERT          PICTURE S9(15)V99
000230                                                 PACKED-DECIMAL.
000240        07 ERG-DE-6-WERT          PICTURE S9(15)V99
000250                                                 PACKED-DECIMAL.
000260        07 ERG-DE-7-WERT          PICTURE S9(15)V99
000270                                                 PACKED-DECIMAL.
000280        07 ERG-DE-8-WERT          PICTURE S9(15)V99
000290                                                 PACKED-DECIMAL.
000300        07 ERG-DE-9-WERT          PICTURE S9(15)V99
000310                                                 PACKED-DECIMAL.
000320        07 AENDERUNGSSYSZEIT      PICTURE X(26).
000330        07 AENDERUNGSKENNUNG      PICTURE X(8).
000340        07 ERG-DE-10-WERT         PICTURE S9(15)V99
000350                                                 PACKED-DECIMAL.
000360        07 ERG-DE-11-WERT         PICTURE S9(15)V99
000370                                                 PACKED-DECIMAL.
000380        07 ERG-DE-12-WERT         PICTURE S9(15)V99
000390                                                 PACKED-DECIMAL.
000400        07 ERG-DE-13-WERT         PICTURE S9(15)V99
000410                                                 PACKED-DECIMAL.
000420        07 ERG-DE-14-WERT         PICTURE S9(15)V99
000430                                                 PACKED-DECIMAL.
000440        07 ERG-DE-15-WERT         PICTURE S9(15)V99
000450                                                 PACKED-DECIMAL.
000460        07 ERG-DE-16-WERT         PICTURE S9(15)V99
000470                                                 PACKED-DECIMAL.
000480        07 ERG-DE-17-WERT         PICTURE S9(15)V99
000490                                                 PACKED-DECIMAL.
000500        07 ERG-DE-18-WERT         PICTURE S9(15)V99
000510                                                 PACKED-DECIMAL.
000520        07 ERG-DE-19-WERT         PICTURE S9(15)V99
000530                                                 PACKED-DECIMAL.
000540        07 SEL-DE-DAT-WERT1       PICTURE X(10).
000550        07 SEL-DE-DAT-WERT2       PICTURE X(10).
000560        07 SEL-DE-DAT-WERT3       PICTURE X(10).
000570        07 SEL-DE-DAT-WERT4       PICTURE X(10).
000580        07 SEL-DE-DAT-WERT5       PICTURE X(10).
000590        07 SEL-DE-DAT-WERT6       PICTURE X(10).
000600        07 SEL-DE-DAT-WERT7       PICTURE X(10).
000610        07 SEL-DE-DAT-WERT8       PICTURE X(10).
000620        07 SEL-DE-DAT-WERT9       PICTURE X(10).
000630        07 SEL-DE-DAT-WERT10      PICTURE X(10).
000640        07 SEL-DE-DAT-WERT11      PICTURE X(10).
000650        07 SEL-DE-DAT-WERT12      PICTURE X(10).
000660        07 SEL-DE-DAT-WERT13      PICTURE X(10).
000670        07 SEL-DE-DAT-WERT14      PICTURE X(10).
000680        07 SEL-DE-DAT-WERT15      PICTURE X(10).
000690        07 SEL-DE-DAT-WERT16      PICTURE X(10).
000700        07 SEL-DE-DAT-WERT17      PICTURE X(10).
000710        07 SEL-DE-DAT-WERT18      PICTURE X(10).
000720        07 SEL-DE-DAT-WERT19      PICTURE X(10).
000730        07 SEL-DE-DAT-WERT20      PICTURE X(10).
      *
      **************************************************************

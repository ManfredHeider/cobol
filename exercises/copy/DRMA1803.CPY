      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1803.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1803.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:33:44   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Analysedaten für AUSW 7
      *
      *     SOURCE-NAME      :  DRMA1803
      *     DMR-NAME         :  DG-OV1803
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  23.11.00/17:42
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 ANALYSEDATEN-7.
000020        07 SEL-DE-VG-WERT         PICTURE S9(4)  BINARY.
000030        07 SEL-DE-DAT-WERT        PICTURE X(10).
000040        07 DGOV1958.
000050           09 ERG-DE-0-WERT       PICTURE S9(15)V99
000060                                                 PACKED-DECIMAL.
000070           09 ERG-DE-1-WERT       PICTURE S9(15)V99
000080                                                 PACKED-DECIMAL.
000090           09 ERG-DE-2-WERT       PICTURE S9(15)V99
000100                                                 PACKED-DECIMAL.
000110           09 ERG-DE-3-WERT       PICTURE S9(15)V99
000120                                                 PACKED-DECIMAL.
000130           09 ERG-DE-4-WERT       PICTURE S9(15)V99
000140                                                 PACKED-DECIMAL.
000150           09 ERG-DE-5-WERT       PICTURE S9(15)V99
000160                                                 PACKED-DECIMAL.
000170           09 ERG-DE-6-WERT       PICTURE S9(15)V99
000180                                                 PACKED-DECIMAL.
000190           09 ERG-DE-7-WERT       PICTURE S9(15)V99
000200                                                 PACKED-DECIMAL.
000210           09 ERG-DE-8-WERT       PICTURE S9(15)V99
000220                                                 PACKED-DECIMAL.
000230           09 ERG-DE-9-WERT       PICTURE S9(15)V99
000240                                                 PACKED-DECIMAL.
000250           09 ERG-DE-10-WERT      PICTURE S9(15)V99
000260                                                 PACKED-DECIMAL.
000270           09 ERG-DE-11-WERT      PICTURE S9(15)V99
000280                                                 PACKED-DECIMAL.
000290           09 ERG-DE-12-WERT      PICTURE S9(15)V99
000300                                                 PACKED-DECIMAL.
000310           09 ERG-DE-13-WERT      PICTURE S9(15)V99
000320                                                 PACKED-DECIMAL.
000330           09 ERG-DE-14-WERT      PICTURE S9(15)V99
000340                                                 PACKED-DECIMAL.
000350           09 ERG-DE-15-WERT      PICTURE S9(15)V99
000360                                                 PACKED-DECIMAL.
000370           09 ERG-DE-16-WERT      PICTURE S9(15)V99
000380                                                 PACKED-DECIMAL.
000390           09 ERG-DE-17-WERT      PICTURE S9(15)V99
000400                                                 PACKED-DECIMAL.
000410           09 ERG-DE-18-WERT      PICTURE S9(15)V99
000420                                                 PACKED-DECIMAL.
000430           09 ERG-DE-19-WERT      PICTURE S9(15)V99
000440                                                 PACKED-DECIMAL.
000450        07 DGOV1959 REDEFINES DGOV1958.
000460           09 ERG-DE-WERT         OCCURS 20 TIMES
000470                                  PICTURE S9(15)V99
000480                                                 PACKED-DECIMAL.
000490        07 SEL-DE-DAT-WERT1       PICTURE X(10).
000500        07 SEL-DE-DAT-WERT2       PICTURE X(10).
000510        07 SEL-DE-DAT-WERT3       PICTURE X(10).
000520        07 SEL-DE-DAT-WERT4       PICTURE X(10).
000530        07 SEL-DE-DAT-WERT5       PICTURE X(10).
000540        07 SEL-DE-DAT-WERT6       PICTURE X(10).
000550        07 SEL-DE-DAT-WERT7       PICTURE X(10).
000560        07 SEL-DE-DAT-WERT8       PICTURE X(10).
000570        07 SEL-DE-DAT-WERT9       PICTURE X(10).
000580        07 SEL-DE-DAT-WERT10      PICTURE X(10).
000590        07 SEL-DE-DAT-WERT11      PICTURE X(10).
000600        07 SEL-DE-DAT-WERT12      PICTURE X(10).
000610        07 SEL-DE-DAT-WERT13      PICTURE X(10).
000620        07 SEL-DE-DAT-WERT14      PICTURE X(10).
000630        07 SEL-DE-DAT-WERT15      PICTURE X(10).
000640        07 SEL-DE-DAT-WERT16      PICTURE X(10).
000650        07 SEL-DE-DAT-WERT17      PICTURE X(10).
000660        07 SEL-DE-DAT-WERT18      PICTURE X(10).
000670        07 SEL-DE-DAT-WERT19      PICTURE X(10).
000680        07 SEL-DE-DAT-WERT20      PICTURE X(10).
      *
      **************************************************************

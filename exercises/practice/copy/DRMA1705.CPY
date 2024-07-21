      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1705.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1705.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:23:40   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1704 - OZP
      *
      *     SOURCE-NAME      :  DRMA1705
      *     DMR-NAME         :  DG-OV1705
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  08.08.00/16:08
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1705.
000020        07 EIN-ANALAUSW.
000030           09 ANAL-LFDNR          PICTURE S9(4)  BINARY.
000040           09 SEL-DE-VG-WERT-VON  PICTURE S9(4)  BINARY.
000050           09 SEL-DE-VG-WERT-BIS  PICTURE S9(4)  BINARY.
000060           09 SEL-DE-DAT-WERT-VON PICTURE X(10).
000070           09 SEL-DE-DAT-WERT-BIS PICTURE X(10).
000080           09 SEL-DE-1-WERT-VON   PICTURE X(15).
000090           09 SEL-DE-1-WERT-BIS   PICTURE X(15).
000100           09 SEL-DE-2-WERT-VON   PICTURE X(15).
000110           09 SEL-DE-2-WERT-BIS   PICTURE X(15).
000120           09 SEL-DE-3-WERT-VON   PICTURE X(15).
000130           09 SEL-DE-3-WERT-BIS   PICTURE X(15).
000140           09 SEL-DE-4-WERT-VON   PICTURE X(15).
000150           09 SEL-DE-4-WERT-BIS   PICTURE X(15).
000160           09 ANAN-LFDNR          PICTURE S9(4)  BINARY.
000170           09 ANZEIGE-GROUP-BY    PICTURE X(18).
000180        07 ERG-DE-0-WERT          PICTURE S9(15)V99
000190                                                 PACKED-DECIMAL.
000200        07 ERG-DE-1-WERT          PICTURE S9(15)V99
000210                                                 PACKED-DECIMAL.
000220        07 ERG-DE-2-WERT          PICTURE S9(15)V99
000230                                                 PACKED-DECIMAL.
000240        07 ERG-DE-3-WERT          PICTURE S9(15)V99
000250                                                 PACKED-DECIMAL.
000260        07 ERG-DE-4-WERT          PICTURE S9(15)V99
000270                                                 PACKED-DECIMAL.
000280        07 ERG-DE-5-WERT          PICTURE S9(15)V99
000290                                                 PACKED-DECIMAL.
000300        07 ERG-DE-6-WERT          PICTURE S9(15)V99
000310                                                 PACKED-DECIMAL.
000320        07 ERG-DE-7-WERT          PICTURE S9(15)V99
000330                                                 PACKED-DECIMAL.
000340        07 ERG-DE-8-WERT          PICTURE S9(15)V99
000350                                                 PACKED-DECIMAL.
000360        07 ERG-DE-9-WERT          PICTURE S9(15)V99
000370                                                 PACKED-DECIMAL.
      *
      **************************************************************

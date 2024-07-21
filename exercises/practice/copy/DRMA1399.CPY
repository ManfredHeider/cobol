      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1399.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1399.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 08:56:50   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1398 - OZP-1
      *
      *     SOURCE-NAME      :  DRMA1399
      *     DMR-NAME         :  DG-OV1399
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  04.08.00/15:17
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1399.
000020        07 FUNKTION               PICTURE X(8).
000030        07 EIN-ANALAUSW.
000040           09 ANAL-LFDNR          PICTURE S9(4)  BINARY.
000050           09 SEL-DE-VG-WERT-VON  PICTURE S9(4)  BINARY.
000060           09 SEL-DE-VG-WERT-BIS  PICTURE S9(4)  BINARY.
000070           09 SEL-DE-DAT-WERT-VON PICTURE X(10).
000080           09 SEL-DE-DAT-WERT-BIS PICTURE X(10).
000090           09 SEL-DE-1-WERT-VON   PICTURE X(15).
000100           09 SEL-DE-1-WERT-BIS   PICTURE X(15).
000110           09 SEL-DE-2-WERT-VON   PICTURE X(15).
000120           09 SEL-DE-2-WERT-BIS   PICTURE X(15).
000130           09 SEL-DE-3-WERT-VON   PICTURE X(15).
000140           09 SEL-DE-3-WERT-BIS   PICTURE X(15).
000150           09 SEL-DE-4-WERT-VON   PICTURE X(15).
000160           09 SEL-DE-4-WERT-BIS   PICTURE X(15).
000170           09 ANAN-LFDNR          PICTURE S9(4)  BINARY.
000180           09 ANZEIGE-GROUP-BY    PICTURE X(18).
000190        07 ERG-DE-0-WERT          PICTURE S9(15)V99
000200                                                 PACKED-DECIMAL.
000210        07 ERG-DE-1-WERT          PICTURE S9(15)V99
000220                                                 PACKED-DECIMAL.
000230        07 ERG-DE-2-WERT          PICTURE S9(15)V99
000240                                                 PACKED-DECIMAL.
000250        07 ERG-DE-3-WERT          PICTURE S9(15)V99
000260                                                 PACKED-DECIMAL.
000270        07 ERG-DE-4-WERT          PICTURE S9(15)V99
000280                                                 PACKED-DECIMAL.
000290        07 ERG-DE-5-WERT          PICTURE S9(15)V99
000300                                                 PACKED-DECIMAL.
000310        07 ERG-DE-6-WERT          PICTURE S9(15)V99
000320                                                 PACKED-DECIMAL.
000330        07 ERG-DE-7-WERT          PICTURE S9(15)V99
000340                                                 PACKED-DECIMAL.
000350        07 ERG-DE-8-WERT          PICTURE S9(15)V99
000360                                                 PACKED-DECIMAL.
000370        07 ERG-DE-9-WERT          PICTURE S9(15)V99
000380                                                 PACKED-DECIMAL.
      *
      **************************************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1681.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1681.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:22:18   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1398 - OZP-2
      *
      *     SOURCE-NAME      :  DRMA1681
      *     DMR-NAME         :  DG-OV1681
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  04.08.00/15:16
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1681.
000020        07 FUNKTION               PICTURE X(8).
000030        07 EIN-ANALAUSWVB.
000040           09 ANAL-LFDNR          PICTURE S9(4)  BINARY.
000050           09 SEL-DE-VG-WERT-VON  PICTURE S9(4)  BINARY.
000060           09 SEL-DE-VG-WERT-BIS  PICTURE S9(4)  BINARY.
000070           09 SEL-DE-DAT-WERT-VON PICTURE X(10).
000080           09 SEL-DE-DAT-WERT-BIS PICTURE X(10).
000090           09 SEL-DE-VBNR-WERT-VON
000100                                  PICTURE S9(9)  BINARY.
000110           09 SEL-DE-VBNR-WERT-BIS
000120                                  PICTURE S9(9)  BINARY.
000130           09 SEL-DE-DIR-WERT-VON PICTURE X(4).
000140           09 SEL-DE-DIR-WERT-BIS PICTURE X(4).
000150           09 SEL-DE-STUFE-WERT-VON
000160                                  PICTURE X(4).
000170           09 SEL-DE-STUFE-WERT-BIS
000180                                  PICTURE X(4).
000190           09 SEL-DE-1-WERT-VON   PICTURE X(15).
000200           09 SEL-DE-1-WERT-BIS   PICTURE X(15).
000210           09 SEL-DE-2-WERT-VON   PICTURE X(15).
000220           09 SEL-DE-2-WERT-BIS   PICTURE X(15).
000230           09 SEL-DE-3-WERT-VON   PICTURE X(15).
000240           09 SEL-DE-3-WERT-BIS   PICTURE X(15).
000250           09 SEL-DE-4-WERT-VON   PICTURE X(15).
000260           09 SEL-DE-4-WERT-BIS   PICTURE X(15).
000270           09 ANAN-LFDNR          PICTURE S9(4)  BINARY.
000280           09 ANZEIGE-GROUP-BY    PICTURE X(18).
000290        07 ERG-DE-0-WERT          PICTURE S9(15)V99
000300                                                 PACKED-DECIMAL.
000310        07 ERG-DE-1-WERT          PICTURE S9(15)V99
000320                                                 PACKED-DECIMAL.
000330        07 ERG-DE-2-WERT          PICTURE S9(15)V99
000340                                                 PACKED-DECIMAL.
000350        07 ERG-DE-3-WERT          PICTURE S9(15)V99
000360                                                 PACKED-DECIMAL.
000370        07 ERG-DE-4-WERT          PICTURE S9(15)V99
000380                                                 PACKED-DECIMAL.
000390        07 ERG-DE-5-WERT          PICTURE S9(15)V99
000400                                                 PACKED-DECIMAL.
000410        07 ERG-DE-6-WERT          PICTURE S9(15)V99
000420                                                 PACKED-DECIMAL.
000430        07 ERG-DE-7-WERT          PICTURE S9(15)V99
000440                                                 PACKED-DECIMAL.
000450        07 ERG-DE-8-WERT          PICTURE S9(15)V99
000460                                                 PACKED-DECIMAL.
000470        07 ERG-DE-9-WERT          PICTURE S9(15)V99
000480                                                 PACKED-DECIMAL.
      *
      **************************************************************

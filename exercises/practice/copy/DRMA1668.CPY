      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1668.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1668.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:20:50   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich für AnalyseauswertungVB
      *
      *     SOURCE-NAME      :  DRMA1668
      *     DMR-NAME         :  DG-OV1668
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  03.08.00/16:57
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 EIN-ANALAUSWVB.
000020        07 ANAL-LFDNR             PICTURE S9(4)  BINARY.
000030        07 SEL-DE-VG-WERT-VON     PICTURE S9(4)  BINARY.
000040        07 SEL-DE-VG-WERT-BIS     PICTURE S9(4)  BINARY.
000050        07 SEL-DE-DAT-WERT-VON    PICTURE X(10).
000060        07 SEL-DE-DAT-WERT-BIS    PICTURE X(10).
000070        07 SEL-DE-VBNR-WERT-VON   PICTURE S9(9)  BINARY.
000080        07 SEL-DE-VBNR-WERT-BIS   PICTURE S9(9)  BINARY.
000090        07 SEL-DE-DIR-WERT-VON    PICTURE X(4).
000100        07 SEL-DE-DIR-WERT-BIS    PICTURE X(4).
000110        07 SEL-DE-STUFE-WERT-VON  PICTURE X(4).
000120        07 SEL-DE-STUFE-WERT-BIS  PICTURE X(4).
000130        07 SEL-DE-1-WERT-VON      PICTURE X(15).
000140        07 SEL-DE-1-WERT-BIS      PICTURE X(15).
000150        07 SEL-DE-2-WERT-VON      PICTURE X(15).
000160        07 SEL-DE-2-WERT-BIS      PICTURE X(15).
000170        07 SEL-DE-3-WERT-VON      PICTURE X(15).
000180        07 SEL-DE-3-WERT-BIS      PICTURE X(15).
000190        07 SEL-DE-4-WERT-VON      PICTURE X(15).
000200        07 SEL-DE-4-WERT-BIS      PICTURE X(15).
000210        07 ANAN-LFDNR             PICTURE S9(4)  BINARY.
000220        07 ANZEIGE-GROUP-BY       PICTURE X(18).
      *
      **************************************************************

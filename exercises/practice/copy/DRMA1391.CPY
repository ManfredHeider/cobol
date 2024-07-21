      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1391.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1391.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 08:56:18   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich für Analyseauswertung
      *
      *     SOURCE-NAME      :  DRMA1391
      *     DMR-NAME         :  DG-OV1391
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  03.08.00/16:57
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 EIN-ANALAUSW.
000020        07 ANAL-LFDNR             PICTURE S9(4)  BINARY.
000030        07 SEL-DE-VG-WERT-VON     PICTURE S9(4)  BINARY.
000040        07 SEL-DE-VG-WERT-BIS     PICTURE S9(4)  BINARY.
000050        07 SEL-DE-DAT-WERT-VON    PICTURE X(10).
000060        07 SEL-DE-DAT-WERT-BIS    PICTURE X(10).
000070        07 SEL-DE-1-WERT-VON      PICTURE X(15).
000080        07 SEL-DE-1-WERT-BIS      PICTURE X(15).
000090        07 SEL-DE-2-WERT-VON      PICTURE X(15).
000100        07 SEL-DE-2-WERT-BIS      PICTURE X(15).
000110        07 SEL-DE-3-WERT-VON      PICTURE X(15).
000120        07 SEL-DE-3-WERT-BIS      PICTURE X(15).
000130        07 SEL-DE-4-WERT-VON      PICTURE X(15).
000140        07 SEL-DE-4-WERT-BIS      PICTURE X(15).
000150        07 ANAN-LFDNR             PICTURE S9(4)  BINARY.
000160        07 ANZEIGE-GROUP-BY       PICTURE X(18).
      *
      **************************************************************

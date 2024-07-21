      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1501.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1501.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 09:04:50   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich für AnalyseauswertungVB AUSW 52
      *
      *     SOURCE-NAME      :  DRMA1501
      *     DMR-NAME         :  DG-OV1501
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  03.04.01/17:59
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 EIN-ANALAUSWVB-52.
000020        07 SEL-DE-VG-WERT-VON     PICTURE S9(4)  BINARY.
000030        07 SEL-DE-VG-WERT-BIS     PICTURE S9(4)  BINARY.
000040        07 SEL-DE-DAT-WERT-VON    PICTURE X(10).
000050        07 SEL-DE-DAT-WERT-BIS    PICTURE X(10).
000060        07 SEL-DE-STUFE-WERT-VON  PICTURE X(4).
000070        07 SEL-DE-STUFE-WERT-BIS  PICTURE X(4).
      *
      **************************************************************

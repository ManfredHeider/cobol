      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1802.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1802.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:33:32   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich für Analyseauswertung AUSW 7
      *
      *     SOURCE-NAME      :  DRMA1802
      *     DMR-NAME         :  DG-OV1802
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  17.04.01/07:51
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 EIN-ANALAUSW-7.
000020        07 SEL-DE-VG-WERT-VON     PICTURE S9(4)  BINARY.
000030        07 SEL-DE-VG-WERT-BIS     PICTURE S9(4)  BINARY.
000040        07 SEL-DE-DAT-WERT-VON    PICTURE X(10).
000050        07 SEL-DE-DAT-WERT-BIS    PICTURE X(10).
000060        07 ANAN-LFDNR             PICTURE S9(4)  BINARY.
      *
      **************************************************************

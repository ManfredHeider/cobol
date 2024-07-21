      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1395.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1395.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 08:56:28   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1394 - Lesen AUSW-LFDNR = 1
      *
      *     SOURCE-NAME      :  DRMA1395
      *     DMR-NAME         :  DG-OV1395
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  04.08.00/16:50
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1395.
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
      *
      **************************************************************

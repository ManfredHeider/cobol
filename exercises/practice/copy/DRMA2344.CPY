      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2344.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA2344.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 11:30:36   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1394 - Lesen AUSW-LFDNR = 53
      *
      *     SOURCE-NAME      :  DRMA2344
      *     DMR-NAME         :  DG-OV2344
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  27.04.01/16:42
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA2344.
000020        07 EIN-ANALAUSWVB.
000030           09 ANAL-LFDNR          PICTURE S9(4)  BINARY.
000040           09 SEL-DE-VG-WERT-VON  PICTURE S9(4)  BINARY.
000050           09 SEL-DE-VG-WERT-BIS  PICTURE S9(4)  BINARY.
000060           09 SEL-DE-DAT-WERT-VON PICTURE X(10).
000070           09 SEL-DE-DAT-WERT-BIS PICTURE X(10).
000080           09 SEL-DE-VBNR-WERT-VON
000090                                  PICTURE S9(9)  BINARY.
000100           09 SEL-DE-VBNR-WERT-BIS
000110                                  PICTURE S9(9)  BINARY.
000120           09 SEL-DE-DIR-WERT-VON PICTURE X(4).
000130           09 SEL-DE-DIR-WERT-BIS PICTURE X(4).
000140           09 SEL-DE-STUFE-WERT-VON
000150                                  PICTURE X(4).
000160           09 SEL-DE-STUFE-WERT-BIS
000170                                  PICTURE X(4).
000180           09 SEL-DE-1-WERT-VON   PICTURE X(15).
000190           09 SEL-DE-1-WERT-BIS   PICTURE X(15).
000200           09 SEL-DE-2-WERT-VON   PICTURE X(15).
000210           09 SEL-DE-2-WERT-BIS   PICTURE X(15).
000220           09 SEL-DE-3-WERT-VON   PICTURE X(15).
000230           09 SEL-DE-3-WERT-BIS   PICTURE X(15).
000240           09 SEL-DE-4-WERT-VON   PICTURE X(15).
000250           09 SEL-DE-4-WERT-BIS   PICTURE X(15).
000260           09 ANAN-LFDNR          PICTURE S9(4)  BINARY.
000270           09 ANZEIGE-GROUP-BY    PICTURE X(18).
      *
      **************************************************************

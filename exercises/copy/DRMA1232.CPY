      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1232.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1232.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:19:18   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1231 - Listen-1
      *
      *     SOURCE-NAME      :  DRMA1232
      *     DMR-NAME         :  DG-OV1232
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  23.03.01/13:57
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1232.
000020        07 FUNKTION               PICTURE X(8).
000030        07 OPERATION              PICTURE XXX.
000040     88 KST-SELECT                VALUE 'SEL'.
000050     88 KST-INSERT                VALUE 'INS'.
000060     88 KST-UPDATE                VALUE 'UPD'.
000070     88 KST-DELETE                VALUE 'DEL'.
000080     88 KST-START                 VALUE 'ST '.
000090     88 KST-BACKWARD              VALUE 'BW '.
000100     88 KST-FORWARD               VALUE 'FW '.
000110     88 KST-OPEN                  VALUE 'OPE'.
000120     88 KST-CLOSE                 VALUE 'CLO'.
000130     88 KST-FETCH                 VALUE 'FET'.
000140        07 EIN-ANALAUSWVB.
000150           09 ANAL-LFDNR          PICTURE S9(4)  BINARY.
000160           09 SEL-DE-VG-WERT-VON  PICTURE S9(4)  BINARY.
000170           09 SEL-DE-VG-WERT-BIS  PICTURE S9(4)  BINARY.
000180           09 SEL-DE-DAT-WERT-VON PICTURE X(10).
000190           09 SEL-DE-DAT-WERT-BIS PICTURE X(10).
000200           09 SEL-DE-VBNR-WERT-VON
000210                                  PICTURE S9(9)  BINARY.
000220           09 SEL-DE-VBNR-WERT-BIS
000230                                  PICTURE S9(9)  BINARY.
000240           09 SEL-DE-DIR-WERT-VON PICTURE X(4).
000250           09 SEL-DE-DIR-WERT-BIS PICTURE X(4).
000260           09 SEL-DE-STUFE-WERT-VON
000270                                  PICTURE X(4).
000280           09 SEL-DE-STUFE-WERT-BIS
000290                                  PICTURE X(4).
000300           09 SEL-DE-1-WERT-VON   PICTURE X(15).
000310           09 SEL-DE-1-WERT-BIS   PICTURE X(15).
000320           09 SEL-DE-2-WERT-VON   PICTURE X(15).
000330           09 SEL-DE-2-WERT-BIS   PICTURE X(15).
000340           09 SEL-DE-3-WERT-VON   PICTURE X(15).
000350           09 SEL-DE-3-WERT-BIS   PICTURE X(15).
000360           09 SEL-DE-4-WERT-VON   PICTURE X(15).
000370           09 SEL-DE-4-WERT-BIS   PICTURE X(15).
000380           09 ANAN-LFDNR          PICTURE S9(4)  BINARY.
000390           09 ANZEIGE-GROUP-BY    PICTURE X(18).
      *
      **************************************************************

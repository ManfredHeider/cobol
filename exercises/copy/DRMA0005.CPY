      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0005.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA0005.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:00:04   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1799 - Listen-2
      *
      *     SOURCE-NAME      :  DRMA0005
      *     DMR-NAME         :  DG-OV0005
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  17.04.01/07:53
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA0005.
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
000140        07 EIN-ANALAUSW-7.
000150           09 SEL-DE-VG-WERT-VON  PICTURE S9(4)  BINARY.
000160           09 SEL-DE-VG-WERT-BIS  PICTURE S9(4)  BINARY.
000170           09 SEL-DE-DAT-WERT-VON PICTURE X(10).
000180           09 SEL-DE-DAT-WERT-BIS PICTURE X(10).
000190           09 ANAN-LFDNR          PICTURE S9(4)  BINARY.
      *
      **************************************************************

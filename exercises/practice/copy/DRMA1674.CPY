      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1674.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1674.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:21:34   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1388 - Steuerung Listen-ALLG
      *
      *     SOURCE-NAME      :  DRMA1674
      *     DMR-NAME         :  DG-OV1674
      *
      *     AUTOR            :  NIEßEN, SVEN
      *     GENERIERUNG      :  12.10.00/11:39
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1674.
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
000140        07 ANALAUSW-ALLG.
000150           09 ANAL-LFDNR          PICTURE S9(4)  BINARY.
000160           09 SEL-DE-VG-WERT-VON  PICTURE S9(4)  BINARY.
000170           09 SEL-DE-VG-WERT-BIS  PICTURE S9(4)  BINARY.
000180           09 SEL-DE-DAT-WERT-VON PICTURE X(10).
000190           09 SEL-DE-DAT-WERT-BIS PICTURE X(10).
000200           09 SEL-DE-1-WERT-VON   PICTURE X(15).
000210           09 SEL-DE-1-WERT-BIS   PICTURE X(15).
000220           09 SEL-DE-2-WERT-VON   PICTURE X(15).
000230           09 SEL-DE-2-WERT-BIS   PICTURE X(15).
000240           09 SEL-DE-3-WERT-VON   PICTURE X(15).
000250           09 SEL-DE-3-WERT-BIS   PICTURE X(15).
000260           09 SEL-DE-4-WERT-VON   PICTURE X(15).
000270           09 SEL-DE-4-WERT-BIS   PICTURE X(15).
000280           09 ANAN-LFDNR          PICTURE S9(4)  BINARY.
000290           09 ANZEIGE-GROUP-BY    PICTURE X(18).
000300        07 VGLAUSW-ALLG.
000310           09 ANAL-LFDNR          PICTURE S9(4)  BINARY.
000320           09 SEL-DE-VG-WERT-VON  PICTURE S9(4)  BINARY.
000330           09 SEL-DE-VG-WERT-BIS  PICTURE S9(4)  BINARY.
000340           09 SEL-DE-DAT-WERT-VON PICTURE X(10).
000350           09 SEL-DE-DAT-WERT-BIS PICTURE X(10).
000360           09 SEL-DE-1-WERT-VON   PICTURE X(15).
000370           09 SEL-DE-1-WERT-BIS   PICTURE X(15).
000380           09 SEL-DE-2-WERT-VON   PICTURE X(15).
000390           09 SEL-DE-2-WERT-BIS   PICTURE X(15).
000400           09 SEL-DE-3-WERT-VON   PICTURE X(15).
000410           09 SEL-DE-3-WERT-BIS   PICTURE X(15).
000420           09 SEL-DE-4-WERT-VON   PICTURE X(15).
000430           09 SEL-DE-4-WERT-BIS   PICTURE X(15).
000440           09 ANAN-LFDNR          PICTURE S9(4)  BINARY.
000450           09 ANZEIGE-GROUP-BY    PICTURE X(18).
      *
      **************************************************************

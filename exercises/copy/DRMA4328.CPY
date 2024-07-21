      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4328.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA4328.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:27:54   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA4327 - Listen-1
      *     RDz
      *     SOURCE-NAME      :  DRMA4328
      *     DMR-NAME         :  DG-OV4328
      *
      *     AUTOR            :  VON HELDEN, URSULA
      *     GENERIERUNG      :  23.11.2011/14:24:48
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA4328.
              07 FUNKTION               PIC X(8).
              07 OPERATION              PIC XXX.
           88 KST-SELECT                VALUE 'SEL'.
           88 KST-INSERT                VALUE 'INS'.
           88 KST-UPDATE                VALUE 'UPD'.
           88 KST-DELETE                VALUE 'DEL'.
           88 KST-START                 VALUE 'ST '.
           88 KST-BACKWARD              VALUE 'BW '.
           88 KST-FORWARD               VALUE 'FW '.
           88 KST-OPEN                  VALUE 'OPE'.
           88 KST-CLOSE                 VALUE 'CLO'.
           88 KST-FETCH                 VALUE 'FET'.
              07 EIN-ANALAUSW.
                 09 ANAL-LFDNR          PIC S9(4)      BINARY.
                 09 SEL-DE-VG-WERT-VON  PIC S9(4)      BINARY.
                 09 SEL-DE-VG-WERT-BIS  PIC S9(4)      BINARY.
                 09 SEL-DE-DAT-WERT-VON PIC X(10).
                 09 SEL-DE-DAT-WERT-BIS PIC X(10).
                 09 SEL-DE-1-WERT-VON   PIC X(15).
                 09 SEL-DE-1-WERT-BIS   PIC X(15).
                 09 SEL-DE-2-WERT-VON   PIC X(15).
                 09 SEL-DE-2-WERT-BIS   PIC X(15).
                 09 SEL-DE-3-WERT-VON   PIC X(15).
                 09 SEL-DE-3-WERT-BIS   PIC X(15).
                 09 SEL-DE-4-WERT-VON   PIC X(15).
                 09 SEL-DE-4-WERT-BIS   PIC X(15).
                 09 ANAN-LFDNR          PIC S9(4)      BINARY.
                 09 ANZEIGE-GROUP-BY    PIC X(18).
      *
      **************************************************************

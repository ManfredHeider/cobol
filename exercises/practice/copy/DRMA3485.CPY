      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3485.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA3485.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:15:24   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA3484 - Listen-1 (STAMMDATEN01)
      *
      *     SOURCE-NAME      :  DRMA3485
      *     DMR-NAME         :  DG-OV3485
      *
      *     AUTOR            :  BOOST, MANFRED
      *     GENERIERUNG      :  11.02.08/13:17
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA3485.
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
              07 VERSION                PIC X.
              07 VG                     PIC S9(4)      BINARY.
      *
      **************************************************************

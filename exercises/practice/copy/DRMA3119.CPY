      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3119.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA3119.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 11:40:04   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA3116 - Listen-2
      *
      *     SOURCE-NAME      :  DRMA3119
      *     DMR-NAME         :  DG-OV3119
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  13.05.08/11:40
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA3119.
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
              07 BUCH-DAT-1             PIC X(10).
              07 BUCH-DAT-2             PIC X(10).
      *
      **************************************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4255.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA4255.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:24:14   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1980 - Listen-3
      *     RDz
      *     SOURCE-NAME      :  DRMA4255
      *     DMR-NAME         :  DG-OV4255
      *
      *     AUTOR            :  WOLFF,STEFAN
      *     GENERIERUNG      :  12.09.2011/09:11:21
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA4255.
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
              07 SEL-DE-VG-WERT         PIC S9(4)      BINARY.
              07 SEL-DE-DAT-WERT        PIC X(10).
              07 DE-WERT-VBNR           PIC X(15).
      *
      **************************************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4276.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA4276.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:24:36   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe: UPMA4275   Funktion V1
      *     RDz
      *     SOURCE-NAME      :  DRMA4276
      *     DMR-NAME         :  DG-OV4276
      *
      *     AUTOR            :  WOLFF,STEFAN
      *     GENERIERUNG      :  06.02.2012/12:43:59
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA4276.
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
              07 VG                     PIC 9(4).
              07 USERID                 PIC X(8).
              07 ERSTELL-DAT            PIC X(10).
              07 BUCHDAT                PIC X(10).
              07 DGOV4278.
                 09 VBNR                PIC 9(9).
                 09 ADRESSAT-TYP        PIC X(15).
                 09 VBNR-KOPIE          PIC 9(9).
                 09 AKTENZEICHEN        PIC X(70).
                 09 POST-ANSCHR1        PIC X(40).
                 09 POST-ANSCHR2        PIC X(40).
                 09 POST-ANSCHR3        PIC X(40).
                 09 POST-ANSCHR4        PIC X(40).
                 09 POST-ANSCHR5        PIC X(40).
                 09 POST-ANSCHR6        PIC X(40).
                 09 POST-ANSCHR7        PIC X(40).
      *
      **************************************************************

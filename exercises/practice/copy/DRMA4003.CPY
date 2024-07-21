      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4003.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA4003.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:19:42   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe: UPMA3892   Funktion V1
      *
      *     SOURCE-NAME      :  DRMA4003
      *     DMR-NAME         :  DG-OV4003
      *
      *     AUTOR            :  WALBERT, GABY
      *     GENERIERUNG      :  10.08.10/10:14
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA4003.
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
              07 VBNR                   PIC 9(7).
              07 STRUK-STUFE            PIC X(4).
              07 STRUK-STUFE-TEXT       PIC X(4).
      *
      **************************************************************

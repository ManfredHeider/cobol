      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4314.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA4314.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:27:30   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe: UPMA4313   Funktion V1
      *     RDz
      *     SOURCE-NAME      :  DRMA4314
      *     DMR-NAME         :  DG-OV4314
      *
      *     AUTOR            :  WOLFF,STEFAN
      *     GENERIERUNG      :  06.02.2012/12:44:21
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA4314.
              07 FUNKTION               PIC X(8).
              07 VERSION                PIC X.
              07 VG                     PIC 9(4).
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

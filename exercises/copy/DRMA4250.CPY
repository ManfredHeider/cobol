      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4250.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA4250.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:23:42   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datensatz: Referenzbestand Kopie-Empfänger
      *     RDz
      *     SOURCE-NAME      :  DRMA4250
      *     DMR-NAME         :  DR-OV4250
      *
      *     AUTOR            :  WOLFF,STEFAN
      *     GENERIERUNG      :  06.02.2012/12:42:56
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA4250.
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

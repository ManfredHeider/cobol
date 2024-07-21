      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4304.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA4304.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:27:10   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe: UPMA4290   Funktion EXB
      *     RDz
      *     SOURCE-NAME      :  DRMA4304
      *     DMR-NAME         :  DG-OV4304
      *
      *     AUTOR            :  WOLFF,STEFAN
      *     GENERIERUNG      :  04.10.2011/15:29:56
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA4304.
              07 FUNKTION               PIC X(8).
              07 VERSION                PIC X.
              07 VG                     PIC 9(4).
              07 BUCHDAT                PIC X(10).
              07 DGOV4294.
                 09 VBNR                PIC 9(7).
                 09 KTY-EP-TYP          PIC 9(4).
                 09 KNR-EP-TYP          PIC 9(4).
                 09 WERTUNGSZEITRAUM    PIC 9.
      *
      **************************************************************

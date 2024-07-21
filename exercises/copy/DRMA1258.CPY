      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1258.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1258.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:22:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1253 - UPDATE
      *     RDz
      *     SOURCE-NAME      :  DRMA1258
      *     DMR-NAME         :  DG-OV1258
      *
      *     AUTOR            :  WOLFF,STEFAN
      *     GENERIERUNG      :  15.11.2010/16:25:33
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA1258.
              07 FUNKTION               PIC X(8).
              07 BUCHDAT.
                 09 VERSION             PIC X.
                 09 VG                  PIC S9(4)      BINARY.
                 09 VERARB-KZN          PIC X.
                 09 FACH-KENNUNG        PIC X(10).
                 09 BUCH-DAT            PIC X(10).
                 09 DV-KENNUNG          PIC X(10).
                 09 AENDERUNGSSYSZEIT   PIC X(26).
                 09 AENDERUNGSKENNUNG   PIC X(8).
      *
      **************************************************************

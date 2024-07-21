      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4226.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA4226.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:22:24   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe: UPMA4225   Funktion V1
      *     RDz
      *     SOURCE-NAME      :  DRMA4226
      *     DMR-NAME         :  DG-OV4226
      *
      *     AUTOR            :  WOLFF,STEFAN
      *     GENERIERUNG      :  16.06.2011/09:54:09
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA4226.
              07 FUNKTION               PIC X(8).
              07 VBNR                   PIC S9(9)      BINARY.
              07 FAELLIG                PIC X(10).
              07 BUCHDAT-VG             PIC X(10).
              07 VERTRAGS-ID            PIC X(15).
      *
      **************************************************************

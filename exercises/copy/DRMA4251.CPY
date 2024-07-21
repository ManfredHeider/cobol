      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4251.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA4251.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:23:52   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1985 - Kopie-Empfänger
      *     RDz
      *     SOURCE-NAME      :  DRMA4251
      *     DMR-NAME         :  DG-OV4251
      *
      *     AUTOR            :  WOLFF,STEFAN
      *     GENERIERUNG      :  13.09.2011/12:45:50
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA4251.
              07 FUNKTION               PIC X(8).
              07 VERSION                PIC X.
              07 VG                     PIC S9(4)      BINARY.
              07 SEL-DE-DAT-WERT        PIC X(10).
              07 SEL-DE-DAT-WERT-2      PIC X(10).
      *
      **************************************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1986.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1986.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:40:42   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1985 - Upro Erst. Deckblatt
      *
      *     SOURCE-NAME      :  DRMA1986
      *     DMR-NAME         :  DG-OV1986
      *
      *     AUTOR            :  WILHELM, DETLEF
      *     GENERIERUNG      :  12.12.06/14:29
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA1986.
              07 FUNKTION               PIC X(8).
              07 VERSION                PIC X.
              07 VG                     PIC S9(4)      BINARY.
              07 SEL-DE-DAT-WERT        PIC X(10).
              07 SEL-DE-DAT-WERT-2      PIC X(10).
      *
      **************************************************************

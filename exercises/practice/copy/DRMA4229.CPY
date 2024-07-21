      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4229.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA4229.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:22:46   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe: UPMA4228   Funktion V1
      *
      *     SOURCE-NAME      :  DRMA4229
      *     DMR-NAME         :  DG-OV4229
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  31.05.11/13:47
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA4229.
              07 FUNKTION               PIC X(8).
              07 STRUK-STUFE-TEXT       PIC X(4).
              07 PRAXIS-STUFE           PIC S9(4)      BINARY.
              07 PE-VGSATZ              PIC S999V99    PACKED-DECIMAL.
              07 PE-PROVISION           PIC S9(7)V99   PACKED-DECIMAL.
      *
      **************************************************************

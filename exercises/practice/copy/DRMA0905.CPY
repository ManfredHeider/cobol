      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0905.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA0905.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:09:14   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Überweisungen STRUK-STUFE > "09" für GEEV-Table
      *
      *     SOURCE-NAME      :  DRMA0905
      *     DMR-NAME         :  DR-OV0905
      *
      *     AUTOR            :  BOOST, MANFRED
      *     GENERIERUNG      :  08.10.09/14:49
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA0905.
              07 VB-NR                  PIC X(10).
              07 STRUK-STUFE            PIC S9(4)      BINARY.
              07 UEBWEIS                PIC S9(7)V99   PACKED-DECIMAL.
      *
      **************************************************************

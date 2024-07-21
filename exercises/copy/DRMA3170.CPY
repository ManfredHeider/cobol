      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3170.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA3170.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:12:38   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA3163 - DELETE
      *
      *     SOURCE-NAME      :  DRMA3170
      *     DMR-NAME         :  DG-OV3170
      *
      *     AUTOR            :  HAENEL, DAVID
      *     GENERIERUNG      :  27.03.08/14:25
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA3170.
              07 FUNKTION               PIC X(8).
              07 PG                     PIC S9(4)      BINARY.
              07 PRODSCHL               PIC S9(4)      BINARY.
              07 SEL-WERT               PIC X(15).
              07 GUELTIG-AB             PIC X(10).
      *
      **************************************************************

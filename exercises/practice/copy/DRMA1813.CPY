      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1813.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1813.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:34:18   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DRUCKZUS - VB.Nummern mit speziellen Notizen
      *
      *     SOURCE-NAME      :  DRMA1813
      *     DMR-NAME         :  DR-OV1813
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  01.12.00/14:35
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1813.
000020        07 VBNR                   PICTURE 9(9).
000030        07 BUCH-DAT               PICTURE X(10).
000040        07 AUSSTEUERUNGSGRUND     PICTURE X.
000050        07 FILLER                 OCCURS 20 TIMES
000060                                  PICTURE X.
      *
      **************************************************************

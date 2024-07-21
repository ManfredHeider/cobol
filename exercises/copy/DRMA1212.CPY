      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1212.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1212.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:17:36   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1211 - SELECT
      *
      *     SOURCE-NAME      :  DRMA1212
      *     DMR-NAME         :  DG-OV1212
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  06.07.00/10:32
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1212.
000020        07 FUNKTION               PICTURE X(8).
000030        07 VERSION                PICTURE X.
000040        07 VBNR                   PICTURE S9(9)  BINARY.
000050        07 GUELTIG-AB             PICTURE X(10).
      *
      **************************************************************

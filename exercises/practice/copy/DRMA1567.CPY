      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1567.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1567.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 09:08:50   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1264 - SELECT-2
      *
      *     SOURCE-NAME      :  DRMA1567
      *     DMR-NAME         :  DG-OV1567
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  06.07.00/10:56
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1567.
000020        07 FUNKTION               PICTURE X(8).
000030        07 VERSION                PICTURE X.
000040        07 VBNR                   PICTURE S9(9)  BINARY.
000050        07 STICHTAG               PICTURE X(10).
      *
      **************************************************************

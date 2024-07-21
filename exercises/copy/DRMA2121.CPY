      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2121.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA2121.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 11:29:18   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DRUCKAUSGABE ADRESSZEILE
      *
      *     SOURCE-NAME      :  DRMA2121
      *     DMR-NAME         :  DR-OV2121
      *
      *     AUTOR            :  WILHELM, DETLEF
      *     GENERIERUNG      :  24.01.01/17:22
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA2121.
000020        07 VBNR                   PICTURE S9(9)  BINARY.
000030        07 BUCHDAT                PICTURE X(10).
000040        07 DRUCKAUSGABEDATEN      PICTURE X(132).
      *
      **************************************************************

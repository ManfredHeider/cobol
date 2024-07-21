      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2065.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA2065.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 11:28:12   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1483 - DELETE-1
      *
      *     SOURCE-NAME      :  DRMA2065
      *     DMR-NAME         :  DG-OV2065
      *
      *     AUTOR            :  KOCHS, ANDREAS
      *     GENERIERUNG      :  22.12.00/11:17
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA2065.
000020        07 FUNKTION               PICTURE X(8).
000030        07 NOTIZ-ID               PICTURE S9(15) PACKED-DECIMAL.
000040        07 DE-WERT                PICTURE X(15).
000050        07 AENDERUNGSSYSZEIT      PICTURE X(26).
      *
      **************************************************************

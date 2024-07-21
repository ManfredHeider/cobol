      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1489.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1489.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 09:03:20   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1483 - UPDATE
      *
      *     SOURCE-NAME      :  DRMA1489
      *     DMR-NAME         :  DG-OV1489
      *
      *     AUTOR            :  KOCHS, ANDREAS
      *     GENERIERUNG      :  29.05.00/14:19
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1489.
000020        07 FUNKTION               PICTURE X(8).
000030        07 NOTIZR.
000040           09 NOTIZ-ID            PICTURE S9(15) PACKED-DECIMAL.
000050           09 DE-WERT             PICTURE X(15).
000060           09 AENDERUNGSSYSZEIT   PICTURE X(26).
000070           09 AENDERUNGSKENNUNG   PICTURE X(8).
      *
      **************************************************************

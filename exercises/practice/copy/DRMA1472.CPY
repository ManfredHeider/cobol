      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1472.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1472.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 09:01:28   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1469 - INSERT
      *
      *     SOURCE-NAME      :  DRMA1472
      *     DMR-NAME         :  DG-OV1472
      *
      *     AUTOR            :  KOCHS, ANDREAS
      *     GENERIERUNG      :  05.12.00/11:07
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1472.
000020        07 FUNKTION               PICTURE X(8).
000030        07 NOTIZZEILE.
000040           09 NOTIZ-ID            PICTURE S9(15) PACKED-DECIMAL.
000050           09 NOTIZ-LFDNR         PICTURE S9(4)  BINARY.
000060           09 NOTIZ-ZEILE         PICTURE X(80).
000070           09 AENDERUNGSSYSZEIT   PICTURE X(26).
000080           09 AENDERUNGSKENNUNG   PICTURE X(8).
      *
      **************************************************************

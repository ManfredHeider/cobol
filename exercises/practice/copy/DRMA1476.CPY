      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1476.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1476.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 09:02:10   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1469 - DELETE
      *
      *     SOURCE-NAME      :  DRMA1476
      *     DMR-NAME         :  DG-OV1476
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  05.07.00/11:38
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1476.
000020        07 FUNKTION               PICTURE X(8).
000030        07 NOTIZ-ID               PICTURE S9(15) PACKED-DECIMAL.
000040        07 NOTIZ-LFDNR            PICTURE S9(4)  BINARY.
000050        07 AENDERUNGSSYSZEIT      PICTURE X(26).
      *
      **************************************************************

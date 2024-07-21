      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1758.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1758.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:29:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1751 - DELETE
      *
      *     SOURCE-NAME      :  DRMA1758
      *     DMR-NAME         :  DG-OV1758
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  11.08.00/17:04
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1758.
000020        07 FUNKTION               PICTURE X(8).
000030        07 DRUCK-ID               PICTURE S9(15) PACKED-DECIMAL.
000040        07 DRUCK-LFDNR            PICTURE S9(4)  BINARY.
000050        07 AENDERUNGSSYSZEIT      PICTURE X(26).
      *
      **************************************************************

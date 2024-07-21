      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1173.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1173.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:15:40   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1166 - DELETE
      *
      *     SOURCE-NAME      :  DRMA1173
      *     DMR-NAME         :  DG-OV1173
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  30.05.00/07:16
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1173.
000020        07 FUNKTION               PICTURE X(8).
000030        07 ANAL-LFDNR             PICTURE S9(4)  BINARY.
000040        07 SEL-DE-VG-WERT         PICTURE S9(4)  BINARY.
000050        07 SEL-DE-DAT-WERT        PICTURE X(10).
000060        07 SEL-DE-1-WERT          PICTURE X(15).
000070        07 SEL-DE-2-WERT          PICTURE X(15).
000080        07 SEL-DE-3-WERT          PICTURE X(15).
000090        07 SEL-DE-4-WERT          PICTURE X(15).
000100        07 AENDERUNGSSYSZEIT      PICTURE X(26).
      *
      **************************************************************

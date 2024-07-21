      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1660.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1660.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:19:18   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1659 - SELECT
      *
      *     SOURCE-NAME      :  DRMA1660
      *     DMR-NAME         :  DG-OV1660
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  03.08.00/12:16
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1660.
000020        07 FUNKTION               PICTURE X(8).
000030        07 ANAL-LFDNR             PICTURE S9(4)  BINARY.
000040        07 SEL-DE-VG-WERT         PICTURE S9(4)  BINARY.
000050        07 SEL-DE-DAT-WERT        PICTURE X(10).
000060        07 SEL-DE-VBNR-WERT       PICTURE S9(9)  BINARY.
000070        07 SEL-DE-DIR-WERT        PICTURE X(4).
000080        07 SEL-DE-STUFE-WERT      PICTURE X(4).
000090        07 SEL-DE-1-WERT          PICTURE X(15).
000100        07 SEL-DE-2-WERT          PICTURE X(15).
000110        07 SEL-DE-3-WERT          PICTURE X(15).
000120        07 SEL-DE-4-WERT          PICTURE X(15).
      *
      **************************************************************

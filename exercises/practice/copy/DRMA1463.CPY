      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1463.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1463.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 09:00:16   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1458 - UPDATE
      *
      *     SOURCE-NAME      :  DRMA1463
      *     DMR-NAME         :  DG-OV1463
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  08.12.00/17:34
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1463.
000020        07 FUNKTION               PICTURE X(8).
000030        07 NOTIZ.
000040           09 NOTIZ-ID            PICTURE S9(15) PACKED-DECIMAL.
000050           09 USERID              PICTURE X(8).
000060           09 ERSTELL-DAT         PICTURE X(10).
000070           09 SEL-DE-VG-WERT      PICTURE S9(4)  BINARY.
000080           09 SEL-DE-DAT-WERT     PICTURE X(10).
000090           09 DE-LFDNR            PICTURE S9(4)  BINARY.
000100           09 SGWT-LFDNR          PICTURE S9(4)  BINARY.
000110           09 NOAR-LFDNR          PICTURE S9(4)  BINARY.
000120           09 NOZY-LFDNR          PICTURE S9(4)  BINARY.
000130           09 NOTIZ-BESCHR        PICTURE X(20).
000140           09 AENDERUNGSSYSZEIT   PICTURE X(26).
000150           09 AENDERUNGSKENNUNG   PICTURE X(8).
      *
      **************************************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1756.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1756.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:28:38   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1751 - UPDATE
      *
      *     SOURCE-NAME      :  DRMA1756
      *     DMR-NAME         :  DG-OV1756
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  07.05.01/07:45
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1756.
000020        07 FUNKTION               PICTURE X(8).
000030        07 DRUCKZEILE.
000040           09 DRUCK-ID            PICTURE S9(15) PACKED-DECIMAL.
000050           09 DRUCK-LFDNR         PICTURE S9(4)  BINARY.
000060           09 AENDERUNGSSYSZEIT   PICTURE X(26).
000070           09 AENDERUNGSKENNUNG   PICTURE X(8).
000080           09 DRUCK-ZEILE-LEN     PICTURE S9(4)  BINARY.
000090           09 DRUCK-ZEILE         PICTURE X(1000).
      *
      **************************************************************

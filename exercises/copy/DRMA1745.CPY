      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1745.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1745.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:26:46   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1742 - INSERT
      *
      *     SOURCE-NAME      :  DRMA1745
      *     DMR-NAME         :  DG-OV1745
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  11.08.00/17:20
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1745.
000020        07 FUNKTION               PICTURE X(8).
000030        07 DRUCK.
000040           09 DRUCK-ID            PICTURE S9(15) PACKED-DECIMAL.
000050           09 AUSW-LFDNR          PICTURE S9(4)  BINARY.
000060           09 AUDR-LFDNR          PICTURE S9(4)  BINARY.
000070           09 USERID              PICTURE X(8).
000080           09 DRCK-LFDNR          PICTURE S9(4)  BINARY.
000090           09 HALT-KZN            PICTURE X.
000100           09 ERSTELL-DAT         PICTURE X(10).
000110           09 AENDERUNGSSYSZEIT   PICTURE X(26).
000120           09 AENDERUNGSKENNUNG   PICTURE X(8).
      *
      **************************************************************

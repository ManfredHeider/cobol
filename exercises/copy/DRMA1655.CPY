      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1655.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1655.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:18:36   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1648 - DELETE
      *
      *     SOURCE-NAME      :  DRMA1655
      *     DMR-NAME         :  DG-OV1655
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  03.08.00/12:12
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1655.
000020        07 FUNKTION               PICTURE X(8).
000030        07 AUSW-LFDNR             PICTURE S9(4)  BINARY.
000040        07 GUELT-AB               PICTURE X(10).
000050        07 AENDERUNGSSYSZEIT      PICTURE X(26).
      *
      **************************************************************

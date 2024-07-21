      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1378.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1378.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 08:54:32   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1371 - DELETE
      *
      *     SOURCE-NAME      :  DRMA1378
      *     DMR-NAME         :  DG-OV1378
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  02.05.00/13:27
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1378.
000020        07 FUNKTION               PICTURE X(8).
000030        07 AUSW-LFDNR             PICTURE S9(4)  BINARY.
000040        07 GUELT-AB               PICTURE X(10).
000050        07 AENDERUNGSSYSZEIT      PICTURE X(26).
      *
      **************************************************************

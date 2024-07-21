      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1932.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1932.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:36:14   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1985 -
      *
      *     SOURCE-NAME      :  DRMA1932
      *     DMR-NAME         :  DG-OV1932
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  14.12.00/08:31
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1932.
000020        07 FUNKTION               PICTURE X(8).
000030        07 VERSION                PICTURE X.
000040        07 VG                     PICTURE S9(4)  BINARY.
000050        07 SEL-DE-DAT-WERT        PICTURE X(10).
000060        07 VBNR                   PICTURE S9(9)  BINARY.
      *
      **************************************************************

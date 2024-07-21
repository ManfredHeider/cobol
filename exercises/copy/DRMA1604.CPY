      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1604.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1604.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:14:12   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich UPMA1602 - Upro Erm. VERSION und VG
      *
      *     SOURCE-NAME      :  DRMA1604
      *     DMR-NAME         :  DG-OV1604
      *
      *     AUTOR            :  WILHELM, OLIVER
      *     GENERIERUNG      :  27.07.00/12:37
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1604.
000020        07 STATUS-BEREICH.
000030           09 PROGRAMMNAME        PICTURE X(8).
000040           09 RC                  PICTURE 99.
000050           09 MELDUNGS-BESCHREIBUNG.
000060              11 MELDUNGS-NR      PICTURE 9(8).
000070              11 SUB-VAR-1        PICTURE X(20).
000080              11 SUB-VAR-2        PICTURE X(20).
000090           09 FEHL-BER-ALT        PICTURE X(45).
000100           09 FILLER              OCCURS 47 TIMES
000110                                  PICTURE X.
000120        07 VERSION                PICTURE X.
000130        07 VG                     PICTURE 9(4).
      *
      **************************************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0552.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0552.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:06:54   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datum / Offset / Zugriffsart / + 6 Byte Filler
      *
      *     SOURCE-NAME      :  DRAT0552
      *     DMR-NAME         :  DG-VQ0974
      *
      *     AUTOR            :  RAUTENBERG,FRANK
      *     GENERIERUNG      :  06.12.02/09:41
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 EINGABE.
              07 DATUM-TTMMJJJJ.
                 09 TTMMJJJJ-TAG        PICTURE 99.
                 09 FILLER              PICTURE X.
                 09 TTMMJJJJ-MONAT      PICTURE 99.
                 09 FILLER              PICTURE X.
                 09 TTMMJJJJ-JAHR       PICTURE 9(4).
              07 ARBEITSTAG-OFFSET      PICTURE 99.
              07 ZUGRIFFS-ART           PICTURE XXX.
              07 VERARBKZ               PICTURE X.
              07 FILLER                 OCCURS 6 TIMES
                                        PICTURE X.
      *
      **************************************************************

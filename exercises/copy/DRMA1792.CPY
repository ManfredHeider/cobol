      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1792.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1792.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:31:58   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     IDENTBEREICH in einer NOP-Zeile
      *
      *     SOURCE-NAME      :  DRMA1792
      *     DMR-NAME         :  DG-OV1792
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  05.12.08/14:55
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 IDENTBEREICH.
              07 SATZLAENGE             PIC XX.
              07 NOOP                   PIC XXX.
              07 SATZZAEHLER            PIC XXX.
              07 IDENTKZN               PIC X(5).
              07 FILLER                 PIC X.
              07 REPORTNAME             PIC X(20).
              07 FILLER                 PIC XXX.
              07 INDEX1-VBNR            PIC 9(9).
              07 FILLER                 OCCURS 53 TIMES
                                        PIC X.
              07 FACH-KENNUNG           PIC X(10).
              07 VBNR                   PIC S9(9)      BINARY.
              07 SEITENANZAHL           PIC S9(7)      PACKED-DECIMAL.
              07 BUCHDAT                PIC X(10).
              07 FILLER                 OCCURS 5 TIMES
                                        PIC X.
      *
      **************************************************************

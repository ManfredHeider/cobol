      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1961.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1961.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:38:28   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datensatz: OVANALYSEDATENVB00
      *
      *     SOURCE-NAME      :  DRMA1961
      *     DMR-NAME         :  DR-OV1961
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  23.05.08/08:39
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA1961.
              07 ANALYSEDATENVB.
                 09 ANAL-LFDNR          PIC S9(4)      BINARY.
                 09 SEL-DE-VG-WERT      PIC S9(4)      BINARY.
                 09 SEL-DE-DAT-WERT     PIC X(10).
                 09 SEL-DE-VBNR-WERT    PIC S9(9)      BINARY.
                 09 SEL-DE-DIR-WERT     PIC X(4).
                 09 SEL-DE-STUFE-WERT   PIC X(4).
                 09 DGOV1954.
                    11 SEL-DE-1-WERT    PIC X(15).
                    11 SEL-DE-2-WERT    PIC X(15).
                    11 SEL-DE-3-WERT    PIC X(15).
                    11 SEL-DE-4-WERT    PIC X(15).
                 09 DGOV1955 REDEFINES DGOV1954.
                    11 SEL-DE-WERT      OCCURS 4 TIMES
                                        PIC X(15).
                 09 DGOV1956.
                    11 ERG-DE-0-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-1-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-2-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-3-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-4-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-5-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-6-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-7-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-8-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-9-WERT    PIC S9(15)V99  PACKED-DECIMAL.
                 09 DGOV1957 REDEFINES DGOV1956.
                    11 ERG-DE-WERT      OCCURS 10 TIMES
                                        PIC S9(15)V99  PACKED-DECIMAL.
                 09 AENDERUNGSSYSZEIT   PIC X(26).
                 09 AENDERUNGSKENNUNG   PIC X(8).
      *
      **************************************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1596.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1596.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 09:12:12   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DRUCKZUS - Datensatz Steuerdatei
      *
      *     SOURCE-NAME      :  DRMA1596
      *     DMR-NAME         :  DR-OV1596
      *
      *     AUTOR            :  WILHELM, OLIVER
      *     GENERIERUNG      :  08.05.07/19:22
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA1596.
              07 ANALYSEDATENVB-51.
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
                 09 DGOV1958.
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
                    11 ERG-DE-10-WERT   PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-11-WERT   PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-12-WERT   PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-13-WERT   PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-14-WERT   PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-15-WERT   PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-16-WERT   PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-17-WERT   PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-18-WERT   PIC S9(15)V99  PACKED-DECIMAL.
                    11 ERG-DE-19-WERT   PIC S9(15)V99  PACKED-DECIMAL.
                 09 DGOV1959 REDEFINES DGOV1958.
                    11 ERG-DE-WERT      OCCURS 20 TIMES
                                        PIC S9(15)V99  PACKED-DECIMAL.
              07 ALLES-DRUCKEN          PIC X.
      *
      **************************************************************

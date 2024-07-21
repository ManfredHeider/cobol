      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1457.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1457.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 08:59:24   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich UPMA1142 - Daten AnalysedatenVB-52
      *
      *     SOURCE-NAME      :  DRMA1457
      *     DMR-NAME         :  DG-OV1457
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  05.04.01/18:55
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1457.
000020        07 ANALYSEDATENVB-52      OCCURS 8 TIMES.
000030           09 SEL-DE-STUFE-WERT   PICTURE X(4).
000040           09 DGOV1956.
000050              11 ERG-DE-0-WERT    PICTURE S9(15)V99
000060                                                 PACKED-DECIMAL.
000070              11 ERG-DE-1-WERT    PICTURE S9(15)V99
000080                                                 PACKED-DECIMAL.
000090              11 ERG-DE-2-WERT    PICTURE S9(15)V99
000100                                                 PACKED-DECIMAL.
000110              11 ERG-DE-3-WERT    PICTURE S9(15)V99
000120                                                 PACKED-DECIMAL.
000130              11 ERG-DE-4-WERT    PICTURE S9(15)V99
000140                                                 PACKED-DECIMAL.
000150              11 ERG-DE-5-WERT    PICTURE S9(15)V99
000160                                                 PACKED-DECIMAL.
000170              11 ERG-DE-6-WERT    PICTURE S9(15)V99
000180                                                 PACKED-DECIMAL.
000190              11 ERG-DE-7-WERT    PICTURE S9(15)V99
000200                                                 PACKED-DECIMAL.
000210              11 ERG-DE-8-WERT    PICTURE S9(15)V99
000220                                                 PACKED-DECIMAL.
000230              11 ERG-DE-9-WERT    PICTURE S9(15)V99
000240                                                 PACKED-DECIMAL.
000250           09 DGOV1957 REDEFINES DGOV1956.
000260              11 ERG-DE-WERT      OCCURS 10 TIMES
000270                                  PICTURE S9(15)V99
000280                                                 PACKED-DECIMAL.
      *
      **************************************************************

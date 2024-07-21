      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0017.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0017.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:36:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: STEUER-FELDER      (DRAT0017)                 * 00020000
      *                 - PROGRAMMRAHMEN -                            * 00030000
      *    --------------------------------------------------------   * 00040000
      *    VERSION                                                    * 00050000
      *    --------------------------------------------------------   * 00060000
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  * 00070000
      *    DATUM:    21.01.1993                                       * 00080000
      *    STAND:    07.04.1993                                       * 00090008
      ***************************************************************** 00100000
      *                                                                 00110000
      *   STATUS DER BEARBEITUNG DER OP-TABELLE                         00120000
       01  S-BEARB-STATUS           PIC  9(01)  VALUE ZERO.             00130000
           88 BEARB-OP                          VALUE 1.                00140000
           88 BEARB-NOP                         VALUE 2.                00150000
                                                                        00160000
      *   STATUS DER BEARBEITUNG EINER EINZELNEN OPERATION              00170000
       01  S-OP-STATUS              PIC  9(01)  VALUE ZERO.             00180000
      *    88  OP-ERSTAUFRUF                    VALUE 1.                00190000
      *    88  OP-WIEDERAUFRUF                  VALUE 2.                00200000
           88  OP-STATUS-ERSTAUFRUF             VALUE 1.                00201000
           88  OP-STATUS-WIEDERAUFRUF           VALUE 2.                00202000
                                                                        00215000
      *   RETURN ZUR AKTUELLEN OPERATION                                00216000
       01  S-OP-RC                  PIC  9(01).                         00217000
           88  OP-RC-INIT                       VALUE 0.                00218000
           88  OP-RC-BEARBEITUNG-GUELTIG        VALUE 1 THRU 7.         00219000
           88  OP-RC-DIALOG-INIT-GUELTIG        VALUE 1, 3.             00230000
                                                                        00240000
           88  OP-OK                            VALUE 1.                00250000
           88  OP-OK-VERZWEIGEN                 VALUE 2.                00251000
           88  OP-ABBR                          VALUE 3.                00260000
           88  OP-ABBR-VERZWEIGEN               VALUE 4.                00270100
           88  OP-ABBR-BILDLAST                 VALUE 5.                00270200
           88  OP-ABBR-VERZWEIGEN-BILDLAST      VALUE 6.                00271000
           88  OP-UNTERBR-VERZWEIGEN            VALUE 7.                00272000
                                                                        00273200
      *------------------------------------------------------------*    00274000
      *   RETURN ZU PRUEFEN-FTASTE DURCH                           *    00275008
      *   - DIV. RAHMEN-SECTIONS                                   *    00276008
      *   - T1-PRUEFEN-FTASTE (FACHFUNKTION)                       *    00278008
      *------------------------------------------------------------*    00290000
      * (FRUEHER: S-FT-RC)                                              00300010
       01  S-FTASTE-RC              PIC  9(01).                         00301010
           88  FTASTE-RC-INIT                   VALUE 0.                00310009
           88  FTASTE-OK                        VALUE 1.                00330000
           88  FTASTE-ABBR                      VALUE 2.                00342004
           88  FTASTE-ABBR-VERZW                VALUE 3.                00342111
           88  FTASTE-ABBR-BILDLAST             VALUE 4.                00342208
                                                                        00343008
      *  ACHTUNG: IDENTISCH !!!                                         00343108
      *           'UNBEKANNT' NUR VERWENDET IN T1-...                   00343208
           88  FTASTE-ABBR-VERZW-BILDLAST       VALUE 5.                00344011
           88  FTASTE-UNBEKANNT                 VALUE 5.                00350008
                                                                        00350208
           88  FTASTE-BILDAUSGABE               VALUE 6.                00351008
           88  FTASTE-BILDAUSGABE-BILDLAST      VALUE 7.                00353008
      *                                                                 00370000
      ******** ENDE COPY-MEMBER STEUER-FELDER (DRAT0017) ************** 00380000

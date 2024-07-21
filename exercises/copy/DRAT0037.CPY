      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0037.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0037.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:40:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CA-LINK                          (DRAT0037)   * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION                                                    * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  * 00060000
      *    DATUM:    02.04.1993                                       * 00070000
      *    STAND:    05.04.1993                                       * 00080008
      ***************************************************************** 00090000
      *                                                                 00100000
      *    COMMAREA FUER LINK-AUFRUFE                                   00110000
      *                                                                 00120000
      *    ACHTUNG: BEI AENDERUNG DER LAENGE DER CA-LINK IST            00121006
      *             PARALLEL ZU AENDERN:                                00122004
      *             - LAENGE DER DFHCOMMAREA (LINKAGE-SECTION)          00122204
      *               IN ALLEN LINK-PROGRAMMEN                          00123004
      *               (SUCHEN: ALLE LINK-PG, DIE DRAT0037 VERWENDEN)    00124010
      *                                                                 00125003
       01  K-CA-LINK-LEN                 PIC S9(004) COMP VALUE 1056.   00130005
                                                                        00130104
       01  CA-LINK.                                                     00130204
           05  CA-LINK-RAHMEN.                                          00131007
               10  CA-LKR-KZN            PIC  X(001)  VALUE SPACE.      00131107
                   88  CA-LKR-KZN-NEUANW              VALUE 'N'.        00131208
               10  CA-LKR-TSQLKE-NAME    PIC  X(008)  VALUE SPACE.      00131307
               10  CA-LKR-S-TRACELEN     PIC  9(001).                   00131409
               10  FILLER                PIC  X(022).                   00131509
                                                                        00131607
           05  CA-LINK-DATA              PIC  X(1024).                  00132006
                                                                        00140000
      *                                                                 00150000
      ******** ENDE COPY-MEMBER CA-LINK               (DRAT0037) ****** 00160000

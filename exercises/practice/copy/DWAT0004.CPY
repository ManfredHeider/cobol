      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWAT0004.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DWAT0004.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:30:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DWAT0004      TITEL VERSORGEN - HILFEDIALOG   * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   001                                              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    24.04.1993                                       * 00070003
      *    STAND:    24.04.1993                                       * 00080003
      ***************************************************************** 00090000
      *                                                                 00100000
      *    IM HILFEDIALOG WERDEN DIE 2 TITELFELDER IM MASKENKOPF WIE    00110000
      *    FOLGT VERSORGT:                                              00120000
      *    - TITEL 1: KONSTANT                                          00130000
      *    - TITEL 2: ABHÄNGIG VOM CA-AUFRUF-OP                         00140000
      *               DER TITEL 2 IST ZUSAMMEN MIT DEM AUFRUF-OP        00150000
      *               IN EINER TABELLE ABGELEGT                         00160000
      *    (ZUGEHÖRIGES COPY-ELEMENT DER PROCEDURE DIV.: DPAT0004)      00170000
      *                                                                 00180000
      * --------------------------------------------------------------  00190000
      *   TITEL-1-ZEILE FUER MASKEN-KOPF                                00200000
      *   !!! MUSS-FELDER !!!                                           00210000
      *                        1         2  M      3         4          00211001
      *               123456789012345678901234567890123456789012345     00212001
       01  K-DIALOG-TITEL1          PIC  X(045) VALUE                   00213001
                     '                    HILFE                    '.   00213101
                                                                        00280000
      *   TITEL-2-ZEILEN FUER MASKEN-KOPF                               00290000
      *   (ABHAENGIG VOM AUFRUF-OP)                                     00300000
       01  T-TITEL2-TAB.                                                00310000
      *        * INHALT JEWEILS: CA-AUFRUF-OP, ZUGEH. TITEL-2           00320000
           05  FILLER.                                                  00330000
               10  FILLER                PIC  X(003)  VALUE 'F1 '.      00340000
               10  FILLER                PIC  X(045)  VALUE             00350001
      *                        1         2  M      3         4          00351001
      *               123456789012345678901234567890123456789012345     00352001
                     '                 DIALOG-BILD                 '.   00380001
           05  FILLER.                                                  00390000
               10  FILLER                PIC  X(003)  VALUE 'HFG'.      00400000
               10  FILLER                PIC  X(045)  VALUE             00410101
                     '                 FELD-GRUPPE                 '.   00420001
           05  FILLER.                                                  00430000
               10  FILLER                PIC  X(003)  VALUE 'HFT'.      00440000
               10  FILLER                PIC  X(045)  VALUE             00441001
                     '               FUNKTIONSTASTEN               '.   00460001
                                                                        00470000
       01  T-TITEL2-TAB-RED REDEFINES T-TITEL2-TAB.                     00480000
           05  T-T2-ZEILE OCCURS  3                                     00490000
                          INDEXED BY I-TITEL2.                          00491000
               10  T-TITEL2-OP           PIC  X(003).                   00492000
               10  T-TITEL2-TEXT         PIC  X(045).                   00493001
      *                                                                 00500000
      ******* ENDE COPY-MEMBER DWAT0004 (TITEL - HILFEDIALOG) ********* 00510000
CCC009*    EJECT                                                        00520000

      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWAT0025.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DWAT0025.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:32:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DWAT0025  PARMS PRUEFEN TRANS IN NEUANW.      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   001                                              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    14.04.1993                                       * 00070000
      *    STAND:    20.04.1993                                       * 00080003
      ***************************************************************** 00090000
      *                                                                 00100000
      *    FELDER FUER PRUEFEN TRANS IN NEUANWENDUNGEN                  00110000
      *    (ZUGEHÖRIGES COPY-ELEMENT DER PROCEDURE DIV.: DPAT0025)      00110100
      *                                                                 00110200
      *    EINGABE: ALLE EINGABE-FELDER MUESSEN GEFUELLT SEIN           00110300
      *                                                                 00110400
      *    AUSGABE GEFUELLT ABH. VON TR-STATUS:                         00110500
      *     ----------------------+-------------+------------+          00110600
      *                           !  MELD-KEY   ! TRANS-SATZ !          00110700
      *     ----------------------+-------------+------------+          00110800
      *     TR-OK                 !             !     X      !          00110900
      *     TR-OK-ALTANW          !             !     X      !          00111000
      *     TR-OK-ALTANW-KS       !             !            !          00111100
      *     TR-UNGUELTIG          !      X      !            !          00111200
      *     TR-GESPERRT           !      X      !     X      !          00111300
      *     TR-KEINE-KOMPETENZ    !      X      !     X      !          00111400
      *     TR-UNGUELT-BILDSCHIRM !      X      !     X      !          00111500
      *     TR-UNGUELT-IM-UMODUS  !      X      !    (-)     !          00111603
      *     ----------------------+-------------+------------+          00111700
      *                                                                 00111800
      *     (-) DER TRANS-SATZ IST ZWAR GEFUELLT, FALLS                 00111903
      *         TRANS /= DVAG,                                          00162003
      *         ABER WG. DER UNEINDEUTIGKEIT SOLLTE NICHT MIT IHM       00163003
      *         GEARBEITET WERDEN                                       00164003
      *                                                                 00165003
       01  ZI-DWAT0025-EINGABE.                                         00181100
           05  ZI-DWAT0025-TRANS-AKT           PIC  X(004).             00181200
           05  ZI-DWAT0025-USITZG-ANZ          PIC  9(001).             00181300
                                                                        00181400
       01  ZO-DWAT0025-AUSGABE.                                         00181500
           05  ZO-DWAT0025-TR-STATUS           PIC  9(002).             00181600
               88  TR-OK                                    VALUE  1.   00181700
               88  TR-OK-ALTANW                             VALUE  2.   00181800
               88  TR-OK-ALTANW-KS                          VALUE  3.   00181900
               88  TR-UNGUELTIG                             VALUE  4.   00182000
               88  TR-GESPERRT                              VALUE  5.   00182100
               88  TR-KEINE-KOMPETENZ                       VALUE  6.   00182200
               88  TR-UNGUELT-BILDSCHIRM                    VALUE  7.   00182300
               88  TR-UNGUELT-IM-UMODUS                     VALUE  8.   00182400
           05  ZO-DWAT0025-TR-MELD-KEY.                                 00182500
               10  ZO-DWAT0025-TR-MELD-PGM     PIC  X(008).             00182602
               10  ZO-DWAT0025-TR-MELD-TYP     PIC  X(001).             00182702
               10  ZO-DWAT0025-TR-MELD-NR      PIC  9(004) COMP.        00182802
      * --> DIREKT IN TRANS-SATZ IN WORKING-ST.                         00182901
      *    05  ZO-DWAT0025-TRANS-SATZ          PIC  X(200).             00183001
      *                                                                 00184000
      ******* ENDE COPY-MEMBER DWAT0025 (PRUEF TRANS IN NEUANW) ******* 00190000

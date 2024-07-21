      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0025.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0025.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:38:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: AUFRUFPARAMETER UPAT0005  (DRAT0025)          * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION                                                    * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  * 00060000
      *    DATUM:    04.03.1993                                       * 00070000
      *    STAND:    05.03.1993                                       * 00080002
      ***************************************************************** 00090000
      *                                                                 00100000
      *    AUFRUF-PARAMETER FUER UPRO UPAT0005                          00110001
      *    (AUSGABE: NUR ERR-RC IN ERR-ERROR-BEREICH)                   00120001
      *                                                                 00260000
       01  PI-UPAT0005-EINGABE.                                         00270001
           05  PI-UPAT0005-FILLER-1    PIC  X(007).                     00280004
           05  PI-UPAT0005-HPROG-ID    PIC  X(008).                     00280404
           05  PI-UPAT0005-FILLER-2    PIC  X(001).                     00281004
           05  PI-UPAT0005-UPROG-ID    PIC  X(008).                     00290002
           05  PI-UPAT0005-FILLER-3    PIC  X(001).                     00291004
           05  PI-UPAT0005-SQLCODE     PIC  +Z99.                       00300003
           05  PI-UPAT0005-FILLER-4    PIC  X(001).                     00301004
           05  PI-UPAT0005-SQLERRMC    PIC  X(070).                     00310002
                                                                        00530002
      *                                                                 00542002
      ******** ENDE COPY-MEMBER AUFRUF UPAT0005    (DRAT0025) ********* 00550001

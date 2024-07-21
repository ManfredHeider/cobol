      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0034.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0034.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:40:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: AUFRUFPARAMETER UPAT0009  (DRAT0034)          * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION                                                    * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  * 00060000
      *    DATUM:    01.04.1993                                       * 00070000
      *    STAND:    15.04.1993                                       * 00080004
      ***************************************************************** 00090000
      *                                                                 00100000
      *    AUFRUF-PARAMETER FUER UPRO UPAT0009 (AUSSER E-ERROR-BEREICH).00110000
      *    - KONVERTIERUNG TRANS (BEIDE RICHTUNGEN) -                   00120003
      *                                                                 00130003
      *    ABHAENGIG VON PI-UPAT0009-UMS SIND FOLGENDE PARAMS ZU FUELLEN00140002
      *       UND WERDEN FORGENDE WERTE ZURÜCKGEGEBEN (AUSSER ERROR):   00150000
      *                                                                 00160000
      *    UMS                         !  N  !  A  !                    00170002
      *    ----------------------------+-----+-----+                    00180000
      *    PI-UPAT0009-TRANS-IN-NUM    !     !  X  !                    00190004
      *    PI-UPAT0009-TRANS-IN-ANUM   !  X  !     !                    00200004
      *    ----------------------------+-----+-----+                    00210000
      *    PO-UPAT0009-TRANS-OUT-NUM   !  X  !     !                    00220004
      *    PO-UPAT0009-TRANS-OUT-ANUM  !     !  X  !                    00230004
      *    ----------------------------+-----+-----+                    00240000
      *                                                                 00250000
       01  PI-UPAT0009-EINGABE.                                         00260000
           05  PI-UPAT0009-UMS                 PIC  X(001).             00270002
               88  PI-UPAT0009-UMS-NACH-NUM                VALUE 'N'.   00280002
               88  PI-UPAT0009-UMS-NACH-ANUM               VALUE 'A'.   00290002
           05  PI-UPAT0009-TRANS-IN-NUM.                                00300004
               10  PI-UPAT0009-TRANS-IN-NUM-N  PIC  9(006).             00310004
           05  PI-UPAT0009-TRANS-IN-ANUM.                               00320004
               10  FILLER                      PIC  X(004).             00330000
                                                                        00340000
       01  PO-UPAT0009-AUSGABE.                                         00350000
           05  PO-UPAT0009-TRANS-OUT-NUM.                               00360004
               10  PO-UPAT0009-TRANS-OUT-NUM-N PIC  9(006).             00370004
           05  PO-UPAT0009-TRANS-OUT-ANUM.                              00380004
               10  FILLER                      PIC  X(004).             00390000
      *                                                                 00400000
      ******** ENDE COPY-MEMBER AUFRUF UPAT0009    (DRAT0034) ********* 00410000

      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0028.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0028.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:39:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: AUFRUFPARAMETER UPAT0007  (DRAT0028)          * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION                                                    * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  * 00060000
      *    DATUM:    05.03.1993                                       * 00070000
      *    STAND:    05.03.1993                                       * 00080000
      ***************************************************************** 00090000
      *                                                                 00100000
      *    AUFRUF-PARAMETER FUER UPRO UPAT0007 (ERMITTELN TRANS-SCHALT) 00110000
      *                                                                 00120000
      *    TRANS-SCHALT:                                                00120100
      *    ZU JEDEM TRANSAKTIONSCODE DER NEUANWENDUNGEN GIBT ES EINEN   00121000
      *    ZWILLINGS-TRANSAKTIONSCODE, DEM DER GLEICHE DB2-PLAN         00122000
      *    ZUGEORDNET IST, MIT DEM ABER DAS VORSCHALTPROGRAMM AUFGE-    00123000
      *    RUFEN WIRD (MIT START TRANSID), UM DEN PLAN ZU WECHSELN.     00124000
      *                                                                 00190000
       01  PI-UPAT0007-EINGABE.                                         00200000
           05  PI-UPAT0007-TRANS               PIC  X(004).             00210000
                                                                        00330000
       01  PO-UPAT0007-AUSGABE.                                         00340000
           05  PO-UPAT0007-TRANS-SCHALT        PIC  X(004).             00350000
      *                                                                 00360000
      ******** ENDE COPY-MEMBER AUFRUF UPAT0007    (DRAT0028) ********* 00370000

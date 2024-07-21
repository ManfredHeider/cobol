      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0038.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0038.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:41:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRAT0038  COMMAREA FÜR KOMPETENZPRÜFUNG       * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   2    26.08.94 16:33:33
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: H. LEISTE                                        * 00060000
      *    DATUM:    05.04.1993                                       * 00070000
      *    STAND:    14.04.1993                                       * 00080000
      *                                                               * 00080100
      *    GEAENDERT: WIECHERS                                        * 00081000
      *    GRUND:    AFRZ0013 - UMSTELLUNG KUS-SYSTEM AUF RACF        * 00082000
      *    STAND:    26.08.9494                                       * 00083001
      ***************************************************************** 00084000
      *                                                                 00085000
           05  FILLER  REDEFINES  CA-LINK-DATA.                         00086000
               10  CA-LINK-EINGABE.                                     00087000
260894*            15  CA-LINK-PERSNR           PIC  9(007) COMP-3.     00088001
260894             15  CA-LINK-USERID           PIC  X(007).            00089001
                   15  CA-LINK-TRANSID          PIC  X(004).            00090000
               10  CA-LINK-AUSGABE.                                     00100000
                   15  CA-LINK-TRANS-SATZ       PIC  X(200).            00110000
                   15  CA-LINK-PERS-SATZ        PIC  X(120).            00120000
               10  CA-LINK-FEHLER-NR            PIC  9(002).            00121000
260894*        10  FILLER                       PIC  X(694).            00121101
260894         10  FILLER                       PIC  X(691).            00121201
      *                                                                 00121300
      ******** ENDE COPY-MEMBER DRAT0038 (CA FUER KOMPETENZPRÜF) ****** 00121400

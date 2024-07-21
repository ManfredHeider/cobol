      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0036.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0036.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:40:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRAT0036   DRUCKAUFBER. FELDER LINK-RAHMEN    * 00020000
      *    --------------------------------------------------------   * 00040000
      *    VERSION                                                    * 00050000
      *    --------------------------------------------------------   * 00060000
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  * 00070000
      *    DATUM:    05.04.1993                                       * 00080000
      *    STAND:    05.04.1993                                       * 00090000
      ***************************************************************** 00100000
      *                                                                 00110000
       01  D-CA-LINK-ERR.                                               00120000
           05 FILLER                PIC  X(041) VALUE                   00130000
                            'FALSCHE LAENGE DER DFHCOMMAREA BEI LINK: '.00140000
           05 D-CA-LINK-ERR-LAENGE  PIC  ----9.                         00150000
       01  D-READQ-ERR.                                                 00160000
           05 FILLER                PIC  X(037) VALUE                   00170000
                                'FEHLER BEI LESEN TSQLKE AUFGETRETEN: '.00180000
           05 D-READQ-ERR-RC        PIC  ----9.                         00190000
       01  D-REWRITEQ-ERR.                                              00200000
           05 FILLER                PIC  X(039) VALUE                   00210000
                              'FEHLER BEI REWRITE TSQLKE AUFGETRETEN: '.00220000
           05 D-REWRITEQ-ERR-RC     PIC  ----9.                         00230000
                                                                        00457000
      *                                                                 00458000
      ******** ENDE COPY-MEMBER DRUCK-FELDER LINK-RAHMEN (DRAT0036) *** 00459000

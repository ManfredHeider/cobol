      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0030.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0030.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:39:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: ERROR-DRUCKAUFB. TSQ-SERVICE      (DRAT0030)  * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION                                                    * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  * 00060000
      *    DATUM:    19.03.1993                                       * 00070000
      *    STAND:    23.03.1993                                       * 00080002
      ***************************************************************** 00090000
      *                                                                 00100000
      *    DRUCKAUFBEREITETE SCHNITTSTELLE ZUR TS-QUEUE-SERVICE-FKT.    00110000
      *    AUSGABE IM ERROR-BILD (D-ERR-VAR-RAHMEN)                     00120000
      *                                                                 00130000
       01  D-TSQ.                                                       00140000
                                                                        00150000
           05  FILLER.                                                  00160000
               10 FILLER                 PIC  X(010) VALUE              00170000
                           'TSQ-INPUT:'.                                00180000
               10 FILLER                 PIC  X(010) VALUE              00190000
                           '  SERVICE='.                                00200000
               10 D-TSQ-SERVICE          PIC  X(001).                   00210000
               10 FILLER                 PIC  X(001) VALUE              00220000
                           '='.                                         00230000
               10 D-TSQ-SERVICE-T        PIC  X(007).                   00240000
               10 FILLER                 PIC  X(008) VALUE              00250000
                           '  PLACE='.                                  00260000
               10 D-TSQ-PLACE            PIC  9(001).                   00270002
               10 FILLER                 PIC  X(001) VALUE              00280000
                           '='.                                         00290000
               10 D-TSQ-PLACE-T          PIC  X(007).                   00300000
               10 FILLER                 PIC  X(007) VALUE              00310000
                           '  NAME='.                                   00320000
               10 D-TSQ-NAME             PIC  X(005).                   00330002
               10 FILLER                 PIC  X(001) VALUE              00331001
                           '/'.                                         00332001
               10 D-TSQ-NAME-T1          PIC  9(004).                   00333002
               10 FILLER                 PIC  X(001) VALUE              00334002
                           '/'.                                         00335002
               10 D-TSQ-NAME-T2          PIC  9(004).                   00336002
               10 FILLER                 PIC  X(007) VALUE              00340000
                           '  ITEM='.                                   00350000
               10 D-TSQ-ITEM             PIC  ----9.                    00360000
                                                                        00380000
           05  FILLER.                                                  00440000
               10 FILLER                 PIC  X(011) VALUE              00450000
                           'TSQ-OUTPUT:'.                               00460000
               10 FILLER                 PIC  X(009) VALUE              00470000
                           '  TSQ-RC='.                                 00480000
               10 D-TSQ-RC               PIC  X(001).                   00490000
               10 FILLER                 PIC  X(001) VALUE              00500000
                           '='.                                         00510000
               10 D-TSQ-RC-T             PIC  X(010).                   00520000
               10 FILLER                 PIC  X(048).                   00530000
      *                                                                 00590000
      ******** ENDE COPY-MEMBER ERROR-DRUCKAUFB. TSQ  (DRAT0030) ****** 00600000

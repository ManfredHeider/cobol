      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0021.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0021.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:37:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: ERROR-DRUCKAUFB. DB2-SCHREIBMODUL (DRAT0021)  * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION                                                    * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  * 00060000
      *    DATUM:    22.02.1993                                       * 00070000
      *    STAND:    22.02.1993                                       * 00080000
      ***************************************************************** 00090000
      *                                                                 00100000
      *    DRUCKAUFBEREITETE SCHNITTSTELLE ZU SCHREIBMODULN FUER        00110000
      *    AUSGABE IM ERROR-BILD (D-ERR-VAR-RAHMEN)                     00120001
      *                                                                 00130000
       01  D-DB2S.                                                      00140000
                                                                        00150000
           05  FILLER.                                                  00760000
               10 FILLER                 PIC  X(021) VALUE              00770000
                           'DB2-SCHREIBMODUL/ PS:'.                     00780000
               10 FILLER                 PIC  X(009) VALUE              00790000
                           '  PGM-ID='.                                 00800000
               10 D-PS-PGM-ID            PIC  X(008).                   00810000
               10 FILLER                 PIC  X(009) VALUE              00820000
                           '  PGM-KZ='.                                 00830000
               10 D-PS-PGM-KZ            PIC  X(001).                   00840000
               10 FILLER                 PIC  X(010) VALUE              00850000
                           '  USER-ID='.                                00860000
               10 D-PS-USER-ID           PIC  X(007).                   00870000
               10 FILLER                 PIC  X(010) VALUE              00880000
                           '  TERM-ID='.                                00890000
               10 D-PS-TERM-ID           PIC  X(004).                   00900000
               10 FILLER                 PIC  X(001).                   00910000
                                                                        00920000
           05  FILLER.                                                  00930000
               10 FILLER                 PIC  X(021) VALUE SPACE.       00940000
               10 FILLER                 PIC  X(013) VALUE              00950000
                           '   DATE-AEND='.                             00960000
               10 D-PS-DATE-AE           PIC  X(010).                   00970000
               10 FILLER                 PIC  X(012) VALUE              00980000
                           '   DATE-VON='.                              00990000
               10 D-PS-DATE-VON          PIC  X(010).                   01000000
               10 FILLER                 PIC  X(011) VALUE              01010000
                           '   AEND-KZ='.                               01020000
               10 D-PS-AEND-KZ           PIC  X(001).                   01030000
               10 FILLER                 PIC  X(002).                   01040000
                                                                        01050000
           05  FILLER.                                                  01060000
               10 FILLER                 PIC  X(021) VALUE              01070000
                           'DB2-SCHREIBMODUL/ PR:'.                     01080000
               10 FILLER                 PIC  X(008) VALUE              01090000
                           '   CODE='.                                  01100000
               10 D-PR-CODE              PIC  X(001).                   01110000
               10 FILLER                 PIC  X(007) VALUE              01120000
                           '   TYP='.                                   01130000
               10 D-PR-TYP               PIC  X(001).                   01140000
               10 FILLER                 PIC  X(010) VALUE              01150000
                           '   PGM-ID='.                                01160000
               10 D-PR-PGM-ID            PIC  X(008).                   01170000
               10 FILLER                 PIC  X(013) VALUE              01180000
                           '   FEHLER-NR='.                             01190000
               10 D-PR-F-NR              PIC  -(004)9.                  01200000
               10 FILLER                 PIC  X(006).                   01210000
                                                                        01220000
           05  FILLER.                                                  01230000
               10 FILLER                 PIC  X(010).                   01240000
               10 D-PR-F-TEXT            PIC  X(070).                   01250000
      *                                                                 02640000
      ******** ENDE COPY-MEMBER ERROR-DRUCKAUFB. DB2S (DRAT0021) ****** 02650000

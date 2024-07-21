      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU012S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU012S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:45:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   2    11.08.93 17:50:14
      ******************************************************
      *                                                    *
      *            COPY-MEMBER TRANSIENT-DATA  VB-DATEI    *
      *            DATEINUMMER: DD06                       *
      *            COPYMEMBER:  UU012S                     *
      *            SATZART:     12                         *
      *            SATZLAENGE:  740                        *
      *              GEANDERT AM 09.09.91 VON 504 BYTES  *
      *                                                    *
      * AENDERUNG AM 15.12.86 - GRUENING                   *
      * NEUE FELDER ANGELEGT:                              *
      * M2BESCHRAENK, M2KOFFER1, M2KOFFER2, M2GOLDUHR,     *
      * M2GOLDPNR, M2GOLDPEN.                              *
      * AENDERUNG AM 13.04.87 - GRUENING                   *
      * NEUES FELD : M2PCNR                                *
      * UU89025 EINFUEHRUNG 2.PCNR M2PCNR2                 *
      * AENDERUNG AM 26.04.89 - GEIGER                     *
      *                                                    *
      * AENDERUNG AM 24.07.90 (BIEHL):                     *
      *   MAUSL   PIC X(3)   NEU                           *
      *   MPLZ    PIC X(6)   GEAEND./VORHER: PIC 9(4)      *
      *   MFILLER PIC X(31)  GEAEND./VORHER: PIC X(36)     *
      *    ==> SATZLAENGE BLEIBT BESTEHEN                  *
      *                                                    *
      * AENDERUNG AM 15.08.90 (PRIGGEN)                    *
      *   M2AUSL        PIC X(3)   NEU                     *
      *   M2PLZ         PIC X(6)   GEAEND./VORH.: PIC 9(4) *
      *   M2ZUSATZVER3  PIC XX     NEU                     *
      *   M2ZUSATZVERKZ PIC X      NEU                     *
      *   M2VSW-G-KZ    PIC XX     NEU                     *
      *   M2VSW-ZVKZ    PIC 9(4)   COMP-3 NEU              *
      *   M2VSW-G-VSNR  PIC 9(10)  COMP-3 NEU              *
      *   M2VSW-ZV-VSNR PIC 9(10)  COMP-3 NEU              *
      *   M2VSW-A-KZ    PIC XX     NEU                     *
      *   M2VSW-IG-DAT  PIC 9(4)   COMP-3 NEU              *
      *   MFILLER       PIC X(1)   GEAEND./VORH.: PIC X(31)*
      *    ==> SATZLAENGE BLEIBT BESTEHEN                  *
      * AENDERUNG AM 04.10.90 (NAUMANN)                    *
      *   MLBDAT2       PIC 9(6)   C-3 WIRD ZU             *
      *   MVARPROV      PIC 9(5)V9 C-3                     *
      *    ==> SATZLAENGE BLEIBT ERHALTEN                  *
      *                                                    *
      * AENDERUNG AM 06.05.91 (NAUMANN)                    *
      *   M2VBHANDB     PIC X      NEU                     *
      *   FILLER        PIC X(1)   ENTFAELLT               *
      *    ==> SATZLAENGE BLEIBT ERHALTEN                  *
      *                                                    *
      * AENDERUNG AM 09.09.91 (VON DER GOLTZ)              *
      *   MERSTELL-DAT  PIC 9(06)  C-3  NEU                *
      *   M2FAP-VSNR    PIC 9(10)  C-3  NEU                *
      *   M2FAX         PIC X(14)  NEU                     *
      *   M2ZV-KUEDAT   PIC 9(06)  C-3  NEU                *
      *   M2INSTITUT    PIC X(22)  NEU                     *
      *   M2KTOINH      PIC X(25)  NEU                     *
      *   M2BLZ         PIC 9(08)  C-3  NEU                *
      *   M2KTONR       PIC 9(11)  C-3  NEU                *
      *   FILLER        PIC X(150) NEU                     *
      *    ==> SATZLAENGE GEAENDERT                        *
      *                                                    *
      * AENDERUNG AM 17.12.91 (MUSALL)                     *
      *   M2AFOKZ       PIC X      NEU                     *
      *   FILLER        PIC X(149) VORHER: PIC X(150)      *
      *    ==> SATZLAENGE BLEIBT ERHALTEN                  *
      *                                                    *
      * AENDERUNG AM 18.08.92 (V.D.GOLTZ)                  *
      * NEUE FELDER: M2PCNR3     PIC X(5)                  *
      *              M2PCNR3-KZ  PIC X(1)                  *
      *              FILLER   PIC X(143) VORHER: PIC X(149)*
      *    ==> SATZLAENGE BLEIBT ERHALTEN                  *
      *                                                    *
      * AENDERUNG AM 12.07.93 (DECKER)                     *
      * NEUE FELDER: M2PCNR4     PIC X(5)                  *
      *              FILLER   PIC X(138) VORHER: PIC X(143)*
      *    ==> SATZLAENGE BLEIBT ERHALTEN                  *
      ******************************************************
       01  UU012S.
           05  MMODUS                  PIC XX.
           05  MDAT                    PIC 9(7)    COMP-3.
           05  MUHR                    PIC 9(9)    COMP-3.
           05  MTERM                   PIC X(4).
           05  MPERSNR                 PIC 9(7)    COMP-3.
           05  MERSTELL-DAT            PIC 9(06)   COMP-3.
           05  MSATZ.
               10  MSA                 PIC 99.
               10  MMANR               PIC 9(7)    COMP-3.
               10  MPZ                 PIC X.
               10  MANSCHR.
                   20  MANREDE         PIC 9.
                   20  MZUNAM          PIC X(20).
                   20  MVNAME          PIC X(20).
                   20  MSTRASSE        PIC X(20).
                   20  MAUSL           PIC X(3).
                   20  MPLZ            PIC X(6).
                   20  MORT            PIC X(16).
               10  MBANK.
                   20  MINSTITUT       PIC X(22).
                   20  MBLZ            PIC 9(8)    COMP-3.
                   20  MKTONR          PIC 9(11)   COMP-3.
                   20  MKTOINH         PIC X(25).
               10  MSPERRE             PIC 9.
               10  FILLER              PIC XXX.
               10  MPROVRUECK          PIC 999V9   COMP-3.
               10  MEINDAT             PIC 9(6)    COMP-3.
               10  MAUSDAT             PIC 9(6)    COMP-3.
               10  MAUSGR              PIC 99.
               10  MLBDAT1             PIC 9(6)    COMP-3.
               10  MSANR               PIC 999     COMP-3.
               10  MBETREUER.
                   20  MVONDAT         PIC 9(6)    COMP-3.
                   20  MBISDAT         PIC 9(6)    COMP-3.
                   20  MDIR            PIC 999     COMP-3.
                   20  MSTUFE          PIC 999     COMP-3.
                   20  MSTATUS         PIC X.
                   20  MBMANR          PIC 9(7)    COMP-3.
                   20  MAUSPROV        PIC 999V9   COMP-3.
                   20  MEPROVKZ        PIC X.
                   20  MVARPROV        PIC 9(5)V9  COMP-3.
               10  MGEBDAT             PIC 9(6)    COMP-3.
               10  MTELNR              PIC X(14).
               10  MNBETR              PIC 9(7)    COMP-3.
               10  MANSCHRART          PIC X.
               10  MWVBNR              PIC 9(7)    COMP-3.
           05  M2SATZ.
               10  M2SA                PIC 99.
               10  M2MANR              PIC 9(7).
               10  M2ANSCHR.
                   20  M2STRASSE       PIC X(20).
                   20  M2AUSL          PIC X(3).
                   20  M2PLZ           PIC X(6).
                   20  M2ORT           PIC X(16).
                   20  M2ANSCHRART     PIC X.
                   20  M2TELNR         PIC X(14).
               10  M2VBDATEN.
                   20  M2SEIT          PIC 9(6)    COMP-3.
                   20  M2VERTRVOM1     PIC 9(6)    COMP-3.
                   20  M2VERTRVOM2     PIC 9(6)    COMP-3.
                   20  M2VBNADEL       PIC X(3).
                   20  M2ZUSATZVER1    PIC XX.
                   20  M2ZUSATZVER2    PIC 9(6)    COMP-3.
                   20  M2ZUSATZVER3    PIC XX.
                   20  M2ZUSATZVERKZ   PIC X.
               10  M2AUSKUNFT.
                   20  M2SCHUFADAT     PIC 9(6)    COMP-3.
                   20  M2SCHUFAAUSK    PIC X(10).
                   20  M2GEWO          PIC 9(6)    COMP-3.
                   20  M2DARLKZ        PIC X.
               10  M2VBSEMINARE.
                   20  M2VORSTSEM      PIC 9(4)    COMP-3.
                   20  M2ALSEM         PIC 9(4)    COMP-3.
                   20  M2GAPRUE        PIC 9(4)    COMP-3.
                   20  M2BDVPRUE       PIC 9(4)    COMP-3.
                   20  M2MITGL         PIC X.
                   20  M2RAUT1         PIC 9(4)    COMP-3.
                   20  M2RAUT2         PIC 9(4)    COMP-3.
                   20  M2RAUT3         PIC 9(4)    COMP-3.
                   20  M2RAUT4         PIC 9(4)    COMP-3.
                   20  M2GABVON        PIC 9(4)    COMP-3.
                   20  M2GABBIS        PIC 9(4)    COMP-3.
                   20  M2GABABS        PIC 9(4)    COMP-3.
               10  M2HAUSTARIFKZ       PIC X.
               10  M2CORRELL           PIC X.
               10  M2FAP.
                   20  M2FAPAB         PIC 9(4)    COMP-3.
                   20  M2FAPSTUFE      PIC X.
               10  M2VSW.
                   20  M2VSWZUS        PIC 9(6)    COMP-3.
                   20  M2VSWGR         PIC 9(6)    COMP-3.
                   20  M2GRVSU         PIC 9(6)    COMP-3.
                   20  M2VSW-G-KZ      PIC X(2).
                   20  M2VSW-ZVKZ      PIC 9(4)    COMP-3.
                   20  M2VSW-G-VSNR    PIC 9(10)   COMP-3.
                   20  M2VSWZU         PIC 9(6)    COMP-3.
                   20  M2ZUVSU         PIC 9(6)    COMP-3.
                   20  M2VSW-ZV-VSNR   PIC 9(10)   COMP-3.
                   20  M2AUFBAU.
                       30 M2VSW-A-KZ   PIC X(2).
                       30 M2VSW-IG-DAT PIC 9(4)    COMP-3.
                       30 M2VSWGES     PIC 9(6)    COMP-3.
                       30 M2VSWLZ      PIC 9(6)    COMP-3.
                       30 M2VSWAM      PIC 9(4)    COMP-3.
               10  M2OLZ.
                   20  M2OLZQ          PIC X(4).
                   20  M2OLZORG        PIC 9(5)    COMP-3.
                   20  M2OLZBU         PIC 9(5)    COMP-3.
                   20  M2OLZGER        PIC 9(5)    COMP-3.
               10  M2BESCHRAENK        PIC X(20).
               10  M2KOFFER1           PIC 9(4)    COMP-3.
               10  M2KOFFER2           PIC 9(4)    COMP-3.
               10  M2GOLDUHR           PIC 9(4)    COMP-3.
               10  M2GOLDPNR           PIC XX.
               10  M2GOLDPEN           PIC 9(4)    COMP-3.
               10  M2PCNR              PIC 9(5)    COMP-3.
               10  M2PCNR2             PIC 9(5)    COMP-3.
               10  M2VBHANDB           PIC X(1).
               10  M2FAP-VSNR          PIC 9(10)   COMP-3.
               10  M2FAX               PIC X(14).
               10  M2ZV-KUEDAT         PIC 9(6)    COMP-3.
               10  M2INSTITUT          PIC X(22).
               10  M2KTOINH            PIC X(25).
               10  M2BLZ               PIC 9(8)    COMP-3.
               10  M2KTONR             PIC 9(11)   COMP-3.
               10  M2AFOKZ             PIC X.
               10  M2PCNR3             PIC X(5).
               10  M2PCNR3-KZ          PIC X(1).
120793         10  M2PCNR4             PIC X(5).
120793         10  FILLER              PIC X(138).


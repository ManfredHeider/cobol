      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU019S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU019S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:46:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   4    16.09.93 17:15:55
      ******************************************************
      *                                                    *
      *            COPY-MEMBER VB-DATEI2                   *
      *            DATEINUMMER: UU0017                     *
      *            COPYMEMBER:  UU019S                     *
      *            SATZART:     19                         *
      *            SATZLAENGE:  568 BYTES                  *
      *            GEANDERT AM 09.09.91 VON 400 BYTES      *
      *                                                    *
      * AENDERUNG AM 15.12.86 - GRUENING                   *
      * NEUE FELDER ANGELEGT:                              *
      * M2BESCHRAENK, M2KOFFER1, M2KOFFER2, M2GOLDUHR,     *
      * M2GOLDPNR, M2GOLDPEN.                              *
      * AENDERUNG AM 13.04.87 - GRUENING                   *
      * NEUES FELD : M2PCNR                                *
      * UU89025 EINFUEHRUNG 2.PCNR M2PCNR2                 *
      * AENDERUNG AM 26.04.89 - GEIGER                     *
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
      *   FILLER        PIC X(65)  GEAEND./VORH.: PIC X(95)*
      *    ==> SATZLAENGE BLEIBT BESTEHEN                  *
      * AENDERUNG AM 06.05.91 - NAUMANN                    *
      * NEUES FELD : M2VBHANDB                             *
      *    M2VBHANDB   PIC X      NEU                      *
      *    FILLER      PIC X(64)  GEAEND./VORH.:  PIC X(65)*
      *    ==> SATZLAENGE BLEIBT BESTEHEN                  *
      * AENDERUNG AM 09.09.91 - VON DER GOLTZ              *
      * NEUE FELDER:                                       *
      *    M2FAP-VSNR  PIC 9(10)  C-9  NEU                 *
      *    M2FAX       PIC X(14)  NEU                      *
      *    M2ZV-KUEDAT PIC 9(06)  C-9  NEU                 *
      *    M2INSTITUT  PIC X(22)  NEU                      *
      *    M2KTOINH    PIC X(25)  NEU                      *
      *    M2BLZ       PIC 9(08)  C-9  NEU                 *
      *    M2KTONR     PIC 9(11)  C-9  NEU                 *
      *    FILLER      PIC X(150) GEAEND./VORH.:  PIC X(64)*
      *    ==> SATZLAENGE GEAENDERT                        *
      * AENDERUNG AM 17.12.91 - MUSALL                     *
      * NEUES FELD : M2AFOKZ                               *
      *    M2AFOKZ     PIC X      NEU                      *
      *    FILLER      PIC X(149) GEAEND./VORH.: PIC X(150)*
      *    ==> SATZLAENGE BLEIBT BESTEHEN                  *
      *                                                    *
      * AENDERUNG AM 18.08.92 - V.D.GOLTZ                  *
      * NEUE FELDER: M2PCNR3     PIC X(5)                  *
      *              M2PCNR3-KZ  PIC X(1)                  *
      *              FILLER   PIC X(143) VORHER: PIC X(149)*
      *    ==> SATZLAENGE BLEIBT BESTEHEN                  *
      *                                                    *
      * AENDERUNG AM 25.03.93 - V.D.GOLTZ                  *
      * NEUES FELD : M2PCNR4     PIC X(5)                  *
      *              FILLER   PIC X(138) VORHER: PIC X(143)*
      *    ==> SATZLAENGE BLEIBT BESTEHEN                  *
      *                                                    *
      * AENDERUNG AM 29.06.93 - SCHMIDT                    *
      * NEUES FELD : M2PLZALT    PIC X(6)                  *
      *              FILLER   PIC X(132) VORHER: PIC X(138)*
      *    ==> SATZLAENGE BLEIBT BESTEHEN                  *
      *                                                    *
      * AENDERUNG AM 16.09.93 - NAUMANN                    *
      * NEUES FELD : M2RAUT5     PIC 9(4) COMP-3           *
      *              FILLER   PIC X(129) VORHER: PIC X(132)*
      *    ==> SATZLAENGE BLEIBT BESTEHEN                  *
      *                                                    *
      ******************************************************
       01  UU019S.
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
                   20  M2RAUT11        PIC 9(4)    COMP-3.
                   20  M2RAUT12        PIC 9(4)    COMP-3.
                   20  M2RAUT21        PIC 9(4)    COMP-3.
                   20  M2RAUT22        PIC 9(4)    COMP-3.
                   20  M2RAUT31        PIC 9(4)    COMP-3.
                   20  M2RAUT32        PIC 9(4)    COMP-3.
                   20  M2RAUT41        PIC 9(4)    COMP-3.
                   20  M2RAUT42        PIC 9(4)    COMP-3.
                   20  M2GABVON        PIC 9(4)    COMP-3.
                   20  M2GABBIS        PIC 9(4)    COMP-3.
                   20  M2GABABS        PIC 9(4)    COMP-3.
               10  M2HAUSTARIFKZ       PIC X.
               10  M2CORRELL           PIC X.
               10  M2FAP               OCCURS 4.
                   20  M2FAPAB         PIC 9(4)    COMP-3.
                   20  M2FAPSTUFE      PIC 9.
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
                       30 M2VSW-LZ-AM  OCCURS 3.
                          35 M2VSWLZ   PIC 9(6)    COMP-3.
                          35 M2VSWAM   PIC 9(4)    COMP-3.
               10  M2OLZ               OCCURS 4.
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
               10  M2VBHANDB           PIC X.
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
               10  M2PCNR4             PIC X(5).
               10  M2PLZALT            PIC X(06).
               10  M2RAUT5             PIC 9(4)    COMP-3.
               10  FILLER              PIC X(129).
           05  M2DUMMY  REDEFINES  M2SATZ.
               10  M2DSA               PIC 99.
               10  M2DMANR             PIC 9(7).
               10  FILLER              PIC X(559).

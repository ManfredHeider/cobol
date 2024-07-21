      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU010S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU010S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:44:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   1    30.06.93 16:44:24
      ******************************************************************
      *****                                                        *****
      *****        COPY-MEMBER VB-DATEI                            *****
      *****        DATEINUMMER: UU0003                             *****
      *****        COPYMEMBER:  UU010S                             *****
      *****        SATZART:     10     NORMALFALL                  *****
      *****        SATZART:     11     CPD-KONTO                   *****
      *****        SATZLAENGE:  512    BYTES                       *****
      *****                                                        *****
      *****  AENDERUNG: MAUSL PIC X(3)   NEU                       *****
      *****  27.07.90   MPLZ  PIC X(6)   GEAEND. (VORHER: PIC 9(4))*****
      *****            FILLER PIC X(33)  NEU                       *****
      *****     ==> SATZERWEITERUNG UM 38 STELLEN                  *****
      *****     ==> NEUE SATZLAENGE: 550 BYTE                      *****
      *****     (BEI DUMMYSATZ FILLER PIC X(532) EINGEFUEGT)       *****
      *****  AENDERUNG: MLBDAT2 PIC 9(6) C-3 WIRD ZU               *****
      *****  04.10.90   MVARPROV PIC 9(5)V9 C-3                    *****
      *****     ==> SATZLAENGE BLEIBT ERHALTEN                     *****
      *****  AENDERUNG: DIREKTIONENSATZ REDEF GEAENDERT            *****
      *****  07.03.91   OCCURS-EINTRAGUNG VON 99 AUF 200 ERHOEHT   *****
      *****     ==> SATZLAENGE BLEIBT ERHALTEN                     *****
      *****  AENDERUNG: NEUES FELD MPLZALT PIC X(6)                *****
      *****  29.06.93   FILLER VON PIC X(138) AUF PIC X(132) GEÄN. *****
      *****     ==> SATZLAENGE BLEIBT ERHALTEN                     *****
      ******************************************************************
       01  UU010S.
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
               10  MAUSGR23            PIC 9(4)    COMP-3.
               10  MPROVRUECK          PIC 999V9   COMP-3.
               10  MEINDAT             PIC 9(6)    COMP-3.
               10  MAUSDAT             PIC 9(6)    COMP-3.
               10  MAUSGR              PIC 99.
               10  MLBDAT1             PIC 9(6)    COMP-3.
               10  MSANR               PIC 999     COMP-3.
               10  MBETREUER           OCCURS 12.
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
               10  MPROVDAT            PIC 9(6)    COMP-3.
               10  MWVBNR              PIC 9(7)    COMP-3.
               10  MZUSATZKZ           PIC 9.
               10  MEINAUSDATHIST.
                   20  MEINDAT2        PIC 9(6)    COMP-3.
                   20  MEINDAT3        PIC 9(6)    COMP-3.
                   20  MAUSDAT2        PIC 9(6)    COMP-3.
                   20  MAUSDAT3        PIC 9(6)    COMP-3.
               10  MPLZALT             PIC X(06).
               10  FILLER              PIC X(27).
           05  CMD0010     REDEFINES   MSATZ.
               10  MDSA                PIC 99.
               10  MDMANR              PIC 9(7)    COMP-3.
               10  MDANZ               PIC 9(7)    COMP-3.
               10  MDLLDAT             PIC 9(6)    COMP-3.
               10  MDENTLDAT           PIC 9(6)    COMP-3.
               10  FILLER              PIC X(532).
           05  REDEF       REDEFINES   MSATZ.
               10  MPSA                PIC 99.
               10  MPMANR              PIC 9(7)    COMP-3.
               10  MPPZ                PIC X.
               10  MPDIR               PIC 9(3)    COMP-3    OCCURS 200.
               10  FILLER              PIC X(143).


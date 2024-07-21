      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0601.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/WDKI0601.CPv  $
      *
      *   Rev 2.3   Dec 16 2002 12:03:34   t13ex27
      *AKI00321 - DB-Änderung für Fixpack 31
      *
      *   Rev 2.2   Apr 15 2002 08:37:14   T13EX27
      *AKI00278 DB-Änderung für Fixpack 15
      *
      *   Rev 2.1   Apr 16 1999 10:13:24   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:00   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-ARBEITSSTATION-GESAMT.
         10 EA-NI-ARBEITSSTATION-TAB.
           20 EA-NI-ARBEITSSTATION-ELE PIC X
                  OCCURS 45 TIMES.
         10 EA-NI-ARBEITSSTATION
                 REDEFINES EA-NI-ARBEITSSTATION-TAB.
           20 EA-NI-SP-PC-NR                 PIC X.
           20 EA-NI-SP-PC-NR-NEU             PIC X.
           20 EA-NI-SP-PC-LINIE              PIC X.
           20 EA-NI-SP-VB-ID                 PIC X.
           20 EA-NI-SP-VB-ROLLEN-KZ          PIC X.
           20 EA-NI-SP-VB-NR                 PIC X.
           20 EA-NI-SP-ANREDEKZ              PIC X.
           20 EA-NI-SP-NAME                  PIC X.
           20 EA-NI-SP-VORNAME               PIC X.
           20 EA-NI-SP-ALKZ                  PIC X.
           20 EA-NI-SP-STRASSE               PIC X.
           20 EA-NI-SP-PLZ                   PIC X.
           20 EA-NI-SP-ORT                   PIC X.
           20 EA-NI-SP-TELEFON               PIC X.
           20 EA-NI-SP-TELEFAX               PIC X.
           20 EA-NI-SP-VW-PC-NR              PIC X.
           20 EA-NI-SP-ISDN1                 PIC X.
           20 EA-NI-SP-ISDN2                 PIC X.
           20 EA-NI-SP-ISDN3                 PIC X.
           20 EA-NI-SP-ISDN-SUBADR           PIC X.
           20 EA-NI-SP-TELEFONANLAGE         PIC X.
           20 EA-NI-SP-TERMINALID            PIC X.
           20 EA-NI-SP-DFUE-ART              PIC X.
           20 EA-NI-SP-TARIFZONE             PIC X.
           20 EA-NI-SP-DIALOGKZ              PIC X.
           20 EA-NI-SP-ABAKUSKZ              PIC X.
           20 EA-NI-SP-ABAKUSDATUM           PIC X.
           20 EA-NI-SP-DASIKZ                PIC X.
           20 EA-NI-SP-ZWDFUEMAXTAGE         PIC X.
           20 EA-NI-SP-LGESDFUEDAT           PIC X.
           20 EA-NI-SP-LALLGDFUEDAT          PIC X.
           20 EA-NI-SP-LEADAT                PIC X.
           20 EA-NI-SP-LREPLDFUEDAT          PIC X.
           20 EA-NI-SP-REPLSPERREZAEHL       PIC X.
           20 EA-NI-SP-ZWPROGAUSTZAEHL       PIC X.
           20 EA-NI-SP-LPRGADFUEDAT          PIC X.
           20 EA-NI-SP-ZWDASIMAXTAGE         PIC X.
           20 EA-NI-SP-LETZTEDASIDAT         PIC X.
           20 EA-NI-SP-LOESCHDATUM           PIC X.
           20 EA-NI-SP-PCTYP                 PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
           20 EA-NI-SP-PC-INSTALLIERT        PIC X.
           20 EA-NI-SP-PC-ART                PIC X.
           20 EA-NI-SP-BERECHNET             PIC X.
           20 EA-NI-SP-RECHNUNGSEMPF         PIC X.
         10 EA-DCL-ARBEITSSTATION.
           20 EA-SP-PC-NR                    PIC --------9.
           20 EA-SP-PC-NR-NEU                PIC --------9.
           20 EA-SP-PC-LINIE                 PIC X(2).
           20 EA-SP-VB-ID                    PIC X(13).
           20 EA-SP-VB-ROLLEN-KZ             PIC X(1).
           20 EA-SP-VB-NR                    PIC --------9.
           20 EA-SP-ANREDEKZ                 PIC -----9.
           20 EA-SP-NAME                     PIC X(30).
           20 EA-SP-VORNAME                  PIC X(15).
           20 EA-SP-ALKZ                     PIC X(3).
           20 EA-SP-STRASSE                  PIC X(45).
           20 EA-SP-PLZ                      PIC X(15).
           20 EA-SP-ORT                      PIC X(30).
           20 EA-SP-TELEFON                  PIC X(15).
           20 EA-SP-TELEFAX                  PIC X(15).
           20 EA-SP-VW-PC-NR                 PIC --------9.
           20 EA-SP-ISDN1                    PIC X(20).
           20 EA-SP-ISDN2                    PIC X(20).
           20 EA-SP-ISDN3                    PIC X(20).
           20 EA-SP-ISDN-SUBADR              PIC X(1).
           20 EA-SP-TELEFONANLAGE            PIC X(30).
           20 EA-SP-TERMINALID               PIC X(4).
           20 EA-SP-DFUE-ART                 PIC X(1).
           20 EA-SP-TARIFZONE                PIC -----9.
           20 EA-SP-DIALOGKZ                 PIC X(1).
           20 EA-SP-ABAKUSKZ                 PIC X(1).
           20 EA-SP-ABAKUSDATUM              PIC X(010).
           20 EA-SP-DASIKZ                   PIC X(1).
           20 EA-SP-ZWDFUEMAXTAGE            PIC -----9.
           20 EA-SP-LGESDFUEDAT              PIC X(010).
           20 EA-SP-LALLGDFUEDAT             PIC X(010).
           20 EA-SP-LEADAT                   PIC X(010).
           20 EA-SP-LREPLDFUEDAT             PIC X(010).
           20 EA-SP-REPLSPERREZAEHL          PIC -----9.
           20 EA-SP-ZWPROGAUSTZAEHL          PIC -----9.
           20 EA-SP-LPRGADFUEDAT             PIC X(010).
           20 EA-SP-ZWDASIMAXTAGE            PIC -----9.
           20 EA-SP-LETZTEDASIDAT            PIC X(010).
           20 EA-SP-LOESCHDATUM              PIC X(010).
           20 EA-SP-PCTYP                    PIC X(001).
           20 EA-LETZTEAENDERUNG             PIC X(026).
           20 EA-SP-PC-INSTALLIERT           PIC X(010).
           20 EA-SP-PC-ART                   PIC -----9.
           20 EA-SP-BERECHNET                PIC X(1).
           20 EA-SP-RECHNUNGSEMPF            PIC X(45).
      *****************************************************************

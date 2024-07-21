      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0618.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0618.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:29:46   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:06   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-TMPARBEITSSTATION-GESAMT.
         10 EA-NI-TMPARBEITSSTATION-TAB.
           20 EA-NI-TMPARBEITSSTATION-ELE PIC X
                  OCCURS 41 TIMES.
         10 EA-NI-TMPARBEITSSTATION
                 REDEFINES EA-NI-TMPARBEITSSTATION-TAB.
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
         10 EA-DCL-TMPARBEITSSTATION.
           20 EA-SP-PC-NR                    PIC --------9 .
           20 EA-SP-PC-NR-NEU                PIC --------9 .
           20 EA-SP-PC-LINIE                 PIC X(2).
           20 EA-SP-VB-ID                    PIC X(13).
           20 EA-SP-VB-ROLLEN-KZ             PIC X(1).
           20 EA-SP-VB-NR                    PIC --------9 .
           20 EA-SP-ANREDEKZ                 PIC -----9    .
           20 EA-SP-NAME                     PIC X(30).
           20 EA-SP-VORNAME                  PIC X(15).
           20 EA-SP-ALKZ                     PIC X(3).
           20 EA-SP-STRASSE                  PIC X(45).
           20 EA-SP-PLZ                      PIC X(15).
           20 EA-SP-ORT                      PIC X(30).
           20 EA-SP-TELEFON                  PIC X(15).
           20 EA-SP-TELEFAX                  PIC X(15).
           20 EA-SP-VW-PC-NR                 PIC --------9 .
           20 EA-SP-ISDN1                    PIC X(20).
           20 EA-SP-ISDN2                    PIC X(20).
           20 EA-SP-ISDN3                    PIC X(20).
           20 EA-SP-ISDN-SUBADR              PIC X(1).
           20 EA-SP-TELEFONANLAGE            PIC X(30).
           20 EA-SP-TERMINALID               PIC X(4).
           20 EA-SP-DFUE-ART                 PIC X(1).
           20 EA-SP-TARIFZONE                PIC -----9    .
           20 EA-SP-DIALOGKZ                 PIC X(1).
           20 EA-SP-ABAKUSKZ                 PIC X(1).
           20 EA-SP-ABAKUSDATUM              PIC X(10).
           20 EA-SP-DASIKZ                   PIC X(1).
           20 EA-SP-ZWDFUEMAXTAGE            PIC -----9    .
           20 EA-SP-LGESDFUEDAT              PIC X(10).
           20 EA-SP-LALLGDFUEDAT             PIC X(10).
           20 EA-SP-LEADAT                   PIC X(10).
           20 EA-SP-LREPLDFUEDAT             PIC X(10).
           20 EA-SP-REPLSPERREZAEHL          PIC -----9    .
           20 EA-SP-ZWPROGAUSTZAEHL          PIC -----9    .
           20 EA-SP-LPRGADFUEDAT             PIC X(10).
           20 EA-SP-ZWDASIMAXTAGE            PIC -----9    .
           20 EA-SP-LETZTEDASIDAT            PIC X(10).
           20 EA-SP-LOESCHDATUM              PIC X(10).
           20 EA-SP-PCTYP                    PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
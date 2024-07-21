      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0601.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/WUKI0601.CPv  $
      *
      *   Rev 2.6   Jun 11 2003 13:13:56   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:36   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-ARBEITSSTATION.
           10 SP-PC-NR                    PIC S9(9) COMP.
           10 SP-PC-NR-NEU                PIC S9(9) COMP.
           10 NIL-SP-PC-NR-NEU            PIC X(01).
           10 SP-PC-LINIE                 PIC X(2).
           10 NIL-SP-PC-LINIE             PIC X(01).
           10 SP-VB-ID                    PIC X(13).
           10 NIL-SP-VB-ID                PIC X(01).
           10 SP-VB-ROLLEN-KZ             PIC X(1).
           10 NIL-SP-VB-ROLLEN-KZ         PIC X(01).
           10 SP-VB-NR                    PIC S9(9) COMP.
           10 NIL-SP-VB-NR                PIC X(01).
           10 SP-ANREDEKZ                 PIC S9(4) COMP.
           10 NIL-SP-ANREDEKZ             PIC X(01).
           10 SP-NAME                     PIC X(30).
           10 NIL-SP-NAME                 PIC X(01).
           10 SP-VORNAME                  PIC X(15).
           10 NIL-SP-VORNAME              PIC X(01).
           10 SP-ALKZ                     PIC X(3).
           10 NIL-SP-ALKZ                 PIC X(01).
           10 SP-STRASSE                  PIC X(45).
           10 NIL-SP-STRASSE              PIC X(01).
           10 SP-PLZ                      PIC X(15).
           10 NIL-SP-PLZ                  PIC X(01).
           10 SP-ORT                      PIC X(30).
           10 NIL-SP-ORT                  PIC X(01).
           10 SP-TELEFON                  PIC X(15).
           10 NIL-SP-TELEFON              PIC X(01).
           10 SP-TELEFAX                  PIC X(15).
           10 NIL-SP-TELEFAX              PIC X(01).
           10 SP-VW-PC-NR                 PIC S9(9) COMP.
           10 NIL-SP-VW-PC-NR             PIC X(01).
           10 SP-ISDN1                    PIC X(20).
           10 NIL-SP-ISDN1                PIC X(01).
           10 SP-ISDN2                    PIC X(20).
           10 NIL-SP-ISDN2                PIC X(01).
           10 SP-ISDN3                    PIC X(20).
           10 NIL-SP-ISDN3                PIC X(01).
           10 SP-ISDN-SUBADR              PIC X(1).
           10 NIL-SP-ISDN-SUBADR          PIC X(01).
           10 SP-TELEFONANLAGE            PIC X(30).
           10 NIL-SP-TELEFONANLAGE        PIC X(01).
           10 SP-TERMINALID               PIC X(4).
           10 NIL-SP-TERMINALID           PIC X(01).
           10 SP-DFUE-ART                 PIC X(1).
           10 NIL-SP-DFUE-ART             PIC X(01).
           10 SP-TARIFZONE                PIC S9(4) COMP.
           10 NIL-SP-TARIFZONE            PIC X(01).
           10 SP-DIALOGKZ                 PIC X(1).
           10 NIL-SP-DIALOGKZ             PIC X(01).
           10 SP-ABAKUSKZ                 PIC X(1).
           10 NIL-SP-ABAKUSKZ             PIC X(01).
           10 SP-ABAKUSDATUM              PIC X(010).
           10 NIL-SP-ABAKUSDATUM          PIC X(01).
           10 SP-DASIKZ                   PIC X(1).
           10 NIL-SP-DASIKZ               PIC X(01).
           10 SP-ZWDFUEMAXTAGE            PIC S9(4) COMP.
           10 NIL-SP-ZWDFUEMAXTAGE        PIC X(01).
           10 SP-LGESDFUEDAT              PIC X(010).
           10 NIL-SP-LGESDFUEDAT          PIC X(01).
           10 SP-LALLGDFUEDAT             PIC X(010).
           10 NIL-SP-LALLGDFUEDAT         PIC X(01).
           10 SP-LEADAT                   PIC X(010).
           10 NIL-SP-LEADAT               PIC X(01).
           10 SP-LREPLDFUEDAT             PIC X(010).
           10 NIL-SP-LREPLDFUEDAT         PIC X(01).
           10 SP-REPLSPERREZAEHL          PIC S9(4) COMP.
           10 NIL-SP-REPLSPERREZAEHL      PIC X(01).
           10 SP-ZWPROGAUSTZAEHL          PIC S9(4) COMP.
           10 NIL-SP-ZWPROGAUSTZAEHL      PIC X(01).
           10 SP-LPRGADFUEDAT             PIC X(010).
           10 NIL-SP-LPRGADFUEDAT         PIC X(01).
           10 SP-ZWDASIMAXTAGE            PIC S9(4) COMP.
           10 NIL-SP-ZWDASIMAXTAGE        PIC X(01).
           10 SP-LETZTEDASIDAT            PIC X(010).
           10 NIL-SP-LETZTEDASIDAT        PIC X(01).
           10 SP-LOESCHDATUM              PIC X(010).
           10 NIL-SP-LOESCHDATUM          PIC X(01).
           10 SP-PCTYP                    PIC X(1).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
           10 SP-PC-INSTALLIERT           PIC X(010).
           10 NIL-SP-PC-INSTALLIERT       PIC X(01).
           10 SP-PC-ART                   PIC S9(4) COMP.
           10 NIL-SP-PC-ART               PIC X(01).
           10 SP-BERECHNET                PIC X(1).
           10 SP-RECHNUNGSEMPF            PIC X(45).
           10 NIL-SP-RECHNUNGSEMPF        PIC X(01).
      *****************************************************************

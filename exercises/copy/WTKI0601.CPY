      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WTKI0601.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WTKI0601.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 13:00:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:56:30   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   18 Aug 1997 11:00:38   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER/ARBEITSBEREICH FÜR DIE DB2-TABLE               *
      *                                                               *
      *    ARBEITSSTATION                                             *
      *                                                               *
      *****************************************************************
      *    VERSION   1    31.03.94 18:12:47
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AKI00001!10.07.98!01.01! NEU                       !Zgur-Vujic
      * --------!--------!-----!---------------------------!---------*
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
        05  WB-NI-ARBEITSSTATION-TAB.
           10 WB-NI-ARBEITSSTATION-ELE PIC S9(04) COMP OCCURS 41 TIMES.
        05  WB-NI-ARBEITSSTATION REDEFINES WB-NI-ARBEITSSTATION-TAB.
           10 WB-NI-SP-PC-NR              PIC S9(4) COMP.
           10 WB-NI-SP-PC-NR-NEU          PIC S9(4) COMP.
           10 WB-NI-SP-PC-LINIE           PIC S9(4) COMP.
           10 WB-NI-SP-VB-ID              PIC S9(4) COMP.
           10 WB-NI-SP-VB-ROLLEN-KZ       PIC S9(4) COMP.
           10 WB-NI-SP-VB-NR              PIC S9(4) COMP.
           10 WB-NI-SP-ANREDEKZ           PIC S9(4) COMP.
           10 WB-NI-SP-NAME               PIC S9(4) COMP.
           10 WB-NI-SP-VORNAME            PIC S9(4) COMP.
           10 WB-NI-SP-ALKZ               PIC S9(4) COMP.
           10 WB-NI-SP-STRASSE            PIC S9(4) COMP.
           10 WB-NI-SP-PLZ                PIC S9(4) COMP.
           10 WB-NI-SP-ORT                PIC S9(4) COMP.
           10 WB-NI-SP-TELEFON            PIC S9(4) COMP.
           10 WB-NI-SP-TELEFAX            PIC S9(4) COMP.
           10 WB-NI-SP-VW-PC-NR           PIC S9(4) COMP.
           10 WB-NI-SP-ISDN1              PIC S9(4) COMP.
           10 WB-NI-SP-ISDN2              PIC S9(4) COMP.
           10 WB-NI-SP-ISDN3              PIC S9(4) COMP.
           10 WB-NI-SP-ISDN-SUBADR        PIC S9(4) COMP.
           10 WB-NI-SP-TELEFONANLAGE      PIC S9(4) COMP.
           10 WB-NI-SP-TERMINALID         PIC S9(4) COMP.
           10 WB-NI-SP-DFUE-ART           PIC S9(4) COMP.
           10 WB-NI-SP-TARIFZONE          PIC S9(4) COMP.
           10 WB-NI-SP-DIALOGKZ           PIC S9(4) COMP.
           10 WB-NI-SP-ABAKUSKZ           PIC S9(4) COMP.
           10 WB-NI-SP-ABAKUSDATUM        PIC S9(4) COMP.
           10 WB-NI-SP-DASIKZ             PIC S9(4) COMP.
           10 WB-NI-SP-ZWDFUEMAXTAGE      PIC S9(4) COMP.
           10 WB-NI-SP-LGESDFUEDAT        PIC S9(4) COMP.
           10 WB-NI-SP-LALLGDFUEDAT       PIC S9(4) COMP.
           10 WB-NI-SP-LEADAT             PIC S9(4) COMP.
           10 WB-NI-SP-LREPLDFUEDAT       PIC S9(4) COMP.
           10 WB-NI-SP-REPLSPERREZAEHL    PIC S9(4) COMP.
           10 WB-NI-SP-ZWPROGAUSTZAEHL    PIC S9(4) COMP.
           10 WB-NI-SP-LPRGADFUEDAT       PIC S9(4) COMP.
           10 WB-NI-SP-ZWDASIMAXTAGE      PIC S9(4) COMP.
           10 WB-NI-SP-LETZTEDASIDAT      PIC S9(4) COMP.
           10 WB-NI-SP-LOESCHDATUM        PIC S9(4) COMP.
           10 WB-NI-SP-PCTYP              PIC S9(4) COMP.
           10 WB-NI-LETZTEAENDERUNG       PIC S9(4) COMP.
        05  DCL-ARBEITSSTATION.
           10 WB-SP-PC-NR                 PIC S9(9) COMP.
           10 WB-SP-PC-NR-NEU             PIC S9(9) COMP.
           10 WB-SP-PC-LINIE              PIC X(2).
           10 WB-SP-VB-ID                 PIC X(13).
           10 WB-SP-VB-ROLLEN-KZ          PIC X(1).
           10 WB-SP-VB-NR                 PIC S9(9) COMP.
           10 WB-SP-ANREDEKZ              PIC S9(4) COMP.
           10 WB-SP-NAME                  PIC X(30).
           10 WB-SP-VORNAME               PIC X(15).
           10 WB-SP-ALKZ                  PIC X(3).
           10 WB-SP-STRASSE               PIC X(45).
           10 WB-SP-PLZ                   PIC X(15).
           10 WB-SP-ORT                   PIC X(30).
           10 WB-SP-TELEFON               PIC X(15).
           10 WB-SP-TELEFAX               PIC X(15).
           10 WB-SP-VW-PC-NR              PIC S9(9) COMP.
           10 WB-SP-ISDN1                 PIC X(20).
           10 WB-SP-ISDN2                 PIC X(20).
           10 WB-SP-ISDN3                 PIC X(20).
           10 WB-SP-ISDN-SUBADR           PIC X(1).
           10 WB-SP-TELEFONANLAGE         PIC X(30).
           10 WB-SP-TERMINALID            PIC X(4).
           10 WB-SP-DFUE-ART              PIC X(1).
           10 WB-SP-TARIFZONE             PIC S9(4) COMP.
           10 WB-SP-DIALOGKZ              PIC X(1).
           10 WB-SP-ABAKUSKZ              PIC X(1).
           10 WB-SP-ABAKUSDATUM           PIC X(10).
           10 WB-SP-DASIKZ                PIC X(1).
           10 WB-SP-ZWDFUEMAXTAGE         PIC S9(4) COMP.
           10 WB-SP-LGESDFUEDAT           PIC X(10).
           10 WB-SP-LALLGDFUEDAT          PIC X(10).
           10 WB-SP-LEADAT                PIC X(10).
           10 WB-SP-LREPLDFUEDAT          PIC X(10).
           10 WB-SP-REPLSPERREZAEHL       PIC S9(4) COMP.
           10 WB-SP-ZWPROGAUSTZAEHL       PIC S9(4) COMP.
           10 WB-SP-LPRGADFUEDAT          PIC X(10).
           10 WB-SP-ZWDASIMAXTAGE         PIC S9(4) COMP.
           10 WB-SP-LETZTEDASIDAT         PIC X(10).
           10 WB-SP-LOESCHDATUM           PIC X(10).
           10 WB-SP-PCTYP                 PIC X(01).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************

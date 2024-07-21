      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0010.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0010.CPv  $
      *
      *   Rev 2.1   Dec 07 2009 18:24:06   t13ex48
      *AKI00667: Bereinigung der KI-DB: Neue Tablespaces für Tabs. 
      *der VB-SST
      *
      *   Rev 2.0   Feb 27 1999 12:47:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Apr 18 1997 09:52:18   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  ARBEITSSTATION  TABLE
CCC004*    ( SP_PC_NR           INTEGER   NOT NULL,
CCC004*      SP_PC_NR_NEU       INTEGER,
CCC004*      SP_PC_LINIE        CHAR(2),
CCC004*      SP_VB_ID           CHAR(13),
CCC004*      SP_VB_ROLLEN_KZ    CHAR(1),
CCC004*      SP_VB_NR           INTEGER,
CCC004*      SP_ANREDEKZ        SMALLINT,
CCC004*      SP_NAME            CHAR(30),
CCC004*      SP_VORNAME         CHAR(15),
CCC004*      SP_ALKZ            CHAR(3),
CCC004*      SP_STRASSE         CHAR(45),
CCC004*      SP_PLZ             CHAR(15),
CCC004*      SP_ORT             CHAR(30),
CCC004*      SP_TELEFON         CHAR(15),
CCC004*      SP_TELEFAX         CHAR(15),
CCC004*      SP_VW_PC_NR        INTEGER,
CCC004*      SP_ISDN1           CHAR(20),
CCC004*      SP_ISDN2           CHAR(20),
CCC004*      SP_ISDN3           CHAR(20),
CCC004*      SP_ISDN_SUBADR     CHAR(1),
CCC004*      SP_TELEFONANLAGE   CHAR(30),
CCC004*      SP_TERMINALID      CHAR(4),
CCC004*      SP_DFUE_ART        CHAR(1),
CCC004*      SP_TARIFZONE       SMALLINT,
CCC004*      SP_DIALOGKZ        CHAR(1),
CCC004*      SP_ABAKUSKZ        CHAR(1),
CCC004*      SP_ABAKUSDATUM     DATE,
CCC004*      SP_DASIKZ          CHAR(1),
CCC004*      SP_ZWDFUEMAXTAGE   SMALLINT,
CCC004*      SP_LGESDFUEDAT     DATE,
CCC004*      SP_LALLGDFUEDAT    DATE,
CCC004*      SP_LEADAT          DATE,
CCC004*      SP_LREPLDFUEDAT    DATE,
CCC004*      SP_REPLSPERREZAEHL SMALLINT,
CCC004*      SP_ZWPROGAUSTZAEHL SMALLINT,
CCC004*      SP_LPRGADFUEDAT    DATE,
CCC004*      SP_ZWDASIMAXTAGE   SMALLINT,
CCC004*      SP_LETZTEDASIDAT   DATE,
CCC004*      SP_LOESCHDATUM     DATE,
CCC004*      SP_PCTYP           CHAR      NOT NULL,
CCC004*      LETZTEAENDERUNG    TIMESTAMP,
CCC004*      SP_PC_INSTALLIERT  DATE,
CCC004*      SP_PC_ART          SMALLINT,
CCC004*      SP_BERECHNET       CHAR(1)   NOT NULL,
CCC004*      SP_RECHNUNGSEMPF   CHAR(45)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  ARBEITSSTATION-GESAMT.
        05  NI-ARBEITSSTATION-TAB.
020402*    10 NI-ARBEITSSTATION-ELE PIC S9(04) COMP OCCURS 41 TIMES.
021202*    10 NI-ARBEITSSTATION-ELE PIC S9(04) COMP OCCURS 43 TIMES.
021202     10 NI-ARBEITSSTATION-ELE PIC S9(04) COMP OCCURS 45 TIMES.
        05  NI-ARBEITSSTATION  REDEFINES  NI-ARBEITSSTATION-TAB.
           10 NI-SP-PC-NR                 PIC S9(4) COMP.
           10 NI-SP-PC-NR-NEU             PIC S9(4) COMP.
           10 NI-SP-PC-LINIE              PIC S9(4) COMP.
           10 NI-SP-VB-ID                 PIC S9(4) COMP.
           10 NI-SP-VB-ROLLEN-KZ          PIC S9(4) COMP.
           10 NI-SP-VB-NR                 PIC S9(4) COMP.
           10 NI-SP-ANREDEKZ              PIC S9(4) COMP.
           10 NI-SP-NAME                  PIC S9(4) COMP.
           10 NI-SP-VORNAME               PIC S9(4) COMP.
           10 NI-SP-ALKZ                  PIC S9(4) COMP.
           10 NI-SP-STRASSE               PIC S9(4) COMP.
           10 NI-SP-PLZ                   PIC S9(4) COMP.
           10 NI-SP-ORT                   PIC S9(4) COMP.
           10 NI-SP-TELEFON               PIC S9(4) COMP.
           10 NI-SP-TELEFAX               PIC S9(4) COMP.
           10 NI-SP-VW-PC-NR              PIC S9(4) COMP.
           10 NI-SP-ISDN1                 PIC S9(4) COMP.
           10 NI-SP-ISDN2                 PIC S9(4) COMP.
           10 NI-SP-ISDN3                 PIC S9(4) COMP.
           10 NI-SP-ISDN-SUBADR           PIC S9(4) COMP.
           10 NI-SP-TELEFONANLAGE         PIC S9(4) COMP.
           10 NI-SP-TERMINALID            PIC S9(4) COMP.
           10 NI-SP-DFUE-ART              PIC S9(4) COMP.
           10 NI-SP-TARIFZONE             PIC S9(4) COMP.
           10 NI-SP-DIALOGKZ              PIC S9(4) COMP.
           10 NI-SP-ABAKUSKZ              PIC S9(4) COMP.
           10 NI-SP-ABAKUSDATUM           PIC S9(4) COMP.
           10 NI-SP-DASIKZ                PIC S9(4) COMP.
           10 NI-SP-ZWDFUEMAXTAGE         PIC S9(4) COMP.
           10 NI-SP-LGESDFUEDAT           PIC S9(4) COMP.
           10 NI-SP-LALLGDFUEDAT          PIC S9(4) COMP.
           10 NI-SP-LEADAT                PIC S9(4) COMP.
           10 NI-SP-LREPLDFUEDAT          PIC S9(4) COMP.
           10 NI-SP-REPLSPERREZAEHL       PIC S9(4) COMP.
           10 NI-SP-ZWPROGAUSTZAEHL       PIC S9(4) COMP.
           10 NI-SP-LPRGADFUEDAT          PIC S9(4) COMP.
           10 NI-SP-ZWDASIMAXTAGE         PIC S9(4) COMP.
           10 NI-SP-LETZTEDASIDAT         PIC S9(4) COMP.
           10 NI-SP-LOESCHDATUM           PIC S9(4) COMP.
           10 NI-SP-PCTYP                 PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
020402     10 NI-SP-PC-INSTALLIERT        PIC S9(4) COMP.
020402     10 NI-SP-PC-ART                PIC S9(4) COMP.
021202     10 NI-SP-BERECHNET             PIC S9(4) COMP.
021202     10 NI-SP-RECHNUNGSEMPF         PIC S9(4) COMP.
        05  DCL-ARBEITSSTATION.
           10 SP-PC-NR                    PIC S9(9) COMP.
           10 SP-PC-NR-NEU                PIC S9(9) COMP.
           10 SP-PC-LINIE                 PIC X(2).
           10 SP-VB-ID                    PIC X(13).
           10 SP-VB-ROLLEN-KZ             PIC X(1).
           10 SP-VB-NR                    PIC S9(9) COMP.
           10 SP-ANREDEKZ                 PIC S9(4) COMP.
           10 SP-NAME                     PIC X(30).
           10 SP-VORNAME                  PIC X(15).
           10 SP-ALKZ                     PIC X(3).
           10 SP-STRASSE                  PIC X(45).
           10 SP-PLZ                      PIC X(15).
           10 SP-ORT                      PIC X(30).
           10 SP-TELEFON                  PIC X(15).
           10 SP-TELEFAX                  PIC X(15).
           10 SP-VW-PC-NR                 PIC S9(9) COMP.
           10 SP-ISDN1                    PIC X(20).
           10 SP-ISDN2                    PIC X(20).
           10 SP-ISDN3                    PIC X(20).
           10 SP-ISDN-SUBADR              PIC X(1).
           10 SP-TELEFONANLAGE            PIC X(30).
           10 SP-TERMINALID               PIC X(4).
           10 SP-DFUE-ART                 PIC X(1).
           10 SP-TARIFZONE                PIC S9(4) COMP.
           10 SP-DIALOGKZ                 PIC X(1).
           10 SP-ABAKUSKZ                 PIC X(1).
           10 SP-ABAKUSDATUM              PIC X(10).
           10 SP-DASIKZ                   PIC X(1).
           10 SP-ZWDFUEMAXTAGE            PIC S9(4) COMP.
           10 SP-LGESDFUEDAT              PIC X(10).
           10 SP-LALLGDFUEDAT             PIC X(10).
           10 SP-LEADAT                   PIC X(10).
           10 SP-LREPLDFUEDAT             PIC X(10).
           10 SP-REPLSPERREZAEHL          PIC S9(4) COMP.
           10 SP-ZWPROGAUSTZAEHL          PIC S9(4) COMP.
           10 SP-LPRGADFUEDAT             PIC X(10).
           10 SP-ZWDASIMAXTAGE            PIC S9(4) COMP.
           10 SP-LETZTEDASIDAT            PIC X(10).
           10 SP-LOESCHDATUM              PIC X(10).
           10 SP-PCTYP                    PIC X(01).
           10 LETZTEAENDERUNG             PIC X(26).
020402     10 SP-PC-INSTALLIERT           PIC X(10).
020402     10 SP-PC-ART                   PIC S9(4) COMP.
021202     10 SP-BERECHNET                PIC X(01).
021202     10 SP-RECHNUNGSEMPF            PIC X(45).
      ******************************************************************


      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0507.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0507.CPv  $
      *
      *   Rev 2.1   Oct 22 2018 11:49:00   t13ex39
      *AKI01615: Generali Invest  OZPs  WHO   
      *
      *   Rev 2.0   Feb 27 1999 12:51:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:31:06   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   15 Jan 1998 18:05:36   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE G42_VERTRAG TABLE
CCC004*    ( G42VVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G42VMANDANT                    INTEGER NOT NULL,
CCC004*      G42VVBNR                       INTEGER NOT NULL,
CCC004*      G42VVRTRGBEGINN                CHAR(8) NOT NULL,
CCC004*      G42VDEPOTSTATUS                CHAR(1) NOT NULL,
CCC004*      G42VINFOSPRREKZ                CHAR(1) NOT NULL,
CCC004*      G42VINFOSPERDAT                CHAR(8) NOT NULL,
CCC004*      G42VANACHLASSKZ                CHAR(1) NOT NULL,
CCC004*      G42VTELBANKING                 CHAR(1) NOT NULL,
CCC004*      G42VONLBANKING                 CHAR(1) NOT NULL,
CCC004*      G42VNACHNAME                   CHAR(35) NOT NULL,
CCC004*      G42VVORNAME                    CHAR(35) NOT NULL,
CCC004*      G42VGEBDAT                     CHAR(8) NOT NULL,
CCC004*      G42VGESCHLECHT                 CHAR(1) NOT NULL,
CCC004*      G42VANREDE                     CHAR(1) NOT NULL,
CCC004*      G42VSTRHNR                     CHAR(40) NOT NULL,
CCC004*      G42VPLZ                        CHAR(9) NOT NULL,
CCC004*      G42VORT                        CHAR(40) NOT NULL,
CCC004*      G42VLKZ                        CHAR(3) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G42_VERTRAG                 *
      ******************************************************************
       01  G42-VERTRAG-GESAMT.
        05 NI-G42-VERTRAG-TAB.
           10 NI-G42-VERTRAG-ELE PIC S9(04) COMP OCCURS 19 TIMES.
        05 NI-G42-VERTRAG REDEFINES NI-G42-VERTRAG-TAB.
           10 NI-G42VVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G42VMANDANT        PIC S9(4) COMP.
           10 NI-G42VVBNR           PIC S9(4) COMP.
           10 NI-G42VVRTRGBEGINN    PIC S9(4) COMP.
           10 NI-G42VDEPOTSTATUS    PIC S9(4) COMP.
           10 NI-G42VINFOSPRREKZ    PIC S9(4) COMP.
           10 NI-G42VINFOSPERDAT    PIC S9(4) COMP.
           10 NI-G42VANACHLASSKZ    PIC S9(4) COMP.
           10 NI-G42VTELBANKING     PIC S9(4) COMP.
           10 NI-G42VONLBANKING     PIC S9(4) COMP.
           10 NI-G42VNACHNAME       PIC S9(4) COMP.
           10 NI-G42VVORNAME        PIC S9(4) COMP.
           10 NI-G42VGEBDAT         PIC S9(4) COMP.
           10 NI-G42VGESCHLECHT     PIC S9(4) COMP.
           10 NI-G42VANREDE         PIC S9(4) COMP.
           10 NI-G42VSTRHNR         PIC S9(4) COMP.
           10 NI-G42VPLZ            PIC S9(4) COMP.
           10 NI-G42VORT            PIC S9(4) COMP.
           10 NI-G42VLKZ            PIC S9(4) COMP.
        05 DCL-G42_VERTRAG.
           10 G42VVERTRAGSNR       PIC X(15).
           10 G42VMANDANT          PIC S9(9) USAGE COMP.
           10 G42VVBNR             PIC S9(9) USAGE COMP.
           10 G42VVRTRGBEGINN      PIC X(8).
           10 G42VDEPOTSTATUS      PIC X(1).
           10 G42VINFOSPRREKZ      PIC X(1).
           10 G42VINFOSPERDAT      PIC X(8).
           10 G42VANACHLASSKZ      PIC X(1).
           10 G42VTELBANKING       PIC X(1).
           10 G42VONLBANKING       PIC X(1).
           10 G42VNACHNAME         PIC X(35).
           10 G42VVORNAME          PIC X(35).
           10 G42VGEBDAT           PIC X(8).
           10 G42VGESCHLECHT       PIC X(1).
           10 G42VANREDE           PIC X(1).
           10 G42VSTRHNR           PIC X(40).
           10 G42VPLZ              PIC X(9).
           10 G42VORT              PIC X(40).
           10 G42VLKZ              PIC X(3).
      ************************** ENDE-DCLGEN *************************


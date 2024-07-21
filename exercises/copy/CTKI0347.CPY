      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0347.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0347.CPv  $
      *
      *   Rev 2.1   Sep 26 2005 13:53:56   t13ex27
      *AKI00521: DB-Änderung für Fix 80 CTKI's
      *
      *   Rev 2.0   Feb 27 1999 12:50:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:40   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:59:16   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERSGEFAHRBEZ  TABLE
CCC004*    ( VEGVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VEGVERSTARLFDNR                SMALLINT NOT NULL,
CCC004*      VEGGEFAHRCODE                  SMALLINT NOT NULL,
CCC004*      VEGSELBSTBETEIL                DECIMAL(11, 2),
CCC004*      VEGSELBSTBETWKZ                CHAR(1) NOT NULL,
CCC004*      VEGTARIFZONE                   SMALLINT,
CCC004*      VEGSBVSUMME                    DECIMAL(2, 0),
CCC004*      VEGSBSCHADEN                   DECIMAL(2, 0),
CCC004*      VEGSBMIN                       DECIMAL(5, 0),
CCC004*      VEGSBMAX                       DECIMAL(5, 0),
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      VEGKZSBNL                      CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERSGEFAHRBEZ-GESAMT.
        05  NI-VERSGEFAHRBEZ-TAB.
           10 NI-VERSGEFAHRBEZ-ELE PIC S9(04) COMP OCCURS 12 TIMES.
        05  NI-VERSGEFAHRBEZ  REDEFINES  NI-VERSGEFAHRBEZ-TAB.
           10 NI-VEGVERTRAGSID            PIC S9(4) COMP.
           10 NI-VEGVERSTARLFDNR          PIC S9(4) COMP.
           10 NI-VEGGEFAHRCODE            PIC S9(4) COMP.
           10 NI-VEGSELBSTBETEIL          PIC S9(4) COMP.
           10 NI-VEGSELBSTBETWKZ          PIC S9(4) COMP.
           10 NI-VEGTARIFZONE             PIC S9(4) COMP.
           10 NI-VEGSBVSUMME              PIC S9(4) COMP.
           10 NI-VEGSBSCHADEN             PIC S9(4) COMP.
           10 NI-VEGSBMIN                 PIC S9(4) COMP.
           10 NI-VEGSBMAX                 PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
           10 NI-VEGKZSBNL                PIC S9(4) COMP.
        05  DCL-VERSGEFAHRBEZ.
           10 VEGVERTRAGSID               PIC X(13).
           10 VEGVERSTARLFDNR             PIC S9(4) COMP.
           10 VEGGEFAHRCODE               PIC S9(4) COMP.
           10 VEGSELBSTBETEIL             PIC S9(9)V9(2) COMP-3.
           10 VEGSELBSTBETWKZ             PIC X(01).
           10 VEGTARIFZONE                PIC S9(4) COMP.
           10 VEGSBVSUMME                 PIC S9(2)V  COMP-3.
           10 VEGSBSCHADEN                PIC S9(2)V  COMP-3.
           10 VEGSBMIN                    PIC S9(5)V  COMP-3.
           10 VEGSBMAX                    PIC S9(5)V  COMP-3.
           10 LETZTEAENDERUNG             PIC X(26).
           10 VEGKZSBNL                   PIC X(1).
      ******************************************************************

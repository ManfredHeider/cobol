      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0001.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0001.CPv  $
      *
      *   Rev 2.1   Jun 01 2017 17:30:20   t13ex27
      *AKI01523: Verschieben TBKD0001 nach KDAS
      *
      *   Rev 1.1   25 Mar 1998 18:14:00   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:51:44   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE KDAS TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      KDAS_ANZ_KD                    INTEGER NOT NULL,
CCC004*      KDAS_ANZ_VT                    INTEGER NOT NULL,
CCC004*      KDAS_ANZ_KD2                   INTEGER NOT NULL,
CCC004*      KDAS_BEITRAG                   DECIMAL(11, 2) NOT NULL,
CCC004*      KDAS_BEITRAG_INV               DECIMAL(11, 2) NOT NULL,
CCC004*      KDAS_BEITRAG_KTO               DECIMAL(11, 2) NOT NULL,
CCC004*      KDAS_MONAT                     DATE NOT NULL,
CCC004*      KDAS_BEITRAG_CKV               DECIMAL(11, 2) NOT NULL,
CCC004*      KDAS_BEITRAG_AML               DECIMAL(11, 2) NOT NULL,
CCC004*      KDAS_TYP                       SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE KDAS                        *
      ******************************************************************
       01  KDAS-GESAMT.
        05 NI-KDAS-TAB.
           10 NI-KDAS-ELE PIC S9(04) COMP OCCURS 11 TIMES.
        05 NI-KDAS REDEFINES NI-KDAS-TAB.
           10 NI-ST-NR              PIC S9(4) COMP.
           10 NI-KDAS-ANZ-KD        PIC S9(4) COMP.
           10 NI-KDAS-ANZ-VT        PIC S9(4) COMP.
           10 NI-KDAS-ANZ-KD2       PIC S9(4) COMP.
           10 NI-KDAS-BEITRAG       PIC S9(4) COMP.
           10 NI-KDAS-BEITRAG-INV   PIC S9(4) COMP.
           10 NI-KDAS-BEITRAG-KTO   PIC S9(4) COMP.
           10 NI-KDAS-MONAT         PIC S9(4) COMP.
           10 NI-KDAS-BEITRAG-CKV   PIC S9(4) COMP.
           10 NI-KDAS-BEITRAG-AML   PIC S9(4) COMP.
           10 NI-KDAS-TYP           PIC S9(4) COMP.
        05 DCL-KDAS.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 KDAS-ANZ-KD          PIC S9(9) USAGE COMP.
           10 KDAS-ANZ-VT          PIC S9(9) USAGE COMP.
           10 KDAS-ANZ-KD2         PIC S9(9) USAGE COMP.
           10 KDAS-BEITRAG         PIC S9(9)V9(2) USAGE COMP-3.
           10 KDAS-BEITRAG-INV     PIC S9(9)V9(2) USAGE COMP-3.
           10 KDAS-BEITRAG-KTO     PIC S9(9)V9(2) USAGE COMP-3.
           10 KDAS-MONAT           PIC X(10).
           10 KDAS-BEITRAG-CKV     PIC S9(9)V9(2) USAGE COMP-3.
           10 KDAS-BEITRAG-AML     PIC S9(9)V9(2) USAGE COMP-3.
           10 KDAS-TYP             PIC S9(4) USAGE COMP.
      ************************** ENDE-DCLGEN *************************


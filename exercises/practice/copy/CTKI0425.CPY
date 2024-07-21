      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0425.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0425.CPv  $
      *
      *   Rev 2.1   27 Feb 2008 14:30:22   t13ex27
      *AKL00665: Allfinanz; DCLGENs und DB-Änderung Privatkunden
      *
      *   Rev 2.0   27 Sep 2007 12:02:28   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE UNFMITVERSPERSON
      ******************************************************************
CCC005*    EXEC SQL DECLARE UNFMITVERSPERSON TABLE
CCC004*    ( UMVVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      UMVVETLFDNR                    SMALLINT NOT NULL,
CCC004*      UMVPUFVPLFDNR                  SMALLINT NOT NULL,
CCC004*      UMVPUFMVPLFDNR                 SMALLINT NOT NULL,
CCC004*      UMVEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      UMVMITVPNAME                   CHAR(30) NOT NULL,
CCC004*      UMVMITVPVORNAME                CHAR(30) NOT NULL,
CCC004*      UMVMITGESCHL_KTY               SMALLINT NOT NULL,
CCC004*      UMVMITGESCHL_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      UMVMITGEBDAT                   DATE,
CCC004*      UMVBEGINN                      DATE,
CCC004*      UMVENDE                        DATE,
CCC004*      UMVMITPARTHOSTID               CHAR(15) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  UNFMITVERSPERSON
      ******************************************************************
       01  UNFMITVERSPERSON-GESAMT.
         05 NI-UNFMITVERSPERSON-TAB.
           10 NI-UNFMITVERSPERSON-ELE PIC S9(4) COMP OCCURS 14 TIMES.
         05 NI-UNFMITVERSPERSON REDEFINES NI-UNFMITVERSPERSON-TAB.
           10 NI-UMVVERTRAGSID        PIC S9(4) COMP.
           10 NI-UMVVETLFDNR          PIC S9(4) COMP.
           10 NI-UMVPUFVPLFDNR        PIC S9(4) COMP.
           10 NI-UMVPUFMVPLFDNR       PIC S9(4) COMP.
           10 NI-UMVEINZVTGHOSTID     PIC S9(4) COMP.
           10 NI-UMVMITVPNAME         PIC S9(4) COMP.
           10 NI-UMVMITVPVORNAME      PIC S9(4) COMP.
           10 NI-UMVMITGESCHL-KTY     PIC S9(4) COMP.
           10 NI-UMVMITGESCHL-KNR     PIC S9(4) COMP.
           10 NI-UMVMITGEBDAT         PIC S9(4) COMP.
           10 NI-UMVBEGINN            PIC S9(4) COMP.
           10 NI-UMVENDE              PIC S9(4) COMP.
           10 NI-UMVMITPARTHOSTID     PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.
         05 DCL-UNFMITVERSPERSON.
           10 UMVVERTRAGSID        PIC X(13).
           10 UMVVETLFDNR          PIC S9(4)  COMP.
           10 UMVPUFVPLFDNR        PIC S9(4)  COMP.
           10 UMVPUFMVPLFDNR       PIC S9(4)  COMP.
           10 UMVEINZVTGHOSTID     PIC X(15).
           10 UMVMITVPNAME         PIC X(30).
           10 UMVMITVPVORNAME      PIC X(30).
           10 UMVMITGESCHL-KTY     PIC S9(4)  COMP.
           10 UMVMITGESCHL-KNR     PIC S9(15)V  COMP-3.
           10 UMVMITGEBDAT         PIC X(10).
           10 UMVBEGINN            PIC X(10).
           10 UMVENDE              PIC X(10).
           10 UMVMITPARTHOSTID     PIC X(15).
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************
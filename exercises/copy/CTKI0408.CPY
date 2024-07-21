      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.02.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0408.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0408.CPv  $
      *
      *   Rev 2.2   27 Feb 2008 14:28:24   t13ex27
      *AKL00665: Allfinanz; DCLGENs und DB-Änderung Privatkunden
      *
      *   Rev 2.1   Feb 07 2006 15:42:46   t13ex27
      *AKI00555: DB-Änderung OKI 02/2006
      *
      *   Rev 2.0   Feb 07 2006 12:48:20   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE GLASVERTRAG
      ******************************************************************
CCC005*    EXEC SQL DECLARE GLASVERTRAG TABLE
CCC004*    ( GLVVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      GLVVETLFDNR                    SMALLINT NOT NULL,
CCC004*      GLVEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      GLVRANSCHRIFTPLZ               CHAR(6) NOT NULL,
CCC004*      GLVRANSCHRIFTSTR               CHAR(40) NOT NULL,
CCC004*      GLVRANSCHRIFTORT               CHAR(30) NOT NULL,
CCC004*      GLVRANSCHRHAUSNR               CHAR(15) NOT NULL,
CCC004*      GLVRANSCHRIFTLKZ               INTEGER NOT NULL,
CCC004*      GLVKLAUSELTARIF                CHAR(80) NOT NULL,
CCC004*      GLVBEDINGUNG                   CHAR(16) NOT NULL,
CCC004*      GLVGEBAEUTYP_KTY               SMALLINT NOT NULL,
CCC004*      GLVGEBAEUTYP_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      GLVRISIKO                      CHAR(50) NOT NULL,
CCC004*      GLVMINBEITRAGKZ                CHAR(2) NOT NULL,
CCC004*      GLVSBPROZENT                   DECIMAL(4, 2) NOT NULL,
CCC004*      GLVSBFEST                      INTEGER NOT NULL,
CCC004*      GLVZUSATZINFO                  CHAR(100) NOT NULL,
CCC004*      GLVLFREIVONDAT                 DATE,
CCC004*      GLVLFREIBISDAT                 DATE,
CCC004*      GLVMAILINGAKTION               CHAR(70) NOT NULL,
CCC004*      GLVDIFDECKKZ                   CHAR(2) NOT NULL,
CCC004*      GLVUMSTELLUNGDAT               DATE,
CCC004*      GLVGESELLSCHAFT                CHAR(50) NOT NULL,
CCC004*      GLVVERSNUMMER                  CHAR(40) NOT NULL,
CCC004*      GLVKUENDIGUNGKZ                CHAR(2) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  GLASVERTRAG
      ******************************************************************
       01  GLASVERTRAG-GESAMT.
         05 NI-GLASVERTRAG-TAB.
           10 NI-GLASVERTRAG-ELE PIC S9(4) COMP OCCURS 26 TIMES.
         05 NI-GLASVERTRAG REDEFINES NI-GLASVERTRAG-TAB.
           10 NI-GLVVERTRAGSID      PIC S9(4) COMP.
           10 NI-GLVVETLFDNR        PIC S9(4) COMP.
           10 NI-GLVEINZVTGHOSTID   PIC S9(4) COMP.
           10 NI-GLVRANSCHRIFTPLZ   PIC S9(4) COMP.
           10 NI-GLVRANSCHRIFTSTR   PIC S9(4) COMP.
           10 NI-GLVRANSCHRIFTORT   PIC S9(4) COMP.
           10 NI-GLVRANSCHRHAUSNR   PIC S9(4) COMP.
           10 NI-GLVRANSCHRIFTLKZ   PIC S9(4) COMP.
           10 NI-GLVKLAUSELTARIF    PIC S9(4) COMP.
           10 NI-GLVBEDINGUNG       PIC S9(4) COMP.
           10 NI-GLVGEBAEUTYP-KTY   PIC S9(4) COMP.
           10 NI-GLVGEBAEUTYP-KNR   PIC S9(4) COMP.
           10 NI-GLVRISIKO          PIC S9(4) COMP.
           10 NI-GLVMINBEITRAGKZ    PIC S9(4) COMP.
           10 NI-GLVSBPROZENT       PIC S9(4) COMP.
           10 NI-GLVSBFEST          PIC S9(4) COMP.
           10 NI-GLVZUSATZINFO      PIC S9(4) COMP.
           10 NI-GLVLFREIVONDAT     PIC S9(4) COMP.
           10 NI-GLVLFREIBISDAT     PIC S9(4) COMP.
           10 NI-GLVMAILINGAKTION   PIC S9(4) COMP.
           10 NI-GLVDIFDECKKZ       PIC S9(4) COMP.
           10 NI-GLVUMSTELLUNGDAT   PIC S9(4) COMP.
           10 NI-GLVGESELLSCHAFT    PIC S9(4) COMP.
           10 NI-GLVVERSNUMMER      PIC S9(4) COMP.
           10 NI-GLVKUENDIGUNGKZ    PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
                     
         05 DCL-GLASVERTRAG.
           10 GLVVERTRAGSID        PIC X(13).
           10 GLVVETLFDNR          PIC S9(4)  COMP.
           10 GLVEINZVTGHOSTID     PIC X(15).
           10 GLVRANSCHRIFTPLZ     PIC X(6).
           10 GLVRANSCHRIFTSTR     PIC X(40).
           10 GLVRANSCHRIFTORT     PIC X(30).
           10 GLVRANSCHRHAUSNR     PIC X(15).
           10 GLVRANSCHRIFTLKZ     PIC S9(9)  COMP.
           10 GLVKLAUSELTARIF      PIC X(80).
           10 GLVBEDINGUNG         PIC X(16).
           10 GLVGEBAEUTYP-KTY     PIC S9(4)  COMP.
           10 GLVGEBAEUTYP-KNR     PIC S9(15)V  COMP-3.
           10 GLVRISIKO            PIC X(50).
           10 GLVMINBEITRAGKZ      PIC X(2).
           10 GLVSBPROZENT         PIC S9(2)V9(2)  COMP-3.
           10 GLVSBFEST            PIC S9(9)  COMP.
           10 GLVZUSATZINFO        PIC X(100).
           10 GLVLFREIVONDAT       PIC X(10).
           10 GLVLFREIBISDAT       PIC X(10).
           10 GLVMAILINGAKTION     PIC X(70).
           10 GLVDIFDECKKZ         PIC X(2).
           10 GLVUMSTELLUNGDAT     PIC X(10).
           10 GLVGESELLSCHAFT      PIC X(50).
           10 GLVVERSNUMMER        PIC X(40).
           10 GLVKUENDIGUNGKZ      PIC X(2).
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************
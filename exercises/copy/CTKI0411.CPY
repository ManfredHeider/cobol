      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.02.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0411.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0411.CPv  $
      *
      *   Rev 2.2   27 Feb 2008 14:29:10   t13ex27
      *AKL00665: Allfinanz; DCLGENs und DB-Änderung Privatkunden
      *
      *   Rev 2.1   Feb 07 2006 15:43:06   t13ex27
      *AKI00555: DB-Änderung OKI 02/2006
      *
      *   Rev 2.0   Feb 07 2006 12:48:46   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE UNFALLVERTRAG
      ******************************************************************
CCC005*    EXEC SQL DECLARE UNFALLVERTRAG TABLE
CCC004*    ( UNFVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      UNFVETLFDNR                    SMALLINT NOT NULL,
CCC004*      UNFEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      UNFBEDINGUNG                   CHAR(16) NOT NULL,
CCC004*      UNFRISIKO                      CHAR(50) NOT NULL,
CCC004*      UNFKLAUSELTARIF                CHAR(80) NOT NULL,
CCC004*      UNFZUSATZINFO                  CHAR(100) NOT NULL,
CCC004*      UNFANZVERSPERSON               INTEGER NOT NULL,
CCC004*      UNFLEISTFREIVDAT               DATE,
CCC004*      UNFLEISTFREIBDAT               DATE,
CCC004*      UNFSELBSTBKZ                   CHAR(2) NOT NULL,
CCC004*      UNFSELBSTBPROZ                 DECIMAL(4, 2) NOT NULL,
CCC004*      UNFSELBSTBFEST                 INTEGER NOT NULL,
CCC004*      UNFSELBSTBTEXT                 CHAR(50) NOT NULL,
CCC004*      UNFMINDBEITRAGKZ               CHAR(2) NOT NULL,
CCC004*      UNFMAILINGAKTION               CHAR(70) NOT NULL,
CCC004*      UNFUMSTELLUNGDAT               DATE,
CCC004*      UNFGESELLSCHAFT                CHAR(50) NOT NULL,
CCC004*      UNFVERSNUMMER                  CHAR(40) NOT NULL,
CCC004*      UNFKUENDIGUNGKZ                CHAR(2) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  UNFALLVERTRAG
      ******************************************************************
       01  UNFALLVERTRAG-GESAMT.
         05 NI-UNFALLVERTRAG-TAB.
           10 NI-UNFALLVERTRAG-ELE PIC S9(4) COMP OCCURS 21 TIMES.
         05 NI-UNFALLVERTRAG REDEFINES NI-UNFALLVERTRAG-TAB.
           10 NI-UNFVERTRAGSID        PIC S9(4) COMP.
           10 NI-UNFVETLFDNR          PIC S9(4) COMP.
           10 NI-UNFEINZVTGHOSTID     PIC S9(4) COMP.
           10 NI-UNFBEDINGUNG         PIC S9(4) COMP.
           10 NI-UNFRISIKO            PIC S9(4) COMP.
           10 NI-UNFKLAUSELTARIF      PIC S9(4) COMP.
           10 NI-UNFZUSATZINFO        PIC S9(4) COMP.
           10 NI-UNFANZVERSPERSON     PIC S9(4) COMP.
           10 NI-UNFLEISTFREIVDAT     PIC S9(4) COMP.
           10 NI-UNFLEISTFREIBDAT     PIC S9(4) COMP.
           10 NI-UNFSELBSTBKZ         PIC S9(4) COMP.
           10 NI-UNFSELBSTBPROZ       PIC S9(4) COMP.
           10 NI-UNFSELBSTBFEST       PIC S9(4) COMP.
           10 NI-UNFSELBSTBTEXT       PIC S9(4) COMP.
           10 NI-UNFMINDBEITRAGKZ     PIC S9(4) COMP.
           10 NI-UNFMAILINGAKTION     PIC S9(4) COMP.
           10 NI-UNFUMSTELLUNGDAT     PIC S9(4) COMP.
           10 NI-UNFGESELLSCHAFT      PIC S9(4) COMP.
           10 NI-UNFVERSNUMMER        PIC S9(4) COMP.
           10 NI-UNFKUENDIGUNGKZ      PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.
         05 DCL-UNFALLVERTRAG.
           10 UNFVERTRAGSID        PIC X(13).
           10 UNFVETLFDNR          PIC S9(4)  COMP.
           10 UNFEINZVTGHOSTID     PIC X(15).
           10 UNFBEDINGUNG         PIC X(16).
           10 UNFRISIKO            PIC X(50).
           10 UNFKLAUSELTARIF      PIC X(80).
           10 UNFZUSATZINFO        PIC X(100).
           10 UNFANZVERSPERSON     PIC S9(9)  COMP.
           10 UNFLEISTFREIVDAT     PIC X(10).
           10 UNFLEISTFREIBDAT     PIC X(10).
           10 UNFSELBSTBKZ         PIC X(2).
           10 UNFSELBSTBPROZ       PIC S9(2)V9(2)  COMP-3.
           10 UNFSELBSTBFEST       PIC S9(9)  COMP.
           10 UNFSELBSTBTEXT       PIC X(50).
           10 UNFMINDBEITRAGKZ     PIC X(2).
           10 UNFMAILINGAKTION     PIC X(70).
           10 UNFUMSTELLUNGDAT     PIC X(10).
           10 UNFGESELLSCHAFT      PIC X(50).
           10 UNFVERSNUMMER        PIC X(40).
           10 UNFKUENDIGUNGKZ      PIC X(2).
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************
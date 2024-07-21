      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0423.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0423.CPv  $
      *
      *   Rev 2.1   27 Feb 2008 14:29:54   t13ex27
      *AKL00665: Allfinanz; DCLGENs und DB-Änderung Privatkunden
      *
      *   Rev 2.0   27 Sep 2007 12:02:08   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE VGVSELBSTBETEIL
      ******************************************************************
CCC005*    EXEC SQL DECLARE VGVSELBSTBETEIL TABLE
CCC004*    ( VGSVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VGSVETLFDNR                    SMALLINT NOT NULL,
CCC004*      VGSPVGSBLFDNR                  SMALLINT NOT NULL,
CCC004*      VGSEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      VGSSELBSTBART                  INTEGER NOT NULL,
CCC004*      VGSSELBSTBBEZ                  CHAR(30) NOT NULL,
CCC004*      VGSSELBSTBPRZ                  CHAR(14) NOT NULL,
CCC004*      VGSSELBSTBMIN                  DECIMAL(7, 2) NOT NULL,
CCC004*      VGSSELBSTBMAX                  DECIMAL(7, 2) NOT NULL,
CCC004*      VGSSELBSTBFEST                 DECIMAL(7, 2) NOT NULL,
CCC004*      VGSSELBSTBTEXT                 CHAR(30) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  VGVSELBSTBETEIL
      ******************************************************************
       01  VGVSELBSTBETEIL-GESAMT.
         05 NI-VGVSELBSTBETEIL-TAB.
           10 NI-VGVSELBSTBETEIL-ELE PIC S9(4) COMP OCCURS 12 TIMES.
         05 NI-VGVSELBSTBETEIL REDEFINES NI-VGVSELBSTBETEIL-TAB.
           10 NI-VGSVERTRAGSID        PIC S9(4) COMP.
           10 NI-VGSVETLFDNR          PIC S9(4) COMP.
           10 NI-VGSPVGSBLFDNR        PIC S9(4) COMP.
           10 NI-VGSEINZVTGHOSTID     PIC S9(4) COMP.
           10 NI-VGSSELBSTBART        PIC S9(4) COMP.
           10 NI-VGSSELBSTBBEZ        PIC S9(4) COMP.
           10 NI-VGSSELBSTBPRZ        PIC S9(4) COMP.
           10 NI-VGSSELBSTBMIN        PIC S9(4) COMP.
           10 NI-VGSSELBSTBMAX        PIC S9(4) COMP.
           10 NI-VGSSELBSTBFEST       PIC S9(4) COMP.
           10 NI-VGSSELBSTBTEXT       PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.
         05 DCL-VGVSELBSTBETEIL.
           10 VGSVERTRAGSID        PIC X(13).
           10 VGSVETLFDNR          PIC S9(4)  COMP.
           10 VGSPVGSBLFDNR        PIC S9(4)  COMP.
           10 VGSEINZVTGHOSTID     PIC X(15).
           10 VGSSELBSTBART        PIC S9(9)  COMP.
           10 VGSSELBSTBBEZ        PIC X(30).
           10 VGSSELBSTBPRZ        PIC X(14).
           10 VGSSELBSTBMIN        PIC S9(5)V9(2)  COMP-3.
           10 VGSSELBSTBMAX        PIC S9(5)V9(2)  COMP-3.
           10 VGSSELBSTBFEST       PIC S9(5)V9(2)  COMP-3.
           10 VGSSELBSTBTEXT       PIC X(30).
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************
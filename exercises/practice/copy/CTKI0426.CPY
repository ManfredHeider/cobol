      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0426.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0426.CPv  $
      *
      *   Rev 2.1   17 Oct 2007 11:07:54   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-DCLGENs und DB-Änderung
      *
      *   Rev 2.0   27 Sep 2007 12:02:38   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE VERTSELBSTBETEIL
      ******************************************************************
CCC005*    EXEC SQL DECLARE VERTSELBSTBETEIL TABLE
CCC004*    ( VSBVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VSBVETLFDNR                    SMALLINT NOT NULL,
CCC004*      VSBRISIGRUPPEID                SMALLINT NOT NULL,
CCC004*      VSBSELBSTBETID                 SMALLINT NOT NULL,
CCC004*      VSBEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      VSBBEZEICHNUNG                 CHAR(50) NOT NULL,
CCC004*      VSBWERTPROZENT                 SMALLINT NOT NULL,
CCC004*      VSBWERT                        DECIMAL(10, 2) NOT NULL,
CCC004*      VSBMINDESTBEITR                DECIMAL(7, 2) NOT NULL,
CCC004*      VSBHOECHSTBEITR                DECIMAL(7, 2) NOT NULL,
CCC004*      VSBSELBSTBTEXT                 CHAR(30) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  VERTSELBSTBETEIL
      ******************************************************************
       01  VERTSELBSTBETEIL-GESAMT.
         05 NI-VERTSELBSTBETEIL-TAB.
           10 NI-VERTSELBSTBETEIL-ELE PIC S9(4) COMP OCCURS 12 TIMES.
         05 NI-VERTSELBSTBETEIL REDEFINES NI-VERTSELBSTBETEIL-TAB.
           10 NI-VSBVERTRAGSID        PIC S9(4) COMP.
           10 NI-VSBVETLFDNR          PIC S9(4) COMP.
           10 NI-VSBRISIGRUPPEID      PIC S9(4) COMP.
           10 NI-VSBSELBSTBETID       PIC S9(4) COMP.
           10 NI-VSBEINZVTGHOSTID     PIC S9(4) COMP.
           10 NI-VSBBEZEICHNUNG       PIC S9(4) COMP.
           10 NI-VSBWERTPROZENT       PIC S9(4) COMP.
           10 NI-VSBWERT              PIC S9(4) COMP.
           10 NI-VSBMINDESTBEITR      PIC S9(4) COMP.
           10 NI-VSBHOECHSTBEITR      PIC S9(4) COMP.
           10 NI-VSBSELBSTBTEXT       PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.
         05 DCL-VERTSELBSTBETEIL.
           10 VSBVERTRAGSID        PIC X(13).
           10 VSBVETLFDNR          PIC S9(4) COMP.
           10 VSBRISIGRUPPEID      PIC S9(4) COMP.
           10 VSBSELBSTBETID       PIC S9(4) COMP.
           10 VSBEINZVTGHOSTID     PIC X(15).
           10 VSBBEZEICHNUNG       PIC X(50).
           10 VSBWERTPROZENT       PIC S9(4) COMP.
           10 VSBWERT              PIC S9(8)V9(2) COMP-3.
           10 VSBMINDESTBEITR      PIC S9(5)V9(2) COMP-3.
           10 VSBHOECHSTBEITR      PIC S9(5)V9(2) COMP-3.
           10 VSBSELBSTBTEXT       PIC X(30).
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************

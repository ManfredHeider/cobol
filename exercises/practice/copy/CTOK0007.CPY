      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTOK0007.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTOK0007.CPv  $
      *
      *   Rev 2.2   Feb 18 2014 16:22:24   t13ex19
      *FKI01448: Anpassung VN-Wechsel für Cloud
      *
      *   Rev 2.1   Feb 27 2007 13:03:30   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - DB-Änderung
      *
      *   Rev 2.0   Mar 14 2006 16:40:00   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE HISTNAMENAENDERUNG
      ******************************************************************
CCC005*    EXEC SQL DECLARE HISTNAMENAENDERUNG TABLE
CCC004*    ( HNAKUNDENID                    CHAR(13) NOT NULL,
CCC004*      HNALFDNR                       SMALLINT NOT NULL,
CCC004*      HNAERSTELLTVON                 CHAR(30),
CCC004*      HNAPARTGESELLSCHID             SMALLINT,
CCC004*      HNAVERTRAGSNUMMER              CHAR(15),
CCC004*      HNAERSTELLT                    TIMESTAMP NOT NULL,
CCC004*      HNATICKETID                    DECIMAL(18, 0) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      HNA_KTY                        SMALLINT NOT NULL,
CCC004*      HNA_KNR                        SMALLINT NOT NULL,
CCC004*      HNAALTANREDEID                 SMALLINT NOT NULL,
CCC004*      HNAALTVORNAME                  CHAR(30) NOT NULL,
CCC004*      HNAALTNACHNAME                 CHAR(30) NOT NULL,
CCC004*      HNAALTGEBGRUENDDAT             DATE NOT NULL,
CCC004*      HNANEUANREDEID                 SMALLINT NOT NULL,
CCC004*      HNANEUVORNAME                  CHAR(30) NOT NULL,
CCC004*      HNANEUNACHNAME                 CHAR(30) NOT NULL,
CCC004*      HNANEUGEBGRUENDDAT             DATE NOT NULL,
CCC004*      HNANEUKUNDENID                 CHAR(13)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE HISTNAMENAENDERUNG          *
      ******************************************************************
       01  HISTNAMENAENDERUNG-GESAMT.
        05 NI-HISTNAMENAENDERUNG-TAB.
           10 NI-HISTNAMENAENDERUNG-ELE PIC S9(04) COMP OCCURS 19 TIMES.
        05 NI-HISTNAMENAENDERUNG REDEFINES NI-HISTNAMENAENDERUNG-TAB.
           10 NI-HNAKUNDENID        PIC S9(4) COMP.
           10 NI-HNALFDNR           PIC S9(4) COMP.
           10 NI-HNAERSTELLTVON     PIC S9(4) COMP.
           10 NI-HNAPARTGESELLSCHID PIC S9(4) COMP.
           10 NI-HNAVERTRAGSNUMMER  PIC S9(4) COMP.
           10 NI-HNAERSTELLT        PIC S9(4) COMP.
           10 NI-HNATICKETID        PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-HNA-KTY            PIC S9(4) COMP.
           10 NI-HNA-KNR            PIC S9(4) COMP.
           10 NI-HNAALTANREDEID     PIC S9(4) COMP.
           10 NI-HNAALTVORNAME      PIC S9(4) COMP.
           10 NI-HNAALTNACHNAME     PIC S9(4) COMP.
           10 NI-HNAALTGEBGRUENDDAT PIC S9(4) COMP.
           10 NI-HNANEUANREDEID     PIC S9(4) COMP.
           10 NI-HNANEUVORNAME      PIC S9(4) COMP.
           10 NI-HNANEUNACHNAME     PIC S9(4) COMP.
           10 NI-HNANEUGEBGRUENDDAT PIC S9(4) COMP.
           10 NI-HNANEUKUNDENID     PIC S9(4) COMP.
        05 DCL-HISTNAMENAENDERUNG.
           10 HNAKUNDENID          PIC X(13).
           10 HNALFDNR             PIC S9(4) USAGE COMP.
           10 HNAERSTELLTVON       PIC X(30).
           10 HNAPARTGESELLSCHID   PIC S9(4) USAGE COMP.
           10 HNAVERTRAGSNUMMER    PIC X(15).
           10 HNAERSTELLT          PIC X(26).
           10 HNATICKETID          PIC S9(18)V USAGE COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).
           10 HNA-KTY              PIC S9(4) USAGE COMP.
           10 HNA-KNR              PIC S9(4) USAGE COMP.
           10 HNAALTANREDEID       PIC S9(4) USAGE COMP.
           10 HNAALTVORNAME        PIC X(30).
           10 HNAALTNACHNAME       PIC X(30).
           10 HNAALTGEBGRUENDDAT   PIC X(10).
           10 HNANEUANREDEID       PIC S9(4) USAGE COMP.
           10 HNANEUVORNAME        PIC X(30).
           10 HNANEUNACHNAME       PIC X(30).
           10 HNANEUGEBGRUENDDAT   PIC X(10).
           10 HNANEUKUNDENID       PIC X(13).
      ************************** ENDE-DCLGEN *************************

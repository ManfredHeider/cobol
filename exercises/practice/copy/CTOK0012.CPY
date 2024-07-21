      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.04.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTOK0012.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTOK0012.CPv  $
      *
      *   Rev 2.1   Feb 27 2007 13:03:50   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - DB-Änderung
      *
      *   Rev 2.0   Apr 06 2006 15:43:42   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE HISTADRESSAENDERG
      ******************************************************************
CCC005*    EXEC SQL DECLARE HISTADRESSAENDERG TABLE
CCC004*    ( HADPERSONENID                  CHAR(13) NOT NULL,
CCC004*      HADVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      HADADRESSEALTID                CHAR(13) NOT NULL,
CCC004*      HADLFDNR                       SMALLINT NOT NULL,
CCC004*      HADLANDID                      CHAR(3) NOT NULL,
CCC004*      HADORT                         CHAR(30),
CCC004*      HADPLZ                         CHAR(15),
CCC004*      HADSTRHAUSNUM                  CHAR(45),
CCC004*      HADORTSTEIL                    CHAR(30),
CCC004*      HADVBID                        CHAR(13) NOT NULL,
CCC004*      HADAENDGRUNDID                 SMALLINT NOT NULL,
CCC004*      HADTICKETID                    DECIMAL(18, 0) NOT NULL,
CCC004*      HADDTAERLEDIGT                 CHAR(1) NOT NULL,
CCC004*      HADDTAERLDATUM                 DATE,
CCC004*      HADGEAENDERTAM                 DATE,
CCC004*      HADSTATUS                      SMALLINT NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  HISTADRESSAENDERG
      ******************************************************************
       01  HISTADRESSAENDERG-GESAMT.
         05 NI-HISTADRESSAENDERG-TAB.
           10 NI-HISTADRESSAENDERG-ELE PIC S9(4) COMP OCCURS 17 TIMES.
         05 NI-HISTADRESSAENDERG REDEFINES NI-HISTADRESSAENDERG-TAB.
           10 NI-HADPERSONENID        PIC S9(4) COMP.
           10 NI-HADVERTRAGSID        PIC S9(4) COMP.
           10 NI-HADADRESSEALTID      PIC S9(4) COMP.
           10 NI-HADLFDNR             PIC S9(4) COMP.
           10 NI-HADLANDID            PIC S9(4) COMP.
           10 NI-HADORT               PIC S9(4) COMP.
           10 NI-HADPLZ               PIC S9(4) COMP.
           10 NI-HADSTRHAUSNUM        PIC S9(4) COMP.
           10 NI-HADORTSTEIL          PIC S9(4) COMP.
           10 NI-HADVBID              PIC S9(4) COMP.
           10 NI-HADAENDGRUNDID       PIC S9(4) COMP.
           10 NI-HADTICKETID          PIC S9(4) COMP.
           10 NI-HADDTAERLEDIGT       PIC S9(4) COMP.
           10 NI-HADDTAERLDATUM       PIC S9(4) COMP.
           10 NI-HADGEAENDERTAM       PIC S9(4) COMP.
           10 NI-HADSTATUS            PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.
         05 DCL-HISTADRESSAENDERG.
           10 HADPERSONENID        PIC X(13).
           10 HADVERTRAGSID        PIC X(13).
           10 HADADRESSEALTID      PIC X(13).
           10 HADLFDNR             PIC S9(4) COMP.
           10 HADLANDID            PIC X(3).
           10 HADORT               PIC X(30).
           10 HADPLZ               PIC X(15).
           10 HADSTRHAUSNUM        PIC X(45).
           10 HADORTSTEIL          PIC X(30).
           10 HADVBID              PIC X(13).
           10 HADAENDGRUNDID       PIC S9(4) COMP.
           10 HADTICKETID          PIC S9(18)V COMP-3.
           10 HADDTAERLEDIGT       PIC X(1).
           10 HADDTAERLDATUM       PIC X(10).
           10 HADGEAENDERTAM       PIC X(10).
           10 HADSTATUS            PIC S9(4) COMP.
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************
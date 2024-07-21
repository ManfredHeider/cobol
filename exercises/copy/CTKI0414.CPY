      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.06.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0414.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0414.CPv  $
      *
      *   Rev 2.1   Jun 28 2006 15:55:32   t13ex27
      *AKI00604: DB-Änderung für Fix 90 - DCLGENs
      *
      *   Rev 2.0   Jun 22 2006 12:23:20   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE DBUMSATZ
      ******************************************************************
CCC005*    EXEC SQL DECLARE DBUMSATZ TABLE
CCC004*    ( DBUVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      DBUGHPLFN                      SMALLINT NOT NULL,
CCC004*      DBULFDNR                       SMALLINT NOT NULL,
CCC004*      DBUJAHR                        SMALLINT NOT NULL,
CCC004*      DBUMONAT                       SMALLINT NOT NULL,
CCC004*      DBUHABEN                       DECIMAL(14, 2),
CCC004*      DBUSOLL                        DECIMAL(14, 2),    
CCC004*      LETZTEAENDERUNG                TIMESTAMP          
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  DBUMSATZ
      ******************************************************************
       01  DBUMSATZ-GESAMT.
         05 NI-DBUMSATZ-TAB.
           10 NI-DBUMSATZ-ELE PIC S9(4) COMP OCCURS 8 TIMES.
         05 NI-DBUMSATZ REDEFINES NI-DBUMSATZ-TAB.
           10 NI-DBUVERTRAGSID     PIC S9(4) COMP.
           10 NI-DBUGHPLFN         PIC S9(4) COMP.
           10 NI-DBULFDNR          PIC S9(4) COMP.
           10 NI-DBUJAHR           PIC S9(4) COMP.
           10 NI-DBUMONAT          PIC S9(4) COMP.
           10 NI-DBUHABEN          PIC S9(4) COMP.
           10 NI-DBUSOLL           PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG   PIC S9(4) COMP.
         05 DCL-DBUMSATZ.
           10 DBUVERTRAGSID        PIC X(13).
           10 DBUGHPLFN            PIC S9(4) COMP.
           10 DBULFDNR             PIC S9(4) COMP.
           10 DBUJAHR              PIC S9(4) COMP.
           10 DBUMONAT             PIC S9(4) COMP.
           10 DBUHABEN             PIC S9(12)V9(2) COMP-3.
           10 DBUSOLL              PIC S9(12)V9(2) COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).      
      ******************************************************************
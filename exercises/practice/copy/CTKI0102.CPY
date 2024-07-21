      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.07.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0102.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0102.CPv  $
      *
      *   Rev 2.1   Jul 22 2005 14:49:32   t13ex27
      *AKI00510: DB-Änderung für Fix 78
      *
      *   Rev 2.0   Jul 22 2005 14:35:50   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE KUNDENDRUCKLOG
      ******************************************************************
CCC005*    EXEC SQL DECLARE KUNDENDRUCKLOG TABLE
CCC004*    ( KDLID                          CHAR(13) NOT NULL,
CCC004*      KDLANGEMVBNR                   INTEGER NOT NULL,
CCC004*      KDLKENNUNG                     CHAR(1) NOT NULL,
CCC004*      KDLKUNDENNR                    INTEGER NOT NULL,
CCC004*      KDLANZAHLEV                    SMALLINT NOT NULL,
CCC004*      KDLDRUCKDATUM                  DATE NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  KUNDENDRUCKLOG
      ******************************************************************
       01  KUNDENDRUCKLOG-GESAMT.
         05 NI-KUNDENDRUCKLOG-TAB.
           10 NI-KUNDENDRUCKLOG-ELE PIC S9(4) COMP OCCURS 6 TIMES.
         05 NI-KUNDENDRUCKLOG REDEFINES NI-KUNDENDRUCKLOG-TAB.
           10 NI-KDLID             PIC S9(4) COMP.
           10 NI-KDLANGEMVBNR      PIC S9(4) COMP.
           10 NI-KDLKENNUNG        PIC S9(4) COMP.
           10 NI-KDLKUNDENNR       PIC S9(4) COMP.
           10 NI-KDLANZAHLEV       PIC S9(4) COMP.
           10 NI-KDLDRUCKDATUM     PIC S9(4) COMP.
         05 DCL-KUNDENDRUCKLOG.
           10 KDLID                PIC X(13).
           10 KDLANGEMVBNR         PIC S9(9) COMP.
           10 KDLKENNUNG           PIC X(1).
           10 KDLKUNDENNR          PIC S9(9) COMP.
           10 KDLANZAHLEV          PIC S9(4) COMP.
           10 KDLDRUCKDATUM        PIC X(10).
      ******************************************************************

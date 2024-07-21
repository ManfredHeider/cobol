      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0421.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0421.CPv  $
      *
      *   Rev 2.1   17 Oct 2007 11:07:32   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-DCLGENs und DB-Änderung
      *
      *   Rev 2.0   27 Sep 2007 12:01:48   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE VERTRRISIKOADR
      ******************************************************************
CCC005*    EXEC SQL DECLARE VERTRRISIKOADR TABLE
CCC004*    ( VRAVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VRARISIKOHOSTID                INTEGER NOT NULL,
CCC004*      VRARISIKOSTRASSE               CHAR(40) NOT NULL,
CCC004*      VRARISIKOPLZ                   CHAR(6) NOT NULL,
CCC004*      VRARISIKOORT                   CHAR(40) NOT NULL,
CCC004*      VRARISIKOLKZ                   INTEGER NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  VERTRRISIKOADR
      ******************************************************************
       01  VERTRRISIKOADR-GESAMT.
         05 NI-VERTRRISIKOADR-TAB.
           10 NI-VERTRRISIKOADR-ELE PIC S9(4) COMP OCCURS 7 TIMES.
         05 NI-VERTRRISIKOADR REDEFINES NI-VERTRRISIKOADR-TAB.
           10 NI-VRAVERTRAGSID        PIC S9(4) COMP.
           10 NI-VRARISIKOHOSTID      PIC S9(4) COMP.
           10 NI-VRARISIKOSTRASSE     PIC S9(4) COMP.
           10 NI-VRARISIKOPLZ         PIC S9(4) COMP.
           10 NI-VRARISIKOORT         PIC S9(4) COMP.
           10 NI-VRARISIKOLKZ         PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.
         05 DCL-VERTRRISIKOADR.
           10 VRAVERTRAGSID        PIC X(13).
           10 VRARISIKOHOSTID      PIC S9(9) COMP.
           10 VRARISIKOSTRASSE     PIC X(40).
           10 VRARISIKOPLZ         PIC X(6).
           10 VRARISIKOORT         PIC X(40).
           10 VRARISIKOLKZ         PIC S9(9) COMP.
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************
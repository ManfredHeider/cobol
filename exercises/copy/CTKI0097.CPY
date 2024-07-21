      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS VM
      *            Datum      :  24.03.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0097.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/CTKI0097.CPv  $
      *
      *   Rev 2.1   Apr 02 2003 16:04:28   t13ex27
      *AKI00347 - DB-Änderung für Fix 38
      *
      *   Rev 2.0   Mar 24 2003 11:15:58   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE DARLEHENSART
      ******************************************************************
CCC005*    EXEC SQL DECLARE DARLEHENSART TABLE
CCC004*    ( DARLEHENSARTID                 SMALLINT NOT NULL,
CCC004*      DARLEHENSARTTEXT               CHAR(25) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  DARLEHENSART
      ******************************************************************
       01  DARLEHENSART-GESAMT.
         05 NI-DARLEHENSART-TAB.
           10 NI-DARLEHENSART-ELE PIC S9(4) COMP OCCURS 2 TIMES.
         05 NI-DARLEHENSART REDEFINES NI-DARLEHENSART-TAB.
           10 NI-DARLEHENSARTID     PIC S9(4) COMP.
           10 NI-DARLEHENSARTTEXT   PIC S9(4) COMP.
         05 DCL-DARLEHENSART.
           10 DARLEHENSARTID       PIC S9(4) COMP.
           10 DARLEHENSARTTEXT     PIC X(25).
      ******************************************************************
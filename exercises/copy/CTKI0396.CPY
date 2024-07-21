      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0396.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/CTKI0396.CPv  $
      *
      *   Rev 2.1   Apr 02 2003 16:04:34   t13ex27
      *AKI00347 - DB-Änderung für Fix 38
      *
      *   Rev 2.0   15 May 2001 13:47:02   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE BILANZSCHLUESSEL                                  *
      ******************************************************************
CCC005*    EXEC SQL DECLARE BILANZSCHLUESSEL TABLE
CCC004*    ( BILANZSCHL                     SMALLINT NOT NULL,
CCC004*      BILANZSCHLTEXT                 CHAR(30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  BILANZSCHLUESSEL                  *
      ******************************************************************
       01  BILANZSCHLUESSEL-GESAMT.
         05 NI-BILANZSCHLUESSEL-TAB.
           10 NI-BILANZSCHLUESSEL-ELE PIC S9(4) COMP OCCURS 2 TIMES.
         05 NI-BILANZSCHLUESSEL REDEFINES NI-BILANZSCHLUESSEL-TAB.
           10 NI-BILANZSCHL        PIC S9(4) COMP.
           10 NI-BILANZSCHLTEXT    PIC S9(4) COMP.
         05 DCL-BILANZSCHLUESSEL.
           10 BILANZSCHL           PIC S9(4) COMP.
           10 BILANZSCHLTEXT       PIC X(30).
      ******************************************************************

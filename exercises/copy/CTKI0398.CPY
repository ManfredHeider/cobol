      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0398.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0398.CPv  $
      *
      *   Rev 2.1   Feb 11 2004 12:51:16   t13ex27
      *AKI00403 - DB-Änderung Fix 57
      *
      *   Rev 2.0   15 May 2001 13:47:04   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE VERTRAGSTEILART
      ******************************************************************
CCC005*    EXEC SQL DECLARE VERTRAGSTEILART TABLE
CCC004*    ( VTAKEY                         SMALLINT NOT NULL,
CCC004*      VTATEXT                        CHAR(30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  VERTRAGSTEILART
      ******************************************************************
       01  VERTRAGSTEILART-GESAMT.
         05 NI-VERTRAGSTEILART-TAB.
           10 NI-VERTRAGSTEILART-ELE PIC S9(4) COMP OCCURS yy TIMES.
         05 NI-VERTRAGSTEILART REDEFINES NI-VERTRAGSTEILART-TAB.
           10 NI-VTAKEY                   PIC S9(4) COMP.
           10 NI-VTATEXT                  PIC S9(4) COMP.
         05 DCL-VERTRAGSTEILART.
           10 VTAKEY                      PIC S9(4) COMP.
           10 VTATEXT                     PIC X(30).
      ******************************************************************

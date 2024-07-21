      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0397.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/CTKI0397.CPv  $
      *
      *   Rev 2.1   Apr 02 2003 16:04:36   t13ex27
      *AKI00347 - DB-Änderung für Fix 38
      *
      *   Rev 2.0   15 May 2001 13:47:04   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE FILIALNRBLZ                                       *
      ******************************************************************
CCC005*    EXEC SQL DECLARE FILIALNRBLZ TABLE
CCC004*    ( FILIALNUMMER                   SMALLINT      NOT NULL,
CCC004*      FILIALBLZ                      DECIMAL(8, 0) NOT NULL,
CCC004*      FILIALNAME                     CHAR(30)      NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE FILIALNRBLZ                        *
      ******************************************************************
       01  FILIALNRBLZ-GESAMT.
         05 NI-FILIALNRBLZ-TAB.
           10 NI-FILIALNRBLZ-ELE PIC S9(4) COMP OCCURS 3 TIMES.
         05 NI-FILIALNRBLZ REDEFINES NI-FILIALNRBLZ-TAB.
           10 NI-FILIALNUMMER       PIC S9(4) COMP.
           10 NI-FILIALBLZ          PIC S9(4) COMP.
           10 NI-FILIALNAME         PIC S9(4) COMP.
         05 DCL-FILIALNRBLZ.
           10 FILIALNUMMER          PIC S9(4) COMP.
           10 FILIALBLZ             PIC S9(8) COMP-3.
           10 FILIALNAME            PIC X(30).
      ******************************************************************
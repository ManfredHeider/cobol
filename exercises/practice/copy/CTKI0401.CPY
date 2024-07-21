      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0401.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0401.CPv  $
      *
      *   Rev 2.1   Mar 03 2005 10:13:08   t13ex27
      *AKI00474: DB-Änderung für Fix 73 CTKIxxxx
      *
      *   Rev 2.0   Mar 03 2005 09:38:06   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE LVVERFUEGUNG
      ******************************************************************
CCC005*    EXEC SQL DECLARE LVVERFUEGUNG TABLE
CCC004*    ( LVVERFUEGUNGID                 CHAR(3) NOT NULL,
CCC004*      LVVERFUEGUNGTEXT               CHAR(40) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  LVVERFUEGUNG
      ******************************************************************
       01  LVVERFUEGUNG-GESAMT.
         05 NI-LVVERFUEGUNG-TAB.
           10 NI-LVVERFUEGUNG-ELE PIC S9(4) COMP OCCURS 2 TIMES.
         05 NI-LVVERFUEGUNG REDEFINES NI-LVVERFUEGUNG-TAB.
           10 NI-LVVERFUEGUNGID       PIC S9(4) COMP.
           10 NI-LVVERFUEGUNGTEXT     PIC S9(4) COMP.
         05 DCL-LVVERFUEGUNG.
           10 LVVERFUEGUNGID       PIC X(3).
           10 LVVERFUEGUNGTEXT     PIC X(40).
      ******************************************************************

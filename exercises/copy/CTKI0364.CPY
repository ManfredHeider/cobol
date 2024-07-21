      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctki0364.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\ctki0364.cpv  $
      *
      *   Rev 2.1   Apr 10 2000 14:24:04   T13EX41
      *AKI00065
      *- Längenänderung Schlüsselfelder
      *
      *   Rev 2.0   Feb 27 1999 12:51:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:30:10   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:00:36   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00065 !30.03.00!    !-WAGNISID IST              !C. HEINECK*
      *         !        !    ! JETZT INTEGER             !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  WAGNIS  TABLE
CCC004*    ( WAGNISID INTEGER NOT NULL,
CCC004*      WAGNISTEXT CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  WAGNIS-GESAMT.
        05  NI-WAGNIS-TAB.
           10 NI-WAGNIS-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-WAGNIS  REDEFINES  NI-WAGNIS-TAB.
           10 NI-WAGNISID                 PIC S9(4) COMP.
           10 NI-WAGNISTEXT               PIC S9(4) COMP.
        05  DCL-WAGNIS.
000330     10 WAGNISID                    PIC S9(9) COMP.
           10 WAGNISTEXT                  PIC X(30).
      ******************************************************************

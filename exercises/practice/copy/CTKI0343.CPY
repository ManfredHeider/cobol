      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctki0343.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\ctki0343.cpv  $
      *
      *   Rev 2.1   Apr 10 2000 14:23:58   T13EX41
      *AKI00065
      *- Längenänderung Schlüsselfelder
      *
      *   Rev 2.0   Feb 27 1999 12:50:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:32   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:59:00   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00065 !30.03.00!    !-VEBEDINGUNGID IST JETZT   !C. HEINECK*
      *         !        !    ! INTEGER                   !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERSBEDINGUNG  TABLE
CCC004*    ( VEBEDINGUNGID           INTEGER NOT NULL,
CCC004*      VEBPARTNERGESID         SMALLINT NOT NULL,
CCC004*      VEBEDINGUNGTEXT         CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERSBEDINGUNG-GESAMT.
        05  NI-VERSBEDINGUNG-TAB.
           10 NI-VERSBEDINGUNG-ELE PIC S9(04) COMP OCCURS 3 TIMES.
        05  NI-VERSBEDINGUNG  REDEFINES  NI-VERSBEDINGUNG-TAB.
           10 NI-VEBEDINGUNGID            PIC S9(4) COMP.
           10 NI-VEBPARTNERGESID          PIC S9(4) COMP.
           10 NI-VEBEDINGUNGTEXT          PIC S9(4) COMP.
        05  DCL-VERSBEDINGUNG.
000330     10 VEBEDINGUNGID               PIC S9(9) COMP.
           10 VEBPARTNERGESID             PIC S9(4) COMP.
           10 VEBEDINGUNGTEXT             PIC X(30).
      ******************************************************************

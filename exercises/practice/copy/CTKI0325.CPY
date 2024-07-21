      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctki0325.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\ctki0325.cpv  $
      *
      *   Rev 2.1   Apr 10 2000 14:23:56   T13EX41
      *AKI00065
      *- Längenänderung Schlüsselfelder
      *
      *   Rev 2.0   Feb 27 1999 12:50:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:04   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:57:50   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00065 !30.03.00!    !-KLAKLAUSELID IST JETZT    !C. HEINECK*
      *         !        !    ! INTEGER                   !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  KLAUSEL  TABLE
CCC004*    ( KLAPARTNERGESID         SMALLINT NOT NULL,
CCC004*      KLAKLAUSELID            INTEGER NOT NULL,
CCC004*      KLAKLAUSELTEXT          CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  KLAUSEL-GESAMT.
        05  NI-KLAUSEL-TAB.
           10 NI-KLAUSEL-ELE PIC S9(04) COMP OCCURS 3 TIMES.
        05  NI-KLAUSEL  REDEFINES  NI-KLAUSEL-TAB.
           10 NI-KLAPARTNERGESID          PIC S9(4) COMP.
           10 NI-KLAKLAUSELID             PIC S9(4) COMP.
           10 NI-KLAKLAUSELTEXT           PIC S9(4) COMP.
        05  DCL-KLAUSEL.
           10 KLAPARTNERGESID             PIC S9(4) COMP.
000330     10 KLAKLAUSELID                PIC S9(9) COMP.
           10 KLAKLAUSELTEXT              PIC X(30).
      ******************************************************************

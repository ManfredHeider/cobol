      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctki0349.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\ctki0349.cpv  $
      *
      *   Rev 2.1   Apr 10 2000 14:24:02   T13EX41
      *AKI00065
      *- Längenänderung Schlüsselfelder
      *
      *   Rev 2.0   Feb 27 1999 12:50:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:42   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:59:24   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00065 !30.03.00!    !-VKBKLAUSELID IST          !C. HEINECK*
      *         !        !    ! JETZT INTEGER             !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERSKLAUSELBEZ  TABLE
CCC004*    ( VKBVERTRAGSID CHAR (13) NOT NULL,
CCC004*      VKBVERSTARLFDNR SMALLINT NOT NULL,
CCC004*      VKBKLAUSELID INTEGER NOT NULL,
CCC004*      VKBKLAUSELLFDNR SMALLINT NOT NULL,
CCC004*      VKBKLAUSELPARTGID SMALLINT,
CCC004*      VKBKLAUSEL CHAR(10),
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERSKLAUSELBEZ-GESAMT.
        05  NI-VERSKLAUSELBEZ-TAB.
           10 NI-VERSKLAUSELBEZ-ELE PIC S9(04) COMP OCCURS 7 TIMES.
        05  NI-VERSKLAUSELBEZ  REDEFINES  NI-VERSKLAUSELBEZ-TAB.
           10 NI-VKBVERTRAGSID            PIC S9(4) COMP.
           10 NI-VKBVERSTARLFDNR          PIC S9(4) COMP.
           10 NI-VKBKLAUSELID             PIC S9(4) COMP.
           10 NI-VKBKLAUSELLFDNR          PIC S9(4) COMP.
           10 NI-VKBKLAUSELPARTGID        PIC S9(4) COMP.
           10 NI-VKBKLAUSEL               PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-VERSKLAUSELBEZ.
           10 VKBVERTRAGSID               PIC X(13).
           10 VKBVERSTARLFDNR             PIC S9(4) COMP.
000330     10 VKBKLAUSELID                PIC S9(9) COMP.
           10 VKBKLAUSELLFDNR             PIC S9(4) COMP.
           10 VKBKLAUSELPARTGID           PIC S9(4) COMP.
           10 VKBKLAUSEL                  PIC X(10).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************

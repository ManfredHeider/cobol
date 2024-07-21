      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctki0365.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\ctki0365.cpv  $
      *
      *   Rev 2.1   Apr 10 2000 14:24:06   T13EX41
      *AKI00065
      *- Längenänderung Schlüsselfelder
      *
      *   Rev 2.0   Feb 27 1999 12:51:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:30:12   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:00:42   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00065 !30.03.00!    !-WABWAGNISID IST           !C. HEINECK*
      *         !        !    ! JETZT INTEGER             !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  WAGNISBEZ  TABLE
CCC004*    ( WABVERTRAGSID CHAR (13) NOT NULL,
CCC004*      WABVERSTARIFLFDNR SMALLINT NOT NULL,
CCC004*      WABHPFLGRZUSATZKZ CHAR NOT NULL,
CCC004*      WABWAGNISID INTEGER NOT NULL,
CCC004*      WABWAGNISLFDNR SMALLINT NOT NULL,
CCC004*      WABWAGNISTEXT VARCHAR (25) NOT NULL,
CCC004*      WABWAGNISKZ CHAR NOT NULL,
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  WAGNISBEZ-GESAMT.
        05  NI-WAGNISBEZ-TAB.
           10 NI-WAGNISBEZ-ELE PIC S9(04) COMP OCCURS 8 TIMES.
        05  NI-WAGNISBEZ  REDEFINES  NI-WAGNISBEZ-TAB.
           10 NI-WABVERTRAGSID            PIC S9(4) COMP.
           10 NI-WABVERSTARIFLFDNR        PIC S9(4) COMP.
           10 NI-WABHPFLGRZUSATZKZ        PIC S9(4) COMP.
           10 NI-WABWAGNISID              PIC S9(4) COMP.
           10 NI-WABWAGNISLFDNR           PIC S9(4) COMP.
           10 NI-WABWAGNISTEXT            PIC S9(4) COMP.
           10 NI-WABWAGNISKZ              PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-WAGNISBEZ.
           10 WABVERTRAGSID               PIC X(13).
           10 WABVERSTARIFLFDNR           PIC S9(4) COMP.
           10 WABHPFLGRZUSATZKZ           PIC X(01).
000330     10 WABWAGNISID                 PIC S9(9) COMP.
           10 WABWAGNISLFDNR              PIC S9(4) COMP.
           10 WABWAGNISTEXT.
              49 WABWAGNISTEXT-LEN        PIC S9(4) COMP.
              49 WABWAGNISTEXT-TEXT       PIC X(25).
           10 WABWAGNISKZ                 PIC X(01).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************

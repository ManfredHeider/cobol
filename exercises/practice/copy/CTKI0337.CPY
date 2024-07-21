      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctki0337.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\ctki0337.cpv  $
      *
      *   Rev 2.1   Apr 10 2000 14:23:56   T13EX41
      *AKI00065
      *- Längenänderung Schlüsselfelder
      *
      *   Rev 2.0   Feb 27 1999 12:50:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:22   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:58:36   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00065 !30.03.00!    !-UMFANGID IST JETZT INTEGER!C. HEINECK*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  UMFANG  TABLE
CCC004*    ( UMFANGID                INTEGER NOT NULL,
CCC004*      UMFANGTEXT              CHAR (30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  UMFANG-GESAMT.
        05  NI-UMFANG-TAB.
           10 NI-UMFANG-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-UMFANG  REDEFINES  NI-UMFANG-TAB.
           10 NI-UMFANGID                 PIC S9(4) COMP.
           10 NI-UMFANGTEXT               PIC S9(4) COMP.
        05  DCL-UMFANG.
000330     10 UMFANGID                    PIC S9(9) COMP.
           10 UMFANGTEXT                  PIC X(30).
      ******************************************************************

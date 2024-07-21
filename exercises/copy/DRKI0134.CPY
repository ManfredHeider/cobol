      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0134.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0134.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 May 1998 17:51:28   P13SP99
      *akib0001 - Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:50:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      * THESE ARE THE PARAMETER ENTRIES PASSED TO AND FROM PROGRAM UPKI0003
      *   AND OTHER DB2 IO PROGRAMS ZB. UPKI10002, ETC.
      *

       01  DRKI0134-PARAM.

           03  DRKI0134-FUNCTION-CODE.
      *
      *
      *            THIS TABLE NUMBER IS USED FOR EASIER IDENTIFICATION
      *            OF THE SQL.  THE PURPOSE OF PROGRAM UPKI0003 IS TO
      *            HAVE ALL THE SQLS IN ONE PROGRAM SO THAT EFFECTIVELY
      *            THE SAME CODE CAN BE USED AGAIN BY ANOTHER PROCESS
      *            WHERE THERE ARE GOING TO BE MULTIPLE TABLES USED,
      *            LIKE IN THE CASE OF JOINS, THEN THIS TABLE NUMBER
      *            MUST BE THE MOST IMPORTANT ONE FOR THAT QUERY.
      *            REMEMBER THAT THIS NUMBER IS FOR REFERENCE PURPOSES
      *            ONLY.
      *
               05  DRKI0134-TABLE-NUMBER       PIC 9(04).
               05  DRKI0134-FUNCTION-SEQUENCE  PIC 9(04).
               05  DRKI0134-ACTION-CODE        PIC X(1).
                   88  DRKI0134-SELECT-ACTION       VALUE 'S'.
                   88  DRKI0134-OPEN-CURSOR-ACTION  VALUE 'O'.
                   88  DRKI0134-FETCH-CURSOR-ACTION VALUE 'F'.
                   88  DRKI0134-CLOSE-CURSOR-ACTION VALUE 'C'.
                   88  DRKI0134-INSERT-ACTION       VALUE 'I'.
                   88  DRKI0134-DELETE-ACTION       VALUE 'D'.
                   88  DRKI0134-UPDATE-ACTION       VALUE 'U'.
           03  DRKI0134-DATA-AREA              PIC X(1024).
           03  DRKI0134-SQLCA.
               05 DRKI0134-SQLCAID PIC X(8).
               05 DRKI0134-SQLCABC PIC S9(9) COMP-4.
               05 DRKI0134-SQLCODE PIC S9(9) COMP-4.
               05 DRKI0134-SQLERRM.
                  49 DRKI0134-SQLERRML PIC S9(4) COMP-4.
                  49 DRKI0134-SQLERRMC PIC X(70).
               05 DRKI0134-SQLERRP PIC X(8).
               05 DRKI0134-SQLERRD OCCURS 6 TIMES
                              PIC S9(9) COMP-4.
               05 DRKI0134-SQLWARN.
                  10 DRKI0134-SQLWARN0 PIC X.
                  10 DRKI0134-SQLWARN1 PIC X.
                  10 DRKI0134-SQLWARN2 PIC X.
                  10 DRKI0134-SQLWARN3 PIC X.
                  10 DRKI0134-SQLWARN4 PIC X.
                  10 DRKI0134-SQLWARN5 PIC X.
                  10 DRKI0134-SQLWARN6 PIC X.
                  10 DRKI0134-SQLWARN7 PIC X.
               05 DRKI0134-SQLEXT.
                  10 DRKI0134-SQLWARN8 PIC X.
                  10 DRKI0134-SQLWARN9 PIC X.
                  10 DRKI0134-SQLWARNA PIC X.
                  10 DRKI0134-SQLSTATE PIC X(5).

      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0067.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTKI0067.CPv  $
      *
      *   Rev 2.4   Sep 18 2012 15:14:56   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-KVB
      *
      *   Rev 2.3   May 13 2004 14:12:14   t13ex27
      *DB-Änderung für Fix 61 CTKI's
      *
      *   Rev 2.2   Feb 11 2004 12:49:38   t13ex27
      *AKI00403 - DB-Änderung Fix 57
      *
      *   Rev 2.1   22 Oct 2001 14:48:28   t13ex27
      *AKI00240 - DB-Änderung Fixpack 6
      *
      *   Rev 2.0   Feb 27 1999 12:49:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   09 May 1998 13:27:52   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:56:00   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE KUNDENINTVBBEZ TABLE
CCC004*    ( KVBVBPERSONENID                CHAR(13) NOT NULL,
CCC004*      KVBVBPERSONENROLLE             CHAR(1) NOT NULL,
CCC004*      KVBKIPERSONENID                CHAR(13) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      KVBLOESCHKZ                    CHAR(1),
CCC004*      KVBKUNDEBETREUUNG              CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE KUNDENINTVBBEZ              *
      ******************************************************************
       01  KUNDENINTVBBEZ-GESAMT.
        05 NI-KUNDENINTVBBEZ-TAB.
           10 NI-KUNDENINTVBBEZ-ELE PIC S9(04) COMP OCCURS 06 TIMES.
        05 NI-KUNDENINTVBBEZ REDEFINES NI-KUNDENINTVBBEZ-TAB.
           10 NI-KVBVBPERSONENID    PIC S9(4) COMP.
           10 NI-KVBVBPERSONENROLLE PIC S9(4) COMP.
           10 NI-KVBKIPERSONENID    PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-KVBLOESCHKZ        PIC S9(4) COMP.
           10 NI-KVBKUNDEBETREUUNG  PIC S9(4) COMP.
        05 DCL-KUNDENINTVBBEZ.
           10 KVBVBPERSONENID      PIC X(13).
           10 KVBVBPERSONENROLLE   PIC X(1).
           10 KVBKIPERSONENID      PIC X(13).
           10 LETZTEAENDERUNG      PIC X(26).
           10 KVBLOESCHKZ          PIC X(1).
           10 KVBKUNDEBETREUUNG    PIC X(1).
      ************************** ENDE-DCLGEN *************************

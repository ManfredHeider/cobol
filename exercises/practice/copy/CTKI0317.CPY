      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0317.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTKI0317.CPv  $
      *
      *   Rev 2.4   May 23 2017 10:51:02   t13ac17
      *AKI01510: Standard OZP
      *
      *   Rev 2.3   Sep 26 2005 13:53:30   t13ex27
      *AKI00521: DB-Änderung für Fix 80 CTKI's
      *
      *   Rev 2.2   May 14 2004 13:42:54   t13ex27
      *DB-Änderung für Fix 61 CTKI's
      *
      *   Rev 2.1   May 13 2004 14:12:50   t13ex27
      *DB-Änderung für Fix 61 CTKI's
      *
      *   Rev 2.0   Feb 27 1999 12:50:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:50   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:57:18   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE LEBENRWVASP TABLE
CCC004*    ( LRVVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      LRVVERSTARLFDNR                SMALLINT NOT NULL,
CCC004*      LRVRKWHV                       DECIMAL(13, 2),
CCC004*      LRVRKWHVBKZ                    CHAR(1),
CCC004*      LRVRKWBUZV                     DECIMAL(13, 2),
CCC004*      LRVRKWBUZVBKZ                  CHAR(1),
CCC004*      LRVRKWTZV                      DECIMAL(13, 2),
CCC004*      LRVRKWTZVBKZ                   CHAR(1),
CCC004*      LRVRKWPZV                      DECIMAL(13, 2),
CCC004*      LRVRKWPZVBKZ                   CHAR(1),
CCC004*      LRVRKWEUZV                     DECIMAL(13, 2),
CCC004*      LRVRKWEUZVBKZ                  CHAR(1),
CCC004*      LRVBTFRSUMBUZV                 DECIMAL(13, 2),
CCC004*      LRVBTFRSUMBUZVBKZ              CHAR(1),
CCC004*      LRVBTFRSUMTZV                  DECIMAL(13, 2),
CCC004*      LRVBTFRSUMTZVBKZ               CHAR(1),
CCC004*      LRVBTFRSUMPZV                  DECIMAL(13, 2),
CCC004*      LRVBTFRSUMPZVBKZ               CHAR(1),
CCC004*      LRVBTFRSUMEUZV                 DECIMAL(13, 2),
CCC004*      LRVBTFRSUMEUZVBKZ              CHAR(1),
CCC004*      LRVFMBTFRTTBUZV                DATE,
CCC004*      LRVFMBTFRTTBUZVBKZ             CHAR(1),
CCC004*      LRVFMBTFRTTTZV                 DATE,
CCC004*      LRVFMBTFRTTTZVBKZ              CHAR(1),
CCC004*      LRVFMBTFRTTPZV                 DATE,
CCC004*      LRVFMBTFRTTPZVBKZ              CHAR(1),
CCC004*      LRVFMBTFRTTEUZV                DATE,
CCC004*      LRVFMBTFRTTEUZVBKZ             CHAR(1),
CCC004*      LRVBTFRISTWERTBUZV             DECIMAL(13, 2),
CCC004*      LRVBTFRISTWERTTZV              DECIMAL(13, 2),
CCC004*      LRVBTFRISTWERTPZV              DECIMAL(13, 2),
CCC004*      LRVBTFRISTWERTEUZV             DECIMAL(13, 2),
CCC004*      LRVBTFRMINDWERTBUZ             DECIMAL(13, 2),
CCC004*      LRVBTFRMINDWERTTZV             DECIMAL(13, 2),
CCC004*      LRVBTFRMINDWERTPZV             DECIMAL(13, 2),
CCC004*      LRVBTFRMINDWERTEUZ             DECIMAL(13, 2),
CCC004*      LRVBETBWRRKHV                  DECIMAL(13, 2),
CCC004*      LRVBETBWRRKHVBKZ               CHAR(1),
CCC004*      LRVBETBWRRKBUZV                DECIMAL(13, 2),
CCC004*      LRVBETBWRRKBUZVBKZ             CHAR(1),
CCC004*      LRVBETBWRRKTZV                 DECIMAL(13, 2),
CCC004*      LRVBETBWRRKTZVBKZ              CHAR(1),
CCC004*      LRVBETBWRRKPZV                 DECIMAL(13, 2),
CCC004*      LRVBETBWRRKPZVBKZ              CHAR(1),
CCC004*      LRVBETBWRRKEUZV                DECIMAL(13, 2),
CCC004*      LRVBETBWRRKEUZVBKZ             CHAR(1),
CCC004*      LETZTEAENDERUNG                TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE LEBENRWVASP                 *
      ******************************************************************
       01  LEBENRWVASP-GESAMT.
        05 NI-LEBENRWVASP-TAB.
           10 NI-LEBENRWVASP-ELE PIC S9(04) COMP OCCURS 47 TIMES.
        05 NI-LEBENRWVASP REDEFINES NI-LEBENRWVASP-TAB.
           10 NI-LRVVERTRAGSID      PIC S9(4) COMP.
           10 NI-LRVVERSTARLFDNR    PIC S9(4) COMP.
           10 NI-LRVRKWHV           PIC S9(4) COMP.
           10 NI-LRVRKWHVBKZ        PIC S9(4) COMP.
           10 NI-LRVRKWBUZV         PIC S9(4) COMP.
           10 NI-LRVRKWBUZVBKZ      PIC S9(4) COMP.
           10 NI-LRVRKWTZV          PIC S9(4) COMP.
           10 NI-LRVRKWTZVBKZ       PIC S9(4) COMP.
           10 NI-LRVRKWPZV          PIC S9(4) COMP.
           10 NI-LRVRKWPZVBKZ       PIC S9(4) COMP.
           10 NI-LRVRKWEUZV         PIC S9(4) COMP.
           10 NI-LRVRKWEUZVBKZ      PIC S9(4) COMP.
           10 NI-LRVBTFRSUMBUZV     PIC S9(4) COMP.
           10 NI-LRVBTFRSUMBUZVBKZ  PIC S9(4) COMP.
           10 NI-LRVBTFRSUMTZV      PIC S9(4) COMP.
           10 NI-LRVBTFRSUMTZVBKZ   PIC S9(4) COMP.
           10 NI-LRVBTFRSUMPZV      PIC S9(4) COMP.
           10 NI-LRVBTFRSUMPZVBKZ   PIC S9(4) COMP.
           10 NI-LRVBTFRSUMEUZV     PIC S9(4) COMP.
           10 NI-LRVBTFRSUMEUZVBKZ  PIC S9(4) COMP.
           10 NI-LRVFMBTFRTTBUZV    PIC S9(4) COMP.
           10 NI-LRVFMBTFRTTBUZVBKZ PIC S9(4) COMP.
           10 NI-LRVFMBTFRTTTZV     PIC S9(4) COMP.
           10 NI-LRVFMBTFRTTTZVBKZ  PIC S9(4) COMP.
           10 NI-LRVFMBTFRTTPZV     PIC S9(4) COMP.
           10 NI-LRVFMBTFRTTPZVBKZ  PIC S9(4) COMP.
           10 NI-LRVFMBTFRTTEUZV    PIC S9(4) COMP.
           10 NI-LRVFMBTFRTTEUZVBKZ PIC S9(4) COMP.
           10 NI-LRVBTFRISTWERTBUZV PIC S9(4) COMP.
           10 NI-LRVBTFRISTWERTTZV  PIC S9(4) COMP.
           10 NI-LRVBTFRISTWERTPZV  PIC S9(4) COMP.
           10 NI-LRVBTFRISTWERTEUZV PIC S9(4) COMP.
           10 NI-LRVBTFRMINDWERTBUZ PIC S9(4) COMP.
           10 NI-LRVBTFRMINDWERTTZV PIC S9(4) COMP.
           10 NI-LRVBTFRMINDWERTPZV PIC S9(4) COMP.
           10 NI-LRVBTFRMINDWERTEUZ PIC S9(4) COMP.
           10 NI-LRVBETBWRRKHV      PIC S9(4) COMP.
           10 NI-LRVBETBWRRKHVBKZ   PIC S9(4) COMP.
           10 NI-LRVBETBWRRKBUZV    PIC S9(4) COMP.
           10 NI-LRVBETBWRRKBUZVBKZ PIC S9(4) COMP.
           10 NI-LRVBETBWRRKTZV     PIC S9(4) COMP.
           10 NI-LRVBETBWRRKTZVBKZ  PIC S9(4) COMP.
           10 NI-LRVBETBWRRKPZV     PIC S9(4) COMP.
           10 NI-LRVBETBWRRKPZVBKZ  PIC S9(4) COMP.
           10 NI-LRVBETBWRRKEUZV    PIC S9(4) COMP.
           10 NI-LRVBETBWRRKEUZVBKZ PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-LEBENRWVASP.
           10 LRVVERTRAGSID        PIC X(13).
           10 LRVVERSTARLFDNR      PIC S9(4) USAGE COMP.
           10 LRVRKWHV             PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVRKWHVBKZ          PIC X(1).
           10 LRVRKWBUZV           PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVRKWBUZVBKZ        PIC X(1).
           10 LRVRKWTZV            PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVRKWTZVBKZ         PIC X(1).
           10 LRVRKWPZV            PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVRKWPZVBKZ         PIC X(1).
           10 LRVRKWEUZV           PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVRKWEUZVBKZ        PIC X(1).
           10 LRVBTFRSUMBUZV       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBTFRSUMBUZVBKZ    PIC X(1).
           10 LRVBTFRSUMTZV        PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBTFRSUMTZVBKZ     PIC X(1).
           10 LRVBTFRSUMPZV        PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBTFRSUMPZVBKZ     PIC X(1).
           10 LRVBTFRSUMEUZV       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBTFRSUMEUZVBKZ    PIC X(1).
           10 LRVFMBTFRTTBUZV      PIC X(10).
           10 LRVFMBTFRTTBUZVBKZ   PIC X(1).
           10 LRVFMBTFRTTTZV       PIC X(10).
           10 LRVFMBTFRTTTZVBKZ    PIC X(1).
           10 LRVFMBTFRTTPZV       PIC X(10).
           10 LRVFMBTFRTTPZVBKZ    PIC X(1).
           10 LRVFMBTFRTTEUZV      PIC X(10).
           10 LRVFMBTFRTTEUZVBKZ   PIC X(1).
           10 LRVBTFRISTWERTBUZV   PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBTFRISTWERTTZV    PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBTFRISTWERTPZV    PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBTFRISTWERTEUZV   PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBTFRMINDWERTBUZ   PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBTFRMINDWERTTZV   PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBTFRMINDWERTPZV   PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBTFRMINDWERTEUZ   PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBETBWRRKHV        PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBETBWRRKHVBKZ     PIC X(1).
           10 LRVBETBWRRKBUZV      PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBETBWRRKBUZVBKZ   PIC X(1).
           10 LRVBETBWRRKTZV       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBETBWRRKTZVBKZ    PIC X(1).
           10 LRVBETBWRRKPZV       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBETBWRRKPZVBKZ    PIC X(1).
           10 LRVBETBWRRKEUZV      PIC S9(11)V9(2) USAGE COMP-3.
           10 LRVBETBWRRKEUZVBKZ   PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************


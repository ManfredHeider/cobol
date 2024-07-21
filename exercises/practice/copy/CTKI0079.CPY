      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0079.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0079.CPv  $
      *
      *   Rev 2.1   Oct 23 2003 16:21:44   t13ex27
      *AKI00373 - DB-Änderung für Fixpack 51
      *
      *   Rev 2.0   Feb 27 1999 12:50:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   29 Jan 1999 11:33:28   P13SP13
      *Neuerstellung
      *
      *   Rev 1.1   09 May 1998 13:28:10   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   31 Mar 1998 13:10:24   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.100 19980722'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  ADRESSAENDERUNG  TABLE
CCC004*    ( ADAPERSONENID                  CHAR(13) NOT NULL,
CCC004*      ADAVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      ADAADRESSEALTID                CHAR(13) NOT NULL,
CCC004*      ADAADRESSENEUID                CHAR(13) NOT NULL,
CCC004*      ADAVBID                        CHAR(13) NOT NULL,
CCC004*      ADAVBROLLE                     CHAR(1) NOT NULL,
CCC004*      ADAAENDGRUNDID                 SMALLINT NOT NULL,
CCC004*      ADADTAERLEDIGT                 CHAR(1) NOT NULL,
CCC004*      ADGEAENDERTAM                  DATE NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  ADRESSAENDERUNG-GESAMT.
        05  NI-ADRESSAENDERUNG-TAB.
221003*    10 NI-ADRESSAENDERUNG-ELE PIC S9(04) COMP OCCURS 9 TIMES.
221003     10 NI-ADRESSAENDERUNG-ELE PIC S9(04) COMP OCCURS 10 TIMES.
        05  NI-ADRESSAENDERUNG  REDEFINES  NI-ADRESSAENDERUNG-TAB.
           10 NI-ADAPERSONENID            PIC S9(4) COMP.
           10 NI-ADAVERTRAGSID            PIC S9(4) COMP.
           10 NI-ADAADRESSEALTID          PIC S9(4) COMP.
           10 NI-ADAADRESSENEUID          PIC S9(4) COMP.
           10 NI-ADAVBID                  PIC S9(4) COMP.
           10 NI-ADAVBROLLE               PIC S9(4) COMP.
           10 NI-ADAAENDGRUNDID           PIC S9(4) COMP.
           10 NI-ADADTAERLEDIGT           PIC S9(4) COMP.
           10 NI-ADGEAENDERTAM            PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-ADRESSAENDERUNG.
           10 ADAPERSONENID        PIC X(13).
           10 ADAVERTRAGSID        PIC X(13).
           10 ADAADRESSEALTID      PIC X(13).
           10 ADAADRESSENEUID      PIC X(13).
           10 ADAVBID              PIC X(13).
           10 ADAVBROLLE           PIC X(1).
           10 ADAAENDGRUNDID       PIC S9(4) COMP.
           10 ADADTAERLEDIGT       PIC X(1).
           10 ADGEAENDERTAM        PIC X(10).
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************

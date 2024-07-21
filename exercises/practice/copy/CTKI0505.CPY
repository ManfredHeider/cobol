      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0505.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0505.CPv  $
      *
      *   Rev 2.2   Dec 19 2016 13:23:16   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.1   Dec 07 2016 10:53:34   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.0   Feb 27 1999 12:51:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:31:02   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   15 Jan 1998 18:05:24   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE KVPERSONEN TABLE
CCC004*    ( KVPVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      KVPLFDNR                       SMALLINT NOT NULL,
CCC004*      KVPPRODUKTID                   SMALLINT NOT NULL,
CCC004*      KVPANREDE_KTY                  SMALLINT NOT NULL,
CCC004*      KVPANREDE_KNR                  SMALLINT NOT NULL,
CCC004*      KVPNACHNAME                    CHAR(30),
CCC004*      KVPVORNAME                     CHAR(30),
CCC004*      KVPGEBDATUM                    DATE,
CCC004*      KVPBEGINNDATUM                 DATE,
CCC004*      KVPENDEDATUM                   DATE,
CCC004*      KVPVORVENDDATUM                DATE,
CCC004*      KVPVORVGES_KTY                 SMALLINT,
CCC004*      KVPVORVGES_KNR                 DECIMAL(15, 0),
CCC004*      KVPVORVKUENDKZ                 CHAR(1),
CCC004*      KVPVERSART                     CHAR(1),
CCC004*      KVPABRECHKZ                    CHAR(1),
CCC004*      KVPZUSRAHMEN                   CHAR(1),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE KVPERSONEN                  *
      ******************************************************************
       01  KVPERSONEN-GESAMT.
        05 NI-KVPERSONEN-TAB.
           10 NI-KVPERSONEN-ELE PIC S9(04) COMP OCCURS 18 TIMES.
        05 NI-KVPERSONEN REDEFINES NI-KVPERSONEN-TAB.
           10 NI-KVPVERTRAGSID      PIC S9(4) COMP.
           10 NI-KVPLFDNR           PIC S9(4) COMP.
           10 NI-KVPPRODUKTID       PIC S9(4) COMP.
           10 NI-KVPANREDE-KTY      PIC S9(4) COMP.
           10 NI-KVPANREDE-KNR      PIC S9(4) COMP.
           10 NI-KVPNACHNAME        PIC S9(4) COMP.
           10 NI-KVPVORNAME         PIC S9(4) COMP.
           10 NI-KVPGEBDATUM        PIC S9(4) COMP.
           10 NI-KVPBEGINNDATUM     PIC S9(4) COMP.
           10 NI-KVPENDEDATUM       PIC S9(4) COMP.
           10 NI-KVPVORVENDDATUM    PIC S9(4) COMP.
           10 NI-KVPVORVGES-KTY     PIC S9(4) COMP.
           10 NI-KVPVORVGES-KNR     PIC S9(4) COMP.
           10 NI-KVPVORVKUENDKZ     PIC S9(4) COMP.
           10 NI-KVPVERSART         PIC S9(4) COMP.
           10 NI-KVPABRECHKZ        PIC S9(4) COMP.
           10 NI-KVPZUSRAHMEN       PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-KVPERSONEN.
           10 KVPVERTRAGSID        PIC X(13).
           10 KVPLFDNR             PIC S9(4) USAGE COMP.
           10 KVPPRODUKTID         PIC S9(4) USAGE COMP.
           10 KVPANREDE-KTY        PIC S9(4) USAGE COMP.
           10 KVPANREDE-KNR        PIC S9(4) USAGE COMP.
           10 KVPNACHNAME          PIC X(30).
           10 KVPVORNAME           PIC X(30).
           10 KVPGEBDATUM          PIC X(10).
           10 KVPBEGINNDATUM       PIC X(10).
           10 KVPENDEDATUM         PIC X(10).
           10 KVPVORVENDDATUM      PIC X(10).
           10 KVPVORVGES-KTY       PIC S9(4) USAGE COMP.
           10 KVPVORVGES-KNR       PIC S9(15)V USAGE COMP-3.
           10 KVPVORVKUENDKZ       PIC X(1).
           10 KVPVERSART           PIC X(1).
           10 KVPABRECHKZ          PIC X(1).
           10 KVPZUSRAHMEN         PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************


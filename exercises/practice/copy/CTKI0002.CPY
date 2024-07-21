      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0002.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0002.CPv  $
      *
      *   Rev 2.3   Feb 11 2004 12:48:44   t13ex27
      *AKI00403 - DB-Änderung Fix 57
      *
      *   Rev 2.2   Aug 11 2003 10:52:26   t13ex27
      *AKI00360 DB-Änderung für Fix 46 und
      *         neues Adressverfahren
      *
      *   Rev 2.1   Jul 02 2003 15:55:16   t13ex27
      *AKI00268 - DB-Änderung Neues Adressverfahren
      *
      *   Rev 2.0   Feb 27 1999 12:47:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   14 May 1998 16:22:10   P13SP99
      *Neuerstellung
      *
      *   Rev 1.2   09 May 1998 13:25:38   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:51:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0002.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/CTKI0002.CPv  $
      *
      *   Rev 1.0   Apr 18 1997 09:51:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  ADRESSE  TABLE
CCC004*    ( ADRESSEID                      CHAR(13) NOT NULL,
CCC004*      ADRESSEBDSLDID                 CHAR(3),
CCC004*      ADRESSELANDID                  CHAR(3) NOT NULL,
CCC004*      ADRESSEORT                     CHAR(30),
CCC004*      ADRESSEPLZ                     CHAR(15),
CCC004*      ADRESSESTRHAUSNUM              CHAR(45),
CCC004*      ADRESSEISTVTADR                CHAR(1) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      ADRESSESUCHSTRASSE             CHAR(45),
CCC004*      ADRESSEGEPRUEFT                CHAR(1),
CCC004*      ADRESSEORTSTEIL                CHAR(30) NOT NULL,
CCC004*      ADRESSEPOSTPRUEF               CHAR(25) NOT NULL,
CCC004*      ADRESSEPOSTPRUEFID             SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  ADRESSE-GESAMT.
        05  NI-ADRESSE-TAB.
020703*    10 NI-ADRESSE-ELE PIC S9(04) COMP OCCURS 8 TIMES.
020703*    10 NI-ADRESSE-ELE PIC S9(04) COMP OCCURS 10 TIMES.
100204     10 NI-ADRESSE-ELE PIC S9(04) COMP OCCURS 13 TIMES.
        05  NI-ADRESSE  REDEFINES  NI-ADRESSE-TAB.
           10 NI-ADRESSEID                PIC S9(4) COMP.
           10 NI-ADRESSEBDSLDID           PIC S9(4) COMP.
           10 NI-ADRESSELANDID            PIC S9(4) COMP.
           10 NI-ADRESSEORT               PIC S9(4) COMP.
           10 NI-ADRESSEPLZ               PIC S9(4) COMP.
           10 NI-ADRESSESTRHAUSNUM        PIC S9(4) COMP.
           10 NI-ADRESSEISTVTADR          PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
020703     10 NI-ADRESSESUCHSTRASSE       PIC S9(4) COMP.
020703     10 NI-ADRESSEGEPRUEFT          PIC S9(4) COMP.
100204     10 NI-ADRESSEORTSTEIL          PIC S9(4) COMP.
100204     10 NI-ADRESSEPOSTPRUEF         PIC S9(4) COMP.
100204     10 NI-ADRESSEPOSTPRUEFID       PIC S9(4) COMP.
        05  DCL-ADRESSE.
           10 ADRESSEID            PIC X(13).
           10 ADRESSEBDSLDID       PIC X(3).
           10 ADRESSELANDID        PIC X(3).
           10 ADRESSEORT           PIC X(30).
           10 ADRESSEPLZ           PIC X(15).
           10 ADRESSESTRHAUSNUM    PIC X(45).
           10 ADRESSEISTVTADR      PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
020703     10 ADRESSESUCHSTRASSE   PIC X(45).
020703     10 ADRESSEGEPRUEFT      PIC X(1).
100204     10 ADRESSEORTSTEIL      PIC X(30).
100204     10 ADRESSEPOSTPRUEF     PIC X(25).
100204     10 ADRESSEPOSTPRUEFID   PIC S9(4) COMP.
      ******************************************************************

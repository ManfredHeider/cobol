      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0206.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/CTKI0206.CPv  $
      *
      *   Rev 2.5   19 May 2009 15:36:04   t13ex27
      *AKI00765: Mandantenfähige Internetanfragen-DB-Änderung
      *
      *   Rev 2.4   Aug 22 2005 08:41:28   t13ex27
      *AKI00474: DB-Änderung für Fix 73
      *
      *   Rev 2.3   Apr 27 2005 13:58:50   t13ex49
      *AKI00474: DB-Änderung für Fix 73
      *
      *   Rev 2.2   Apr 25 2005 10:59:48   t13ex49
      *AKI00474: DB-Änderung für Fix 73
      *
      *   Rev 2.1   Mar 03 2005 10:12:30   t13ex27
      *AKI00474: DB-Änderung für Fix 73 CTKIxxxx
      *
      *   Rev 2.0   Feb 27 1999 12:50:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   04 Sep 1997 09:52:46   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE INTERNETADRESSEN
      ******************************************************************
CCC005*    EXEC SQL DECLARE INTERNETADRESSEN TABLE
CCC004*    ( IADID                          CHAR(13) NOT NULL,
CCC004*      IADANREDE                      CHAR(10) NOT NULL,
CCC004*      IADNACHNAME                    CHAR(30) NOT NULL,
CCC004*      IADVORNAME                     CHAR(30) NOT NULL,
CCC004*      IADLKZ                         CHAR(3) NOT NULL,
CCC004*      IADPLZ                         CHAR(10) NOT NULL,
CCC004*      IADORT                         CHAR(30) NOT NULL,
CCC004*      IADSTR                         CHAR(45) NOT NULL,
CCC004*      IADTELEFON                     CHAR(25) NOT NULL,
CCC004*      IADTELEFAX                     CHAR(25) NOT NULL,
CCC004*      IADEMAIL                       CHAR(70) NOT NULL,
CCC004*      IADINFOKZ1                     CHAR(1) NOT NULL,
CCC004*      IADINFOKZ2                     CHAR(1) NOT NULL,
CCC004*      IADINFOKZ3                     CHAR(1) NOT NULL,
CCC004*      IADINFOKZ4                     CHAR(1) NOT NULL,
CCC004*      IADINFOKZ5                     CHAR(1) NOT NULL,
CCC004*      IADINFOKZ6                     CHAR(1) NOT NULL,
CCC004*      IADINFOKZ7                     CHAR(1) NOT NULL,
CCC004*      IADINFOKZ8                     CHAR(1) NOT NULL,
CCC004*      IADINFOKZ9                     CHAR(1) NOT NULL,
CCC004*      IADINFOKZ10                    CHAR(1) NOT NULL,
CCC004*      IADVBSUCHEAB8                  INTEGER NOT NULL,
CCC004*      IADVBSUCHEAB9                  INTEGER NOT NULL,
CCC004*      IADVBSUCHEAB10                 INTEGER NOT NULL,
CCC004*      IADTEXT8                       CHAR(20) NOT NULL,
CCC004*      IADTEXT9                       CHAR(20) NOT NULL,
CCC004*      IADTEXT10                      CHAR(20) NOT NULL,
CCC004*      IADMAXSTUFE                    INTEGER NOT NULL,
CCC004*      IADERREICHBAR                  CHAR(25) NOT NULL,
CCC004*      IADZUGVBID                     CHAR(13),
CCC004*      IADPOSTEKZ                     CHAR(1) NOT NULL,
CCC004*      IADBGAM                        DATE NOT NULL,
CCC004*      IADBGAUSGABE                   CHAR(10) NOT NULL,
CCC004*      IADGEWSPANT                    CHAR(50) NOT NULL,
CCC004*      IADKUNDENID                    CHAR(13),
CCC004*      IADKUNDENSTATUSID              CHAR(1) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      IADINDIVTEXT                   VARCHAR(16000) NOT NULL,
CCC004*      IADANFMANDANT                  INTEGER,
CCC004*      IADZIELMANDANT                 INTEGER,
CCC004*      IADMITARBEITERNR               INTEGER,
CCC004*      IADSBMITARBEITNR               INTEGER,
CCC004*      IADKUNDE_NUMMER                DECIMAL(18, 0)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  INTERNETADRESSEN
      ******************************************************************
       01  INTERNETADRESSEN-GESAMT.
         05 NI-INTERNETADRESSEN-TAB.
           10 NI-INTERNETADRESSEN-ELE PIC S9(4) COMP OCCURS 43 TIMES.
         05 NI-INTERNETADRESSEN REDEFINES NI-INTERNETADRESSEN-TAB.
           10 NI-IADID                     PIC S9(4) COMP.
           10 NI-IADANREDE                 PIC S9(4) COMP.
           10 NI-IADNACHNAME               PIC S9(4) COMP.
           10 NI-IADVORNAME                PIC S9(4) COMP.
           10 NI-IADLKZ                    PIC S9(4) COMP.
           10 NI-IADPLZ                    PIC S9(4) COMP.
           10 NI-IADORT                    PIC S9(4) COMP.
           10 NI-IADSTR                    PIC S9(4) COMP.
           10 NI-IADTELEFON                PIC S9(4) COMP.
           10 NI-IADTELEFAX                PIC S9(4) COMP.
           10 NI-IADEMAIL                  PIC S9(4) COMP.
           10 NI-IADINFOKZ1                PIC S9(4) COMP.
           10 NI-IADINFOKZ2                PIC S9(4) COMP.
           10 NI-IADINFOKZ3                PIC S9(4) COMP.
           10 NI-IADINFOKZ4                PIC S9(4) COMP.
           10 NI-IADINFOKZ5                PIC S9(4) COMP.
           10 NI-IADINFOKZ6                PIC S9(4) COMP.
           10 NI-IADINFOKZ7                PIC S9(4) COMP.
           10 NI-IADINFOKZ8                PIC S9(4) COMP.
           10 NI-IADINFOKZ9                PIC S9(4) COMP.
           10 NI-IADINFOKZ10               PIC S9(4) COMP.
           10 NI-IADVBSUCHEAB8             PIC S9(4) COMP.
           10 NI-IADVBSUCHEAB9             PIC S9(4) COMP.
           10 NI-IADVBSUCHEAB10            PIC S9(4) COMP.
           10 NI-IADTEXT8                  PIC S9(4) COMP.
           10 NI-IADTEXT9                  PIC S9(4) COMP.
           10 NI-IADTEXT10                 PIC S9(4) COMP.
           10 NI-IADMAXSTUFE               PIC S9(4) COMP.
           10 NI-IADERREICHBAR             PIC S9(4) COMP.
           10 NI-IADZUGVBID                PIC S9(4) COMP.
           10 NI-IADPOSTEKZ                PIC S9(4) COMP.
           10 NI-IADBGAM                   PIC S9(4) COMP.
           10 NI-IADBGAUSGABE              PIC S9(4) COMP.
           10 NI-IADGEWSPANT               PIC S9(4) COMP.
           10 NI-IADKUNDENID               PIC S9(4) COMP.
           10 NI-IADKUNDENSTATUSID         PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG           PIC S9(4) COMP.
           10 NI-IADINDIVTEXT              PIC S9(4) COMP.
           10 NI-IADANFMANDANT             PIC S9(4) COMP.
           10 NI-IADZIELMANDANT            PIC S9(4) COMP.
           10 NI-IADMITARBEITERNR          PIC S9(4) COMP.
           10 NI-IADSBMITARBEITNR          PIC S9(4) COMP.
           10 NI-IADKUNDE-NUMMER           PIC S9(4) COMP.
         05 DCL-INTERNETADRESSEN.
           10 IADID                PIC X(13).
           10 IADANREDE            PIC X(10).
           10 IADNACHNAME          PIC X(30).
           10 IADVORNAME           PIC X(30).
           10 IADLKZ               PIC X(3).
           10 IADPLZ               PIC X(10).
           10 IADORT               PIC X(30).
           10 IADSTR               PIC X(45).
           10 IADTELEFON           PIC X(25).
           10 IADTELEFAX           PIC X(25).
           10 IADEMAIL             PIC X(70).
           10 IADINFOKZ1           PIC X(1).
           10 IADINFOKZ2           PIC X(1).
           10 IADINFOKZ3           PIC X(1).
           10 IADINFOKZ4           PIC X(1).
           10 IADINFOKZ5           PIC X(1).
           10 IADINFOKZ6           PIC X(1).
           10 IADINFOKZ7           PIC X(1).
           10 IADINFOKZ8           PIC X(1).
           10 IADINFOKZ9           PIC X(1).
           10 IADINFOKZ10          PIC X(1).
           10 IADVBSUCHEAB8        PIC S9(9)  COMP.
           10 IADVBSUCHEAB9        PIC S9(9)  COMP.
           10 IADVBSUCHEAB10       PIC S9(9)  COMP.
           10 IADTEXT8             PIC X(20).
           10 IADTEXT9             PIC X(20).
           10 IADTEXT10            PIC X(20).
           10 IADMAXSTUFE          PIC S9(9)  COMP.
           10 IADERREICHBAR        PIC X(25).
           10 IADZUGVBID           PIC X(13).
           10 IADPOSTEKZ           PIC X(1).
           10 IADBGAM              PIC X(10).
           10 IADBGAUSGABE         PIC X(10).
           10 IADGEWSPANT          PIC X(50).
           10 IADKUNDENID          PIC X(13).
           10 IADKUNDENSTATUSID    PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
           10 IADINDIVTEXT.
              49 IADINDIVTEXT-LEN  PIC S9(4) COMP.
              49 IADINDIVTEXT-TEXT  PIC X(16000).
           10 IADANFMANDANT        PIC S9(9) COMP.
           10 IADZIELMANDANT       PIC S9(9) COMP.
           10 IADMITARBEITERNR     PIC S9(9) COMP.
           10 IADSBMITARBEITNR     PIC S9(9) COMP.
           10 IADKUNDE-NUMMER      PIC S9(18)V COMP-3.
      ******************************************************************

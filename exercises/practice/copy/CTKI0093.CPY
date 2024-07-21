      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0093.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/CTKI0093.CPv  $
      *
      *   Rev 2.1   Sep 13 2002 08:33:40   t13ex27
      *AKI00300 Neue Tabelle ARBEITGEBERADRESSE
      *
      *   Rev 2.0   Aug 30 2002 12:10:34   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE ARBEITGEBERADRESSE                                *
      ******************************************************************
CCC005*    EXEC SQL DECLARE ARBEITGEBERADRESSE TABLE
CCC004*    ( AGAPERSONENID                  CHAR(13) NOT NULL,
CCC004*      AGAADRESSEBDSLDID              CHAR(3),
CCC004*      AGAADRESSELANDID               CHAR(3) NOT NULL,
CCC004*      AGAADRESSEORT                  CHAR(30),
CCC004*      AGAADRESSEPLZ                  CHAR(15),
CCC004*      AGAADRESSESTHAUSNR             CHAR(45),
CCC004*      AGAADRESSEISTVTADR             CHAR(1) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      AGAADRESSESUCHSTR              CHAR(45),
CCC004*      AGAADRESSEGEPRUEFT             CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE ARBEITGEBERADRESSE                 *
      ******************************************************************
       01  ARBEITGEBERADRESSE-GESAMT.
        05  NI-ARBEITGEBERADRESSE-TAB.
           10 NI-ARBEITGEBERADRESSE-ELE PIC S9(04) COMP OCCURS 10 TIMES.
        05  NI-ARBEITGEBERADRESSE REDEFINES NI-ARBEITGEBERADRESSE-TAB.
           10 NI-AGAPERSONENID       PIC S9(4) COMP.
           10 NI-AGAADRESSEBDSLDID   PIC S9(4) COMP.
           10 NI-AGAADRESSELANDID    PIC S9(4) COMP.
           10 NI-AGAADRESSEORT       PIC S9(4) COMP.
           10 NI-AGAADRESSEPLZ       PIC S9(4) COMP.
           10 NI-AGAADRESSESTHAUSNR  PIC S9(4) COMP.
           10 NI-AGAADRESSEISTVTADR  PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG     PIC S9(4) COMP.
           10 NI-AGAADRESSESUCHSTR   PIC S9(4) COMP.
           10 NI-AGAADRESSEGEPRUEFT  PIC S9(4) COMP.
        05 DCL-ARBEITGEBERADRESSE.
           10 AGAPERSONENID        PIC X(13).
           10 AGAADRESSEBDSLDID    PIC X(3).
           10 AGAADRESSELANDID     PIC X(3).
           10 AGAADRESSEORT        PIC X(30).
           10 AGAADRESSEPLZ        PIC X(15).
           10 AGAADRESSESTHAUSNR   PIC X(45).
           10 AGAADRESSEISTVTADR   PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
           10 AGAADRESSESUCHSTR    PIC X(45).
           10 AGAADRESSEGEPRUEFT   PIC X(1).
      ******************************************************************

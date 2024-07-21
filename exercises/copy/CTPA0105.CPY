      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0105.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTPA0105.CPv  $
      *
      *   Rev 2.4   Jul 16 2015 15:10:26   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.3   Jun 19 2012 09:32:20   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:57:18   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 14:27:30   t13ex21
      *APA00622: Einführung Allfinanz (MST)
      *
      *   Rev 2.0   May 27 1999 14:24:08   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   04 Aug 1997 13:11:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0105)                                         *
      *        VERSION 1.4                                             *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0105 TABLE
CCC004*    ( PNAP_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PNAP_VERSION                   SMALLINT NOT NULL,
CCC004*      KNR_OBJEKT_HKF_TAB             SMALLINT NOT NULL,
CCC004*      KNR_PNAP_OBJEKT                SMALLINT NOT NULL,
CCC004*      KNR_PNAP_ZUSTAND               SMALLINT NOT NULL,
CCC004*      KTY_PNAP_ZUSTAND               SMALLINT NOT NULL,
CCC004*      PNAP_FAELLIG_VON               DATE NOT NULL,
CCC004*      PRINTE                         DECIMAL(15, 0) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PNAP_VNR                       CHAR(15) NOT NULL,
CCC004*      PNAP_PIN                       INTEGER NOT NULL,
CCC004*      PVER_KUNDE_NAME                CHAR(25) NOT NULL,
CCC004*      PNAP_GSMT_PROV                 DECIMAL(11, 2) NOT NULL,
CCC004*      PNAP_PG_ABR_DAT                DATE NOT NULL,
CCC004*      PNAP_PROD_SCHL_1               SMALLINT NOT NULL,
CCC004*      PNAP_PROD_SCHL_2               SMALLINT NOT NULL,
CCC004*      PNAP_PROD_SCHL_3               SMALLINT NOT NULL,
CCC004*      PNAP_PROD_SCHL_KZ              CHAR(1) NOT NULL,
CCC004*      PNAP_VSTAT                     CHAR(4) NOT NULL,
CCC004*      PNAP_VSTAT_UNR                 SMALLINT NOT NULL,
CCC004*      PNAP_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PNAP_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      KNR_MANDANT                    SMALLINT NOT NULL,
CCC004*      PNAP_PG_UST_BETRAG             DECIMAL(11, 2) NOT NULL,
CCC004*      PNAP_PG_UST_SATZ               DECIMAL(6, 3) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0105                           *
      ******************************************************************
       01  TBPA0105-GESAMT.
         05  DCL-TBPA0105.
           10 PNAP-ID              PIC S9(15)V USAGE COMP-3.
           10 PNAP-VERSION         PIC S9(4) USAGE COMP.
           10 KNR-OBJEKT-HKF-TAB   PIC S9(4) USAGE COMP.
           10 KNR-PNAP-OBJEKT      PIC S9(4) USAGE COMP.
           10 KNR-PNAP-ZUSTAND     PIC S9(4) USAGE COMP.
           10 KTY-PNAP-ZUSTAND     PIC S9(4) USAGE COMP.
           10 PNAP-FAELLIG-VON     PIC X(10).
           10 PRINTE               PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PNAP-VNR             PIC X(15).
           10 PNAP-PIN             PIC S9(9) USAGE COMP.
           10 PVER-KUNDE-NAME      PIC X(25).
           10 PNAP-GSMT-PROV       PIC S9(9)V9(2) USAGE COMP-3.
           10 PNAP-PG-ABR-DAT      PIC X(10).
           10 PNAP-PROD-SCHL-1     PIC S9(4) USAGE COMP.
           10 PNAP-PROD-SCHL-2     PIC S9(4) USAGE COMP.
           10 PNAP-PROD-SCHL-3     PIC S9(4) USAGE COMP.
           10 PNAP-PROD-SCHL-KZ    PIC X(1).
           10 PNAP-VSTAT           PIC X(4).
           10 PNAP-VSTAT-UNR       PIC S9(4) USAGE COMP.
           10 PNAP-LAST-UPDATE     PIC X(26).
           10 PNAP-KENNUNG         PIC X(8).
230212     10 KNR-MANDANT          PIC S9(4) USAGE COMP.
040515     10 PNAP-PG-UST-BETRAG   PIC S9(9)V9(2) USAGE COMP-3.
040515     10 PNAP-PG-UST-SATZ     PIC S9(3)V9(3) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 22      *
      ******************************************************************


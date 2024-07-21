      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0331.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/CTKI0331.CPv  $
      *
      *   Rev 2.5   Feb 02 2021 15:09:04   t13ac20
      *AKI01825: Leben: Schüler BU Erweiterung und Anpassung im DBO
      *: Sourcen Db2
      *
      *   Rev 2.4   Mar 12 2019 07:33:16   t13ac17
      *AKI01664: Garantierte Leistungsdynamik im Falle von BU bzw.
      *GF - Anpassung Host-Komponenten Standard-OZP
      *
      *   Rev 2.3   Sep 20 2017 15:23:10   t13ac20
      *AKI01503: Bereinigung AML-Tabellen
      *
      *   Rev 2.2   Jun 23 2016 10:42:02   t13ex27
      *AKI01434: AML-Neue Produkte zum RT 06/16
      *
      *   Rev 2.1   Sep 12 2011 10:05:44   t13ex27
      *AKI01065: AMLeben - Einführung Impulsprodukt - VASP-Vertrags
      *daten
      *
      *   Rev 1.1   09 May 1998 13:29:14   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:58:14   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE LEBENTEILTYP TABLE
CCC004*    ( LTTVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      LTTVERSTARLFDNR                SMALLINT NOT NULL,
CCC004*      LTTTTNR                        SMALLINT NOT NULL,
CCC004*      LTTPROVHAFTZVON                DATE,
CCC004*      LTTPROVHAFTZBIS                DATE,
CCC004*      LTTABSCHLUSSVERM               INTEGER,
CCC004*      LTTDYNPROZ                     DECIMAL(5, 2),
CCC004*      LTTVERTRAGSABLAUF              DATE,
CCC004*      LTTVERTRAGSBEGINN              DATE,
CCC004*      LTTBEITRZAHLDAUER              DATE,
CCC004*      LTTDYNAMIKKZ                   CHAR(1) NOT NULL,
CCC004*      LTTDYNLTGRVID                  SMALLINT,
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      LTTRZINS                       DECIMAL(5, 2),
CCC004*      LTTDYNWAHL_KTY                 SMALLINT,
CCC004*      LTTDYNWAHL_KNR                 DECIMAL(15, 0),
CCC004*      LTTABSICHART_KTY               SMALLINT,
CCC004*      LTTABSICHART_KNR               DECIMAL(15, 0),
CCC004*      LTTABSICHVAR_KTY               SMALLINT,
CCC004*      LTTABSICHVAR_KNR               DECIMAL(15, 0),
CCC004*      LTTLSTERHPROZ                  DECIMAL(3, 1),
CCC004*      LTTBUZBERUFGRP_KTY             SMALLINT,
CCC004*      LTTBUZBERUFGRP_KNR             DECIMAL(15, 0)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE LEBENTEILTYP                *
      ******************************************************************
       01  LEBENTEILTYP-GESAMT.
        05 NI-LEBENTEILTYP-TAB.
           10 NI-LEBENTEILTYP-ELE PIC S9(04) COMP OCCURS 23 TIMES.
        05 NI-LEBENTEILTYP REDEFINES NI-LEBENTEILTYP-TAB.
           10 NI-LTTVERTRAGSID      PIC S9(4) COMP.
           10 NI-LTTVERSTARLFDNR    PIC S9(4) COMP.
           10 NI-LTTTTNR            PIC S9(4) COMP.
           10 NI-LTTPROVHAFTZVON    PIC S9(4) COMP.
           10 NI-LTTPROVHAFTZBIS    PIC S9(4) COMP.
           10 NI-LTTABSCHLUSSVERM   PIC S9(4) COMP.
           10 NI-LTTDYNPROZ         PIC S9(4) COMP.
           10 NI-LTTVERTRAGSABLAUF  PIC S9(4) COMP.
           10 NI-LTTVERTRAGSBEGINN  PIC S9(4) COMP.
           10 NI-LTTBEITRZAHLDAUER  PIC S9(4) COMP.
           10 NI-LTTDYNAMIKKZ       PIC S9(4) COMP.
           10 NI-LTTDYNLTGRVID      PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-LTTRZINS           PIC S9(4) COMP.
           10 NI-LTTDYNWAHL-KTY     PIC S9(4) COMP.
           10 NI-LTTDYNWAHL-KNR     PIC S9(4) COMP.
           10 NI-LTTABSICHART-KTY   PIC S9(4) COMP.
           10 NI-LTTABSICHART-KNR   PIC S9(4) COMP.
           10 NI-LTTABSICHVAR-KTY   PIC S9(4) COMP.
           10 NI-LTTABSICHVAR-KNR   PIC S9(4) COMP.
           10 NI-LTTLSTERHPROZ      PIC S9(4) COMP.
           10 NI-LTTBUZBERUFGRP-KTY PIC S9(4) COMP.
           10 NI-LTTBUZBERUFGRP-KNR PIC S9(4) COMP.
        05 DCL-LEBENTEILTYP.
           10 LTTVERTRAGSID        PIC X(13).
           10 LTTVERSTARLFDNR      PIC S9(4) USAGE COMP.
           10 LTTTTNR              PIC S9(4) USAGE COMP.
           10 LTTPROVHAFTZVON      PIC X(10).
           10 LTTPROVHAFTZBIS      PIC X(10).
           10 LTTABSCHLUSSVERM     PIC S9(9) USAGE COMP.
           10 LTTDYNPROZ           PIC S9(3)V9(2) USAGE COMP-3.
           10 LTTVERTRAGSABLAUF    PIC X(10).
           10 LTTVERTRAGSBEGINN    PIC X(10).
           10 LTTBEITRZAHLDAUER    PIC X(10).
           10 LTTDYNAMIKKZ         PIC X(1).
           10 LTTDYNLTGRVID        PIC S9(4) USAGE COMP.
           10 LETZTEAENDERUNG      PIC X(26).
           10 LTTRZINS             PIC S9(3)V9(2) USAGE COMP-3.
           10 LTTDYNWAHL-KTY       PIC S9(4) USAGE COMP.
           10 LTTDYNWAHL-KNR       PIC S9(15)V USAGE COMP-3.
           10 LTTABSICHART-KTY     PIC S9(4) USAGE COMP.
           10 LTTABSICHART-KNR     PIC S9(15)V USAGE COMP-3.
           10 LTTABSICHVAR-KTY     PIC S9(4) USAGE COMP.
           10 LTTABSICHVAR-KNR     PIC S9(15)V USAGE COMP-3.
           10 LTTLSTERHPROZ        PIC S9(2)V9(1) USAGE COMP-3.
           10 LTTBUZBERUFGRP-KTY   PIC S9(4) USAGE COMP.
           10 LTTBUZBERUFGRP-KNR   PIC S9(15)V USAGE COMP-3.
      ************************** ENDE-DCLGEN *************************


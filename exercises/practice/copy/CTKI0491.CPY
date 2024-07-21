      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22/02/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0491.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0491.CPv  $
      *
      *   Rev 2.2   Dec 15 2020 15:16:32   t13ae31
      *AKI01812: G21 Leben Basis Viridium DB2
      *
      *   Rev 2.1   Apr 13 2018 10:15:10   t13ex19
      *AKI01570: FW - Generali-Partnergesellschaften - Basisdaten-V
      *ertrag
      *
      *   Rev 2.0   Feb 22 2018 15:41:55   T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE GEN_VERTRAG TABLE
CCC004*    ( GENVGESELLSCHID                CHAR(2) NOT NULL,
CCC004*      GENVVNR                        CHAR(15) NOT NULL,
CCC004*      GENVEZPNR                      SMALLINT NOT NULL,
CCC004*      GENVVERARB_KZN                 CHAR(1) NOT NULL,
CCC004*      GENVAENDKZ                     CHAR(1) NOT NULL,
CCC004*      GENVVBNR                       INTEGER NOT NULL,
CCC004*      GENVBEGINN                     CHAR(8) NOT NULL,
CCC004*      GENVABLAUF                     CHAR(8) NOT NULL,
CCC004*      GENVZUSTAND                    SMALLINT NOT NULL,
CCC004*      GENVBEITRAG                    DECIMAL(10, 2) NOT NULL,
CCC004*      GENVNETTOBEITRAG               DECIMAL(10, 2) NOT NULL,
CCC004*      GENVZAHLWEISE                  CHAR(2) NOT NULL,
CCC004*      GENVSPARTE                     CHAR(3) NOT NULL,
CCC004*      GENVPRODUKT                    CHAR(50) NOT NULL,
CCC004*      GENVTARIF                      CHAR(50) NOT NULL,
CCC004*      GENVSUMME                      DECIMAL(10, 2) NOT NULL,
CCC004*      GENVAMTLKENNZ                  CHAR(12) NOT NULL,
CCC004*      GENVWAGNISKZ                   SMALLINT NOT NULL,
CCC004*      GENVGUTHABEN                   DECIMAL(10, 2) NOT NULL,
CCC004*      GENVAKTWERT                    DECIMAL(10, 2) NOT NULL,
CCC004*      GENVRENTENBEZUG                CHAR(1) NOT NULL,
CCC004*      GENVVORRENTE                   DECIMAL(6, 0) NOT NULL,
CCC004*      GENVGARRENTE                   DECIMAL(6, 0) NOT NULL,
CCC004*      GENVSTAND                      CHAR(8) NOT NULL,
CCC004*      GENVGZWEIG                     CHAR(2) NOT NULL,
CCC004*      GENVFLAECHE                    SMALLINT NOT NULL,
CCC004*      GENVPRNUMMER                   INTEGER NOT NULL,
CCC004*      GENVMIGKZN                     CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GEN_VERTRAG                 *
      ******************************************************************
       01  GEN-VERTRAG-GESAMT.
        05 NI-GEN-VERTRAG-TAB.
           10 NI-GEN-VERTRAG-ELE PIC S9(04) COMP OCCURS 28 TIMES.
        05 NI-GEN-VERTRAG REDEFINES NI-GEN-VERTRAG-TAB.
           10 NI-GENVGESELLSCHID    PIC S9(4) COMP.
           10 NI-GENVVNR            PIC S9(4) COMP.
           10 NI-GENVEZPNR          PIC S9(4) COMP.
           10 NI-GENVVERARB-KZN     PIC S9(4) COMP.
           10 NI-GENVAENDKZ         PIC S9(4) COMP.
           10 NI-GENVVBNR           PIC S9(4) COMP.
           10 NI-GENVBEGINN         PIC S9(4) COMP.
           10 NI-GENVABLAUF         PIC S9(4) COMP.
           10 NI-GENVZUSTAND        PIC S9(4) COMP.
           10 NI-GENVBEITRAG        PIC S9(4) COMP.
           10 NI-GENVNETTOBEITRAG   PIC S9(4) COMP.
           10 NI-GENVZAHLWEISE      PIC S9(4) COMP.
           10 NI-GENVSPARTE         PIC S9(4) COMP.
           10 NI-GENVPRODUKT        PIC S9(4) COMP.
           10 NI-GENVTARIF          PIC S9(4) COMP.
           10 NI-GENVSUMME          PIC S9(4) COMP.
           10 NI-GENVAMTLKENNZ      PIC S9(4) COMP.
           10 NI-GENVWAGNISKZ       PIC S9(4) COMP.
           10 NI-GENVGUTHABEN       PIC S9(4) COMP.
           10 NI-GENVAKTWERT        PIC S9(4) COMP.
           10 NI-GENVRENTENBEZUG    PIC S9(4) COMP.
           10 NI-GENVVORRENTE       PIC S9(4) COMP.
           10 NI-GENVGARRENTE       PIC S9(4) COMP.
           10 NI-GENVSTAND          PIC S9(4) COMP.
           10 NI-GENVGZWEIG         PIC S9(4) COMP.
           10 NI-GENVFLAECHE        PIC S9(4) COMP.
           10 NI-GENVPRNUMMER       PIC S9(4) COMP.
           10 NI-GENVMIGKZN         PIC S9(4) COMP.
        05 DCL-GEN-VERTRAG.
           10 GENVGESELLSCHID      PIC X(2).
           10 GENVVNR              PIC X(15).
           10 GENVEZPNR            PIC S9(4) USAGE COMP.
           10 GENVVERARB-KZN       PIC X(1).
           10 GENVAENDKZ           PIC X(1).
           10 GENVVBNR             PIC S9(9) USAGE COMP.
           10 GENVBEGINN           PIC X(8).
           10 GENVABLAUF           PIC X(8).
           10 GENVZUSTAND          PIC S9(4) USAGE COMP.
           10 GENVBEITRAG          PIC S9(8)V9(2) USAGE COMP-3.
           10 GENVNETTOBEITRAG     PIC S9(8)V9(2) USAGE COMP-3.
           10 GENVZAHLWEISE        PIC X(2).
           10 GENVSPARTE           PIC X(3).
           10 GENVPRODUKT          PIC X(50).
           10 GENVTARIF            PIC X(50).
           10 GENVSUMME            PIC S9(8)V9(2) USAGE COMP-3.
           10 GENVAMTLKENNZ        PIC X(12).
           10 GENVWAGNISKZ         PIC S9(4) USAGE COMP.
           10 GENVGUTHABEN         PIC S9(8)V9(2) USAGE COMP-3.
           10 GENVAKTWERT          PIC S9(8)V9(2) USAGE COMP-3.
           10 GENVRENTENBEZUG      PIC X(1).
           10 GENVVORRENTE         PIC S9(6)V USAGE COMP-3.
           10 GENVGARRENTE         PIC S9(6)V USAGE COMP-3.
           10 GENVSTAND            PIC X(8).
           10 GENVGZWEIG           PIC X(2).
           10 GENVFLAECHE          PIC S9(4) USAGE COMP.
           10 GENVPRNUMMER         PIC S9(9) USAGE COMP.
           10 GENVMIGKZN           PIC X(1).
      ************************** ENDE-DCLGEN *************************


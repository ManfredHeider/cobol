      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.02.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0410.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0410.CPv  $
      *
      *   Rev 2.3   Sep 19 2016 07:41:54   t13ae31
      *AKI01460: SUH SMART-HOME OZP TCKI0410 Hausrat
      *
      *   Rev 2.1   Feb 07 2006 15:43:00   t13ex27
      *AKI00555: DB-Änderung OKI 02/2006
      *
      *   Rev 2.0   Feb 07 2006 12:48:38   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE HAUSRATVERTRAG
      ******************************************************************
CCC005*    EXEC SQL DECLARE HAUSRATVERTRAG TABLE
CCC004*    ( HRVVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      HRVVETLFDNR                    SMALLINT NOT NULL,
CCC004*      HRVEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      HRVRANSCHRIFTPLZ               CHAR(6) NOT NULL,
CCC004*      HRVRANSCHRIFTSTR               CHAR(40) NOT NULL,
CCC004*      HRVRANSCHRIFTORT               CHAR(30) NOT NULL,
CCC004*      HRVRANSCHRIFTLKZ               INTEGER NOT NULL,
CCC004*      HRVBEDINGUNG                   CHAR(16) NOT NULL,
CCC004*      HRVRANSCHRHAUSNR               CHAR(15) NOT NULL,
CCC004*      HRVKLAUSELTARIF                CHAR(80) NOT NULL,
CCC004*      HRVZUSATZINFO                  CHAR(100) NOT NULL,
CCC004*      HRVLEIFREIVONDAT               DATE,
CCC004*      HRVLEIFREIBISDAT               DATE,
CCC004*      HRVSELBSTBKZ                   CHAR(2) NOT NULL,
CCC004*      HRVSELBSTBPROZ                 DECIMAL(4, 2) NOT NULL,
CCC004*      HRVSELBSTBFEST                 INTEGER NOT NULL,
CCC004*      HRVSELBSTBTEXT                 CHAR(50) NOT NULL,
CCC004*      HRVNUTZUNG_KTY                 SMALLINT NOT NULL,
CCC004*      HRVNUTZUNG_KNR                 DECIMAL(15, 0) NOT NULL,
CCC004*      HRVVERSSUMME                   INTEGER NOT NULL,
CCC004*      HRVWOHNFLAECHE                 INTEGER NOT NULL,
CCC004*      HRVUNTVERSVERZKZ               CHAR(2) NOT NULL,
CCC004*      HRVGEBAEUTYP_KTY               SMALLINT NOT NULL,
CCC004*      HRVGEBAEUTYP_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      HRVEIVERHAEL_KTY               SMALLINT NOT NULL,
CCC004*      HRVEIVERHAEL_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      HRVTARIFZONE                   CHAR(4) NOT NULL,
CCC004*      HRVBAUARTKL_KTY                SMALLINT NOT NULL,
CCC004*      HRVBAUARTKL_KNR                DECIMAL(15, 0) NOT NULL,
CCC004*      HRVALARMANLAGEKZ               CHAR(2) NOT NULL,
CCC004*      HRVFEUERKZ                     CHAR(2) NOT NULL,
CCC004*      HRVSTURMKZ                     CHAR(2) NOT NULL,
CCC004*      HRVHAGELKZ                     CHAR(2) NOT NULL,
CCC004*      HRVEINBRDIEBSTKZ               CHAR(2) NOT NULL,
CCC004*      HRVGLASKZ                      CHAR(2) NOT NULL,
CCC004*      HRVLEITUNGSWASKZ               CHAR(2) NOT NULL,
CCC004*      HRVELEMSCHKZ                   CHAR(2) NOT NULL,
CCC004*      HRVBEZUGSPRAEMIE               DECIMAL(6, 3) NOT NULL,
CCC004*      HRVLEISTUNGSTEXT               CHAR(40) NOT NULL,
CCC004*      HRVSUMANPKLAUSEL               CHAR(32) NOT NULL,
CCC004*      HRVPRODUKT                     CHAR(30) NOT NULL,
CCC004*      HRVMAILINGAKTION               CHAR(70) NOT NULL,
CCC004*      HRVDIFFDECKKZ                  CHAR(2) NOT NULL,
CCC004*      HRVUMSTELLUNGDAT               DATE,
CCC004*      HRVGESELLSCHAFT                CHAR(50) NOT NULL,
CCC004*      HRVVERSNUMMER                  CHAR(40) NOT NULL,
CCC004*      HRVKUENDIGUNGKZ                CHAR(2) NOT NULL,
CCC004*      HRVZIELGRUPPE                  CHAR(30) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      HRVSMHPAKET                    CHAR(2),
CCC004*      HRVSMHDATUM                    DATE,
CCC004*      HRVSMHTRACKID                  CHAR(60),
CCC004*      HRVSMHCONTROLZ                 CHAR(60)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE HAUSRATVERTRAG              *
      ******************************************************************
       01  HAUSRATVERTRAG-GESAMT.
        05 NI-HAUSRATVERTRAG-TAB.
           10 NI-HAUSRATVERTRAG-ELE PIC S9(04) COMP OCCURS 53 TIMES.
        05 NI-HAUSRATVERTRAG REDEFINES NI-HAUSRATVERTRAG-TAB.
           10 NI-HRVVERTRAGSID      PIC S9(4) COMP.
           10 NI-HRVVETLFDNR        PIC S9(4) COMP.
           10 NI-HRVEINZVTGHOSTID   PIC S9(4) COMP.
           10 NI-HRVRANSCHRIFTPLZ   PIC S9(4) COMP.
           10 NI-HRVRANSCHRIFTSTR   PIC S9(4) COMP.
           10 NI-HRVRANSCHRIFTORT   PIC S9(4) COMP.
           10 NI-HRVRANSCHRIFTLKZ   PIC S9(4) COMP.
           10 NI-HRVBEDINGUNG       PIC S9(4) COMP.
           10 NI-HRVRANSCHRHAUSNR   PIC S9(4) COMP.
           10 NI-HRVKLAUSELTARIF    PIC S9(4) COMP.
           10 NI-HRVZUSATZINFO      PIC S9(4) COMP.
           10 NI-HRVLEIFREIVONDAT   PIC S9(4) COMP.
           10 NI-HRVLEIFREIBISDAT   PIC S9(4) COMP.
           10 NI-HRVSELBSTBKZ       PIC S9(4) COMP.
           10 NI-HRVSELBSTBPROZ     PIC S9(4) COMP.
           10 NI-HRVSELBSTBFEST     PIC S9(4) COMP.
           10 NI-HRVSELBSTBTEXT     PIC S9(4) COMP.
           10 NI-HRVNUTZUNG-KTY     PIC S9(4) COMP.
           10 NI-HRVNUTZUNG-KNR     PIC S9(4) COMP.
           10 NI-HRVVERSSUMME       PIC S9(4) COMP.
           10 NI-HRVWOHNFLAECHE     PIC S9(4) COMP.
           10 NI-HRVUNTVERSVERZKZ   PIC S9(4) COMP.
           10 NI-HRVGEBAEUTYP-KTY   PIC S9(4) COMP.
           10 NI-HRVGEBAEUTYP-KNR   PIC S9(4) COMP.
           10 NI-HRVEIVERHAEL-KTY   PIC S9(4) COMP.
           10 NI-HRVEIVERHAEL-KNR   PIC S9(4) COMP.
           10 NI-HRVTARIFZONE       PIC S9(4) COMP.
           10 NI-HRVBAUARTKL-KTY    PIC S9(4) COMP.
           10 NI-HRVBAUARTKL-KNR    PIC S9(4) COMP.
           10 NI-HRVALARMANLAGEKZ   PIC S9(4) COMP.
           10 NI-HRVFEUERKZ         PIC S9(4) COMP.
           10 NI-HRVSTURMKZ         PIC S9(4) COMP.
           10 NI-HRVHAGELKZ         PIC S9(4) COMP.
           10 NI-HRVEINBRDIEBSTKZ   PIC S9(4) COMP.
           10 NI-HRVGLASKZ          PIC S9(4) COMP.
           10 NI-HRVLEITUNGSWASKZ   PIC S9(4) COMP.
           10 NI-HRVELEMSCHKZ       PIC S9(4) COMP.
           10 NI-HRVBEZUGSPRAEMIE   PIC S9(4) COMP.
           10 NI-HRVLEISTUNGSTEXT   PIC S9(4) COMP.
           10 NI-HRVSUMANPKLAUSEL   PIC S9(4) COMP.
           10 NI-HRVPRODUKT         PIC S9(4) COMP.
           10 NI-HRVMAILINGAKTION   PIC S9(4) COMP.
           10 NI-HRVDIFFDECKKZ      PIC S9(4) COMP.
           10 NI-HRVUMSTELLUNGDAT   PIC S9(4) COMP.
           10 NI-HRVGESELLSCHAFT    PIC S9(4) COMP.
           10 NI-HRVVERSNUMMER      PIC S9(4) COMP.
           10 NI-HRVKUENDIGUNGKZ    PIC S9(4) COMP.
           10 NI-HRVZIELGRUPPE      PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-HRVSMHPAKET        PIC S9(4) COMP.
           10 NI-HRVSMHDATUM        PIC S9(4) COMP.
           10 NI-HRVSMHTRACKID      PIC S9(4) COMP.
           10 NI-HRVSMHCONTROLZ     PIC S9(4) COMP.
        05 DCL-HAUSRATVERTRAG.
           10 HRVVERTRAGSID        PIC X(13).
           10 HRVVETLFDNR          PIC S9(4) USAGE COMP.
           10 HRVEINZVTGHOSTID     PIC X(15).
           10 HRVRANSCHRIFTPLZ     PIC X(6).
           10 HRVRANSCHRIFTSTR     PIC X(40).
           10 HRVRANSCHRIFTORT     PIC X(30).
           10 HRVRANSCHRIFTLKZ     PIC S9(9) USAGE COMP.
           10 HRVBEDINGUNG         PIC X(16).
           10 HRVRANSCHRHAUSNR     PIC X(15).
           10 HRVKLAUSELTARIF      PIC X(80).
           10 HRVZUSATZINFO        PIC X(100).
           10 HRVLEIFREIVONDAT     PIC X(10).
           10 HRVLEIFREIBISDAT     PIC X(10).
           10 HRVSELBSTBKZ         PIC X(2).
           10 HRVSELBSTBPROZ       PIC S9(2)V9(2) USAGE COMP-3.
           10 HRVSELBSTBFEST       PIC S9(9) USAGE COMP.
           10 HRVSELBSTBTEXT       PIC X(50).
           10 HRVNUTZUNG-KTY       PIC S9(4) USAGE COMP.
           10 HRVNUTZUNG-KNR       PIC S9(15)V USAGE COMP-3.
           10 HRVVERSSUMME         PIC S9(9) USAGE COMP.
           10 HRVWOHNFLAECHE       PIC S9(9) USAGE COMP.
           10 HRVUNTVERSVERZKZ     PIC X(2).
           10 HRVGEBAEUTYP-KTY     PIC S9(4) USAGE COMP.
           10 HRVGEBAEUTYP-KNR     PIC S9(15)V USAGE COMP-3.
           10 HRVEIVERHAEL-KTY     PIC S9(4) USAGE COMP.
           10 HRVEIVERHAEL-KNR     PIC S9(15)V USAGE COMP-3.
           10 HRVTARIFZONE         PIC X(4).
           10 HRVBAUARTKL-KTY      PIC S9(4) USAGE COMP.
           10 HRVBAUARTKL-KNR      PIC S9(15)V USAGE COMP-3.
           10 HRVALARMANLAGEKZ     PIC X(2).
           10 HRVFEUERKZ           PIC X(2).
           10 HRVSTURMKZ           PIC X(2).
           10 HRVHAGELKZ           PIC X(2).
           10 HRVEINBRDIEBSTKZ     PIC X(2).
           10 HRVGLASKZ            PIC X(2).
           10 HRVLEITUNGSWASKZ     PIC X(2).
           10 HRVELEMSCHKZ         PIC X(2).
           10 HRVBEZUGSPRAEMIE     PIC S9(3)V9(3) USAGE COMP-3.
           10 HRVLEISTUNGSTEXT     PIC X(40).
           10 HRVSUMANPKLAUSEL     PIC X(32).
           10 HRVPRODUKT           PIC X(30).
           10 HRVMAILINGAKTION     PIC X(70).
           10 HRVDIFFDECKKZ        PIC X(2).
           10 HRVUMSTELLUNGDAT     PIC X(10).
           10 HRVGESELLSCHAFT      PIC X(50).
           10 HRVVERSNUMMER        PIC X(40).
           10 HRVKUENDIGUNGKZ      PIC X(2).
           10 HRVZIELGRUPPE        PIC X(30).
           10 LETZTEAENDERUNG      PIC X(26).
           10 HRVSMHPAKET          PIC X(2).
           10 HRVSMHDATUM          PIC X(10).
           10 HRVSMHTRACKID        PIC X(60).
           10 HRVSMHCONTROLZ       PIC X(60).
      ************************** ENDE-DCLGEN *************************


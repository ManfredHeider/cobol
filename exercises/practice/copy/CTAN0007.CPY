      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.07.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0007.CPY  $
      *    $Revision:   2.15  $
      *
      *    $Log:   L:/mf/CPY/CTAN0007.CPv  $
      *
      *   Rev 2.15   Sep 12 2018 09:02:12   t13ae34
      *AAN01379: AMLeben: Überarbeitung KIZ-Produkt
      *
      *   Rev 2.14   May 15 2018 08:25:32   t13ae34
      *AAN01368: AM-Leben: Neue ungezillmerte bAV-Tarife
      *
      *   Rev 2.13   Jun 03 2016 16:54:38   t13ae34
      *AAN01263: Produktpaket Arbeitskraftabsicherung
      *
      *   Rev 2.12   Jun 03 2016 14:13:34   t13ae34
      *AAN01263: Produktpaket Arbeitskraftabsicherung
      *
      *   Rev 2.11   May 28 2015 09:55:50   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.10   Apr 23 2015 13:26:14   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.9   Jun 26 2014 08:15:34   t13ae34
      *AAN01168: VERMÖGENS- & EINKOMMENSPLAN - VEP
      *
      *   Rev 2.8   Dec 09 2013 14:40:58   t13ae34
      *AAN01139: neuer Pensionskassentarif PRGV
      *
      *   Rev 2.7   Sep 30 2011 09:36:06   t13ae34
      *AAN01025: AMLeben - Einführung Impulsprodukt - VASP mit TZV
      *(PS 124)
      *
      *   Rev 2.6   Sep 13 2011 13:11:04   t13ae23
      *AAN01025: AMLeben - Einführung Impulsprodukt - VASP mit TZV
      *(PS 124)
      *
      *   Rev 2.5   Aug 25 2011 10:53:56   t13ae34
      *AAN01025: AMLeben - Einführung Impulsprodukt - VASP mit TZV
      *(PS 124)
      *
      *   Rev 2.4   Dec 10 2010 13:01:02   t13ae34
      *AAN00977: Anpassung der Schnittstellen und Ressourcen
      *
      *   Rev 2.3   Sep 03 2010 16:24:42   t13ae34
      *AAN00971: PDP Ergänzung Ablaufdatum u. Ablaufsumme
      *
      *   Rev 2.2   27 Dec 2007 11:11:42   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 20 AM-Leben
      *
      *   Rev 2.1   08 Sep 2006 09:42:06   T13AE23
      *AAN00685: elektr. Anträge AM-Leben: Umstellung auf "Header/B
      *lackbox"
      *
      *   Rev 2.0   19 Jul 2006 10:31:10   t13ae23
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: CTAN0007                                     *
      *    FUNKTION   : DCLGEN FÜR TABELLE TBAN0007 AM-LEBEN         *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN00730!28.12.07!2.3  !ALLFINANZ                  !RAHNER   *
      * AAN00977!08.12.10!2.4  !EU-TARIFE + XGDV AUF 4000  !HEIDER   *
      *         !        !     !+ XGDV AN TABELLENENDE     !         *
      * AAN01025!25.07.11!2.5  !VASP IMPULSPRODUKT         !HEIDER   *
      * AAN01139!06.12.13!2.8  !XGDV VERGRÖSSERN AUF 5000  !HEIDER   *
      * AAN01168!24.04.14!2.9  !VEP, XGDV -> 20.000        !HEIDER   *
      * AAN01211!10.03.15!2.10 !ESIGN                      !HEIDER   *
      * AAN01263!30.05.16!2.12 !ARBEITSKRAFTABSICHERUNG    !HEIDER   *
      * AAN01368!09.05.18!2.14 !BRGU                       !HEIDER   *
      * AAN01379!11.09.18!2.15 !SKN SKNE (NEUE KIZ)        !HEIDER   *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE T13AES.TBAN0007 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PZM_PIN                        INTEGER NOT NULL,
CCC004*      PZM_HVANTNUM                   CHAR(9) NOT NULL,
CCC004*      PZM_VDABVERM                   INTEGER NOT NULL,
CCC004*      PZM_VDABVERMPZ                 CHAR(1) NOT NULL,
CCC004*      PZM_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      PZM_KENNUNG                    CHAR(8) NOT NULL,
CCC004*      PZM_VDVMNR                     INTEGER NOT NULL,
CCC004*      PZM_VDVMNRPZ                   CHAR(1) NOT NULL,
CCC004*      PZM_VDVMVORNA                  CHAR(16) NOT NULL,
CCC004*      PZM_VDVMZUNA                   CHAR(21) NOT NULL,
CCC004*      PZM_VDDIREKT                   SMALLINT NOT NULL,
CCC004*      PZM_KD_BEST_ID                 DECIMAL(18, 0) NOT NULL,
CCC004*      PZM_PDVNANRED                  CHAR(2) NOT NULL,
CCC004*      PZM_PDVNVORNA                  CHAR(16) NOT NULL,
CCC004*      PZM_PDVNZUNA                   CHAR(33) NOT NULL,
CCC004*      PZM_PDVNGBDAT                  CHAR(10) NOT NULL,
CCC004*      PZM_PDVNPLZ                    CHAR(6) NOT NULL,
CCC004*      PZM_PDVNORT                    CHAR(30) NOT NULL,
CCC004*      PZM_PDVNLKZ                    CHAR(3) NOT NULL,
CCC004*      PZM_PDVNLAND                   CHAR(35) NOT NULL,
CCC004*      PZM_PDV1VORNA                  CHAR(16) NOT NULL,
CCC004*      PZM_PDV1ZUNA                   CHAR(21) NOT NULL,
CCC004*      PZM_PDV1GBDAT                  CHAR(10) NOT NULL,
CCC004*      PZM_PDV2GBDAT                  CHAR(10) NOT NULL,
CCC004*      PZM_HVTARIF                    CHAR(15) NOT NULL,
CCC004*      PZM_HVVBEGINN                  DATE NOT NULL,
CCC004*      PZM_HVN                        SMALLINT NOT NULL,
CCC004*      PZM_HVNS                       SMALLINT NOT NULL,
CCC004*      PZM_HV8ENDALT                  SMALLINT NOT NULL,
CCC004*      PZM_HVENDNS                    CHAR(10) NOT NULL,
CCC004*      PZM_HVBEGREN                   CHAR(10) NOT NULL,
CCC004*      PZM_HVBEITRAG                  DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_HVVSUMME                   DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_EZAHLW                     SMALLINT NOT NULL,
CCC004*      PZM_EUROKZ                     SMALLINT NOT NULL,
CCC004*      PZM_ZBE                        CHAR(1) NOT NULL,
CCC004*      PZM_DEPOT                      CHAR(1) NOT NULL,
CCC004*      PZM_DIVE                       CHAR(1) NOT NULL,
CCC004*      PZM_ASAMNR                     SMALLINT NOT NULL,
CCC004*      PZM_VDPRODKEY                  SMALLINT NOT NULL,
CCC004*      PZM_VDABLEIN                   DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_VDPRODKEYBUZI              SMALLINT NOT NULL,
CCC004*      PZM_VDABLEINBUZI               DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_BEITRAGBUZI                DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_VDPRODKEYBUZR              SMALLINT NOT NULL,
CCC004*      PZM_VDABLEINBUZR               DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_BEITRAGBUZR                DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_VDPRODKEYKIZ               SMALLINT NOT NULL,
CCC004*      PZM_VDABLEINKIZ                DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_BEITRAGKIZ                 DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_VDPRODKEYTZV               SMALLINT NOT NULL,
CCC004*      PZM_VDABLEINTZV                DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_BEITRAGTZV                 DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_VDPRODKEYHZV               SMALLINT NOT NULL,
CCC004*      PZM_VDABLEINHZV                DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_BEITRAGHZV                 DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_VDPRODKEYP                 SMALLINT NOT NULL,
CCC004*      PZM_VDABLEINP                  DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_BEITRAGP                   DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_VDPRODKEYPO                SMALLINT NOT NULL,
CCC004*      PZM_VDABLEINPO                 DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_BEITRAGPO                  DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_VERSION                    CHAR(5) NOT NULL,
CCC004*      PZM_VB_GES                     SMALLINT NOT NULL,
CCC004*      PZM_BEITRABUZI                 DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_BEITRABUZR                 DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_BEITRAKIZ                  DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_BEITRATZV                  DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_BEITRAHZV                  DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_BEITRAB                    DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_BEITRAPO                   DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_BEITRAHV                   DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_HVENDN                     CHAR(10) NOT NULL,
CCC004*      PZM_HVENDSUMME                 DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_VDPRODKEYEUZ               SMALLINT NOT NULL,
CCC004*      PZM_VDABLEINEUZ                DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_BEITRAGEUZ                 DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_VDPRODKEYEUR               SMALLINT NOT NULL,
CCC004*      PZM_VDABLEINEUR                DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_BEITRAGEUR                 DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_BEITRAEUZ                  DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_BEITRAEUR                  DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_XGDV                       VARCHAR(20000) NOT NULL,
CCC004*      PZM_VDPRODKEYPA                SMALLINT NOT NULL,
CCC004*      PZM_VDABLEINPA                 DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_BEITRAPA                   DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_BEITRAGPA                  DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_VDPRODKEY_VEPBP            SMALLINT NOT NULL,
CCC004*      PZM_VDABLEIN_VEPBP             DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_ESIGN                      CHAR(1) NOT NULL,
CCC004*      PZM_VDPRODKEYSK                SMALLINT NOT NULL,
CCC004*      PZM_VDABLEINSK                 DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_BEITRASK                   DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_BEITRAGSK                  DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_VDPSBUZI_VEPBP             SMALLINT NOT NULL,
CCC004*      PZM_VDEHBUZI_VEPBP             DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_VDPSBUZR_VEPBP             SMALLINT NOT NULL,
CCC004*      PZM_VDEHBUZR_VEPBP             DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_VDPRODKEYBO                SMALLINT NOT NULL,
CCC004*      PZM_VDABLEINBO                 DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_BEITRAGBO                  DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_VDPRODKEYVTZV              SMALLINT NOT NULL,
CCC004*      PZM_VDABLEINVTZV               DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_BEITRAGVTZV                DECIMAL(11, 2) NOT NULL,
CCC004*      PZM_VDPRODKEYVBUZ              SMALLINT NOT NULL,
CCC004*      PZM_VDABLEINVBUZ               DECIMAL(7, 1) NOT NULL,
CCC004*      PZM_BEITRAGVBUZ                DECIMAL(11, 2) NOT NULL

CCC003*) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AES.TBAN0007                    *
      ******************************************************************
       01  DCLTBAN0007.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PZM-PIN              PIC S9(9) USAGE COMP.
           10 PZM-HVANTNUM         PIC X(9).
           10 PZM-VDABVERM         PIC S9(9) USAGE COMP.
           10 PZM-VDABVERMPZ       PIC X(1).
           10 PZM-LAST-UPDATE      PIC X(26).
           10 PZM-KENNUNG          PIC X(8).
           10 PZM-VDVMNR           PIC S9(9) USAGE COMP.
           10 PZM-VDVMNRPZ         PIC X(1).
           10 PZM-VDVMVORNA        PIC X(16).
           10 PZM-VDVMZUNA         PIC X(21).
           10 PZM-VDDIREKT         PIC S9(4) USAGE COMP.
           10 PZM-KD-BEST-ID       PIC S9(18)V USAGE COMP-3.
           10 PZM-PDVNANRED        PIC X(2).
           10 PZM-PDVNVORNA        PIC X(16).
           10 PZM-PDVNZUNA         PIC X(33).
           10 PZM-PDVNGBDAT        PIC X(10).
           10 PZM-PDVNPLZ          PIC X(6).
           10 PZM-PDVNORT          PIC X(30).
           10 PZM-PDVNLKZ          PIC X(3).
           10 PZM-PDVNLAND         PIC X(35).
           10 PZM-PDV1VORNA        PIC X(16).
           10 PZM-PDV1ZUNA         PIC X(21).
           10 PZM-PDV1GBDAT        PIC X(10).
           10 PZM-PDV2GBDAT        PIC X(10).
           10 PZM-HVTARIF          PIC X(15).
           10 PZM-HVVBEGINN        PIC X(10).
           10 PZM-HVN              PIC S9(4) USAGE COMP.
           10 PZM-HVNS             PIC S9(4) USAGE COMP.
           10 PZM-HV8ENDALT        PIC S9(4) USAGE COMP.
           10 PZM-HVENDNS          PIC X(10).
           10 PZM-HVBEGREN         PIC X(10).
           10 PZM-HVBEITRAG        PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-HVVSUMME         PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-EZAHLW           PIC S9(4) USAGE COMP.
           10 PZM-EUROKZ           PIC S9(4) USAGE COMP.
           10 PZM-ZBE              PIC X(1).
           10 PZM-DEPOT            PIC X(1).
           10 PZM-DIVE             PIC X(1).
           10 PZM-ASAMNR           PIC S9(4) USAGE COMP.
           10 PZM-VDPRODKEY        PIC S9(4) USAGE COMP.
           10 PZM-VDABLEIN         PIC S9(6)V9(1) USAGE COMP-3.
           10 PZM-VDPRODKEYBUZI    PIC S9(4) USAGE COMP.
           10 PZM-VDABLEINBUZI     PIC S9(6)V9(1) USAGE COMP-3.
           10 PZM-BEITRAGBUZI      PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-VDPRODKEYBUZR    PIC S9(4) USAGE COMP.
           10 PZM-VDABLEINBUZR     PIC S9(6)V9(1) USAGE COMP-3.
           10 PZM-BEITRAGBUZR      PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-VDPRODKEYKIZ     PIC S9(4) USAGE COMP.
           10 PZM-VDABLEINKIZ      PIC S9(6)V9(1) USAGE COMP-3.
           10 PZM-BEITRAGKIZ       PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-VDPRODKEYTZV     PIC S9(4) USAGE COMP.
           10 PZM-VDABLEINTZV      PIC S9(6)V9(1) USAGE COMP-3.
           10 PZM-BEITRAGTZV       PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-VDPRODKEYHZV     PIC S9(4) USAGE COMP.
           10 PZM-VDABLEINHZV      PIC S9(6)V9(1) USAGE COMP-3.
           10 PZM-BEITRAGHZV       PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-VDPRODKEYP       PIC S9(4) USAGE COMP.
           10 PZM-VDABLEINP        PIC S9(6)V9(1) USAGE COMP-3.
           10 PZM-BEITRAGP         PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-VDPRODKEYPO      PIC S9(4) USAGE COMP.
           10 PZM-VDABLEINPO       PIC S9(6)V9(1) USAGE COMP-3.
           10 PZM-BEITRAGPO        PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-VERSION          PIC X(5).
           10 PZM-VB-GES           PIC S9(4) USAGE COMP.
           10 PZM-BEITRABUZI       PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-BEITRABUZR       PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-BEITRAKIZ        PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-BEITRATZV        PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-BEITRAHZV        PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-BEITRAB          PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-BEITRAPO         PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-BEITRAHV         PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-HVENDN           PIC X(10).
           10 PZM-HVENDSUMME       PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-VDPRODKEYEUZ     PIC S9(4) USAGE COMP.
           10 PZM-VDABLEINEUZ      PIC S9(6)V9(1) USAGE COMP-3.
           10 PZM-BEITRAGEUZ       PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-VDPRODKEYEUR     PIC S9(4) USAGE COMP.
           10 PZM-VDABLEINEUR      PIC S9(6)V9(1) USAGE COMP-3.
           10 PZM-BEITRAGEUR       PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-BEITRAEUZ        PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-BEITRAEUR        PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-XGDV.
              49 PZM-XGDV-LEN      PIC S9(4) USAGE COMP.
              49 PZM-XGDV-TEXT     PIC X(20000).
           10 PZM-VDPRODKEYPA      PIC S9(4) USAGE COMP.
           10 PZM-VDABLEINPA       PIC S9(6)V9(1) USAGE COMP-3.
           10 PZM-BEITRAPA         PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-BEITRAGPA        PIC S9(9)V9(2) USAGE COMP-3.
           10 PZM-VDPRODKEY-VEPBP  PIC S9(4) USAGE COMP.
           10 PZM-VDABLEIN-VEPBP   PIC S9(6)V9(1) USAGE COMP-3.
           10 PZM-ESIGN            PIC X(1).
300516     10 PZM-VDPRODKEYSK      PIC S9(4) USAGE COMP.
300516     10 PZM-VDABLEINSK       PIC S9(6)V9(1) USAGE COMP-3.
300516     10 PZM-BEITRASK         PIC S9(9)V9(2) USAGE COMP-3.
300516     10 PZM-BEITRAGSK        PIC S9(9)V9(2) USAGE COMP-3.
090518     10 PZM-VDPSBUZI-VEPBP   PIC S9(4) USAGE COMP.
090518     10 PZM-VDEHBUZI-VEPBP   PIC S9(6)V9(1) USAGE COMP-3.
090518     10 PZM-VDPSBUZR-VEPBP   PIC S9(4) USAGE COMP.
090518     10 PZM-VDEHBUZR-VEPBP   PIC S9(6)V9(1) USAGE COMP-3.
110918     10 PZM-VDPRODKEYBO      PIC S9(4) USAGE COMP.
110918     10 PZM-VDABLEINBO       PIC S9(6)V9(1) USAGE COMP-3.
110918     10 PZM-BEITRAGBO        PIC S9(9)V9(2) USAGE COMP-3.
110918     10 PZM-VDPRODKEYVTZV    PIC S9(4) USAGE COMP.
110918     10 PZM-VDABLEINVTZV     PIC S9(6)V9(1) USAGE COMP-3.
110918     10 PZM-BEITRAGVTZV      PIC S9(9)V9(2) USAGE COMP-3.
110918     10 PZM-VDPRODKEYVBUZ    PIC S9(4) USAGE COMP.
110918     10 PZM-VDABLEINVBUZ     PIC S9(6)V9(1) USAGE COMP-3.
110918     10 PZM-BEITRAGVBUZ      PIC S9(9)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 108     *
      ******************************************************************


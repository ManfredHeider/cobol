      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0418.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0418.CPv  $
      *
      *   Rev 2.2   Mar 18 2015 15:08:28   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Standard-OZP 
      *für TCKI0418
      *
      *   Rev 2.0   27 Sep 2007 12:01:16   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE VGVVERTRAG
      ******************************************************************
CCC005*    EXEC SQL DECLARE VGVVERTRAG TABLE
CCC004*    ( VGVVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VGVVETLFDNR                    SMALLINT NOT NULL,
CCC004*      VGVEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      VGVRANSCHRIFTPLZ               CHAR(6) NOT NULL,
CCC004*      VGVRANSCHRSTR                  CHAR(40) NOT NULL,
CCC004*      VGVRANSCHRHAUSNR               CHAR(15) NOT NULL,
CCC004*      VGVRANSCHRIFTORT               CHAR(30) NOT NULL,
CCC004*      VGVRANSCHRIFTLKZ               INTEGER NOT NULL,
CCC004*      VGVBEDINGUNG                   CHAR(16) NOT NULL,
CCC004*      VGVRISIKO                      CHAR(50) NOT NULL,
CCC004*      VGVKLAUSELTARIF                CHAR(80) NOT NULL,
CCC004*      VGVBEILAGEN                    CHAR(19) NOT NULL,
CCC004*      VGVZUSATZINFO                  CHAR(100) NOT NULL,
CCC004*      VGVFEUERKZ                     CHAR(2) NOT NULL,
CCC004*      VGVLEITUNGSWASKZ               CHAR(2) NOT NULL,
CCC004*      VGVSTURMKZ                     CHAR(2) NOT NULL,
CCC004*      VGVHAGELKZ                     CHAR(2) NOT NULL,
CCC004*      VGVELEMSCHLKZ                  CHAR(2) NOT NULL,
CCC004*      VGVGEBAUENUTZUNG               CHAR(26) NOT NULL,
CCC004*      VGVGEWERBFLPROZ                SMALLINT NOT NULL,
CCC004*      VGVBAUJAHR                     DECIMAL(8, 0) NOT NULL,
CCC004*      VGVBAUARTKL_KTY                SMALLINT NOT NULL,
CCC004*      VGVBAUARTKL_KNR                DECIMAL(15, 0) NOT NULL,
CCC004*      VGVROHBAUBISDAT                DATE,
CCC004*      VGVLEISARTLB_KTY               SMALLINT NOT NULL,
CCC004*      VGVLEISARTLB_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      VGVUNTERVERSVERZ               CHAR(2) NOT NULL,
CCC004*      VGVGRUNDFUERUVZ                CHAR(40) NOT NULL,
CCC004*      VGVVERSSUM1914                 INTEGER NOT NULL,
CCC004*      VGVSTAFFPRAEMKZ                CHAR(2) NOT NULL,
CCC004*      VGVZLEITWASSER                 SMALLINT NOT NULL,
CCC004*      VGVZSTURM                      SMALLINT NOT NULL,
CCC004*      VGVZERDBEBEN                   SMALLINT NOT NULL,
CCC004*      VGVZELEMENTSCH                 SMALLINT NOT NULL,
CCC004*      VGVMINDBEITRAGKZ               CHAR(2) NOT NULL,
CCC004*      VGVLEISTFREIVDAT               DATE,
CCC004*      VGVLEISTFREIBDAT               DATE,
CCC004*      VGVSELBSTBTEXT                 CHAR(14) NOT NULL,
CCC004*      VGVPRMSATZWAGNIS               DECIMAL(9, 7) NOT NULL,
CCC004*      VGVPRMSATZGEFAHR               DECIMAL(6, 4) NOT NULL,
CCC004*      VGVANZVSCHGEFAHR               SMALLINT NOT NULL,
CCC004*      VGVGEFKLASSE                   CHAR(1) NOT NULL,
CCC004*      VGVGKLASFLAEPROZ               CHAR(20) NOT NULL,
CCC004*      VGVPRODUKT                     CHAR(30) NOT NULL,
CCC004*      VGVGEBAUEDEART                 CHAR(50) NOT NULL,
CCC004*      VGVBETRIEBSARTLB               CHAR(50) NOT NULL,
CCC004*      VGVMODELL                      CHAR(18) NOT NULL,
CCC004*      VGVMONOPOLKZ                   CHAR(2) NOT NULL,
CCC004*      VGVAUSLRISIKOKZ                CHAR(2) NOT NULL,
CCC004*      VGVGEBBEZ                      CHAR(80) NOT NULL,
CCC004*      VGVROHBAUVER_KTY               SMALLINT NOT NULL,
CCC004*      VGVROHBAUVER_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      VGVWOHNFLAECHE                 INTEGER NOT NULL,
CCC004*      VGVANZGESCHOSSE                SMALLINT NOT NULL,
CCC004*      VGVKELLERVORHKZ                CHAR(2) NOT NULL,
CCC004*      VGVSCHWBADGEBKZ                CHAR(2) NOT NULL,
CCC004*      VGVNGEBENTSCHGRZ               INTEGER NOT NULL,
CCC004*      VGVNEBENGEBAEUDE               CHAR(80) NOT NULL,
CCC004*      VGVSONSTIGES                   CHAR(80) NOT NULL,
CCC004*      VGVGARAGENCARPOR               SMALLINT NOT NULL,
CCC004*      VGVZUEBERSCHWEM                SMALLINT NOT NULL,
CCC004*      VGVMAILINGAKTION               CHAR(70) NOT NULL,
CCC004*      VGVDIFFERDECKKZ                CHAR(2) NOT NULL,
CCC004*      VGVUMSTELLDAT                  DATE,
CCC004*      VGVGESELLSCHAFT                CHAR(50) NOT NULL,
CCC004*      VGVVERSNUMMER                  CHAR(40) NOT NULL,
CCC004*      VGVKUENDIGUNGKZ                CHAR(2) NOT NULL,
CCC004*      VGVELEDIFVERSKZ                CHAR(2) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      VGVSNUTZFLAECHE                SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VGVVERTRAG                  *
      ******************************************************************
       01  VGVVERTRAG-GESAMT.
        05 NI-VGVVERTRAG-TAB.
           10 NI-VGVVERTRAG-ELE PIC S9(04) COMP OCCURS 70 TIMES.
        05 NI-VGVVERTRAG REDEFINES NI-VGVVERTRAG-TAB.
           10 NI-VGVVERTRAGSID      PIC S9(4) COMP.
           10 NI-VGVVETLFDNR        PIC S9(4) COMP.
           10 NI-VGVEINZVTGHOSTID   PIC S9(4) COMP.
           10 NI-VGVRANSCHRIFTPLZ   PIC S9(4) COMP.
           10 NI-VGVRANSCHRSTR      PIC S9(4) COMP.
           10 NI-VGVRANSCHRHAUSNR   PIC S9(4) COMP.
           10 NI-VGVRANSCHRIFTORT   PIC S9(4) COMP.
           10 NI-VGVRANSCHRIFTLKZ   PIC S9(4) COMP.
           10 NI-VGVBEDINGUNG       PIC S9(4) COMP.
           10 NI-VGVRISIKO          PIC S9(4) COMP.
           10 NI-VGVKLAUSELTARIF    PIC S9(4) COMP.
           10 NI-VGVBEILAGEN        PIC S9(4) COMP.
           10 NI-VGVZUSATZINFO      PIC S9(4) COMP.
           10 NI-VGVFEUERKZ         PIC S9(4) COMP.
           10 NI-VGVLEITUNGSWASKZ   PIC S9(4) COMP.
           10 NI-VGVSTURMKZ         PIC S9(4) COMP.
           10 NI-VGVHAGELKZ         PIC S9(4) COMP.
           10 NI-VGVELEMSCHLKZ      PIC S9(4) COMP.
           10 NI-VGVGEBAUENUTZUNG   PIC S9(4) COMP.
           10 NI-VGVGEWERBFLPROZ    PIC S9(4) COMP.
           10 NI-VGVBAUJAHR         PIC S9(4) COMP.
           10 NI-VGVBAUARTKL-KTY    PIC S9(4) COMP.
           10 NI-VGVBAUARTKL-KNR    PIC S9(4) COMP.
           10 NI-VGVROHBAUBISDAT    PIC S9(4) COMP.
           10 NI-VGVLEISARTLB-KTY   PIC S9(4) COMP.
           10 NI-VGVLEISARTLB-KNR   PIC S9(4) COMP.
           10 NI-VGVUNTERVERSVERZ   PIC S9(4) COMP.
           10 NI-VGVGRUNDFUERUVZ    PIC S9(4) COMP.
           10 NI-VGVVERSSUM1914     PIC S9(4) COMP.
           10 NI-VGVSTAFFPRAEMKZ    PIC S9(4) COMP.
           10 NI-VGVZLEITWASSER     PIC S9(4) COMP.
           10 NI-VGVZSTURM          PIC S9(4) COMP.
           10 NI-VGVZERDBEBEN       PIC S9(4) COMP.
           10 NI-VGVZELEMENTSCH     PIC S9(4) COMP.
           10 NI-VGVMINDBEITRAGKZ   PIC S9(4) COMP.
           10 NI-VGVLEISTFREIVDAT   PIC S9(4) COMP.
           10 NI-VGVLEISTFREIBDAT   PIC S9(4) COMP.
           10 NI-VGVSELBSTBTEXT     PIC S9(4) COMP.
           10 NI-VGVPRMSATZWAGNIS   PIC S9(4) COMP.
           10 NI-VGVPRMSATZGEFAHR   PIC S9(4) COMP.
           10 NI-VGVANZVSCHGEFAHR   PIC S9(4) COMP.
           10 NI-VGVGEFKLASSE       PIC S9(4) COMP.
           10 NI-VGVGKLASFLAEPROZ   PIC S9(4) COMP.
           10 NI-VGVPRODUKT         PIC S9(4) COMP.
           10 NI-VGVGEBAUEDEART     PIC S9(4) COMP.
           10 NI-VGVBETRIEBSARTLB   PIC S9(4) COMP.
           10 NI-VGVMODELL          PIC S9(4) COMP.
           10 NI-VGVMONOPOLKZ       PIC S9(4) COMP.
           10 NI-VGVAUSLRISIKOKZ    PIC S9(4) COMP.
           10 NI-VGVGEBBEZ          PIC S9(4) COMP.
           10 NI-VGVROHBAUVER-KTY   PIC S9(4) COMP.
           10 NI-VGVROHBAUVER-KNR   PIC S9(4) COMP.
           10 NI-VGVWOHNFLAECHE     PIC S9(4) COMP.
           10 NI-VGVANZGESCHOSSE    PIC S9(4) COMP.
           10 NI-VGVKELLERVORHKZ    PIC S9(4) COMP.
           10 NI-VGVSCHWBADGEBKZ    PIC S9(4) COMP.
           10 NI-VGVNGEBENTSCHGRZ   PIC S9(4) COMP.
           10 NI-VGVNEBENGEBAEUDE   PIC S9(4) COMP.
           10 NI-VGVSONSTIGES       PIC S9(4) COMP.
           10 NI-VGVGARAGENCARPOR   PIC S9(4) COMP.
           10 NI-VGVZUEBERSCHWEM    PIC S9(4) COMP.
           10 NI-VGVMAILINGAKTION   PIC S9(4) COMP.
           10 NI-VGVDIFFERDECKKZ    PIC S9(4) COMP.
           10 NI-VGVUMSTELLDAT      PIC S9(4) COMP.
           10 NI-VGVGESELLSCHAFT    PIC S9(4) COMP.
           10 NI-VGVVERSNUMMER      PIC S9(4) COMP.
           10 NI-VGVKUENDIGUNGKZ    PIC S9(4) COMP.
           10 NI-VGVELEDIFVERSKZ    PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-VGVSNUTZFLAECHE    PIC S9(4) COMP.
        05 DCL-VGVVERTRAG.
           10 VGVVERTRAGSID        PIC X(13).
           10 VGVVETLFDNR          PIC S9(4) USAGE COMP.
           10 VGVEINZVTGHOSTID     PIC X(15).
           10 VGVRANSCHRIFTPLZ     PIC X(6).
           10 VGVRANSCHRSTR        PIC X(40).
           10 VGVRANSCHRHAUSNR     PIC X(15).
           10 VGVRANSCHRIFTORT     PIC X(30).
           10 VGVRANSCHRIFTLKZ     PIC S9(9) USAGE COMP.
           10 VGVBEDINGUNG         PIC X(16).
           10 VGVRISIKO            PIC X(50).
           10 VGVKLAUSELTARIF      PIC X(80).
           10 VGVBEILAGEN          PIC X(19).
           10 VGVZUSATZINFO        PIC X(100).
           10 VGVFEUERKZ           PIC X(2).
           10 VGVLEITUNGSWASKZ     PIC X(2).
           10 VGVSTURMKZ           PIC X(2).
           10 VGVHAGELKZ           PIC X(2).
           10 VGVELEMSCHLKZ        PIC X(2).
           10 VGVGEBAUENUTZUNG     PIC X(26).
           10 VGVGEWERBFLPROZ      PIC S9(4) USAGE COMP.
           10 VGVBAUJAHR           PIC S9(8)V USAGE COMP-3.
           10 VGVBAUARTKL-KTY      PIC S9(4) USAGE COMP.
           10 VGVBAUARTKL-KNR      PIC S9(15)V USAGE COMP-3.
           10 VGVROHBAUBISDAT      PIC X(10).
           10 VGVLEISARTLB-KTY     PIC S9(4) USAGE COMP.
           10 VGVLEISARTLB-KNR     PIC S9(15)V USAGE COMP-3.
           10 VGVUNTERVERSVERZ     PIC X(2).
           10 VGVGRUNDFUERUVZ      PIC X(40).
           10 VGVVERSSUM1914       PIC S9(9) USAGE COMP.
           10 VGVSTAFFPRAEMKZ      PIC X(2).
           10 VGVZLEITWASSER       PIC S9(4) USAGE COMP.
           10 VGVZSTURM            PIC S9(4) USAGE COMP.
           10 VGVZERDBEBEN         PIC S9(4) USAGE COMP.
           10 VGVZELEMENTSCH       PIC S9(4) USAGE COMP.
           10 VGVMINDBEITRAGKZ     PIC X(2).
           10 VGVLEISTFREIVDAT     PIC X(10).
           10 VGVLEISTFREIBDAT     PIC X(10).
           10 VGVSELBSTBTEXT       PIC X(14).
           10 VGVPRMSATZWAGNIS     PIC S9(2)V9(7) USAGE COMP-3.
           10 VGVPRMSATZGEFAHR     PIC S9(2)V9(4) USAGE COMP-3.
           10 VGVANZVSCHGEFAHR     PIC S9(4) USAGE COMP.
           10 VGVGEFKLASSE         PIC X(1).
           10 VGVGKLASFLAEPROZ     PIC X(20).
           10 VGVPRODUKT           PIC X(30).
           10 VGVGEBAUEDEART       PIC X(50).
           10 VGVBETRIEBSARTLB     PIC X(50).
           10 VGVMODELL            PIC X(18).
           10 VGVMONOPOLKZ         PIC X(2).
           10 VGVAUSLRISIKOKZ      PIC X(2).
           10 VGVGEBBEZ            PIC X(80).
           10 VGVROHBAUVER-KTY     PIC S9(4) USAGE COMP.
           10 VGVROHBAUVER-KNR     PIC S9(15)V USAGE COMP-3.
           10 VGVWOHNFLAECHE       PIC S9(9) USAGE COMP.
           10 VGVANZGESCHOSSE      PIC S9(4) USAGE COMP.
           10 VGVKELLERVORHKZ      PIC X(2).
           10 VGVSCHWBADGEBKZ      PIC X(2).
           10 VGVNGEBENTSCHGRZ     PIC S9(9) USAGE COMP.
           10 VGVNEBENGEBAEUDE     PIC X(80).
           10 VGVSONSTIGES         PIC X(80).
           10 VGVGARAGENCARPOR     PIC S9(4) USAGE COMP.
           10 VGVZUEBERSCHWEM      PIC S9(4) USAGE COMP.
           10 VGVMAILINGAKTION     PIC X(70).
           10 VGVDIFFERDECKKZ      PIC X(2).
           10 VGVUMSTELLDAT        PIC X(10).
           10 VGVGESELLSCHAFT      PIC X(50).
           10 VGVVERSNUMMER        PIC X(40).
           10 VGVKUENDIGUNGKZ      PIC X(2).
           10 VGVELEDIFVERSKZ      PIC X(2).
           10 LETZTEAENDERUNG      PIC X(26).
           10 VGVSNUTZFLAECHE      PIC S9(4) USAGE COMP.
      ************************** ENDE-DCLGEN *************************


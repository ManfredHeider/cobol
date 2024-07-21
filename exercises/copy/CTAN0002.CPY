      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0002.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/CTAN0002.CPv  $
      *
      *   Rev 2.14   19 May 2006 15:06:40   t13ae23
      *AAN00666: Erweiterung der Produktpalette um weitere Rürup-Ta
      *rife
      *
      *   Rev 2.13   10 May 2006 17:03:14   T13AE23
      *AAN00666: Erweiterung der Produktpalette um weitere Rürup-Ta
      *rife
      *
      *   Rev 2.12   Nov 14 2005 15:09:24   t13ae34
      *AAN00626: Einführung des Tarifes FRBZ
      *
      *   Rev 2.11   May 23 2005 14:10:16   t13ae34
      *AAN00583: Anpassung AML DFÜ-SST zur Auswahl 4. Fonds 
      *
      *   Rev 2.10   Oct 28 2004 14:33:42   t13ae34
      *AAN00531: Neue Tariflandschaft AML 2005 im eingereichten Ges
      *chäft 
      *
      *   Rev 2.9   Jun 08 2004 17:46:00   t13ae34
      *Neu Selbst. Kinderinvaliditätsversicherung und Kinderin-
      *
      *   Rev 2.8   Jun 01 2004 11:53:14   t13ae34
      *Neu Selbst. Kinderinvaliditätsversicherung und Kinderin-
      *
      *   Rev 2.7   Nov 19 2003 12:59:40   t13ae34
      *AAN00471 - Änderung SST PZM AML
      *
      *   Rev 2.6   Mar 19 2003 11:27:00   t13ae34
      *AAN00426 - AML 2.Produktpaket Risiko -> Korrektur
      *
      *   Rev 2.5   Feb 17 2003 16:56:40   t13ae34
      *AAN00426 - Anpassung AML PZM 2.Produktpaket Risiko
      *
      *   Rev 2.4   Nov 09 2001 15:56:12   t13ae34
      *AAN00356 - PZM-DFÜ Zulagenvertrag+Nettoverfahren
      *
      *   Rev 2.3   30 Apr 2001 09:08:32   T13AE34
      *FAN00302-ZTAN0002 mit Fkt. Insert
      *
      *   Rev 2.2   05 Apr 2001 11:20:12   T13AE34
      *AAN00268-PZM mit Riester
      *
      *   Rev 2.1   07 Mar 2001 09:54:22   T13AE34
      *AAN00268-PZM Antrag
      *
      *   Rev 2.0   Feb 27 1999 11:05:12   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   09 Sep 1997 09:13:24   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*

CCC005*    EXEC SQL DECLARE T13AES.TBAN0002 TABLE
CCC004*    (
CCC004*       PGEB_GES                       SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_PIN                        INT
CCC004*                                      NOT NULL,
CCC004*       PZM_EVNVT1                     CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_EVNVT2                     CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNANRED                  CHAR (2)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNTITEL                  CHAR (14)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNVORNA                  CHAR (16)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNZUNA                   CHAR (33)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNZUS                    CHAR (15)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNGBNA                   CHAR (21)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNSTR                    CHAR (25)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNLKZ                    CHAR (3)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNLAND                   CHAR (35)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNPLZ                    CHAR (6)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNORT                    CHAR (30)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNTELPR                  CHAR (23)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNFAX                    CHAR (23)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNTELDI                  CHAR (23)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNGBDAT                  CHAR (10)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNGBORT                  CHAR (30)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNFAM                    CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNSELBST                 CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNBER                    CHAR (40)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNBERKY                  CHAR (6)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNSTAAT                  CHAR (35)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDVNSTAATKZ                CHAR (3)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1ANRED                  CHAR (2)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1TITEL                  CHAR (14)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1VORNA                  CHAR (16)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1ZUNA                   CHAR (21)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1ZUS                    CHAR (15)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1GBNA                   CHAR (21)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1STR                    CHAR (25)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1LKZ                    CHAR (3)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1LAND                   CHAR (35)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1PLZ                    CHAR (6)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1ORT                    CHAR (30)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1GBDAT                  CHAR (10)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1GBORT                  CHAR (30)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1FAM                    CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1SELBST                 CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1BER                    CHAR (40)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1BERKY                  CHAR (6)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1STAAT                  CHAR (35)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV1STAATKZ                CHAR (3)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2ANRED                  CHAR (2)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2TITEL                  CHAR (14)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2VORNA                  CHAR (16)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2ZUNA                   CHAR (21)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2ZUS                    CHAR (15)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2GBNA                   CHAR (21)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2STR                    CHAR (25)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2LKZ                    CHAR (3)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2LAND                   CHAR (35)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2PLZ                    CHAR (6)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2ORT                    CHAR (30)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2GBDAT                  CHAR (10)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2GBORT                  CHAR (30)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2FAM                    CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2SELBST                 CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2BER                    CHAR (40)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2BERKY                  CHAR (6)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2STAAT                  CHAR (35)
CCC004*                                      NOT NULL,
CCC004*       PZM_PDV2STAATKZ                CHAR (3)
CCC004*                                      NOT NULL,
CCC004*       PZM_VDSPARTE                   CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_VDGKZ                      CHAR (2)
CCC004*                                      NOT NULL,
CCC004*       PZM_VDABVERM                   INT
CCC004*                                      NOT NULL,
CCC004*       PZM_VDABVERMPZ                 CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_VDVMVORNA                  CHAR (16)
CCC004*                                      NOT NULL,
CCC004*       PZM_VDVMZUNA                   CHAR (21)
CCC004*                                      NOT NULL,
CCC004*       PZM_VDVMNR                     INT
CCC004*                                      NOT NULL,
CCC004*       PZM_VDVMNRPZ                   CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_VDVMNRB                    CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_VDPRODKEY                  SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_VDABLEIN                   DECIMAL (7, 1)
CCC004*                                      NOT NULL,
CCC004*       PZM_VDBETREU                   CHAR (9)
CCC004*                                      NOT NULL,
CCC004*       PZM_VDDIREKT                   SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_VDPIF                      CHAR (15)
CCC004*                                      NOT NULL,
CCC004*       PZM_VDPROGKZ                   CHAR (3)
CCC004*                                      NOT NULL,
CCC004*       PZM_VERSION                    CHAR (5)
CCC004*                                      NOT NULL,
CCC004*       PZM_HVANTNUM                   CHAR (9)
CCC004*                                      NOT NULL,
CCC004*       PZM_HVTARIF                    CHAR (15)
CCC004*                                      NOT NULL,
CCC004*       PZM_HVVSNR                     INT
CCC004*                                      NOT NULL,
CCC004*       PZM_HVVBEGINN                  DATE
CCC004*                                      NOT NULL,
CCC004*       PZM_HVN                        SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_HVNS                       SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_HV8ENDALT                  SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_HVBVNL                     SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_EZAHLW                     SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_HVBEITRAG                  DECIMAL (11, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_HVRIS                      DECIMAL (10, 4)
CCC004*                                      NOT NULL,
CCC004*       PZM_ZUEB                       DECIMAL (11, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_HVVSUMME                   DECIMAL (11, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_HVTAZGAR                   INT
CCC004*                                      NOT NULL,
CCC004*       PZM_TAJADF_1                   SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_TAJADF_2                   SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_TAJADF_3                   SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_TAJADF_4                   SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_TAJADF_5                   SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_TAJADF_6                   SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_TAJADF_7                   SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_TAJADF_8                   SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_TAJADF_9                   SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_DM1FONDS                   CHAR (2)
CCC004*                                      NOT NULL,
CCC004*       PZM_DM1FONDSPROZ               SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_DM2FONDS                   CHAR (2)
CCC004*                                      NOT NULL,
CCC004*       PZM_DM2FONDSPROZ               SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_DM3FONDS                   CHAR (2)
CCC004*                                      NOT NULL,
CCC004*       PZM_DM3FONDSPROZ               SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_HVBEGREN                   CHAR (10)
CCC004*                                      NOT NULL,
CCC004*       PZM_HVREZAHLW                  SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_HVGARZEIT                  SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_HVRENTE                    DECIMAL (11, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_HVRENTH2                   DECIMAL (11, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_HVRENTH3                   DECIMAL (11, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_HVKAPAB                    INT
CCC004*                                      NOT NULL,
CCC004*       PZM_HZVRENT1                   DECIMAL (11, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_HZVP                       SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_HVRENTDMN                  DECIMAL (11, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_AVMG                       CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_BBG                        CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_BUZKZ                      CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_BERBUEA                    SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_BUBARREN1                  DECIMAL (11, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_BUBN                       SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_BUBZ                       SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_BUBT                       SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_BUBLI                      SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_BUBLR                      SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_BUZI                       CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_BUZP                       DECIMAL (11, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_BUZR                       DECIMAL (11, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_BUZD                       SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_BUZRIS                     DECIMAL (5, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_BUZBG                      SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_DYDYNPROZ                  DECIMAL (3, 1)
CCC004*                                      NOT NULL,
CCC004*       PZM_UZVKZ                      CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_UZVP                       DECIMAL (5, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_UZVS                       DECIMAL (11, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_UZVRIS                     DECIMAL (5, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_EUN                        DECIMAL (11, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_EUT                        DECIMAL (11, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_EUZS                       CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_EUZRIS                     DECIMAL (5, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_EUZBG                      SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_DEPOT                      CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_SODEPHOE                   DECIMAL (11, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_SODEPINS                   DECIMAL (11, 2)
CCC004*                                      NOT NULL,
CCC004*       PZM_GEWVA                      SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_ASAMNR                     SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_EKAPWR                     CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_ENACHV                     CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_EUMTAU                     CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_EWIEDERANL                 CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_BESV                       CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_GESKZ                      CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PZM_EUROKZ                     SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PZM_LAST_UPDATE                TIMESTAMP
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_KENNUNG                    CHAR (8)
CCC004*                                      NOT NULL,
CCC004*       PZM_HVRENTH4                   DECIMAL (3, 0)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_HVRENTH5                   DECIMAL (3, 0)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_NVERF                      CHAR (1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_ZBE                        CHAR (1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDVNGLKZ                   CHAR (3)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDVNGLAND                  CHAR (35)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_UEBART                     SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_BUZUEBART                  SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_EVNVT3                     CHAR (1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3ANRED                  CHAR (2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3TITEL                  CHAR (14)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3VORNA                  CHAR (16)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3ZUNA                   CHAR (21)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3ZUS                    CHAR (15)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3GBNA                   CHAR (21)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3STR                    CHAR (25)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3LKZ                    CHAR (3)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3LAND                   CHAR (35)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3PLZ                    CHAR (6)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3ORT                    CHAR (30)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3GBDAT                  CHAR (10)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3GBORT                  CHAR (30)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3FAM                    CHAR (1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3SELBST                 CHAR (1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3BER                    CHAR (40)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3BERKY                  CHAR (6)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3STAAT                  CHAR (35)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_PDV3STAATKZ                CHAR (3)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_HVENDNS                    CHAR (10)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_BUZABLGEMBZHV              CHAR (1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_KIZKZ                      CHAR (1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_KIZD                       DECIMAL (2, 0)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_KIZR                       DECIMAL (11, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_FIRMEINTRDATVT             CHAR (10)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_DYDYND2                    CHAR (1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_TZVKZ                      CHAR (1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_TZVD                       SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_TZVS                       DECIMAL (11, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_HZVKZ                      CHAR (1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_HZVR                       DECIMAL (11, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_DIVE                       CHAR (1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VDPRODKEYBUZI              SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VDABLEINBUZI               DECIMAL (7, 1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_BEITRAGBUZI                DECIMAL (11, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VDPRODKEYBUZR              SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VDABLEINBUZR               DECIMAL (7, 1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_BEITRAGBUZR                DECIMAL (11, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VDPRODKEYKIZ               SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VDABLEINKIZ                DECIMAL (7, 1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_BEITRAGKIZ                 DECIMAL (11, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VDPRODKEYTZV               SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VDABLEINTZV                DECIMAL (7, 1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_BEITRAGTZV                 DECIMAL (11, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VDPRODKEYHZV               SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VDABLEINHZV                DECIMAL (7, 1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_BEITRAGHZV                 DECIMAL (11, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_DM4FONDS                   CHAR (2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_DM4FONDSPROZ               SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VAREU_5PRZ1                DECIMAL (5, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VAREU_5PRZ2                DECIMAL (5, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VAREU_5PRZ3                DECIMAL (5, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VAREU_5PRZ4                DECIMAL (5, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VAREU_5PRZ5                DECIMAL (5, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VAREU_5PRZ6                DECIMAL (5, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VAREU_5PRZ7                DECIMAL (5, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VAREU_5PRZ8                DECIMAL (5, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_HVGARZEITMAX               CHAR (1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       PZM_VFPHASEDAT                 CHAR (10)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT

CCC004*    )
CCC003*    END-EXEC.


       01  DCLTBAN0002.
           03 PGEB-GES                       PIC S9(4) COMP.
           03 PZM-PIN                        PIC S9(9) COMP.
           03 PZM-EVNVT1                     PIC X.
           03 PZM-EVNVT2                     PIC X.
           03 PZM-PDVNANRED                  PIC X(2).
           03 PZM-PDVNTITEL                  PIC X(14).
           03 PZM-PDVNVORNA                  PIC X(16).
           03 PZM-PDVNZUNA                   PIC X(33).
           03 PZM-PDVNZUS                    PIC X(15).
           03 PZM-PDVNGBNA                   PIC X(21).
           03 PZM-PDVNSTR                    PIC X(25).
           03 PZM-PDVNLKZ                    PIC X(3).
           03 PZM-PDVNLAND                   PIC X(35).
           03 PZM-PDVNPLZ                    PIC X(6).
           03 PZM-PDVNORT                    PIC X(30).
           03 PZM-PDVNTELPR                  PIC X(23).
           03 PZM-PDVNFAX                    PIC X(23).
           03 PZM-PDVNTELDI                  PIC X(23).
           03 PZM-PDVNGBDAT                  PIC X(10).
           03 PZM-PDVNGBORT                  PIC X(30).
           03 PZM-PDVNFAM                    PIC X.
           03 PZM-PDVNSELBST                 PIC X.
           03 PZM-PDVNBER                    PIC X(40).
           03 PZM-PDVNBERKY                  PIC X(6).
           03 PZM-PDVNSTAAT                  PIC X(35).
           03 PZM-PDVNSTAATKZ                PIC X(3).
           03 PZM-PDV1ANRED                  PIC X(2).
           03 PZM-PDV1TITEL                  PIC X(14).
           03 PZM-PDV1VORNA                  PIC X(16).
           03 PZM-PDV1ZUNA                   PIC X(21).
           03 PZM-PDV1ZUS                    PIC X(15).
           03 PZM-PDV1GBNA                   PIC X(21).
           03 PZM-PDV1STR                    PIC X(25).
           03 PZM-PDV1LKZ                    PIC X(3).
           03 PZM-PDV1LAND                   PIC X(35).
           03 PZM-PDV1PLZ                    PIC X(6).
           03 PZM-PDV1ORT                    PIC X(30).
           03 PZM-PDV1GBDAT                  PIC X(10).
           03 PZM-PDV1GBORT                  PIC X(30).
           03 PZM-PDV1FAM                    PIC X.
           03 PZM-PDV1SELBST                 PIC X.
           03 PZM-PDV1BER                    PIC X(40).
           03 PZM-PDV1BERKY                  PIC X(6).
           03 PZM-PDV1STAAT                  PIC X(35).
           03 PZM-PDV1STAATKZ                PIC X(3).
           03 PZM-PDV2ANRED                  PIC X(2).
           03 PZM-PDV2TITEL                  PIC X(14).
           03 PZM-PDV2VORNA                  PIC X(16).
           03 PZM-PDV2ZUNA                   PIC X(21).
           03 PZM-PDV2ZUS                    PIC X(15).
           03 PZM-PDV2GBNA                   PIC X(21).
           03 PZM-PDV2STR                    PIC X(25).
           03 PZM-PDV2LKZ                    PIC X(3).
           03 PZM-PDV2LAND                   PIC X(35).
           03 PZM-PDV2PLZ                    PIC X(6).
           03 PZM-PDV2ORT                    PIC X(30).
           03 PZM-PDV2GBDAT                  PIC X(10).
           03 PZM-PDV2GBORT                  PIC X(30).
           03 PZM-PDV2FAM                    PIC X.
           03 PZM-PDV2SELBST                 PIC X.
           03 PZM-PDV2BER                    PIC X(40).
           03 PZM-PDV2BERKY                  PIC X(6).
           03 PZM-PDV2STAAT                  PIC X(35).
           03 PZM-PDV2STAATKZ                PIC X(3).
           03 PZM-VDSPARTE                   PIC X.
           03 PZM-VDGKZ                      PIC X(2).
           03 PZM-VDABVERM                   PIC S9(9) COMP.
           03 PZM-VDABVERMPZ                 PIC X.
           03 PZM-VDVMVORNA                  PIC X(16).
           03 PZM-VDVMZUNA                   PIC X(21).
           03 PZM-VDVMNR                     PIC S9(9) COMP.
           03 PZM-VDVMNRPZ                   PIC X.
           03 PZM-VDVMNRB                    PIC X.
           03 PZM-VDPRODKEY                  PIC S9(4) COMP.
           03 PZM-VDABLEIN                   PIC S9(6)V9(1) COMP-3.
           03 PZM-VDBETREU                   PIC X(9).
           03 PZM-VDDIREKT                   PIC S9(4) COMP.
           03 PZM-VDPIF                      PIC X(15).
           03 PZM-VDPROGKZ                   PIC X(3).
           03 PZM-VERSION                    PIC X(5).
           03 PZM-HVANTNUM                   PIC X(9).
           03 PZM-HVTARIF                    PIC X(15).
           03 PZM-HVVSNR                     PIC S9(9) COMP.
           03 PZM-HVVBEGINN                  PIC X(10).
           03 PZM-HVN                        PIC S9(4) COMP.
           03 PZM-HVNS                       PIC S9(4) COMP.
           03 PZM-HV8ENDALT                  PIC S9(4) COMP.
           03 PZM-HVBVNL                     PIC S9(4) COMP.
           03 PZM-EZAHLW                     PIC S9(4) COMP.
           03 PZM-HVBEITRAG                  PIC S9(9)V9(2) COMP-3.
           03 PZM-HVRIS                      PIC S9(6)V9(4) COMP-3.
           03 PZM-ZUEB                       PIC S9(9)V9(2) COMP-3.
           03 PZM-HVVSUMME                   PIC S9(9)V9(2) COMP-3.
           03 PZM-HVTAZGAR                   PIC S9(9) COMP.
           03 PZM-TAJADF-1                   PIC S9(4) COMP.
           03 PZM-TAJADF-2                   PIC S9(4) COMP.
           03 PZM-TAJADF-3                   PIC S9(4) COMP.
           03 PZM-TAJADF-4                   PIC S9(4) COMP.
           03 PZM-TAJADF-5                   PIC S9(4) COMP.
           03 PZM-TAJADF-6                   PIC S9(4) COMP.
           03 PZM-TAJADF-7                   PIC S9(4) COMP.
           03 PZM-TAJADF-8                   PIC S9(4) COMP.
           03 PZM-TAJADF-9                   PIC S9(4) COMP.
           03 PZM-DM1FONDS                   PIC X(2).
           03 PZM-DM1FONDSPROZ               PIC S9(4) COMP.
           03 PZM-DM2FONDS                   PIC X(2).
           03 PZM-DM2FONDSPROZ               PIC S9(4) COMP.
           03 PZM-DM3FONDS                   PIC X(2).
           03 PZM-DM3FONDSPROZ               PIC S9(4) COMP.
           03 PZM-HVBEGREN                   PIC X(10).
           03 PZM-HVREZAHLW                  PIC S9(4) COMP.
           03 PZM-HVGARZEIT                  PIC S9(4) COMP.
           03 PZM-HVRENTE                    PIC S9(9)V9(2) COMP-3.
           03 PZM-HVRENTH2                   PIC S9(9)V9(2) COMP-3.
           03 PZM-HVRENTH3                   PIC S9(9)V9(2) COMP-3.
           03 PZM-HVKAPAB                    PIC S9(9) COMP.
           03 PZM-HZVRENT1                   PIC S9(9)V9(2) COMP-3.
           03 PZM-HZVP                       PIC S9(4) COMP.
           03 PZM-HVRENTDMN                  PIC S9(9)V9(2) COMP-3.
           03 PZM-AVMG                       PIC X.
           03 PZM-BBG                        PIC X.
           03 PZM-BUZKZ                      PIC X.
           03 PZM-BERBUEA                    PIC S9(4) COMP.
           03 PZM-BUBARREN1                  PIC S9(9)V9(2) COMP-3.
           03 PZM-BUBN                       PIC S9(4) COMP.
           03 PZM-BUBZ                       PIC S9(4) COMP.
           03 PZM-BUBT                       PIC S9(4) COMP.
           03 PZM-BUBLI                      PIC S9(4) COMP.
           03 PZM-BUBLR                      PIC S9(4) COMP.
           03 PZM-BUZI                       PIC X.
           03 PZM-BUZP                       PIC S9(9)V9(2) COMP-3.
           03 PZM-BUZR                       PIC S9(9)V9(2) COMP-3.
           03 PZM-BUZD                       PIC S9(4) COMP.
           03 PZM-BUZRIS                     PIC S9(3)V9(2) COMP-3.
           03 PZM-BUZBG                      PIC S9(4) COMP.
           03 PZM-DYDYNPROZ                  PIC S9(2)V9(1) COMP-3.
           03 PZM-UZVKZ                      PIC X.
           03 PZM-UZVP                       PIC S9(3)V9(2) COMP-3.
           03 PZM-UZVS                       PIC S9(9)V9(2) COMP-3.
           03 PZM-UZVRIS                     PIC S9(3)V9(2) COMP-3.
           03 PZM-EUN                        PIC S9(9)V9(2) COMP-3.
           03 PZM-EUT                        PIC S9(9)V9(2) COMP-3.
           03 PZM-EUZS                       PIC X.
           03 PZM-EUZRIS                     PIC S9(3)V9(2) COMP-3.
           03 PZM-EUZBG                      PIC S9(4) COMP.
           03 PZM-DEPOT                      PIC X.
           03 PZM-SODEPHOE                   PIC S9(9)V9(2) COMP-3.
           03 PZM-SODEPINS                   PIC S9(9)V9(2) COMP-3.
           03 PZM-GEWVA                      PIC S9(4) COMP.
           03 PZM-ASAMNR                     PIC S9(4) COMP.
           03 PZM-EKAPWR                     PIC X.
           03 PZM-ENACHV                     PIC X.
           03 PZM-EUMTAU                     PIC X.
           03 PZM-EWIEDERANL                 PIC X.
           03 PZM-BESV                       PIC X.
           03 PZM-GESKZ                      PIC X.
           03 PZM-EUROKZ                     PIC S9(4) COMP.
           03 PZM-LAST-UPDATE                PIC X(26).
           03 PZM-KENNUNG                    PIC X(8).
           03 PZM-HVRENTH4                   PIC S9(3)V COMP-3.
           03 PZM-HVRENTH5                   PIC S9(3)V COMP-3.
           03 PZM-NVERF                      PIC X.
           03 PZM-ZBE                        PIC X.
           03 PZM-PDVNGLKZ                   PIC X(3).
           03 PZM-PDVNGLAND                  PIC X(35).
           03 PZM-UEBART                     PIC S9(4) COMP.
           03 PZM-BUZUEBART                  PIC S9(4) COMP.
           03 PZM-EVNVT3                     PIC X.
           03 PZM-PDV3ANRED                  PIC X(2).
           03 PZM-PDV3TITEL                  PIC X(14).
           03 PZM-PDV3VORNA                  PIC X(16).
           03 PZM-PDV3ZUNA                   PIC X(21).
           03 PZM-PDV3ZUS                    PIC X(15).
           03 PZM-PDV3GBNA                   PIC X(21).
           03 PZM-PDV3STR                    PIC X(25).
           03 PZM-PDV3LKZ                    PIC X(3).
           03 PZM-PDV3LAND                   PIC X(35).
           03 PZM-PDV3PLZ                    PIC X(6).
           03 PZM-PDV3ORT                    PIC X(30).
           03 PZM-PDV3GBDAT                  PIC X(10).
           03 PZM-PDV3GBORT                  PIC X(30).
           03 PZM-PDV3FAM                    PIC X.
           03 PZM-PDV3SELBST                 PIC X.
           03 PZM-PDV3BER                    PIC X(40).
           03 PZM-PDV3BERKY                  PIC X(6).
           03 PZM-PDV3STAAT                  PIC X(35).
           03 PZM-PDV3STAATKZ                PIC X(3).
           03 PZM-HVENDNS                    PIC X(10).
           03 PZM-BUZABLGEMBZHV              PIC X.
           03 PZM-KIZKZ                      PIC X.
           03 PZM-KIZD                       PIC S9(2)V COMP-3.
           03 PZM-KIZR                       PIC S9(9)V9(2) COMP-3.
           03 PZM-FIRMEINTRDATVT             PIC X(10).
           03 PZM-DYDYND2                    PIC X.
           03 PZM-TZVKZ                      PIC X.
           03 PZM-TZVD                       PIC S9(4) COMP.
           03 PZM-TZVS                       PIC S9(9)V9(2) COMP-3.
           03 PZM-HZVKZ                      PIC X.
           03 PZM-HZVR                       PIC S9(9)V9(2) COMP-3.
           03 PZM-DIVE                       PIC X.
           03 PZM-VDPRODKEYBUZI              PIC S9(4) COMP.
           03 PZM-VDABLEINBUZI               PIC S9(6)V9(1) COMP-3.
           03 PZM-BEITRAGBUZI                PIC S9(9)V9(2) COMP-3.
           03 PZM-VDPRODKEYBUZR              PIC S9(4) COMP.
           03 PZM-VDABLEINBUZR               PIC S9(6)V9(1) COMP-3.
           03 PZM-BEITRAGBUZR                PIC S9(9)V9(2) COMP-3.
           03 PZM-VDPRODKEYKIZ               PIC S9(4) COMP.
           03 PZM-VDABLEINKIZ                PIC S9(6)V9(1) COMP-3.
           03 PZM-BEITRAGKIZ                 PIC S9(9)V9(2) COMP-3.
           03 PZM-VDPRODKEYTZV               PIC S9(4) COMP.
           03 PZM-VDABLEINTZV                PIC S9(6)V9(1) COMP-3.
           03 PZM-BEITRAGTZV                 PIC S9(9)V9(2) COMP-3.
           03 PZM-VDPRODKEYHZV               PIC S9(4) COMP.
           03 PZM-VDABLEINHZV                PIC S9(6)V9(1) COMP-3.
           03 PZM-BEITRAGHZV                 PIC S9(9)V9(2) COMP-3.
           03 PZM-DM4FONDS                   PIC X(2).
           03 PZM-DM4FONDSPROZ               PIC S9(4) COMP.
           03 PZM-VAREU-5PRZ1                PIC S9(3)V9(2) COMP-3.
           03 PZM-VAREU-5PRZ2                PIC S9(3)V9(2) COMP-3.
           03 PZM-VAREU-5PRZ3                PIC S9(3)V9(2) COMP-3.
           03 PZM-VAREU-5PRZ4                PIC S9(3)V9(2) COMP-3.
           03 PZM-VAREU-5PRZ5                PIC S9(3)V9(2) COMP-3.
           03 PZM-VAREU-5PRZ6                PIC S9(3)V9(2) COMP-3.
           03 PZM-VAREU-5PRZ7                PIC S9(3)V9(2) COMP-3.
           03 PZM-VAREU-5PRZ8                PIC S9(3)V9(2) COMP-3.
           03 PZM-HVGARZEITMAX               PIC X.
           03 PZM-VFPHASEDAT                 PIC X(10).

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA1526.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA1526.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:18:12   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV1526-T
CCC005*    EXEC SQL DECLARE TBOVPSGS TABLE
CCC004*     (KNR_GRP_SPARTE       SMALLINT             NOT NULL
CCC004*     ,KNR_PROV_TYP         SMALLINT             NOT NULL
CCC004*     ,KNR_PROD_SPARTE      SMALLINT             NOT NULL
CCC004*     ,DAT_VON              TIMESTAMP            NOT NULL
CCC004*     ,LAST_UPDATE          TIMESTAMP            NOT NULL
CCC004*     ,DAT_BIS              DATE                 NOT NULL
CCC004*     ,KTY_GRP_SPARTE       SMALLINT             NOT NULL
CCC004*     ,KTY_PROV_TYP         SMALLINT             NOT NULL
CCC004*     ,KTY_PROD_SPARTE      SMALLINT             NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV1526-T
        01 TBOVPSGS-SATZ.
         10 KNR-GRP-SPARTE                    PIC S9(4) COMP.
         10 KNR-PROV-TYP                      PIC S9(4) COMP.
         10 KNR-PROD-SPARTE                   PIC S9(4) COMP.
         10 DAT-VON                           PIC X(26).
         10 LAST-UPDATE                       PIC X(26).
         10 DAT-BIS                           PIC X(10).
         10 KTY-GRP-SPARTE                    PIC S9(4) COMP.
         10 KTY-PROV-TYP                      PIC S9(4) COMP.
         10 KTY-PROD-SPARTE                   PIC S9(4) COMP.

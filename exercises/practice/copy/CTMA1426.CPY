      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA1426.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA1426.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:17:24   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV1426-T
CCC005*    EXEC SQL DECLARE TBOVLF TABLE
CCC004*     (ST_NR                INTEGER              NOT NULL
CCC004*     ,MONAT                DATE                 NOT NULL
CCC004*     ,KNR_PROD_GEB_GES     SMALLINT             NOT NULL
CCC004*     ,KTY_PROD_GEB_GES     SMALLINT             NOT NULL
CCC004*     ,BQF_EG               DECIMAL(5, 2)        NOT NULL
CCC004*     ,BQF_GS               DECIMAL(5, 2)        NOT NULL
CCC004*     ,BQF_GR               DECIMAL(5, 2)        NOT NULL
CCC004*     ,DYNQ_EG              DECIMAL(5, 2)        NOT NULL
CCC004*     ,DYNQ_GS              DECIMAL(5, 2)        NOT NULL
CCC004*     ,DYNQ_GR              DECIMAL(5, 2)        NOT NULL
CCC004*     ,NEQF_EG              DECIMAL(5, 2)        NOT NULL
CCC004*     ,NEQF_GS              DECIMAL(5, 2)        NOT NULL
CCC004*     ,NEQF_GR              DECIMAL(5, 2)        NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV1426-T
        01 TBOVLF-SATZ.
         10 ST-NR                             PIC S9(9) COMP.
         10 MONAT                             PIC X(10).
         10 KNR-PROD-GEB-GES                  PIC S9(4) COMP.
         10 KTY-PROD-GEB-GES                  PIC S9(4) COMP.
         10 BQF-EG                            PIC S9(3)V9(2) COMP-3.
         10 BQF-GS                            PIC S9(3)V9(2) COMP-3.
         10 BQF-GR                            PIC S9(3)V9(2) COMP-3.
         10 DYNQ-EG                           PIC S9(3)V9(2) COMP-3.
         10 DYNQ-GS                           PIC S9(3)V9(2) COMP-3.
         10 DYNQ-GR                           PIC S9(3)V9(2) COMP-3.
         10 NEQF-EG                           PIC S9(3)V9(2) COMP-3.
         10 NEQF-GS                           PIC S9(3)V9(2) COMP-3.
         10 NEQF-GR                           PIC S9(3)V9(2) COMP-3.

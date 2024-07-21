      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA1422.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA1422.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:17:10   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV1422-T
CCC005*    EXEC SQL DECLARE TBOVAST TABLE
CCC004*     (ST_NR                INTEGER              NOT NULL
CCC004*     ,MONAT                DATE                 NOT NULL
CCC004*     ,JEQF_EG              DECIMAL(5, 2)        NOT NULL
CCC004*     ,JEQF_GR              DECIMAL(5, 2)        NOT NULL
CCC004*     ,JEQF_GS              DECIMAL(5, 2)        NOT NULL
CCC004*     ,EH_AB_78_EG          DECIMAL(10, 1)       NOT NULL
CCC004*     ,EH_AB_78_GR          DECIMAL(10, 1)       NOT NULL
CCC004*     ,EH_BIS_78_GS         DECIMAL(10, 1)       NOT NULL
CCC004*     ,EH_VM_ZU_EG          DECIMAL(10, 1)       NOT NULL
CCC004*     ,EH_VM_ZU_GR          DECIMAL(10, 1)       NOT NULL
CCC004*     ,EH_VM_AB_EG          DECIMAL(10, 1)       NOT NULL
CCC004*     ,EH_VM_AB_GR          DECIMAL(10, 1)       NOT NULL
CCC004*     ,RCK_STZ              DECIMAL(5, 2)        NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV1422-T
        01 TBOVAST-SATZ.
         10 ST-NR                             PIC S9(9) COMP.
         10 MONAT                             PIC X(10).
         10 JEQF-EG                           PIC S9(3)V9(2) COMP-3.
         10 JEQF-GR                           PIC S9(3)V9(2) COMP-3.
         10 JEQF-GS                           PIC S9(3)V9(2) COMP-3.
         10 EH-AB-78-EG                       PIC S9(9)V9(1) COMP-3.
         10 EH-AB-78-GR                       PIC S9(9)V9(1) COMP-3.
         10 EH-BIS-78-GS                      PIC S9(9)V9(1) COMP-3.
         10 EH-VM-ZU-EG                       PIC S9(9)V9(1) COMP-3.
         10 EH-VM-ZU-GR                       PIC S9(9)V9(1) COMP-3.
         10 EH-VM-AB-EG                       PIC S9(9)V9(1) COMP-3.
         10 EH-VM-AB-GR                       PIC S9(9)V9(1) COMP-3.
         10 RCK-STZ                           PIC S9(3)V9(2) COMP-3.

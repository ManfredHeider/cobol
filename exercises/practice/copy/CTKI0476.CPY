      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11/02/15
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0476.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0476.CPv  $
      *
      *   Rev 2.2   Sep 02 2015 15:11:50   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:47:48   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Feb 11 2015 14:19:03   T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G90KSDSVERTRAG TABLE
CCC004*    ( G90VVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G90VMANDANT                    INTEGER NOT NULL,
CCC004*      G90VAENDKZ                     CHAR(2) NOT NULL,
CCC004*      G90VGESID                      SMALLINT NOT NULL,
CCC004*      G90VABSCHLVBNR                 INTEGER NOT NULL,
CCC004*      G90VPRODUKTART                 SMALLINT NOT NULL,
CCC004*      G90VABRGEBSATZ                 DECIMAL(3, 2) NOT NULL,
CCC004*      G90VFACTGEBSATZ                DECIMAL(3, 2) NOT NULL,
CCC004*      G90VFACTANKBETR                DECIMAL(12, 2) NOT NULL,
CCC004*      G90VBEGINN                     DATE NOT NULL,
CCC004*      G90VENDE                       DATE NOT NULL,
CCC004*      G90VEINRGEBBETR                DECIMAL(10, 2) NOT NULL,
CCC004*      G90VIBAN                       CHAR(34) NOT NULL,
CCC004*      G90VMANDATREFNR                CHAR(35) NOT NULL,
CCC004*      G90VABWBZKZ                    CHAR(1) NOT NULL,
CCC004*      G90VABWBZVNAME                 CHAR(30) NOT NULL,
CCC004*      G90VABWBZNNAME                 CHAR(30) NOT NULL,
CCC004*      G90VABWBZSTRASSE               CHAR(40) NOT NULL,
CCC004*      G90VABWBZPLZ                   INTEGER NOT NULL,
CCC004*      G90VABWBZORT                   CHAR(30) NOT NULL,
CCC004*      G90VINFOSPERRE                 CHAR(1) NOT NULL,
CCC004*      G90VPROGVONDAT                 DATE NOT NULL,
CCC004*      G90VPROGBISDAT                 DATE NOT NULL,
CCC004*      G90VPROGANZRECHN               INTEGER NOT NULL,
CCC004*      G90VPROGGESUMS                 DECIMAL(10, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G90KSDSVERTRAG              *
      ******************************************************************
       01  G90KSDSVERTRAG-GESAMT.
        05 NI-G90KSDSVERTRAG-TAB.
           10 NI-G90KSDSVERTRAG-ELE PIC S9(04) COMP OCCURS 25 TIMES.
        05 NI-G90KSDSVERTRAG REDEFINES NI-G90KSDSVERTRAG-TAB.
           10 NI-G90VVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G90VMANDANT        PIC S9(4) COMP.
           10 NI-G90VAENDKZ         PIC S9(4) COMP.
           10 NI-G90VGESID          PIC S9(4) COMP.
           10 NI-G90VABSCHLVBNR     PIC S9(4) COMP.
           10 NI-G90VPRODUKTART     PIC S9(4) COMP.
           10 NI-G90VABRGEBSATZ     PIC S9(4) COMP.
           10 NI-G90VFACTGEBSATZ    PIC S9(4) COMP.
           10 NI-G90VFACTANKBETR    PIC S9(4) COMP.
           10 NI-G90VBEGINN         PIC S9(4) COMP.
           10 NI-G90VENDE           PIC S9(4) COMP.
           10 NI-G90VEINRGEBBETR    PIC S9(4) COMP.
           10 NI-G90VIBAN           PIC S9(4) COMP.
           10 NI-G90VMANDATREFNR    PIC S9(4) COMP.
           10 NI-G90VABWBZKZ        PIC S9(4) COMP.
           10 NI-G90VABWBZVNAME     PIC S9(4) COMP.
           10 NI-G90VABWBZNNAME     PIC S9(4) COMP.
           10 NI-G90VABWBZSTRASSE   PIC S9(4) COMP.
           10 NI-G90VABWBZPLZ       PIC S9(4) COMP.
           10 NI-G90VABWBZORT       PIC S9(4) COMP.
           10 NI-G90VINFOSPERRE     PIC S9(4) COMP.
           10 NI-G90VPROGVONDAT     PIC S9(4) COMP.
           10 NI-G90VPROGBISDAT     PIC S9(4) COMP.
           10 NI-G90VPROGANZRECHN   PIC S9(4) COMP.
           10 NI-G90VPROGGESUMS     PIC S9(4) COMP.
        05 DCL-G90KSDSVERTRAG.
           10 G90VVERTRAGSNR       PIC X(15).
           10 G90VMANDANT          PIC S9(9) USAGE COMP.
           10 G90VAENDKZ           PIC X(2).
           10 G90VGESID            PIC S9(4) USAGE COMP.
           10 G90VABSCHLVBNR       PIC S9(9) USAGE COMP.
           10 G90VPRODUKTART       PIC S9(4) USAGE COMP.
           10 G90VABRGEBSATZ       PIC S9(1)V9(2) USAGE COMP-3.
           10 G90VFACTGEBSATZ      PIC S9(1)V9(2) USAGE COMP-3.
           10 G90VFACTANKBETR      PIC S9(10)V9(2) USAGE COMP-3.
           10 G90VBEGINN           PIC X(10).
           10 G90VENDE             PIC X(10).
           10 G90VEINRGEBBETR      PIC S9(8)V9(2) USAGE COMP-3.
           10 G90VIBAN             PIC X(34).
           10 G90VMANDATREFNR      PIC X(35).
           10 G90VABWBZKZ          PIC X(1).
           10 G90VABWBZVNAME       PIC X(30).
           10 G90VABWBZNNAME       PIC X(30).
           10 G90VABWBZSTRASSE     PIC X(40).
           10 G90VABWBZPLZ         PIC S9(9) USAGE COMP.
           10 G90VABWBZORT         PIC X(30).
           10 G90VINFOSPERRE       PIC X(1).
           10 G90VPROGVONDAT       PIC X(10).
           10 G90VPROGBISDAT       PIC X(10).
           10 G90VPROGANZRECHN     PIC S9(9) USAGE COMP.
           10 G90VPROGGESUMS       PIC S9(8)V9(2) USAGE COMP-3.
      ************************** ENDE-DCLGEN *************************


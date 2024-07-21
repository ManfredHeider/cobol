      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06/12/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0519.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTKI0519.CPv  $
      *
      *   Rev 2.4   Nov 24 2020 08:32:50   u130685
      *AKI01805: G48_Vertrag OZP/Copy
      *
      *   Rev 2.3   Feb 19 2019 06:47:44   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.2   Feb 18 2019 16:31:50   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.1   Jan 30 2019 11:21:10   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.0   Dec 6 2018 15:45:49   T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G48_VERTRAG TABLE
CCC004*    ( G48VVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G48VMANDANT                    INTEGER NOT NULL,
CCC004*      G48VVBNR                       INTEGER NOT NULL,
CCC004*      G48VVERTGBEGINN                CHAR(8) NOT NULL,
CCC004*      G48VVERTENDE                   CHAR(8) NOT NULL,
CCC004*      G48VINFOSPRREKZ                CHAR(1) NOT NULL,
CCC004*      G48VINFOSPERDAT                CHAR(8) NOT NULL,
CCC004*      G48VVERFUEGUNG                 CHAR(1) NOT NULL,
CCC004*      G48VANACHLASSKZ                CHAR(1) NOT NULL,
CCC004*      G48VTELBANKING                 CHAR(1) NOT NULL,
CCC004*      G48VONLBANKING                 CHAR(1) NOT NULL,
CCC004*      G48VPERSONLFDNR                SMALLINT NOT NULL,
CCC004*      G48VNACHNAME                   CHAR(35) NOT NULL,
CCC004*      G48VVORNAME                    CHAR(35) NOT NULL,
CCC004*      G48VGEBDAT                     CHAR(8) NOT NULL,
CCC004*      G48VGESCHLECHT                 CHAR(1) NOT NULL,
CCC004*      G48VANREDE                     CHAR(1) NOT NULL,
CCC004*      G48VSTR                        CHAR(28) NOT NULL,
CCC004*      G48VHAUSNR                     CHAR(9) NOT NULL,
CCC004*      G48VPOSTFACH                   CHAR(9) NOT NULL,
CCC004*      G48VPLZ                        CHAR(9) NOT NULL,
CCC004*      G48VORT                        CHAR(28) NOT NULL,
CCC004*      G48VLKZ                        CHAR(3) NOT NULL,
CCC004*      G48VTELVOR                     CHAR(6) NOT NULL,
CCC004*      G48VTESLNR                     CHAR(9) NOT NULL,
CCC004*      G48VDEPOTKZ                    CHAR(1) NOT NULL,
CCC004*      G48VBLZ                        CHAR(8) NOT NULL,
CCC004*      G48VDELKZ                      CHAR(1) NOT NULL,
CCC004*      G48VBIC                        CHAR(11) NOT NULL,
CCC004*      G48VKNDNAME                    CHAR(28) NOT NULL,
CCC004*      G48VFILIALEART                 CHAR(35) NOT NULL,
CCC004*      G48VFILIALENAME                CHAR(35) NOT NULL,
CCC004*      G48VFILIALESTR                 CHAR(35) NOT NULL,
CCC004*      G48VFILIALEORT                 CHAR(35) NOT NULL,
CCC004*      G48VESAFE                      CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G48_VERTRAG                 *
      ******************************************************************
       01  G48-VERTRAG-GESAMT.
        05 NI-G48-VERTRAG-TAB.
           10 NI-G48-VERTRAG-ELE PIC S9(04) COMP OCCURS 35 TIMES.
        05 NI-G48-VERTRAG REDEFINES NI-G48-VERTRAG-TAB.
           10 NI-G48VVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G48VMANDANT        PIC S9(4) COMP.
           10 NI-G48VVBNR           PIC S9(4) COMP.
           10 NI-G48VVERTGBEGINN    PIC S9(4) COMP.
           10 NI-G48VVERTENDE       PIC S9(4) COMP.
           10 NI-G48VINFOSPRREKZ    PIC S9(4) COMP.
           10 NI-G48VINFOSPERDAT    PIC S9(4) COMP.
           10 NI-G48VVERFUEGUNG     PIC S9(4) COMP.
           10 NI-G48VANACHLASSKZ    PIC S9(4) COMP.
           10 NI-G48VTELBANKING     PIC S9(4) COMP.
           10 NI-G48VONLBANKING     PIC S9(4) COMP.
           10 NI-G48VPERSONLFDNR    PIC S9(4) COMP.
           10 NI-G48VNACHNAME       PIC S9(4) COMP.
           10 NI-G48VVORNAME        PIC S9(4) COMP.
           10 NI-G48VGEBDAT         PIC S9(4) COMP.
           10 NI-G48VGESCHLECHT     PIC S9(4) COMP.
           10 NI-G48VANREDE         PIC S9(4) COMP.
           10 NI-G48VSTR            PIC S9(4) COMP.
           10 NI-G48VHAUSNR         PIC S9(4) COMP.
           10 NI-G48VPOSTFACH       PIC S9(4) COMP.
           10 NI-G48VPLZ            PIC S9(4) COMP.
           10 NI-G48VORT            PIC S9(4) COMP.
           10 NI-G48VLKZ            PIC S9(4) COMP.
           10 NI-G48VTELVOR         PIC S9(4) COMP.
           10 NI-G48VTESLNR         PIC S9(4) COMP.
           10 NI-G48VDEPOTKZ        PIC S9(4) COMP.
           10 NI-G48VBLZ            PIC S9(4) COMP.
           10 NI-G48VDELKZ          PIC S9(4) COMP.
           10 NI-G48VBIC            PIC S9(4) COMP.
           10 NI-G48VKNDNAME        PIC S9(4) COMP.
           10 NI-G48VFILIALEART     PIC S9(4) COMP.
           10 NI-G48VFILIALENAME    PIC S9(4) COMP.
           10 NI-G48VFILIALESTR     PIC S9(4) COMP.
           10 NI-G48VFILIALEORT     PIC S9(4) COMP.
           10 NI-G48VESAFE          PIC S9(4) COMP.
        05 DCL-G48-VERTRAG.
           10 G48VVERTRAGSNR       PIC X(15).
           10 G48VMANDANT          PIC S9(9) USAGE COMP.
           10 G48VVBNR             PIC S9(9) USAGE COMP.
           10 G48VVERTGBEGINN      PIC X(8).
           10 G48VVERTENDE         PIC X(8).
           10 G48VINFOSPRREKZ      PIC X(1).
           10 G48VINFOSPERDAT      PIC X(8).
           10 G48VVERFUEGUNG       PIC X(1).
           10 G48VANACHLASSKZ      PIC X(1).
           10 G48VTELBANKING       PIC X(1).
           10 G48VONLBANKING       PIC X(1).
           10 G48VPERSONLFDNR      PIC S9(4) USAGE COMP.
           10 G48VNACHNAME         PIC X(35).
           10 G48VVORNAME          PIC X(35).
           10 G48VGEBDAT           PIC X(8).
           10 G48VGESCHLECHT       PIC X(1).
           10 G48VANREDE           PIC X(1).
           10 G48VSTR              PIC X(28).
           10 G48VHAUSNR           PIC X(9).
           10 G48VPOSTFACH         PIC X(9).
           10 G48VPLZ              PIC X(9).
           10 G48VORT              PIC X(28).
           10 G48VLKZ              PIC X(3).
           10 G48VTELVOR           PIC X(6).
           10 G48VTESLNR           PIC X(9).
           10 G48VDEPOTKZ          PIC X(1).
           10 G48VBLZ              PIC X(8).
           10 G48VDELKZ            PIC X(1).
           10 G48VBIC              PIC X(11).
           10 G48VKNDNAME          PIC X(28).
           10 G48VFILIALEART       PIC X(35).
           10 G48VFILIALENAME      PIC X(35).
           10 G48VFILIALESTR       PIC X(35).
           10 G48VFILIALEORT       PIC X(35).
           10 G48VESAFE            PIC X(1).
      ************************** ENDE-DCLGEN *************************

      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0351.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRAT0351.CPv  $
      *
      *   Rev 2.8   Nov 25 2015 12:09:04   t13ae13
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - Bef.Vora
      *usschau mit Funktionsviews
      *
      *   Rev 2.7   Jul 22 2010 16:31:44   t13ae27
      *ASL00351: Columbusbonus - 300EEG-Regel - DETAILNP
      *
      *   Rev 2.6   Apr 11 2008 15:56:14   t13ae27
      *AAS00756: Wettbewerb 2008 der Allfinanz
      *
      *   Rev 2.5   20 Mar 2007 10:47:40   t13ae27
      *AAS00681: Jahres-WB 2007 DVAG-Wien - Programme
      *
      *   Rev 2.4   Apr 04 2006 10:44:10   t13ex43
      *ASL00226: PGM - Dimension 7 Zukunft - MAP
      *
      *   Rev 2.3   Jun 01 2004 10:51:02   t13ex43
      *PGM - Ermittlung MAP-Kandidaten
      *
      *   Rev 2.2   30 Apr 2001 10:44:56   T13AE13
      *ASL00073 - neue Konstante für Fiat-Liste
      *
      *   Rev 2.1   08 Mar 2001 10:34:22   T13AE30
      *JAHRESWETTBEWERBE / LEISTUNGSPLAN
      *AKTUALISIERUNG KANDIDATENTABELLE TBVB0078
      *
      *
      *   Rev 2.0   01 Mar 2001 13:03:12   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: KNR-KONSTANTEN FÜR KTY-71       (DRAT0351)    *
      *                 WETTBEWERBSTYP FÜR LEISTUNGSPLAN/WB           *
      *    --------------------------------------------------------   *
      *    VERSION   01                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    02.03.2001                                       *
      *    STAND:    02.03.2001                                       *
      *****************************************************************
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00264!02.03.01! 01 ! NEU                       !TH.WILLIG *
      * ASL00073!26.04.01! 01 ! Neue Konst. für FIAT-Liste!S. Walb   *
280504* ASL00183!28.05.04!2.3 ! MAP-MANAGERAUFPLAN HINZU  !KLEEMEIER *
040406* ASL00226!04.04.06!2.4 ! DIM7-MAP HINZU            !KLEEMEIER *
010207* AAS00681!01.02.07!2.5 ! JWB 2007 DVAG hinzu       !P.Duerr   *
110408* AAS00756!11.04.08!2.6 ! JWB 2008, Eliteclub, Allfi!P.Duerr   *
220710* ASL00351!22.07.10!2.7 ! div. WB eingefügt         !P.Duerr   *
      * AVB01347!23.11.15!2.8 ! neue KNR's wg. st.pfl.Prod!S. Walb   *
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
      *
      *   SCHLÜSSELWERTE
      *
       01  K-KNR-LPLAN-2001            PIC S9(04) COMP VALUE 01.
       01  K-KNR-WETTB-2001            PIC S9(04) COMP VALUE 02.
       01  K-KNR-FIATL-2001            PIC S9(04) COMP VALUE 03.
280504 01  K-KNR-MAP-2004              PIC S9(04) COMP VALUE 09.
040406 01  K-KNR-DIM7-MAP              PIC S9(04) COMP VALUE 20.
010207 01  K-KNR-WETTB-2007-DVAG       PIC S9(04) COMP VALUE 27.
010207 01  K-KNR-WETTB-2007-DVAG-WIEN  PIC S9(04) COMP VALUE 28.
010207 01  K-KNR-WETTB-2008-DVAG       PIC S9(04) COMP VALUE 31.
010207 01  K-KNR-WETTB-DVAG-ELITE      PIC S9(04) COMP VALUE 32.
010207 01  K-KNR-WETTB-DVAG-ELITES     PIC S9(04) COMP VALUE 33.
010207 01  K-KNR-WETTB-2008-DVAG-WIEN  PIC S9(04) COMP VALUE 34.
010207 01  K-KNR-WETTB-2008-DVAG-AIDA  PIC S9(04) COMP VALUE 35.
010207 01  K-KNR-WETTB-2008-DVAG-BEGN  PIC S9(04) COMP VALUE 36.
010207 01  K-KNR-WETTB-ELITE-PANN      PIC S9(04) COMP VALUE 37.
010207 01  K-KNR-WETTB-ELITE-BUDG      PIC S9(04) COMP VALUE 38.
010207 01  K-KNR-WETTB-2008-ALLFI      PIC S9(04) COMP VALUE 39.
010207 01  K-KNR-WETTB-2008-ALLFI-TEAM PIC S9(04) COMP VALUE 40.
220710 01  K-KNR-WETTB-1212-DVAG       PIC S9(04) COMP VALUE 54.
220710 01  K-KNR-WETTB-2412-DVAG       PIC S9(04) COMP VALUE 55.
220710 01  K-KNR-WETTB-D-CPROF-DVAG    PIC S9(04) COMP VALUE 58.
220710 01  K-KNR-WETTB-D-CPROF-WIEN    PIC S9(04) COMP VALUE 64.
010207 01  K-KNR-WETTB-WIEN-ELITE      PIC S9(04) COMP VALUE 68.
220710 01  K-KNR-WETTB-2010-DVAG-WB    PIC S9(04) COMP VALUE 70.
220710 01  K-KNR-WETTB-2010-ALLFI-WB   PIC S9(04) COMP VALUE 71.
220710 01  K-KNR-WETTB-2010-ALLFI-TB   PIC S9(04) COMP VALUE 72.
220710 01  K-KNR-WETTB-2010-ALLFI-TB-G PIC S9(04) COMP VALUE 73.
220710 01  K-KNR-WETTB-2010-DVAG-WB    PIC S9(04) COMP VALUE 70.
220710 01  K-KNR-WETTB-2010-ALLFI-WB   PIC S9(04) COMP VALUE 71.
220710 01  K-KNR-WETTB-2010-ALLFI-TB   PIC S9(04) COMP VALUE 72.
220710 01  K-KNR-WETTB-2010-ALLFI-TB-G PIC S9(04) COMP VALUE 73.
220710 01  K-KNR-WETTB-1212-WIEN       PIC S9(04) COMP VALUE 74.
220710 01  K-KNR-WETTB-2412-WIEN       PIC S9(04) COMP VALUE 75.
220710 01  K-KNR-WETTB-3612-WIEN       PIC S9(04) COMP VALUE 76.
220710 01  K-KNR-WETTB-2010-WIEN-WB    PIC S9(04) COMP VALUE 77.
220710 01  K-KNR-WETTB-COLUMBUS-DVAG   PIC S9(04) COMP VALUE 78.
220710 01  K-KNR-OLB-2010-DVAG         PIC S9(04) COMP VALUE 79.
220710 01  K-KNR-OLB-2010-WIEN         PIC S9(04) COMP VALUE 80.
220710 01  K-KNR-WETTB-2010-DVAG-KKUE  PIC S9(04) COMP VALUE 81.
220710 01  K-KNR-BOZN-DVAG             PIC S9(04) COMP VALUE 82.
220710 01  K-KNR-NP-1212-ALLFI         PIC S9(04) COMP VALUE 994.
220710 01  K-KNR-NP-2412-ALLFI         PIC S9(04) COMP VALUE 995.
220710 01  K-KNR-NP-3612-ALLFI         PIC S9(04) COMP VALUE 996.
220710*01  K-KNR-NP-1212-DVAG          PIC S9(04) COMP VALUE 998.
231115 01  K-KNR-NP-1212-DVAG          PIC S9(04) COMP VALUE 997.
220710 01  K-KNR-NP-2412-DVAG          PIC S9(04) COMP VALUE 998.
220710*01  K-KNR-NP-3612-DVAG          PIC S9(04) COMP VALUE 998.
231115 01  K-KNR-NP-3612-DVAG          PIC S9(04) COMP VALUE 999.
231115 01  K-KNR-NP-1212-ALLFI-USTPR   PIC S9(04) COMP VALUE 794.
231115 01  K-KNR-NP-2412-ALLFI-USTPR   PIC S9(04) COMP VALUE 795.
231115 01  K-KNR-NP-3612-ALLFI-USTPR   PIC S9(04) COMP VALUE 796.
231115 01  K-KNR-NP-1212-DVAG-USTPR    PIC S9(04) COMP VALUE 797.
231115 01  K-KNR-NP-2412-DVAG-USTPR    PIC S9(04) COMP VALUE 798.
231115 01  K-KNR-NP-3612-DVAG-USTPR    PIC S9(04) COMP VALUE 799.
      *
      ******** ENDE COPY-MEMBER KNR-KONSTANTEN (DRAT0351)      ********
      *


      *---------------------------------------------------------------*
      *    STANDARD FUER ALLE OBJEKTE UNTER PVCS VM
      *            DATUM      :  28.06.2013
      *            KATEGORIE  :  COPYSTRECKE
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT (C) 2013 ATLAS DIENSTLEISTUNGEN F. V. GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1753.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRPA1753.CPv  $
      *
      *   Rev 2.11   Mar 01 2017 15:10:38   t13ae37
      *APA02275: DAISY-Erweiterungen zu FlexN - PVW 2
      *
      *   Rev 2.10   Mar 01 2017 08:38:10   t13ae37
      *APA02275: DAISY-Erweiterungen zu FlexN - PVW 2
      *
      *   Rev 2.9   Mar 22 2016 10:00:18   t13ae37
      *APA02208: Anpassungen Korrekturprogramm USt - Übergabe Ust-J
      *ahr zur Abrechnung
      *
      *   Rev 2.8   Feb 11 2016 16:33:10   t13ae37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   REV 2.3   JUL 16 2015 15:09:32   T13AE37
      *APA01932: ABU / SCHNITTSTELLEN UND REFERENZ
      *
      *   REV 2.2   JAN 06 2014 17:43:30   T13AE37
      *APA01526: PROJEKT KALYPSO TEILPROJEKT 5 (PROVISIONSVERTEILUN
      *G): SST-ABRECHNUNG
      *
      *   REV 2.1   DEC 20 2013 10:12:06   T13AE37
      *APA01526: PROJEKT KALYPSO TEILPROJEKT 5 (PROVISIONSVERTEILUN
      *G): SST-ABRECHNUNG
      *
      *   REV 2.0   JUN 28 2013 13:03:42   T13AE37
      *ARCHIVERSTELLUNG
      *
      ****************************************************************
      * COPYBOOK FÜR SATZBESCHREIBUNG ZUR BEWEGUNGSSCHNITTSTELLE     *
      * DER VB-ABRECHNUNG                                            *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!28.06.13!2.0 ! NEU                       ! BUCH     *
      * APA01526!06.01.13!2.2 ! STRASSE UND ORT 30-STELLIG! BUCH     *
      * APA01932!09.04.15!2.3 ! EINFÜHRUNG UMSATZSTEUER   ! BUCH     *
      * APA01932!16.07.15!    ! ZIELSTATUS ERGÄNZT        ! BUCH     *
      * APA01932!07.10.15!    ! ZIELSTATUS IN STRUKTUR    ! BUCH     *
      * APA02208!29.01.16!2.4 ! PMPT-ID ERGÄNZT           ! BUCH     *
      * APA02208!22.03.16!2.5 ! STEUERJAHR ERGÄNZT        ! BUCH     *
      * APA02275!28.02.17!2.11! FlexN ergänzt             ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
             08 BEWEGUNGSSATZ.
                10 SATZ-KZ                     PIC X.
                10 PGEB-GES                    PIC S9(04) COMP.
      ***    PRODUKTSCHLÜSSEL DES HAUPTTARIFS
                10 PROD-PROD-SCHL-SORT         PIC S9(04) COMP.
                10 VNR-SORT                    PIC  X(15).
 
      * KTY 386 (SIEHE UNTEN)
                10 TARIF-KZ                    PIC S9(04) COMP.
                10 TARIF-BEZEICHNUNG           PIC  X(15).
                10 KUNDENNAME                  PIC  X(20).
                10 VP-NAME                     PIC  X(20).
                10 DGV                         PIC S9(04) COMP.
                10 AVERM                       PIC  9(07) COMP.
                10 PRODART                     PIC  9(04) COMP.
      * TT.MM.JJJJ
                10 HAFTZ-VON                   PIC  X(10).
      * TT.MM.JJJJ
                10 HAFTZ-BIS                   PIC  X(10).
                10 PRO-RATA-SUMME              PIC S9(09)V99 COMP-3.
                10 PRO-RATA-SUMME-GES          PIC S9(09)V99 COMP-3.
                10 VM-GESCHFT-KZ               PIC  X(01).
                10 PPFL-SUMME                  PIC S9(09)V99 COMP-3.
      * TT.MM.JJJJ
                10 FAELLIG-VON                 PIC  X(10).
                10 PDPT-ID                     PIC S9(15) COMP-3.
 
                10 VNR                         PIC  X(15).
                10 VNR-AML REDEFINES VNR.
                   15 VNR-FHRND-NULL           PIC  X(01).
                   15 VNR-SPARTE               PIC  X(01).
                   15 VNR-ORD-NR               PIC  X(07).
                   15 VNR-UNTER-NR             PIC  X(03).
                   15 VNR-SCHICHT-NR           PIC  X(03).
 
                10 RFOLGE-NR                   PIC S9(04) COMP.
                10 HAFTZEIT-KZ                 PIC  X(02).
      * TIMESTAMP
                10 FIRST-INSERT                PIC  X(26).
      * TT.MM.JJJJ
                10 MA-ABR-DAT                  PIC  X(10).
      * KTY 383
                10 DYN-KZ-1                    PIC S9(04) COMP.
      * KTY 384
                10 DYN-KZ-2                    PIC S9(04) COMP.
      * TT.MM.JJJJ
                10 POL-DAT                     PIC  X(10).
                10 PRO-RATA-FAK-Z              PIC S9(04) COMP.
                10 PRO-RATA-FAK-N              PIC S9(04) COMP.
                10 PROD-PROD-SCHL              PIC S9(04) COMP.
                10 PROV-SCHL                   PIC  X(05).
      * KTY 381
                10 RAB-STF                     PIC S9(04) COMP.
                10 RWERT-SUMME                 PIC S9(09)V99 COMP-3.
                10 SAMMEL-NR                   PIC S9(09) COMP.
                10 BESVV                       PIC  9(01).
                10 PGV                         PIC S9(04) COMP.
                10 BUZ-TARIF                   PIC S9(04) COMP.
      * VVG-KZ LAUT NOTES IN HAFTZEIT-KZ
                10 VERTRAGSDAUER               PIC  9(04).
      * KTY 100
                10 ZAHLWEISE                   PIC S9(04) COMP.
                10 INKASSOSTAND                PIC  X(10).
                10 VN-STRASSE                  PIC  X(30).
                10 VN-PLZ                      PIC  X(05).
                10 VN-ORT                      PIC  X(30).
                10 PG-PRODART                  PIC S9(04) COMP.
                10 EINH-BEW                    PIC S9(06)V9 COMP-3.
      * BEWEGUNGS-HERKUNFT (1) + BEWEGUNGS-ART (6)
                10 BEW-SCHL                    PIC  X(07).
      * NEUE UMSATZSTEUER ATTRIBUTE
090415          10 UST-KORREKTUR-KZ            PIC  X(01).
160715*         10 KORREKTUR-ZIEL-STATUS       PIC  X(01).
090415          10 UST-SATZ                    PIC  9(02)V99 COMP-3.
090415          10 PG-RECH-NR                  PIC  X(12).
090415          10 FAELLIG-BIS                 PIC  X(10).
290116          10 PMPT-ID                     PIC S9(15) COMP-3.
220316          10 STEUERJAHR                  PIC  X(04).
280217          10 VSP-FN-REL                  PIC  X(01).
280217          10 VSP-FN-FP-KZ                PIC  X(01).
280217          10 VSP-KOMBI-NR                PIC S9(05) COMP.
280217          10 VSP-KOMBI-BZ                PIC  X(03).
280217          10 VSP-FN-BER                  PIC  X(01).
 
      * STRUKTUR ATTRIBUTE
                10 ANZAHL-PROV-EMP             PIC S9(04) COMP.
                10 SATZLAENGE                  PIC S9(04) COMP.
                10 STRUKTUR     OCCURS 20.
090415*            15 PROVAUS                  PIC S9(09)V99 COMP-3.
090415             15 PROV-BETRAG-AUS          PIC S9(09)V99 COMP-3.
090415             15 UST-BETRAG-AUS           PIC S9(09)V99 COMP-3.
090415*            15 PROVRUECK                PIC S9(09)V99 COMP-3.
090415             15 PROV-BETRAG-RUECK        PIC S9(09)V99 COMP-3.
090415             15 UST-BETRAG-RUECK         PIC S9(09)V99 COMP-3.
                   15 BUCH-RELEVANT            PIC  X(01).
                   15 EMP-TYP                  PIC S9(04) COMP.
                   15 DIFF-PROV-STF            PIC S9(04) COMP.
                   15 ST-NR                    PIC S9(07) COMP.
                   15 EINH-EMP                 PIC S9(06)V9 COMP-3.
                   15 EIGEN-KZ                 PIC  X(01).
090415             15 UST-BERECH-KZ            PIC  X(01).
090415             15 UST-ABR-NR               PIC  X(15).
071015             15 KORREKTUR-ZIEL-STATUS    PIC  X(01).
      *---------------------------------------------------------------*
 
 
 
 
 
 
 


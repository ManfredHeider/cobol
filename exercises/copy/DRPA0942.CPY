      * T01JM2E   Stand: 06/01/99    15:06:10
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0942.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0942.CPv  $
      *
      *   Rev 2.1   May 27 1999 09:53:22   T01JM2E
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:16:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0942                                      *
      *    --------------------------------------------------------   *
      *    Arbeitssatz MA-Abrechnung                                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    28.04.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!28.04.98! 01 ! NEU                       !D.WILHELM *
      *         !28.10.98!1.1 ! PDTA-ID                   !D.WILHELM *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       30 DRPA0942-DATEN.
        35 DRPA0942-MA-ARBEITSSATZ.
          37 DRPA0942-STAMM.
             40 PDPT-ID                     PIC S9(015)V USAGE COMP-3.
281098       40 PDTA-ID                     PIC S9(015)V USAGE COMP-3.
             40 PGEB-GES                    PIC S9(004)  USAGE COMP.
             40 PVER-VNR                    PIC  X(015).
270898       40 PAMS-ABR-SORT-NR            PIC  X(015).
             40 PROD-PROD-SCHL              PIC S9(004)  USAGE COMP.
             40 PMPT-FAELLIG-VON            PIC  X(010).
             40 PRINTE                      PIC S9(015)V USAGE COMP-3.
             40 PVER-VERSION                PIC S9(004)  USAGE COMP.
             40 PVER-KUNDE-NAME             PIC  X(025).
             40 PVER-KUNDE-VNAME            PIC  X(020).
             40 PMBE-RFOLGE-NR              PIC S9(004)  USAGE COMP.
             40 PMPT-ID                     PIC S9(015)V USAGE COMP-3.
             40 PMPT-VERSION                PIC S9(004)  USAGE COMP.
             40 PMPT-GSMT-PROV            PIC S9(009)V9(2) USAGE COMP-3.
             40 PMPT-PPFL-BEITRAG         PIC S9(009)V9(2) USAGE COMP-3.
             40 PVPT-VT-PRIO                PIC S9(004)  USAGE COMP.
             40 PEWA-ID                     PIC S9(015)V USAGE COMP-3.
             40 PDPT-PRODART                PIC S9(004)  USAGE COMP.
             40 PDPT-DGV                    PIC S9(004)  USAGE COMP.
             40 PMPT-EINH                 PIC S9(006)V9(2) USAGE COMP-3.
             40 KNR-BERCH-WAEHRUNG          PIC S9(004)  USAGE COMP.
             40 KNR-KZ-EH-MAX               PIC S9(004)  USAGE COMP.
             40 KNR-BUCHUNGS-STAND          PIC S9(004)  USAGE COMP.
             40 PEST-AVERM                  PIC S9(009)  USAGE COMP.
             40 PEST-VM-GESCHFT-KZ          PIC  X(1).
             40 MAX-INDEX-PE                PIC S9(004)  USAGE COMP.
             40 PE-STRUKTUR-DATEN-LEN       PIC S9(004)  USAGE COMP.
      *      40 MAX-USED-PAMS-STRUKTUR      PIC S9(04)   BINARY.
          37 PROV-EMPF              OCCURS 20.
             40 PEBD-ST-NR              PIC S9(009) BINARY.
             40 PEBD-DIFF-PROV-STF      PIC S9(004) BINARY.
             40 KNR-EMP-TYP             PIC S9(004) BINARY.
             40 PDPE-BUCH-RELEVANT      PIC X.
             40 PDPE-EINH               PIC S9(6)V99    PACKED-DECIMAL.
             40 PDPE-NT-PBTRG-BRWG      PIC S9(009)V99  PACKED-DECIMAL.
             40 PDPE-PROV-RCK-BRWG      PIC S9(009)V99  PACKED-DECIMAL.
             40 KNR-AUSZ-WAEHRUNG       PIC S9(004)     BINARY.
             40 PDPE-NT-PBTRG-AZWG      PIC S9(009)V99  PACKED-DECIMAL.
             40 PDPE-PROV-RCK-AZWG      PIC S9(009)V99  PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER DRPA0942 *****************************

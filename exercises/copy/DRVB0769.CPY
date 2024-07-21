      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.04.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0769.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRVB0769.CPv  $
      *
      *   Rev 2.7   Jan 13 2020 16:34:44   t13ae30
      *AAS01307: 1. Erweiterung Cursor OZPs ZJVB0013 und ZJVB0014
      *
      *   Rev 2.6   Oct 30 2019 12:35:48   t13ae30
      *AAS01307: Anpassung Bedingungen AS-Club mit besonderen Leist
      *ungen und AS-Club (VB-System)
      *
      *   Rev 2.5   Dec 22 2010 16:52:50   t13ae30
      *AAS00928: Änderung AS-Club-Bedingungen (Investment)
      *
      *   Rev 2.4   Nov 25 2009 13:50:40   t13ae30
      *AAS00841: Kapitäns-Club 2010
      *
      *   Rev 2.3   Jan 09 2009 17:05:12   t13ae30
      *AAS00779: Allfinanz "Champions-Club"
      *
      *   Rev 2.2   Jan 17 2007 13:32:58   t13ae30
      *AAS00670: Änderung der AS-Club Bedingungen
      *
      *   Rev 2.1   Jul 07 2005 11:56:28   t13ae30
      *AVD00009: Projekt VBI: SST Clubprüfung
      *
      *   Rev 2.0   Apr 04 2005 12:26:50   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0769                                     *
      *    FUNKTION ERMITTELN CLUB-BEDINGUNGEN ZU ZXVB0043           *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    04.04.2005                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00009!04.04.05! 01 ! NEUERSTELLUNG             ! WILLIG   *
      * AAS00670!08.01.07! 02 ! ÄNDERUNG DER AS-CLUB-BED. ! WILLIG   *
      * AAS00779!01.09.08! 03 ! ERW. WEGEN CHAPIONS-CLUB. ! WILLIG   *
      * AAS00841!07.07.09! 04 ! UMSTELLUNG KAPITÄNS-CLUB  ! WILLIG   *
      *         !        !    ! BEDINGUNGEN               !          *
      * AAS00928!06.12.10! 05 ! ÄNDERUNG DER AS-CLUB-BED. ! WILLIG   *
      * VBL-964 !11.09.19! 06 ! NEUE CLUBBEDINGUNGEN      ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0769                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0769.
          05 PI-DRVB0769-PROGRAMM           PIC  X(008).
          05 PI-DRVB0769-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0769-KTY-NR             PIC S9(004)    COMP.
          05 PI-DRVB0769-KNR-NR             PIC S9(004)    COMP.
          05 PI-DRVB0769-KTY-GESELL-TYP     PIC S9(004)    COMP.
          05 PI-DRVB0769-KNR-GESELL-TYP     PIC S9(004)    COMP.
110919    05 PI-DRVB0769-CLUB-KRITERIUM     PIC S9(004)    COMP.
110919    05 PI-DRVB0769-CLBED-DAT-BIS      PIC  X(10).

      *
       01 PO-DRVB0769.
          05 PO-DRVB0769-RC                 PIC  X(005).
             88 PO-DRVB0769-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0769-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB0769-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0769-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0769-RC-EOF                       VALUE 'EOF'.

          05 PO-DRVB0769-DATEN.
             10 PO-DRVB0769-KTY-CLUB-TYP       PIC S9(4) COMP.
             10 PO-DRVB0769-KNR-CLUB-TYP       PIC S9(4) COMP.
             10 PO-DRVB0769-KTY-GESELL-TYP     PIC S9(4) COMP.
             10 PO-DRVB0769-KNR-GESELL-TYP     PIC S9(4) COMP.
             10 PO-DRVB0769-CLUB-KRITERIUM     PIC S9(4) COMP.
             10 PO-DRVB0769-CLUB-MS-NR         PIC S9(4) COMP.
             10 PO-DRVB0769-CLUB-VBP-NR        PIC S9(4) COMP.
             10 PO-DRVB0769-CLUB-ANZ-KDN       PIC S9(4) COMP.
             10 PO-DRVB0769-ANZ-KDN-2-PLUS     PIC S9(4) COMP.
             10 PO-DRVB0769-NET-NEU-EIG-12MON  PIC S9(9)V9(1) COMP-3.
080107       10 PO-DRVB0769-NET-EIG-GES-12MON  PIC S9(9)V9(1) COMP-3.
             10 PO-DRVB0769-MAX-EQF-EIG        PIC S9(3)V9(2) COMP-3.
             10 PO-DRVB0769-BEITRAG-VOL        PIC S9(9)V9(2) COMP-3.
             10 PO-DRVB0769-VB-SEIT-MIN-MON    PIC S9(4) COMP.
             10 PO-DRVB0769-AS-SEIT-MIN-MON    PIC S9(4) COMP.
             10 PO-DRVB0769-NET-EIG-GES-36MO   PIC S9(9)V9(1) COMP-3.
             10 PO-DRVB0769-NET-GES-12MO       PIC S9(9)V9(1) COMP-3.
             10 PO-DRVB0769-KTY-TYP-BEN-CLUB   PIC S9(4) COMP.
             10 PO-DRVB0769-KNR-TYP-BEN-CLUB   PIC S9(4) COMP.
             10 PO-DRVB0769-KTY-TYP-ENDE-CLUB  PIC S9(4) COMP.
             10 PO-DRVB0769-KNR-TYP-ENDE-CLUB  PIC S9(4) COMP.
             10 PO-DRVB0769-ED-ANZ-SOLL        PIC S9(4) COMP.
             10 PO-DRVB0769-ANZ-PART-KAP       PIC S9(4) COMP.
             10 PO-DRVB0769-ANZ-AKT-STRUK-ADM  PIC S9(4) COMP.
             10 PO-DRVB0769-CLBED-LAST-UPDATE  PIC X(26).
             10 PO-DRVB0769-CLBED-DAT-VON      PIC X(10).
             10 PO-DRVB0769-CLBED-DAT-BIS      PIC X(10).
010908       10 PO-DRVB0769-ANZ-HAUPTBEPART    PIC S9(4) COMP.
010908       10 PO-DRVB0769-ANZ-NEU-DIR-PART   PIC S9(4) COMP.
010908       10 PO-DRVB0769-MAX-EQF-GES        PIC S9(3)V9(2) COMP-3.
010908       10 PO-DRVB0769-NET-NEU-GES-12MON  PIC S9(9)V9(1) COMP-3.
070709       10 PO-DRVB0769-ANZ-STERNE         PIC S9(4) COMP.
061210       10 PO-DRVB0769-ANT-INVEST         PIC S9(4) COMP.
110919       10 PO-DRVB0769-FAKTOR-1           PIC S9(03)V9(02) COMP-3.
110919       10 PO-DRVB0769-FAKTOR-2           PIC S9(03)V9(02) COMP-3.
110919       10 PO-DRVB0769-FAKTOR-GES         PIC S9(03)V9(02) COMP-3.
110919       10 PO-DRVB0769-BQF                PIC S9(03)V9(02) COMP-3.
110919       10 PO-DRVB0769-BED-ERF-MIN-MON    PIC S9(04)       BINARY.
110919       10 PO-DRVB0769-KAP-SEIT-MIN-MON   PIC S9(04)       BINARY.
110919       10 PO-DRVB0769-ED-MITGL           PIC  X(01).
110919       10 PO-DRVB0769-FAK-1-BASISWERT    PIC S9(09)V9(01) COMP-3.
110919       10 PO-DRVB0769-FAK-2-BASISWERT    PIC S9(09)V9(01) COMP-3.
110919       10 PO-DRVB0769-GES-EH-EIG-MAX     PIC S9(09)V9(01) COMP-3.























      *
      ******** Ende Copy-Member DRVB0769 *****************************



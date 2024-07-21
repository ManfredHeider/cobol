      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0965.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0965.CPv  $
      *
      *   Rev 2.2   13 Oct 1999 09:32:30   T01JM2E
      *FPA00095 - OCCURS auf 50 erhöht
      *
      *   Rev 2.1   May 27 1999 10:17:00   T01JM2E
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:19:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0965                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF:  ARBEITSSATZPAKET ERMITTELN               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    05.06.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!05.06.98! 01 ! NEU                       !D.WILHELM *
      * AFPA0012!28.10.98! 1.1! PDTA-ID                   !D.WILHELM *
      * FPA00095!13.10.99! 2.2! Erhöhung OCCURS auf 50    !D.WILHELM *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0965                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0965.
          05 PI-DRPA0965-UFKT-CODE               PIC  9(002).
          05 PI-DRPA0965-PROGRAMM                PIC  X(008).
          05 PI-DRPA0965-KEY.
             10 PI-DRPA0965-PRINTE               PIC S9(015)  BINARY.
             10 PI-DRPA0965-PMPT-FAELLIG-VON     PIC  X(010).
      *      10 PI-DRPA0965-PDPT-ID          PIC S9(015)V USAGE COMP-3.
      *
       01 PO-DRPA0965.
          05 PO-DRPA0965-RC                      PIC  X(003).
             88 PO-DRPA0965-RC-OK                     VALUE 'OK '.
             88 PO-DRPA0965-RC-NOT-FOUND              VALUE 'NFD'.
             88 PO-DRPA0965-RC-EOF                    VALUE 'EOF'.
          05 PO-DRPA0965-MAX-IND            PIC S9(04) USAGE COMP.
          05 PO-DRPA0965-KEY.
             10 PO-DRPA0965-PRINTE               PIC S9(015)  BINARY.
             10 PO-DRPA0965-PMPT-FAELLIG-VON     PIC  X(010).
      *      10 PO-DRPA0965-PDPT-ID          PIC S9(015)V USAGE COMP-3.
          05 PO-DRPA0965-DATEN.
131099       10 PO-DRPA0965-MA-ARBEITSSATZ OCCURS 50.
                20 PDPT-ID               PIC S9(015)V USAGE COMP-3.
281098          20 PDTA-ID               PIC S9(015)V USAGE COMP-3.
                20 PGEB-GES              PIC S9(004)  USAGE COMP.
                20 PVER-VNR              PIC  X(015).
                20 PAMS-ABR-SORT-NR      PIC  X(015).
                20 PROD-PROD-SCHL        PIC S9(004)  USAGE COMP.
                20 PMPT-FAELLIG-VON      PIC  X(010).
                20 PRINTE                PIC S9(015)V USAGE COMP-3.
                20 PVER-VERSION          PIC S9(004)  USAGE COMP.
                20 PVER-KUNDE-NAME       PIC  X(025).
                20 PVER-KUNDE-VNAME      PIC  X(020).
                20 PMBE-RFOLGE-NR        PIC S9(004)  USAGE COMP.
                20 PMPT-ID               PIC S9(015)V USAGE COMP-3.
                20 PMPT-VERSION          PIC S9(004)  USAGE COMP.
                20 PMPT-GSMT-PROV        PIC S9(009)V9(2) USAGE COMP-3.
                20 PMPT-PPFL-BEITRAG     PIC S9(009)V9(2) USAGE COMP-3.
                20 PVPT-VT-PRIO          PIC S9(004)  USAGE COMP.
                20 PEWA-ID               PIC S9(015)V USAGE COMP-3.
                20 PDPT-PRODART          PIC S9(004)  USAGE COMP.
                20 PDPT-DGV              PIC S9(004)  USAGE COMP.
                20 PMPT-EINH             PIC S9(006)V9(2) USAGE COMP-3.
                20 KNR-BERCH-WAEHRUNG    PIC S9(004)  USAGE COMP.
                20 KNR-KZ-EH-MAX         PIC S9(004)  USAGE COMP.
                20 KNR-BUCHUNGS-STAND    PIC S9(004)  USAGE COMP.
                20 PEST-AVERM            PIC S9(009)  USAGE COMP.
                20 PEST-VM-GESCHFT-KZ    PIC  X(1).
                20 MAX-INDEX-PE          PIC S9(004)  USAGE COMP.
                20 PE-STRUKTUR-DATEN-LEN PIC S9(004)  USAGE COMP.
      *         20 MAX-USED-PAMS-STRUKTUR PIC S9(04) BINARY.
                20 PROV-EMPF              OCCURS 20.
                   30 PEBD-ST-NR         PIC S9(009) BINARY.
                   30 PEBD-DIFF-PROV-STF PIC S9(004) BINARY.
                   30 KNR-EMP-TYP        PIC S9(004) BINARY.
                   30 PDPE-BUCH-RELEVANT PIC X.
                   30 PDPE-EINH          PIC S9(6)V99    PACKED-DECIMAL.
                   30 PDPE-NT-PBTRG-BRWG PIC S9(009)V99  PACKED-DECIMAL.
                   30 PDPE-PROV-RCK-BRWG PIC S9(009)V99  PACKED-DECIMAL.
                   30 KNR-AUSZ-WAEHRUNG  PIC S9(004)     BINARY.
                   30 PDPE-NT-PBTRG-AZWG PIC S9(009)V99  PACKED-DECIMAL.
                   30 PDPE-PROV-RCK-AZWG PIC S9(009)V99  PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER DRPA0965 *****************************

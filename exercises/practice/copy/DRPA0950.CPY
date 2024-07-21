      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0950.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0950.CPv  $
      *
      *   Rev 2.1   Jun 02 1999 13:51:58   T13EX44
      * 
      *
      *   Rev 2.0   May 26 1999 16:58:12   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:17:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0950  erwartete Ergebnisse MA-Schnittst  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Parent                                          *
      *    DATUM:     6.05.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Daisy 1 ! 6.05.98! 01 ! Neuerstellung             ! Parent   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *---------------------------------------------------------------*
         05 DRPA0950-ERW-ARBEITSSATZ.
            10 TESTFALL-NR            PIC  9(008).
            10 PRINTE                 PIC S9(015)V  USAGE COMP-3.
            10 PVER-VERSION           PIC S9(004)   USAGE COMP.
            10 PVER-VNR               PIC  X(015).
            10 PVER-KUNDE-NAME        PIC  X(025).
            10 PVER-KUNDE-VNAME       PIC  X(020).
            10 PDPT-DGV               PIC S9(004)   COMP.
            10 PEST-AVERM             PIC S9(009)   USAGE COMP.
            10 PDPT-PRODART           PIC S9(004)   USAGE COMP.
            10 PROD-PROD-SCHL         PIC S9(004)   USAGE COMP.
            10 PMPT-EINH              PIC S9(6)V9(2)
                                                    USAGE COMP-3.
            10 PEST-VM-GESCHFT-KZ     PIC X(1).
            10 PMPT-PPFL-BEITRAG      PIC S9(9)V9(2)
                                                    USAGE COMP-3.
            10 PMPT-FAELLIG-VON       PIC X(10).
            10 PMBE-RFOLGE-NR         PIC S9(4)     USAGE COMP.
            10 PMPT-ID                PIC S9(15)V   USAGE COMP-3.
            10 PDPE-NT-PBTRG-AZWG     PIC S9(9)V9(2)
                                                    USAGE COMP-3.
            10 PDPE-PROV-RCK-AZWG     PIC S9(9)V9(2)
                                                    USAGE COMP-3.
            10 KNR-AUSZ-WAEHRUNG      PIC S9(4)     USAGE COMP.
            10 PDPE-EINH              PIC S9(6)V9(2)
                                                    USAGE COMP-3.
            10 PEBD-ST-NR             PIC S9(9)     USAGE COMP.
            10 PEBD-DIFF-PROV-STF     PIC S9(4)     USAGE COMP.
            10 KNR-EMP-TYP            PIC S9(4)     USAGE COMP.
            10 PDPT-ID                PIC S9(15)V   USAGE COMP-3.
            10 PDPE-POSITIONS-NR      PIC S9(4)     USAGE COMP.
            10 KORR-KEZ               PIC X(1).
            10 KNR-KZ-EH-MAX          PIC S9(4)     USAGE COMP.
            10 PDPE-NT-PBTRG-BRWG     PIC S9(9)V99  USAGE COMP-3.
            10 PDPE-PROV-RCK-BRWG     PIC S9(9)V99  USAGE COMP-3.
            10 KNR-BUCHUNGS-STAND     PIC S9(4)     USAGE COMP.
      *


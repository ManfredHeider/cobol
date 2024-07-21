      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0434.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0434.CPv  $
      *
      *   Rev 2.1   Sep 28 2017 14:53:58   T13AE34
      *ARZ00049: Reaktivieren drpa0434 drpa0435
      *
      *   Rev 2.0   Feb 27 1999 14:29:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 15:07:44   T01JM2S
      *APAA0032 - PEW-RELEASE 2.0
      *
      *   Rev 1.0   23 Jul 1997 12:36:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0434                                      *
      *    --------------------------------------------------------   *
      *    PEW-ANTRAGS-DATEN                                          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    11.08.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.08.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * AFPA0012!25.05.98!    ! VERSION 2.8 DER PEWA-     ! WOLFF    *
      *         !        !    ! TABELLE EINGEBAUT         !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN DER PEW-ANTRAGS-DATEN       *
      *                                                              *
      ****************************************************************
      *
             30 PEW-ANTRAGS-DATEN.
                35 PEWA-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PGEB-GES                         PIC S9(004) BINARY.
                35 PEWA-PIN                         PIC S9(009) BINARY.
                35 PEWA-VNR                         PIC  X(015).
                35 PEWA-VOR-VNR-KZ                  PIC  X(001).
                35 PEWA-KUNDE-NAME                  PIC  X(025).
                35 PEWA-KUNDE-VNAME                 PIC  X(020).
                35 KNR-WAEHRUNG                     PIC S9(004) BINARY.
                35 KNR-ZAHLWEISE                    PIC S9(004) BINARY.
                35 PEWA-DIR                         PIC S9(004) BINARY.
                35 PEWA-AVERM                       PIC S9(009) BINARY.
                35 PEWA-VM                          PIC S9(009) BINARY.
                35 PEWA-BEGINN                      PIC  X(010).
                35 PEWA-AN-AUFN-DAT                 PIC  X(010).
                35 PEWA-STRUK-DAT                   PIC  X(010).
                35 PEWA-AN-ARB-DAT                  PIC  X(010).
                35 PEWA-PG-WV-DAT                   PIC  X(010).
                35 PEWA-PG-MELD-DAT                 PIC  X(010).
                35 PDTA-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PEWA-VSTAT                       PIC  X(004).
                35 PEWA-VSTAT-UNR                   PIC S9(004) BINARY.
                35 PEWA-FEHLER-ZAHL                 PIC S9(004) BINARY.
                35 KNR-PEWA-ZUSTAND                 PIC S9(004) BINARY.
                35 KNR-STORNO-KZ                    PIC S9(004) BINARY.
                35 PEWA-STORNO-DAT                  PIC  X(010).
                35 KNR-RUECKMELD-STAT               PIC S9(004) BINARY.
                35 PEWA-RUECKMELD-DAT               PIC  X(010).
                35 PEWA-ANZ-MELD-PG                 PIC S9(004) BINARY.
                35 PEWA-PEST-VSTAT                  PIC  X(004).
      *
      ******* ENDE COPY-MEMBER **************************************


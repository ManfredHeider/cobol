      *---------------------------------------------------------------*
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0410.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0410.CPv  $
      *
      *   Rev 2.2   Aug 06 2013 10:19:40   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben
      *
      *   Rev 2.1   Jul 24 2003 16:32:48   t13ex44
      *APA00245: NEU: PVPT-KDBETR-REDUZ
      *
      *   Rev 2.0   May 27 1999 13:29:36   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:33:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0410                                      *
      *    --------------------------------------------------------   *
      *    PVPT-DATEN                                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    22.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.10.97! 01 ! NEU                       ! WOLFF    *
      * AFPA0012!03.12.97!    ! NEU: KNR-AKZ-PVPT         ! PRIGGEN  *
      * APA00245!24.07.03! 2.1! NEU: PVPT-KDBETR-REDUZ    ! SCHEIDEL *
      * APA01526!08.04.13! 2.2! ERGÄNZUNGEN FÜR KALYPSO   ! BÖTTCHER *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *                                                              *
      ****************************************************************
      *
             30 PVPT-DATEN.
                35 PVPT-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PVPT-VERSION                     PIC S9(004) BINARY.
                35 PVPT-F-GUELTIG-AB                PIC  X(010).
                35 PVPT-F-GUELTIG-BIS               PIC  X(010).
                35 KNR-PVPT-ART                     PIC S9(004) BINARY.
                35 PRINTE                           PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PVER-VERSION                     PIC S9(004) BINARY.
                35 PGEB-GES                         PIC S9(004) BINARY.
                35 PROD-PROD-SCHL                   PIC S9(004) BINARY.
                35 PVPT-VT-PRIO                     PIC S9(004) BINARY.
                35 PVPT-PPFL-BEGINN                 PIC  X(010).
                35 PVPT-PPFL-ENDE                   PIC  X(010).
                35 PVPT-BEWERT-BETRAG               PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
                35 PVPT-PTEIL-BEGINN                PIC  X(010).
                35 PVPT-PTEIL-ABLAUF                PIC  X(010).
                35 PVPT-LAUFZEIT                    PIC S9(004) BINARY.
                35 PRKO-KOND-NR-EE                  PIC S9(004) BINARY.
                35 PVPT-EE-ENTSCHEID                PIC  X(001).
                35 KNR-EVS-GRUND                    PIC S9(004) BINARY.
                35 KNR-WAEHRUNG                     PIC S9(004) BINARY.
                35 PEWA-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PVPT-PIN                         PIC S9(009) BINARY.
                35 PEST-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
240703          35 PVPT-KDBETR-REDUZ                PIC X(001).
080413          35 PVPT-HAUPTTARIF-KZ               PIC X(001).
080413          35 PVPT-BUZTAR                      PIC S9(004) BINARY.
080413*
031297*            NAME BEGINNT NICHT MIT PVPT, UM DAMIT ANZUDEUTEN
031297*            DASS DIESES FELD NICHT ZU SPEICHERN IST
031297          35 KNR-AKZ-PVPT                     PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************

      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0094.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0094.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 15 1995 16:34:40   T13EX11
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0091, SCHNITTSTELLE UPAT0011             *
      *                 ERMITTLUNG STATISTIK-MONAT (AKT. OD ZU DATUM)*
      *    --------------------------------------------------------  *
      *    ERSTELLT: SCHÖLLHAMMER                                    *
      *    DATUM:    24.07.1995                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0226!24.07.95!    ! NEU                       ! SCHÖLLH. *
      *         !07.11.95! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   BESCHREIBUNG DER SCHNITTSTELLE:
      *
      *   PI-UPAT0011
      *
      *     FKT-AKT-STAT-MON      DATUM DES AKTUELLSTEN STATISTIK-
      *                              MONATS ERM. (LT. TABLES IN DB2)
      *     FKT-STAT-MON-ZU-DAT   DATUM DES STATISTIK-MONATS ERM.,
      *                              DER, BEZOGEN AUF DAS ÜBERGEBENE
      *                              DATUM, AKTUELL WAR
      *
      *     ZU-DATUM-ISO          NUR BEI FKT-STAT-MON-ZU-DAT ZU FÜLLEN
      *
      *   PO-UPAT0011
      *
      *     RC-OK               NORMALE VERARBEITUNG
      *     RC-KEIN-STAT-MONAT  MIT DEM VERWENDETEN ALGORYTHMUS KEIN
      *                            STATISTIK-MONAT ZU ERMITTELN
      *                            (NUR BEI FKT-STAT-MON-ZU-DAT MÖGLICH)
      *
      *     STAT-MONAT-ISO      ERMITTELTER STATISTIK-MONAT

      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-UPAT0011.
           05  PI-UPAT0011-FKT                PIC  X(02).
               88  PI-AT11-FKT-AKT-STAT-MON              VALUE 'AS'.
               88  PI-AT11-FKT-STAT-MON-ZU-DAT           VALUE 'SD'.
           05  PI-UPAT0011-ZU-DATUM-ISO       PIC  X(10).
           05  FILLER                         PIC  X(20).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-UPAT0011.
           05  PO-UPAT0011-RC                 PIC  X(02).
               88  PO-AT11-RC-OK                         VALUE 'OK'.
               88  PO-AT11-RC-KEIN-STAT-MONAT            VALUE 'KS'.
           05  PO-UPAT0011-STAT-MONAT-ISO     PIC  X(10).
           05  FILLER                         PIC  X(20).

      ******** ENDE COPY-MEMBER ************************************


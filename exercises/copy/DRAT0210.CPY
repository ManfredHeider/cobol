      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0210.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRAT0210.CPv  $
      *
      *   Rev 2.2   Jul 15 1999 10:08:40   T13AE34
      *AAN00216-PEW-Zustand
      *
      *   Rev 2.1   Mar 24 1999 11:58:52   T13EX22
      *Fuer PGV 0100 nur PEW ermitteln
      *
      *   Rev 2.0   Feb 27 1999 11:57:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   05 Aug 1998 18:34:16   T13EX22
      *APAA0032 - PEW Release 2.0
      *
      *   Rev 1.0   03 Dec 1997 15:46:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0210  KONSTANTEN FÜR     KTY = 148       *
      *                           Zustände eines Antrages            *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    09.12.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.12.97!01.01! NEUERSTELLUNG            ! MUND     *
      *         !        !     !                          !          *
      * PEW-REL.!04.06.98!01.02! NEU:                     ! WOLFF    *
      * 2.0     !        !     !                          !          *
      *         !        !     !                          !          *
      * PEW-REL.!05.06.98!01.03! NEU:                     ! MUND     *
      * 2.0     !        !     !                          !          *
      *         !        !     !                          !          *
      * APA00012!22.03.99!10.04!K-KNR-NEUTRAL-NICHT-BEARB ! MUND     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-ERFASST-AN-PG        PIC S9(004) BINARY VALUE 02.
040698     05 K-KNR-ANTRAG-FEHLERHAFT    PIC S9(004) BINARY VALUE 04.
040698     05 K-KNR-ANTRAG-WARTEND       PIC S9(004) BINARY VALUE 06.
050698     05 K-KNR-ANTRAG-AUFGEGEBEN    PIC S9(004) BINARY VALUE 08.
050698     05 K-KNR-ANTRAG-NICHT-ZUST    PIC S9(004) BINARY VALUE 10.
           05 K-KNR-PROV-BEARB           PIC S9(004) BINARY VALUE 12.
           05 K-KNR-PROV-NICHT-BEARB     PIC S9(004) BINARY VALUE 14.
050698     05 K-KNR-ANTRAG-ABGERECHNET   PIC S9(004) BINARY VALUE 16.
220399     05 K-KNR-NEUTRAL-NICHT-BEARB  PIC S9(004) BINARY VALUE 18.

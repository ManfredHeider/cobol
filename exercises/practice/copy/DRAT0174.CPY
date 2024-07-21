      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0174.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0174.CPv  $
      *
      *   Rev 2.1   07 Sep 2000 15:02:14   T13EX23
      *APA00096 - K-KNR-PGEB eingetragen
      *
      *   Rev 2.0   Feb 27 1999 11:57:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   13 Nov 1998 09:07:16   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:20:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0174  KONSTANTEN FÜR     KTY = 141       *
      *                           Herkunfttabelle eines Objektes     *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    22.10.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!22.10.97! 01  ! NEUERSTELLUNG            ! STORCH   *
      * APA00012!08.12.97!01.02! PMPP ENTFERNT            ! JäGER    *
      * APA00012!26.01.98!01.03! PVER, PVPT EINGETRAGEN   ! STORCH   *
      *         !        !     !                          !          *
      * AFPA0012!03.04.98!01.04! PDTA, KTY EINGETRAGEN    ! WOLFF    *
      *         !        !     !                          !          *
      * AFPA0012!11.08.98!01.05! PEST EINGETRAGEN         ! WOLFF    *
      *         !13.08.98!01.05! PDPT EINGETRAGEN         ! WOLFF    *
      * AFPA0096!08.08.00!01.06! PGEB EINGETRAGEN         ! GRAY     *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-PMPG                 PIC S9(004) BINARY VALUE 1.
           05 K-KNR-PMBE                 PIC S9(004) BINARY VALUE 3.
           05 K-KNR-PMPR                 PIC S9(004) BINARY VALUE 4.
           05 K-KNR-PMPT                 PIC S9(004) BINARY VALUE 5.
           05 K-KNR-PVER                 PIC S9(004) BINARY VALUE 6.
           05 K-KNR-PVPT                 PIC S9(004) BINARY VALUE 7.
030498     05 K-KNR-PDTA                 PIC S9(004) BINARY VALUE 8.
030498     05 K-KNR-KTY                  PIC S9(004) BINARY VALUE 9.
110898     05 K-KNR-PEST                 PIC S9(004) BINARY VALUE 10.
130898     05 K-KNR-PDPT                 PIC S9(004) BINARY VALUE 11.
080800     05 K-KNR-PGEB                 PIC S9(004) BINARY VALUE 12.

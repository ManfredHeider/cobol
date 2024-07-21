      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0204.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drat0204.cpv  $
      *
      *   Rev 2.1   Apr 21 1999 17:29:12   T13EX22
      *KNR Reihenfolgefehler neu
      *
      *   Rev 2.0   Apr 14 1999 15:26:56   T13EX22
      *MFE-UMSTELLUNG
      *
      *   Rev 1.0   03 Dec 1997 15:46:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0204  KONSTANTEN FÜR     KTY = 146       *
      *                           Zustände eines Produkts            *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    08.12.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!08.12.97!01.01! NEUERSTELLUNG            ! Jäger    *
      * APA00012!14.04.99!01.02! NEUE KNR                 ! MUND     *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-PR-OK                PIC S9(004) BINARY VALUE 1.
           05 K-KNR-PR-ZUKUENFTIG        PIC S9(004) BINARY VALUE 2.
           05 K-KNR-PR-FEHLER            PIC S9(004) BINARY VALUE 3.
140499     05 K-KNR-PR-REIHENFOLGEFEHLER PIC S9(004) BINARY VALUE 6.


      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0205.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drat0205.cpv  $
      *
      *   Rev 2.1   Apr 21 1999 17:29:14   T13EX22
      *KNR Reihenfolgefehler neu
      *
      *   Rev 2.0   Apr 14 1999 15:26:58   T13EX22
      *MFE-UMSTELLUNG
      *
      *   Rev 1.0   03 Dec 1997 15:46:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
     ****************************************************************
      *    COPY-MEMBER: DRAT0204  KONSTANTEN FÜR     KTY = 147       *
      *                           Zustände eines Produktteils        *
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
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-PT-OK                PIC S9(004) BINARY VALUE 1.
           05 K-KNR-PT-ZUKUENFTIG        PIC S9(004) BINARY VALUE 2.
           05 K-KNR-PT-FEHLER            PIC S9(004) BINARY VALUE 3.
           05 K-KNR-PT-KOMP-FEHLER       PIC S9(004) BINARY VALUE 4.
140499     05 K-KNR-PT-REIHENFOLGEFEHLER PIC S9(004) BINARY VALUE 6.


      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0325.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRAT0325.CPv  $
      *
      *   Rev 2.2   19 Mar 2001 09:14:40   T13AE23
      *AAN00292 - PEW Banken -
      *
      *   Rev 2.1   15 May 2000 08:48:52   T13EX21
      *APA00090 - Neuerstellung
      *
      *   Rev 2.0   Mar 16 1999 09:57:26   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0325  KONSTANTEN FÜR KTY = 185           *
      *                           (ZUORDNUNG KARDINALITÄTSBEZIEHUNG) *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    11.05.2000                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00090!11.05.00! 2.1 ! NEUERSTELLUNG             ! STORCH  *
      * AAN00292!17.01.01! 2.2 ! NEUE KARDINALITÄT VERTRAG-!         *
      *         !        !     ! PRODUKT-ANTRAG FÜR PEW    ! RAHNER  *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-KARD-VNR-PRINTE         PIC S9(004) BINARY VALUE 1.
           05 K-KNR-KARD-PIN-PRINTE         PIC S9(004) BINARY VALUE 2.
           05 K-KNR-KARD-ANTRAG-PROD        PIC S9(004) BINARY VALUE 3.

      *---------------------------------------------------------------*
      *    Standard f¸r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0165.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0165.CPv  $
      *
      *   Rev 2.0   May 28 1999 18:03:16   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   05 Sep 1997 09:20:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0165  KONSTANTEN F‹R                     *
      *                           BUCHUNGSSTAND            KTY = 132 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    22.10.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA00??!22.10.97! 01 ! NEUERSTELLUNG             ! STORCH   *
      * APA00012!06.05.98! 02 ! NEUE AUSPR‰GUNGEN         ! BOHLINGER*
      * APA00012!21.07.98! 03 ! NEUE AUSPR‰GUNGEN         ! J‰ger    *
      * APA00012!02.02.99! 04 ! NEUE AUSPR‰GUNGEN         ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-BUCH-ERSTELLT        PIC S9(004) BINARY VALUE 1.
           05 K-KNR-BUCH-NOT-RELEV       PIC S9(004) BINARY VALUE 2.
           05 K-KNR-BUCH-ABGERECH        PIC S9(004) BINARY VALUE 3.
060598     05 K-KNR-BUCH-ERSTELLT-NOK    PIC S9(004) BINARY VALUE 4.
060598     05 K-KNR-BUCH-ERSTELLT-N-FREI PIC S9(004) BINARY VALUE 5.
060598     05 K-KNR-BUCH-NEUTRALISIERT   PIC S9(004) BINARY VALUE 6.
020299     05 K-KNR-BUCH-NICHT-PROV-PFL  PIC S9(004) BINARY VALUE 7.
210798     05 K-KNR-BUCH-ZUKUNFT         PIC S9(004) BINARY VALUE 8.

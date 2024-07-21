      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0173.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0173.CPv  $
      *
      *   Rev 2.0   May 29 1999 08:50:08   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   05 Sep 1997 09:20:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0173  KONSTANTEN FÜR                     *
      *                           EVS-GRUND                KTY = 140 *
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
      * AFPA0012!22.10.97! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !30.11.98!1.02! NEUE AUSPRäGUNGEN 8 UND 9 ! BOHLINGER*
      *         !10.12.98!1.03! NEUE AUSPRäGUNG 0         ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-KEIN-EVS-PRODUKT       PIC S9(004) BINARY VALUE 0.
           05 K-KNR-OK-EE                  PIC S9(004) BINARY VALUE 1.
           05 K-KNR-KEIN-EE-KEIN-AVERM     PIC S9(004) BINARY VALUE 2.
           05 K-KNR-KEIN-EE-KEIN-KDB       PIC S9(004) BINARY VALUE 3.
           05 K-KNR-KEIN-EE-KDB            PIC S9(004) BINARY VALUE 4.
           05 K-KNR-KEIN-EE-KEIN-VORVERTR  PIC S9(004) BINARY VALUE 5.
           05 K-KNR-KEIN-EE-LAUFZEIT       PIC S9(004) BINARY VALUE 6.
           05 K-KNR-KEIN-EE-PRAEMIE        PIC S9(004) BINARY VALUE 7.
           05 K-KNR-OK-EE-MANUELL          PIC S9(004) BINARY VALUE 8.
           05 K-KNR-KEIN-EE-MANUELL        PIC S9(004) BINARY VALUE 9.

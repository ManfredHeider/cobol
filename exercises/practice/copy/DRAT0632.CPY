      *****************************************************************
      *    COPY-MEMBER: DRAT0632
      *
      *   DAS COPY-MEMBER BEINHALTET DIE AUSPRÄGUNGEN FÜR DEN
      *   KONSTANTENTYP = 457 (SIEHE DRAT0044.CPY).
      *   SOBALD IN DER TABELLE TBAT0008 EIN EINTRAG MIT DEN WERTEN
      *   KTY_NR = 457 UND KNR_NR=N VORHANDEN IST, WIRD AUF DEN ENT-
      *   SPRECHENDEN KONFIGURATIONSBESTAND DES EGG ZUGEGRIFFEN,
      *   ANSONSTEN AUF AGG.
      *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1118!15.07.21! NEU                          ! HEIDER   *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
       01  FILLER.
      *       PRODUKTKONDITION ENTSPRICHT IM AGG TBPA0023
           05 K-KNR-AN-KONF-EGG-PRKO       PIC S9(004) BINARY VALUE 1.
      *       ZUORDNUNG VERFAHREN ENTSPRICHT IM AGG TBPA0012
           05 K-KNR-AN-KONF-EGG-ZVP        PIC S9(004) BINARY VALUE 2.
      *       PRODUKTGRUPPE ENTSPRICHT IM AGG TBPA0006 + TBPA0005
           05 K-KNR-AN-KONF-EGG-PGRP       PIC S9(004) BINARY VALUE 3.
      *       TARIF ENTSPRICHT IM AGG TBPA0009
           05 K-KNR-AN-KONF-EGG-PGTA       PIC S9(004) BINARY VALUE 4.
      *       PRODUKTGEBERKONFIGURATION ENTSPRICHT IM AGG TBPA0027
           05 K-KNR-AN-KONF-EGG-PGKF       PIC S9(004) BINARY VALUE 5.
      *       PRODUKTINFORMATION IM AGG TBPA0004
           05 K-KNR-AN-KONF-EGG-PROD       PIC S9(004) BINARY VALUE 6.

      *****************************************************************
      *---------------------- ENDE DRAT0632 --------------------------*
      *****************************************************************


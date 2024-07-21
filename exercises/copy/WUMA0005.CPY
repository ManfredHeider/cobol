      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.09.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUMA0005.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/WUMA0005.CPv  $
      *
      *   Rev 2.5   Feb 09 2021 16:05:30   t13ac18
      *AMA00359: temp. Auftrag T13AC18 Copies für PMMA5005
      *
      *   Rev 2.4   Mar 05 2020 11:02:30   t13ac20
      *AKI01471: Testübergaben
      *
      *   Rev 2.3   Oct 08 2018 10:15:08   t13ac20
      *AKI01471: KIGEN200: JCL-Generierung
      *
      *   Rev 2.2   Nov 15 2016 16:05:08   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.1   Oct 15 2013 11:09:16   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.0   Sep 10 2013 08:14:38   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!10.09.13!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00013!15.11.16!2.2  ! VORGABE EQQDELDS FÜR      ! WOLFF
      *         !        !     ! NUTZUNG IN PRODUKTIVER JCL!
      * AKI01471!08.10.18!2.3  ! PARAMETER ENTFERNT        ! WOLFF
      * AKI01471!05.03.20!2.4  ! KEINE ÄNDERUNG; TEST-     ! T13AC20
      *         !        !     ! ÜBERGABE ADB-TOOL         !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
       01  EXEC-EQQDELDS-IND-MAX   pic S9(4) binary sync value +7.
       01  EXEC-EQQDELDS.
           05              pic X(80) value
                               '//*=====================================
      -                        '================================
      -                        ''.
           05              pic X(80) value
                               '//*
      -                        '
      -                        ''.
           05              pic X(80) value
                               '//*  LÖSCHEN AUSGABEDATEIEN
      -                        '
      -                        ''.
           05              pic X(80) value
                               '//*
      -                        '
      -                        ''.
           05              pic X(80) value
                               '//*=====================================
      -                        '================================
      -                        ''.
           05              pic X(80) value
CCC005*                        '//DELDAT   EXEC PGM=EQQDELDS
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//SYSPRINT DD SYSOUT=*
      -                        '
      -                        ''.
      *
      *
CCC004*01  EXEC-EQQDELDS-IND-MAX-P pic S9(4) binary sync value +4.
CCC004*01  EXEC-EQQDELDS-P.
CCC004*    05              pic X(80) value
CCC004*                        '//*  LÖSCHEN AUSGABEDATEIEN
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC005*                        '//DELDAT   EXEC PGM=EQQDELDS
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//SYSPRINT DD SYSOUT=*
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//*
      -                        '
      -                        ''.
      *
      **************************************************************



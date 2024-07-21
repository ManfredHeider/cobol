      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.06.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUMA0001.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/WUMA0001.CPv  $
      *
      *   Rev 2.4   Feb 09 2021 15:59:46   t13ac18
      *AMA00359: temp. Auftrag T13AC18 Copies für PMMA5005
      *
      *   Rev 2.3   Mar 05 2020 11:01:54   t13ac20
      *AKI01471: Testübergaben
      *
      *   Rev 2.2   Oct 08 2018 10:14:16   t13ac20
      *AKI01471: KIGEN200: JCL-Generierung
      *
      *   Rev 2.1   Oct 15 2013 11:08:08   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.0   Jun 19 2013 10:54:52   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!19.06.13!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AKI01471!08.10.18!2.2  ! PARAMETER ENTFERNT        ! WOLFF
      * AKI01471!05.03.20!2.3  ! KEINE ÄNDERUNG; TEST-     ! T13AC20
      *         !        !     ! ÜBERGABE ADB-TOOL         !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
       01  EXEC-SORT-IND-MAX     pic S9(4) binary sync value +2.
       01  EXEC-SORT.
           05  EXEC-VAR-01 pic X(80) value
CCC005*                        '//#VAR#### EXEC PGM=SORT
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//SYSOUT   DD SYSOUT=*
      -                        '
      -                        ''.
      *
      **************************************************************



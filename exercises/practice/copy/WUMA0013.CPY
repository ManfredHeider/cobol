      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.02.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUMA0013.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUMA0013.CPv  $
      *
      *   Rev 2.2   Feb 09 2021 16:13:12   t13ac18
      *AMA00359: temp. Auftrag T13AC18 Copies für PMMA5005
      *
      *   Rev 2.1   Feb 17 2014 09:20:38   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.0   Feb 17 2014 08:35:50   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!17.02.14!2.1  ! NEUERSTELLUNG             ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
       01  EXEC-APKACIF-IND-MAX  pic S9(4) binary sync value +3.
       01  EXEC-APKACIF.
           05  EXEC-VAR-01 pic X(80) value
CCC005*                        '//#VAR#### EXEC PGM=APKACIF,REGION=8M
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//STEPLIB DD DISP=SHR,DSN=SYS1.SAPKMOD1
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//SYSPRINT DD SYSOUT=*
      -                        '
      -                        ''.
      *
      **************************************************************


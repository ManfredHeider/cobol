      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.09.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUMA0004.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUMA0004.CPv  $
      *
      *   Rev 2.2   Feb 09 2021 16:04:14   t13ac18
      *AMA00359: temp. Auftrag T13AC18 Copies für PMMA5005
      *
      *   Rev 2.1   Oct 15 2013 11:08:58   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.0   Sep 03 2013 14:15:52   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!30.09.13!2.1  ! NEUERSTELLUNG             ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
       01  EXEC-IKJEFT1B-IND-MAX pic S9(4) binary sync value +5.
       01  EXEC-IKJEFT1B.
           05  EXEC-VAR-01 pic X(80) value
CCC005*                        '//#VAR#### EXEC PGM=IKJEFT1B,DYNAMNBR=20
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//SYSTSPRT DD SYSOUT=*
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//SYSPRINT DD SYSOUT=*
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//SYSOUT   DD SYSOUT=*
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//SYSUDUMP DD SYSOUT=*
      -                        '
      -                        ''.
      *
      **************************************************************


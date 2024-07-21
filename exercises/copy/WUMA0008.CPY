      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.10.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUMA0008.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUMA0008.CPv  $
      *
      *   Rev 2.2   Feb 09 2021 16:08:58   t13ac18
      *AMA00359: temp. Auftrag T13AC18 Copies für PMMA5005
      *
      *   Rev 2.1   Oct 15 2013 11:10:04   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.0   Oct 07 2013 10:33:28   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!07.10.13!2.1  ! NEUERSTELLUNG             ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
       01  EXEC-IDCAMS-IND-MAX   pic S9(4) binary sync value +2.
       01  EXEC-IDCAMS.
           05  EXEC-VAR-01 pic X(80) value
CCC005*                        '//#VAR#### EXEC PGM=IDCAMS
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//SYSPRINT DD SYSOUT=*
      -                        '
      -                        ''.
      *
      **************************************************************


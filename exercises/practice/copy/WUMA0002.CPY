      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.06.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUMA0002.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUMA0002.CPv  $
      *
      *   Rev 2.3   Feb 09 2021 16:01:24   t13ac18
      *AMA00359: temp. Auftrag T13AC18 Copies für PMMA5005
      *
      *   Rev 2.2   Jan 22 2014 08:24:58   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.1   Oct 15 2013 11:08:26   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.0   Jun 21 2013 09:50:44   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!21.06.13!2.1  ! NEUERSTELLUNG             ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
       01  EXEC-DEFDAT-IND-MAX   pic S9(4) binary sync value +6.
       01  EXEC-DEFDAT.
           05  EXEC-VAR-01 pic X(80) value
                               '//#VAR1### DD DSN=#VAR2###,
      -                        '
      -                        ''.
           05  EXEC-VAR-02 pic X(80) value
                               '//            DISP=#VAR####,
      -                        '
      -                        ''.
           05  EXEC-VAR-03 PIC X(80) value
                               '//            DCB=(#VAR####),
      -                        '
      -                        ''.
           05  EXEC-VAR-04 PIC X(80) value
                               '//            DATACLAS=#VAR####,
      -                        '
      -                        ''.
           05  EXEC-VAR-05 PIC X(80) value
                               '//            MGMTCLAS=#VAR####,
      -                        '
      -                        ''.
           05  EXEC-VAR-06 PIC X(80) value
                               '//            #VAR####
      -                        '
      -                        ''.
      *
      **************************************************************


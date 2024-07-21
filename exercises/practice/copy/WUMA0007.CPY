      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUMA0007.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUMA0007.CPv  $
      *
      *   Rev 2.3   Feb 09 2021 16:07:42   t13ac18
      *AMA00359: temp. Auftrag T13AC18 Copies für PMMA5005
      *
      *   Rev 2.2   Jan 27 2014 15:52:02   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.1   Oct 15 2013 11:09:48   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.0   Sep 17 2013 10:11:54   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00013!17.09.13!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00013!27.01.13!2.2  ! KOMMENTAR MIT "case       ! WOLFF
      *         !        !     ! sensitive" eingefügt      !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
       01  EXEC-UCMDPRC2-IND-MAX pic S9(4) binary sync value +6.
       01  EXEC-UCMDPRC2.
           05              pic X(80) value
                               '//*%OPC SCAN
      -                        '
      -                        ''.
           05              pic X(80) value
                               '//*-------------------------------------
      -                        '----------------------
      -                        ''.
           05              pic X(80) value
                               '//* Achtung!! case sensitive
      -                        '
      -                        ''.
           05              pic X(80) value
                               '//*-------------------------------------
      -                        '----------------------
      -                        ''.
           05  EXEC-VAR-01 pic X(80) value
CCC005*                        '//#VAR#### EXEC UCMDPRC2
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//*%OPC FETCH MEMBER=FTCH&OWSID
      -                        '
      -                        ''.
      **************************************************************


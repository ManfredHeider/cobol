      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.05.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUMA0014.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUMA0014.CPv  $
      *
      *   Rev 2.3   Feb 09 2021 16:14:06   t13ac18
      *AMA00359: temp. Auftrag T13AC18 Copies für PMMA5005
      *
      *   Rev 2.2   May 26 2014 09:59:36   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.1   May 23 2014 09:55:00   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.0   May 23 2014 08:56:18   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!23.05.14!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00020!26.05.14!2.2  ! FEHLERKORREKTUR: FESTE    ! WOLFF
      *         !        !     ! DB2-UMGEBUNG GEGEN VARI-  !
      *         !        !     ! ABLE ERSETZT              !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
       01  EXEC-DB2TEP8-IND-MAX  pic S9(4) binary sync value +1.
       01  EXEC-DB2TEP8.
           05  EXEC-VAR-01 pic X(80) value
CCC005*                        '//#VAR#### EXEC PROC=DB2TEP#NR
      -                        '
      -                        ''.
      *
      **************************************************************


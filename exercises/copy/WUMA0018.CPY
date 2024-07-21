      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.01.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUMA0018.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUMA0018.CPv  $
      *
      *   Rev 2.3   Feb 09 2021 16:18:44   t13ac18
      *AMA00359: temp. Auftrag T13AC18 Copies für PMMA5005
      *
      *   Rev 2.2   Mar 17 2017 13:52:22   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.1   Feb 03 2016 13:38:16   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.0   Jan 25 2016 13:02:58   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00013!25.01.16!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00013!17.03.17!2.2  ! FIXE 8 DURCH #NR ERSETZT  ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
       01  EXEC-REXBTCH8-IND-MAX pic S9(4) binary sync value +1.
       01  EXEC-REXBTCH8.
           05  EXEC-VAR-01 pic X(80) value
CCC005*                        '//#VAR#### EXEC PROC=REXBTCH#NR
      -                        '
      -                        ''.
      *
      **************************************************************
 



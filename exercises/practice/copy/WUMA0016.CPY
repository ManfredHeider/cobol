      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.12.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUMA0016.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUMA0016.CPv  $
      *
      *   Rev 2.2   Feb 09 2021 16:16:08   t13ac18
      *AMA00359: temp. Auftrag T13AC18 Copies für PMMA5005
      *
      *   Rev 2.1   Dec 03 2014 10:16:34   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.0   Dec 03 2014 08:25:44   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00013!03.12.14!2.1  ! NEUERSTELLUNG             ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
       01  EXEC-ISRSUPC-IND-MAX  pic S9(4) binary sync value +1.
       01  EXEC-ISRSUPC.
           05  EXEC-VAR-01 pic X(80) value
CCC005*                        '//#VAR#### EXEC PGM=ISRSUPC§PARM
      -                        '
      -                        ''.
      *
      **************************************************************


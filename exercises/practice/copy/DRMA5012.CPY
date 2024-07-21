      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.06.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5012.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5012.CPv  $
      *
      *   Rev 2.2   Feb 09 2021 15:51:50   t13ac18
      *AMA00359: temp. Auftrag T13AC18 Copies für PMMA5005
      *
      *   Rev 2.1   Oct 15 2013 11:07:02   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.0   Jun 24 2013 13:15:56   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!24.06.13!2.1  ! NEUERSTELLUNG             ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5012.
              07 FUNKTION               pic X(8).
              07 DRMA5014.
                 09 TYP                 pic X(3).
                 09 LRECL               pic X(5).
                 09 ANZ                 pic X(10).
                 09 PRI                 pic X(8).
                 09 SEC                 pic X(8).
                 09 BLKSIZE             pic X(5).
                 09                     pic X(41).
      *
      **************************************************************


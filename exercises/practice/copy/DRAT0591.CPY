      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0591.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0591.CPv  $
      *
      *   Rev 2.1   Apr 29 2014 11:44:14   t13ac20
      *AMA00059: Badenia: Sourcen
      *
      *   Rev 2.0   Apr 24 2014 13:42:54   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DTA-NACHSATZ-NEU
      *
      *     SOURCE-NAME      :  DRAT0591
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00059!24.04.14!2.1  ! NUR ÜBERNAHME AUS AACHEN  ! WOLFF
      *         !        !     ! (DGKP1035)                !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DTA-NACHSATZ-NEU.
              07 SATZ-KZ                PICTURE X.
              07 FSAETZE                PICTURE S9(7)  PACKED-DECIMAL.
              07 FSUMME                 OCCURS 5 TIMES
                                        PICTURE S9(15)V99
                                                       PACKED-DECIMAL.
      *
      **************************************************************

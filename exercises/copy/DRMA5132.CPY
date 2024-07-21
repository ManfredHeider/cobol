      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.02.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5132.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA5132.CPv  $
      *
      *   Rev 2.3   Jan 05 2018 15:40:16   t13ex15
      *AMA00399: FiBu - Neue Buchungssystematik steuerpflichtige Ges
      *
      *   Rev 2.2   May 18 2016 11:21:28   t13ac20
      *AMA00263: USt: LUCY II - KOMA0505-VB-Abrechnung - Copies
      *
      *   Rev 2.1   Apr 20 2016 10:02:10   t13ac20
      *AMA00217: USt: LUCY II - Vorarbeiten / Redesign - Copies
      *
      *   Rev 2.0   Feb 22 2016 10:39:14   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     MITCOPY: DATEN ZU FIBU-GRUNDLAGEN
      *
      *     SOURCE-NAME      :  DRMA5132
      *
      *     GENERIERUNG      :  05.01.2018/15:00:35
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00217!22.02.16!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00263!27.04.16!2.2  ! ÄNDERUNG DEMA0177;        ! WOLFF
      *         !        !     ! FELDNAMEN AUF UNSERE      !
      *         !        !     ! NAMEN ANGEPASST           !
      * AMA00399!05.01.17!2.3  ! NEUES FELD KONTO-AUS      ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5132.
              07 KONTO-IND              pic 9(4).
              07 KONTO                  pic X(12).
              07 KONTO-BEZEICH          pic X(30).
              07 SH-KZN                 pic X.
              07 BNGBETR                pic 9(14)V99.
              07 GEGENKONTO             pic X(12).
              07 BUCHTEXT               pic X(40).
              07 KOSTENSTELLE           pic X(6).
              07 KONTO-AUS              pic X(12).
      *
      **************************************************************


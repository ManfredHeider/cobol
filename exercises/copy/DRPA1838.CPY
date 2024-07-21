      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.08.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1838.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1838.CPv  $
      *
      *   Rev 2.1   Sep 03 2015 13:03:36   t13ex44
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System - MSC2
      *
      *   Rev 2.0   Aug 21 2015 10:12:42   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1838                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1838 DES ZJPA0025       *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * ------------------------------------------------------------- *
      * APA01932!21.08.15! 2.0! NEU                       ! SCHEIDEL  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FUER DIE FUNKTION DRPA1837. MIT DIESER FUNKTION WIRD EINE
      *    KORREKTUR DES UMSATZSTEUERLICHEN STATUS EINES VBS
      *    ANGESTOSSEN.
      *
      *****************************************************************
      *
        01 PI-DRPA1838.
           05 PI-DRPA1838-PGEB-GES          PIC S9(004) BINARY.
           05 PI-DRPA1838-PRINTE            PIC S9(015) PACKED-DECIMAL.
      *
        01 PO-DRPA1838.
           05 PO-DRPA1838-RC                PIC  X(003).
              88 PO-DRPA1838-RC-OK               VALUE 'OK '.
      *
           05 PO-DRPA1838-EINHEITEN         PIC S9(006)V9(02)
                                                        PACKED-DECIMAL.
      *


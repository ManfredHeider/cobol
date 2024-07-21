      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.03.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1655.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1655.CPv  $
      *
      *   Rev 2.2   Apr 11 2006 13:11:56   t13ex49
      *FKI01025: Ändern post.Prüfung f. Berlin
      *
      *   Rev 2.1   Mar 07 2005 09:22:38   t13ex49
      *AKI00268: Postalische Prüfung - Ortsteil - Teil 2
      *
      *   Rev 2.0   Mar 16 2004 08:33:26   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1655 Bundesland definieren               *
      *    -------------------------------------------------------   *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00268!15.03.04! 01 ! NEUERSTELLUNG             ! P. Pohl  *
      * --------!--------!----!---------------------------!----------*
110404* FKI01025!11.04.04!    ! Änderung bei BLN/BLO      ! P. Pohl  *
110404*         !        !    ! Abfrage über PLZ          !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1655                                 *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1655.
          05 PI-DRKI1655-DATEN.
             10  PI-DRKI1655-AdresseLandID    PIC X(03).
             10  PI-DRKI1655-Bundesland       PIC X(02).
             10  PI-DRKI1655-O-W-KZ           PIC X(01).
110404       10  PI-DRKI1655-ADRESSEPLZ       PIC X(15).
      *
       01 PO-DRKI1655.
          05 PO-DRKI1655-DATEN.
             10 PO-DRKI1655-ADRESSEBDSLDID    PIC X(03).

          05 PO-DRKI1655-RC                   PIC X(03).
             88 PO-DRKI1655-RC-OK                         VALUE 'OK '.
             88 PO-DRKI1655-RC-NOK                        VALUE 'NOK'.
      *
      ****** Ende Copy-Member ****************************************


      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1328.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1328.CPv  $
      *
      *   Rev 2.1   Aug 26 2003 08:56:38   t13ex49
      *Test Dubletten
      *
      *   Rev 2.0   Dec 14 2001 09:20:08   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1328 Suchfeld ADRESSE festlegen          *
      *    -------------------------------------------------------   *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00268!15.02.02! 01 ! NEUERSTELLUNG             ! P. Pohl  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1328                                 *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1328.
          05 PI-DRKI1328-DATEN.
             10 PI-DRKI1328-ADRESSESTRHAUSNUM    PIC  X(045).
      *
       01 PO-DRKI1328.
          05 PO-DRKI1328-DATEN.
             10 PO-DRKI1328-SUCHFELD             PIC  X(093).

          05 PO-DRKI1328-RC                      PIC  X(003).
             88 PO-DRKI1328-RC-OK                         VALUE 'OK '.
             88 PO-DRKI1328-RC-NOK                        VALUE 'NOK'.
             88 PO-DRKI1328-RC-TO-LONG                    VALUE 'TLO'.
             88 PO-DRKI1328-RC-LEER                       VALUE 'LER'.
      *
      ****** Ende Copy-Member ****************************************


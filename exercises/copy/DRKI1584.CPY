      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1584.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1584.CPv  $
      *
      *   Rev 2.1   Sep 30 2003 10:22:30   t13ex12
      *AKI00367 Neukundenermittlung
      *
      *   Rev 2.0   Sep 17 2003 10:20:00   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1584                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1584                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  24.09.2003/14:24:49
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1584
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  MIN / MAX <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1584.
           05  PI-DRKI1584-KEY.


             10 PI-DRKI1584-NKDPERSONENID   PIC X(13).

      *
       01  PO-DRKI1584.
           05  PO-DRKI1584-RC                 PIC  X(003).
               88 PO-DRKI1584-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1584-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1584-DATEN.


             10 PO-DRKI1584-NKDLFDNR   PIC S9(4) COMP.


      *
      ******* ENDE COPY-MEMBER **************************************

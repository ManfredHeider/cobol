      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3414.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3414.CPv  $
      *
      *   Rev 2.3   Nov 21 2019 08:29:20   t13ex27
      *AKI01674: DSGVO: Mappen der DB2-Tabelle zu gesperrten/gelösc
      *hten Verträge in die OKI
      *
      *   Rev 2.2   Nov 28 2018 17:28:56   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.1   Sep 11 2018 12:15:38   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.0   Sep 6 2018 08:09:15 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3414                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3414                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.09.2019 / 16:41:08 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3414
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3414.
           05  PI-DRKI3414-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3414-SPALFDNR   PIC S9(4) COMP.
             10 PI-DRKI3414-SPAPARTGESELLID   PIC S9(4) COMP.
             10 PI-DRKI3414-SPAVERTRAGSNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI3414.
           05  PO-DRKI3414-RC                     PIC  X(003).
               88 PO-DRKI3414-RC-OK               VALUE 'OK '.
               88 PO-DRKI3414-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************


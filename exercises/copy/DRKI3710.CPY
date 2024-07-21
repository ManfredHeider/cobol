      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.07.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3710.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3710.CPv  $
      *
      *   Rev 2.1   Sep 02 2019 13:16:38   u130176
      *AKI01689: DSGVO:Löschverfahren für Tabelle VBSTAMMDATEN
      *
      *   Rev 2.0   Jul 1 2019 08:20:50 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3710                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3710                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         01.07.2019 / 08:20:50 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3710
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3710.
           05  PI-DRKI3710-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3710-MITARBEITERNR   PIC S9(9) COMP.
END   *

      *
       01  PO-DRKI3710.
           05  PO-DRKI3710-RC                     PIC  X(003).
               88 PO-DRKI3710-RC-OK               VALUE 'OK '.
               88 PO-DRKI3710-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3710-DATEN.
             10   PO-DRKI3710-COUNTER           PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************


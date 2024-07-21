      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.12.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0570.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0570.CPv  $
      *
      *   Rev 2.1   Jan 22 2015 15:25:18   t13ex27
      *AKI01339: Redesign ZTAT0039
      *
      *   Rev 2.0   Dec 17 2014 13:42:27 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0570                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0570                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.12.2014 / 13:42:27 / T13EX27
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
      *    FÜR DIE FUNKTION DRAT0570
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0570.
           05  PI-DRAT0570-KEY.


ANF   *        %M_PI_KEY%
MANUEL*      10 PI-DRAT0570-KNR   PIC S9(15)V COMP-3.
             10 PI-DRAT0570-BEZEICHNUNG   PIC  X(0100).
             10 PI-DRAT0570-KTY   PIC S9(4) COMP.
END   *

      *
       01  PO-DRAT0570.
           05  PO-DRAT0570-RC                     PIC  X(003).
               88 PO-DRAT0570-RC-OK               VALUE 'OK '.
               88 PO-DRAT0570-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRAT0570-DATEN.


ANF   *        %M_PO_DATEN%
MANUEL       10 PO-DRAT0570-KNR                PIC S9(15)V COMP-3.
             10 PO-DRAT0570-BEZEICHNUNG-KURZ   PIC  X(0020).
             10 PO-DRAT0570-KNR-ALPHA          PIC  X(0015).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************


      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.12.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0571.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0571.CPv  $
      *
      *   Rev 2.1   Jan 22 2015 15:25:24   t13ex27
      *AKI01339: Redesign ZTAT0039
      *
      *   Rev 2.0   Dec 17 2014 13:42:29 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0571                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0571                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.12.2014 / 13:42:29 / T13EX27
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
      *    FÜR DIE FUNKTION DRAT0571
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0571.
           05  PI-DRAT0571-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRAT0571-KNR-ALPHA   PIC  X(0015).
             10 PI-DRAT0571-KTY   PIC S9(4) COMP.
END   *

      *
       01  PO-DRAT0571.
           05  PO-DRAT0571-RC                     PIC  X(003).
               88 PO-DRAT0571-RC-OK               VALUE 'OK '.
               88 PO-DRAT0571-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRAT0571-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRAT0571-BEZEICHNUNG   PIC  X(0100).
             10 PO-DRAT0571-BEZEICHNUNG-KURZ   PIC  X(0020).
             10 PO-DRAT0571-KNR   PIC S9(15)V COMP-3.
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************


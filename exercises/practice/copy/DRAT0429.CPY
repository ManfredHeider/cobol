      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0429.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0429.CPv  $
      *
      *   Rev 2.2   Jul 10 2013 15:27:24   t13ex27
      *FKI01394: Überarbeitung Ticketfehler
      *
      *   Rev 2.1   Nov 13 2007 10:25:08   t13ex51
      *AKL00664: Allfinanz: Supporttabelle KTY_KNR_SCHLUESSEL
      *
      *   Rev 2.0   Sep 03 2007 09:22:36   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0429                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0429                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.07.2013 / 15:03:48 / T13EX27
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
      *    FÜR DIE FUNKTION DRAT0429
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0429.
           05  PI-DRAT0429-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRAT0429-KNR   PIC S9(4) COMP.
             10 PI-DRAT0429-KTY   PIC S9(4) COMP.
END   *

      *
       01  PO-DRAT0429.
           05  PO-DRAT0429-RC                     PIC  X(003).
               88 PO-DRAT0429-RC-OK               VALUE 'OK '.
               88 PO-DRAT0429-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRAT0429-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRAT0429-BEZEICHNUNG   PIC  X(0100).
             10 PO-DRAT0429-BEZEICHNUNG-KURZ   PIC  X(0020).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

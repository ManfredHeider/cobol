      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.12.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0573.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0573.CPv  $
      *
      *   Rev 2.1   Jan 22 2015 15:25:36   t13ex27
      *AKI01339: Redesign ZTAT0039
      *
      *   Rev 2.0   Dec 17 2014 13:42:34 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0573                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0573                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.12.2014 / 13:42:34 / T13EX27
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
      *    FÜR DIE FUNKTION DRAT0573
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  NEXT VALUE<<<
      *
      ****************************************************************
      *
       01  PI-DRAT0573.
           05  PI-DRAT0573-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRAT0573-KTY   PIC S9(4) COMP.
END   *


           05  PI-DRAT0573-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRAT0573-BEZEICHNUNG   PIC  X(0100).
             10 PI-DRAT0573-BEZEICHNUNG-KURZ   PIC  X(0020).
END   *

      *
       01  PO-DRAT0573.
           05  PO-DRAT0573-RC                     PIC  X(003).
               88 PO-DRAT0573-RC-OK                  VALUE 'OK '.
               88 PO-DRAT0573-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
           05  PO-DRAT0573-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRAT0573-KNR   PIC S9(15)V COMP-3.
END   *


      *
      ******* ENDE COPY-MEMBER **************************************


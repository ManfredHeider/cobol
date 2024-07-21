      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3303.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3303.CPv  $
      *
      *   Rev 2.1   Jun 11 2018 14:36:58   t13ex27
      *AKI01570: FW - Generali-Partnergesellschaften-E/A/A-Kunden
      *
      *   Rev 2.0   Feb 27 2018 17:32:23 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3303                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3303                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         27.02.2018 / 17:32:23 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3303
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3303.
           05  PI-DRKI3303-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3303-GUPVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI3303.
           05  PO-DRKI3303-RC                     PIC  X(003).
               88 PO-DRKI3303-RC-OK               VALUE 'OK '.
               88 PO-DRKI3303-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3303-DATEN.
             10   PO-DRKI3303-COUNTER           PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************


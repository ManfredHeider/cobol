      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.03.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2445.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2445.CPv  $
      *
      *   Rev 2.2   Jun 17 2020 10:14:38   t13ae31
      *AKI01785: G10 SUH FLEXN
      *
      *   Rev 2.1   Mar 01 2011 15:28:14   t13ex27
      *AKI01012: VSP Vorteilsargumentation-OZP
      *
      *   Rev 2.0   Mar 1 2011 15:24:12 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2445                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2445                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         16.06.2020 / 12:46:55 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2445
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2445.
           05  PI-DRKI2445-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2445-VEZVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2445-VEZVETLFDNR   PIC S9(4) COMP.
END   *

      *
       01  PO-DRKI2445.
           05  PO-DRKI2445-RC                     PIC  X(003).
               88 PO-DRKI2445-RC-OK               VALUE 'OK '.
               88 PO-DRKI2445-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI2445-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2445-VEZGESCHAEFTSZW   PIC  X(0002).
             10 PO-DRKI2445-VEZSPARTE-KNR   PIC S9(15)V COMP-3.
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************


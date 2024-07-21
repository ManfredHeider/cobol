      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3302.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3302.CPv  $
      *
      *   Rev 2.1   Jun 11 2018 14:36:16   t13ex27
      *AKI01570: FW - Generali-Partnergesellschaften-E/A/A-Kunden
      *
      *   Rev 2.0   Feb 27 2018 17:32:20 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3302                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3302                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         27.02.2018 / 17:32:20 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3302
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3302.
           05  PI-DRKI3302-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3302-GUPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3302-GUPPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3302-GUPVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI3302.
           05  PO-DRKI3302-RC                     PIC  X(003).
               88 PO-DRKI3302-RC-OK               VALUE 'OK '.
               88 PO-DRKI3302-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************


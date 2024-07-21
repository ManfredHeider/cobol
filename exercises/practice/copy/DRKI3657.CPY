      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.05.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3657.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3657.CPv  $
      *
      *   Rev 2.1   Jun 07 2019 12:19:52   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - OZPs
      *
      *   Rev 2.0   May 9 2019 13:40:26 T13AC23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3657                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3657                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         09.05.2019 / 13:40:26 / T13AC23
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
      *    FÜR DIE FUNKTION DRKI3657
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3657.
           05  PI-DRKI3657-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3657-LRWVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3657-LRWVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI3657.
           05  PO-DRKI3657-RC                     PIC  X(003).
               88 PO-DRKI3657-RC-OK               VALUE 'OK '.
               88 PO-DRKI3657-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3657-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3657-LRWZAHLBEITRAG   PIC S9(9)V9(2) COMP-3.
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************


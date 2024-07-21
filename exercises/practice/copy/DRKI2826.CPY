      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.12.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2826.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2826.CPv  $
      *
      *   Rev 2.2   Apr 16 2014 17:12:24   t13ex19
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.1   Jan 23 2014 15:36:08   i13az40
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.0   Dec 10 2013 16:12:20 I13AZ40
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2826                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2826                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.12.2013 / 16:12:20 / I13AZ40
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
      *    FÜR DIE FUNKTION DRKI2826
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  MIN / MAX <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2826.
           05  PI-DRKI2826-KEY.
ANF   *        %M_PI_KEY%
             10 PI-DRKI2826-KKAVERTRAGSID   PIC  X(0013).
END   *
      *
       01  PO-DRKI2826.
           05  PO-DRKI2826-RC                     PIC  X(003).
               88 PO-DRKI2826-RC-OK               VALUE 'OK '.
               88 PO-DRKI2826-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2826-DATEN.
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2826-KKAPRODUKTLFDNR   PIC S9(4) COMP.
END   *
      *
      ******* ENDE COPY-MEMBER **************************************
